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

/* Programmer:  Quincey Koziol
 *              Tuesday, February  1, 2005
 */
#include "h5test.h"

/*
 * This file needs to access private datatypes from the H5B2 package.
 * This file also needs to access the v2 B-tree testing code.
 */
#define H5B2_FRIEND /*suppress error about including H5B2pkg      */
#define H5B2_TESTING
#include "H5B2pkg.h"

/* Other private headers that this test requires */
#include "H5CXprivate.h" /* API Contexts                         */
#include "H5VLprivate.h" /* Virtual Object Layer                     */

const char *FILENAME[] = {"btree2", "btree2_tmp", NULL};

#define INSERT_SPLIT_ROOT_NREC     63
#define INSERT_SPLIT_ROOT_NREC_REC 64
#define INSERT_MANY                (1000 * 1000)
#define INSERT_MANY_REC            (2700 * 1000)
#define FIND_MANY                  (INSERT_MANY / 100)
#define FIND_MANY_REC              (INSERT_MANY_REC / 100)
#define FIND_NEIGHBOR              2000
#define DELETE_SMALL               20
#define DELETE_MEDIUM              200
#define DELETE_LARGE               2000

/* Testing parameters */
typedef struct bt2_test_param_t {
    hbool_t reopen_btree; /* Whether to re-open the B-tree during the test */
} bt2_test_param_t;

/*-------------------------------------------------------------------------
 * Function:    init_cparam
 *
 * Purpose:    Initialize v2 B-tree creation parameter structure
 *
 * Return:    Success:    0
 *        Failure:    -1
 *
 * Programmer:    Quincey Koziol
 *              Thursday, October 29, 2009
 *
 *-------------------------------------------------------------------------
 */
static int
init_cparam(H5B2_create_t *cparam, H5B2_create_t *cparam2)
{
    /* Wipe out background */
    HDmemset(cparam, 0, sizeof(*cparam));

    /* General parameters */
    cparam->cls           = H5B2_TEST;
    cparam->node_size     = (size_t)512;
    cparam->rrec_size     = (size_t)8;
    cparam->split_percent = 100;
    cparam->merge_percent = 40;

    /* Wipe out background */
    HDmemset(cparam2, 0, sizeof(*cparam2));

    /* General parameters */
    cparam2->cls           = H5B2_TEST2;
    cparam2->node_size     = (size_t)1024;
    cparam2->rrec_size     = (size_t)16;
    cparam2->split_percent = 100;
    cparam2->merge_percent = 40;

    return (0);
} /* init_cparam() */

/*-------------------------------------------------------------------------
 * Function:    create_file
 *
 * Purpose:    Perform common "creation" operations on file
 *
 * Return:    Success:    0
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Thursday, November  5, 2009
 *
 *-------------------------------------------------------------------------
 */
static int
create_file(hid_t *file, H5F_t **f, hid_t fapl)
{
    char filename[1024]; /* Filename to use */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if ((*file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if (NULL == (*f = (H5F_t *)H5VL_object(*file)))
        STACK_ERROR

    /* Ignore metadata tags in the file's cache */
    if (H5AC_ignore_tags(*f) < 0)
        STACK_ERROR

    /* Success */
    return (0);

error:
    return (-1);
} /* end create_file() */

/*-------------------------------------------------------------------------
 * Function:    create_btree
 *
 * Purpose:    Perform common "create" operations on B-tree for testing
 *
 * Return:    Success:    0
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Thursday, November  5, 2009
 *
 *-------------------------------------------------------------------------
 */
static int
create_btree(H5F_t *f, const H5B2_create_t *cparam, H5B2_t **bt2, haddr_t *bt2_addr)
{
    /* Create the v2 B-tree & get its address */
    if (NULL == (*bt2 = H5B2_create(f, cparam, f)))
        FAIL_STACK_ERROR
    if (H5B2_get_addr(*bt2, bt2_addr /*out*/) < 0)
        FAIL_STACK_ERROR
    if (!H5F_addr_defined(*bt2_addr))
        FAIL_STACK_ERROR

    /* Success */
    return (0);

error:
    return (-1);
} /* end create_btree() */

/*-------------------------------------------------------------------------
 * Function:    reopen_file
 *
 * Purpose:    Re-open the file
 *
 * Return:    Success:    0
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Monday, December 28, 2015
 *
 *-------------------------------------------------------------------------
 */
static int
reopen_file(hid_t *file, H5F_t **f, hid_t fapl)
{
    char filename[1024]; /* Filename to use */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if ((*file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if (NULL == (*f = (H5F_t *)H5VL_object(*file)))
        STACK_ERROR

    /* Ignore metadata tags in the file's cache */
    if (H5AC_ignore_tags(*f) < 0)
        STACK_ERROR

    /* Success */
    return (0);

error:
    return (-1);
} /* end create_file() */

/*-------------------------------------------------------------------------
 * Function:    reopen_btree
 *
 * Purpose:    Perform common "re-open" operations on B-tree for testing
 *
 * Return:    Success:    0
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Thursday, November  5, 2009
 *
 *-------------------------------------------------------------------------
 */
static int
reopen_btree(H5F_t *f, H5B2_t **bt2, haddr_t bt2_addr, const bt2_test_param_t *tparam)
{
    /* Check for closing & re-opening the B-tree */
    if (tparam->reopen_btree) {
        /* Close (empty) v2 B-tree */
        if (H5B2_close(*bt2) < 0)
            FAIL_STACK_ERROR

        /* Re-open v2 B-tree */
        if (NULL == (*bt2 = H5B2_open(f, bt2_addr, f)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Success */
    return (0);

error:
    return (-1);
} /* end reopen_btree() */

/*-------------------------------------------------------------------------
 * Function:    check_stats
 *
 * Purpose:    Check statistics about v1 B-tree
 *
 * Return:    Success:    0
 *        Failure:    -1
 *
 * Programmer:    Quincey Koziol
 *              Thursday, November  5, 2009
 *
 *-------------------------------------------------------------------------
 */
static int
check_stats(H5B2_t *bt2, const H5B2_stat_t *expected)
{
    H5B2_stat_t actual; /* Actual stats retrieved about v2 B-tree */

    /* Get current stats */
    if (H5B2_stat_info(bt2, &actual) < 0)
        FAIL_STACK_ERROR
    if (actual.depth != expected->depth)
        TEST_ERROR
    if (actual.nrecords != expected->nrecords)
        TEST_ERROR

    /* Success */
    return (0);

error:
    return (-1);
} /* end check_stats() */

/*-------------------------------------------------------------------------
 * Function:    check_node_depth
 *
 * Purpose:    Check the depth of the node containing a record
 *
 * Return:    Success:    0
 *        Failure:    -1
 *
 * Programmer:    Quincey Koziol
 *              Thursday, November  5, 2009
 *
 *-------------------------------------------------------------------------
 */
static int
check_node_depth(H5B2_t *bt2, void *record, unsigned depth)
{
    int rec_depth; /* Depth of record in B-tree */

    if ((rec_depth = H5B2__get_node_depth_test(bt2, record)) < 0)
        FAIL_STACK_ERROR
    if ((unsigned)rec_depth != depth)
        TEST_ERROR

    /* Success */
    return (0);

error:
    return (-1);
} /* end check_node_depth() */

/*-------------------------------------------------------------------------
 * Function:    check_node_info
 *
 * Purpose:    Check the info of the node containing a record
 *
 * Return:    Success:    0
 *        Failure:    -1
 *
 * Programmer:    Quincey Koziol
 *              Friday, November  6, 2009
 *
 *-------------------------------------------------------------------------
 */
static int
check_node_info(H5B2_t *bt2, hsize_t record, H5B2_node_info_test_t *ninfo)
{
    H5B2_node_info_test_t rec_ninfo; /* Node info for record in B-tree */

    if (H5B2__get_node_info_test(bt2, &record, &rec_ninfo) < 0)
        FAIL_STACK_ERROR
    if (rec_ninfo.depth != ninfo->depth)
        TEST_ERROR
    if (rec_ninfo.nrec != ninfo->nrec)
        TEST_ERROR

    /* Success */
    return (0);

error:
    return (-1);
} /* end check_node_info() */

/*-------------------------------------------------------------------------
 * Function:    iter_cb
 *
 * Purpose:    v2 B-tree iterator callback
 *
 * Return:    Success:    0
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Wednesday, February 16, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
iter_cb(const void *_record, void *_op_data)
{
    const hsize_t *record = (const hsize_t *)_record;
    hsize_t *      idx    = (hsize_t *)_op_data;

    if (*record != *idx)
        return (H5_ITER_ERROR);

    (*idx)++;
    return (H5_ITER_CONT);
} /* end iter_cb() */

/*-------------------------------------------------------------------------
 * Function:    iter_rec_cb
 *
 * Purpose:    v2 B-tree iterator callback for H5B2_test_rec_t records
 *
 * Return:    Success:    0
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Friday, December 25, 2015
 *
 *-------------------------------------------------------------------------
 */
static int
iter_rec_cb(const void *_record, void *_op_data)
{
    const H5B2_test_rec_t *record = (const H5B2_test_rec_t *)_record;
    H5B2_test_rec_t *      idx    = (H5B2_test_rec_t *)_op_data;

    if (record->key != idx->key)
        return (H5_ITER_ERROR);
    if (record->val != idx->val)
        return (H5_ITER_ERROR);

    idx->key++;
    idx->val += 2;
    return (H5_ITER_CONT);
} /* end iter_rec_cb() */

/*-------------------------------------------------------------------------
 * Function:    find_cb
 *
 * Purpose:    v2 B-tree find callback
 *
 * Return:    Success:    TRUE/FALSE
 *        Failure:    FAIL
 *
 * Programmer:    Quincey Koziol
 *              Thursday, February 24, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
find_cb(const void *_record, void *_op_data)
{
    const hsize_t *record = (const hsize_t *)_record;
    hsize_t *      search = (hsize_t *)_op_data;

    if (*record != *search)
        return (FALSE);

    return (TRUE);
} /* end find_cb() */

/*-------------------------------------------------------------------------
 * Function:    find_rec_cb
 *
 * Purpose:    v2 B-tree find callback for H5B2_test_rec_t records
 *
 * Return:    Success:    TRUE/FALSE
 *        Failure:    FAIL
 *
 * Programmer:    Quincey Koziol
 *              Friday, December 25, 2015
 *
 *-------------------------------------------------------------------------
 */
static int
find_rec_cb(const void *_record, void *_op_data)
{
    const H5B2_test_rec_t *record = (const H5B2_test_rec_t *)_record;
    H5B2_test_rec_t *      search = (H5B2_test_rec_t *)_op_data;

    if (record->key != search->key)
        return (FALSE);

    search->val = record->val;
    return (TRUE);
} /* end find_rec_cb() */

/*-------------------------------------------------------------------------
 * Function:    find_dec_cb
 *
 * Purpose:    v2 B-tree find callback for indexing in decreasing order
 *
 * Note:    Currently hard-wired to "insert_lots" test
 *
 * Return:    Success:    0
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Tuesday, November  7, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
find_dec_cb(const void *_record, void *_op_data)
{
    const hsize_t *record = (const hsize_t *)_record;
    hsize_t *      search = (hsize_t *)_op_data;

    if (*record != (INSERT_MANY - (*search + 1)))
        return (-1);

    return (0);
} /* end find_dec_cb() */

/*-------------------------------------------------------------------------
 * Function:    index_rec_cb
 *
 * Purpose:    v2 B-tree index callback for H5B2_test_rec_t records
 *
 * Return:    Success:    TRUE/FALSE
 *        Failure:    FAIL
 *
 * Programmer:    Quincey Koziol
 *              Friday, December 25, 2015
 *
 *-------------------------------------------------------------------------
 */
static int
index_rec_cb(const void *_record, void *_op_data)
{
    const H5B2_test_rec_t *record = (const H5B2_test_rec_t *)_record;
    H5B2_test_rec_t *      search = (H5B2_test_rec_t *)_op_data;

    HDassert(record);
    HDassert(search);

    search->key = record->key;
    search->val = record->val;
    return (TRUE);
} /* end index_rec_cb() */

/*-------------------------------------------------------------------------
 * Function:    neighbor_cb
 *
 * Purpose:    v2 B-tree neighbor callback
 *
 * Return:    Success:    0
 *
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Tuesday, March  8, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
neighbor_cb(const void *_record, void *_op_data)
{
    const hsize_t *record = (const hsize_t *)_record;
    hsize_t *      search = (hsize_t *)_op_data;

    *search = *record;

    return (0);
} /* end neighbor_cb() */

/*-------------------------------------------------------------------------
 * Function:    modify_cb
 *
 * Purpose:    v2 B-tree modify callback
 *
 * Return:    Success:    0
 *
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Friday, March 10, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
modify_cb(void *_record, void *_op_data, hbool_t *changed)
{
    hsize_t *record = (hsize_t *)_record;
    hsize_t *modify = (hsize_t *)_op_data;

    *record  = *modify;
    *changed = TRUE;

    return (0);
} /* end modify_cb() */

/*-------------------------------------------------------------------------
 * Function:    modify_rec_cb
 *
 * Purpose:    v2 B-tree modify callback for H5B2_test_rec_t records
 *
 * Return:    Success:    0
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Friday, December 25, 2015
 *
 *-------------------------------------------------------------------------
 */
static int
modify_rec_cb(void *_record, void *_op_data, hbool_t *changed)
{
    H5B2_test_rec_t *record = (H5B2_test_rec_t *)_record;
    H5B2_test_rec_t *modify = (H5B2_test_rec_t *)_op_data;

    HDassert(record->key == modify->key);
    record->val = modify->val;
    *changed    = TRUE;

    return (0);
} /* end modify_rec_cb() */

/*-------------------------------------------------------------------------
 * Function:    no_modify_cb
 *
 * Purpose:    v2 B-tree modify callback for updates which shouldn't change
 *        the record (ie. inserting not modifying)
 *
 * Return:    Success:    0
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Wednesday, December 23, 2015
 *
 *-------------------------------------------------------------------------
 */
static int
no_modify_cb(void H5_ATTR_UNUSED *_record, void H5_ATTR_UNUSED *_op_data, hbool_t *changed)
{
    *changed = FALSE;

    return (1);
} /* end no_modify_cb() */

/*-------------------------------------------------------------------------
 * Function:    remove_cb
 *
 * Purpose:    v2 B-tree remove callback
 *
 * Return:    Success:    0
 *
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Tuesday, August 8, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
remove_cb(const void *_record, void *_op_data)
{
    const hsize_t *record  = (const hsize_t *)_record;
    hsize_t *      rrecord = (hsize_t *)_op_data;

    *rrecord = *record;

    return (0);
} /* end remove_cb() */

/*-------------------------------------------------------------------------
 * Function:    test_insert_basic
 *
 * Purpose:    Basic tests for the B-tree v2 code
 *
 * Return:    Success:    0
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Thursday, February  3, 2005
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_insert_basic(hid_t fapl, const H5B2_create_t *cparam, const bt2_test_param_t *tparam)
{
    hid_t   file = -1;   /* File ID */
    H5F_t * f    = NULL; /* Internal file object pointer */
    H5B2_t *bt2  = NULL; /* v2 B-tree wrapper */
    haddr_t bt2_addr;    /* Address of B-tree created */
    hsize_t record;      /* Record to insert into tree */
    hsize_t idx;         /* Index within B-tree, for iterator */
    hbool_t found;       /* Whether record was found */
    herr_t  ret;         /* Generic error return value */

    /*
     * Test v2 B-tree creation
     */
    TESTING("B-tree creation");

    /* Create the file for the test */
    if (create_file(&file, &f, fapl) < 0)
        TEST_ERROR

    /* Create the v2 B-tree & get its address */
    if (create_btree(f, cparam, &bt2, &bt2_addr) < 0)
        TEST_ERROR

    PASSED();

    /*
     * Test queries/iterations on empty v2 B-tree
     */
    TESTING("B-tree iteration: empty B-tree");

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Attempt to iterate over a B-tree with no records */
    idx = 0;
    if (H5B2_iterate(bt2, iter_cb, &idx) < 0)
        FAIL_STACK_ERROR
    /* Make certain that the index hasn't changed */
    if (idx != 0)
        TEST_ERROR

    /* Attempt to find record in B-tree with no records */
    idx   = 0;
    found = FALSE;
    if (H5B2_find(bt2, &idx, &found, find_cb, NULL) < 0)
        TEST_ERROR
    if (found)
        TEST_ERROR

    /* Attempt to index record in B-tree with no records */
    idx = 0;
    H5E_BEGIN_TRY
    {
        ret = H5B2_index(bt2, H5_ITER_INC, (hsize_t)0, find_cb, NULL);
    }
    H5E_END_TRY;
    /* Should fail */
    if (ret != FAIL)
        TEST_ERROR

    PASSED();

    /*
     * Test inserting first record into v2 B-tree
     */
    TESTING("B-tree insert: first record");

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    record = 42;
    if (H5B2_insert(bt2, &record) < 0)
        FAIL_STACK_ERROR

    /* Attempt to find non-existant record in B-tree with 1 record */
    /* (Should not be found, but not fail) */
    idx   = 41;
    found = FALSE;
    if (H5B2_find(bt2, &idx, &found, find_cb, &idx) < 0)
        TEST_ERROR
    if (found)
        TEST_ERROR

    /* Try again with NULL 'op' */
    /* (Should not be found, but not fail) */
    found = FALSE;
    if (H5B2_find(bt2, &idx, &found, NULL, NULL) < 0)
        TEST_ERROR
    if (found)
        TEST_ERROR

    /* Attempt to find existant record in B-tree with 1 record */
    idx   = 42;
    found = FALSE;
    if (H5B2_find(bt2, &idx, &found, find_cb, &idx) < 0)
        TEST_ERROR
    if (!found)
        TEST_ERROR

    /* Try again with NULL 'op' */
    found = FALSE;
    if (H5B2_find(bt2, &idx, &found, NULL, NULL) < 0)
        TEST_ERROR
    if (!found)
        TEST_ERROR

    /* Attempt to index non-existant record in B-tree with 1 record */
    idx = 0;
    H5E_BEGIN_TRY
    {
        ret = H5B2_index(bt2, H5_ITER_INC, (hsize_t)1, find_cb, NULL);
    }
    H5E_END_TRY;
    /* Should fail */
    if (ret != FAIL)
        TEST_ERROR

    /* Attempt to index existing record in B-tree with 1 record */
    idx = 42;
    if (H5B2_index(bt2, H5_ITER_INC, (hsize_t)0, find_cb, &idx) < 0)
        TEST_ERROR

    PASSED();

    /*
     * Test inserting more records into v2 B-tree
     */
    TESTING("B-tree insert: several records");

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /*
     * Test inserting second record into v2 B-tree, before all other records
     */
    record = 34;
    if (H5B2_insert(bt2, &record) < 0)
        FAIL_STACK_ERROR

    /*
     * Test inserting third record into v2 B-tree, after all other records
     */
    record = 56;
    if (H5B2_insert(bt2, &record) < 0)
        FAIL_STACK_ERROR

    /*
     * Test inserting fourth record into v2 B-tree, in the middle of other records
     */
    record = 38;
    if (H5B2_insert(bt2, &record) < 0)
        FAIL_STACK_ERROR

    /* Attempt to find non-existant record in level-0 B-tree with several records */
    /* (Should not be found, but not fail) */
    idx   = 41;
    found = FALSE;
    if (H5B2_find(bt2, &idx, &found, find_cb, &idx) < 0)
        TEST_ERROR
    if (found)
        TEST_ERROR

    /* Attempt to find existant record in level-0 B-tree with several record */
    idx   = 56;
    found = FALSE;
    if (H5B2_find(bt2, &idx, &found, find_cb, &idx) < 0)
        TEST_ERROR
    if (!found)
        TEST_ERROR

    /* Attempt to index non-existant record in B-tree with several records */
    idx = 0;
    H5E_BEGIN_TRY
    {
        ret = H5B2_index(bt2, H5_ITER_INC, (hsize_t)4, find_cb, NULL);
    }
    H5E_END_TRY;
    /* Should fail */
    if (ret != FAIL)
        TEST_ERROR

    /* Attempt to index existing record in B-tree with several records */
    idx = 34;
    if (H5B2_index(bt2, H5_ITER_INC, (hsize_t)0, find_cb, &idx) < 0)
        TEST_ERROR
    idx = 38;
    if (H5B2_index(bt2, H5_ITER_INC, (hsize_t)1, find_cb, &idx) < 0)
        TEST_ERROR
    idx = 42;
    if (H5B2_index(bt2, H5_ITER_INC, (hsize_t)2, find_cb, &idx) < 0)
        TEST_ERROR
    idx = 56;
    if (H5B2_index(bt2, H5_ITER_INC, (hsize_t)3, find_cb, &idx) < 0)
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    /* Close the file */
    if (H5Fclose(file) < 0)
        TEST_ERROR

    PASSED();

    /* All tests passed */
    return (0);

error:
    H5E_BEGIN_TRY
    {
        if (bt2)
            H5B2_close(bt2);
        H5Fclose(file);
    }
    H5E_END_TRY;
    return (1);
} /* test_insert_basic() */

/*-------------------------------------------------------------------------
 * Function:    test_insert_split_root
 *
 * Purpose:    Basic tests for the B-tree v2 code.  This test inserts enough
 *              records to split the root node and force the tree to depth 1.
 *              It also continues to add a few more records to each of the
 *              left and right leaf nodes after the split
 *
 * Return:    Success:    0
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Thursday, February  3, 2005
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_insert_split_root(hid_t fapl, const H5B2_create_t *cparam, const bt2_test_param_t *tparam)
{
    hid_t       file = -1;   /* File ID */
    H5F_t *     f    = NULL; /* Internal file object pointer */
    H5B2_t *    bt2  = NULL; /* v2 B-tree wrapper */
    haddr_t     bt2_addr;    /* Address of B-tree created */
    hsize_t     record;      /* Record to insert into tree */
    hsize_t     idx;         /* Index within B-tree, for iterator */
    H5B2_stat_t bt2_stat;    /* Statistics about B-tree created */
    unsigned    u;           /* Local index variable */
    hbool_t     found;       /* Whether record was found */
    herr_t      ret;         /* Generic error return value */

    /*
     * Test inserting enough records into v2 B-tree to split the root node
     */
    TESTING("B-tree insert: split root");

    /* Create the file for the test */
    if (create_file(&file, &f, fapl) < 0)
        TEST_ERROR

    /* Create the v2 B-tree & get its address */
    if (create_btree(f, cparam, &bt2, &bt2_addr) < 0)
        TEST_ERROR

    /* Insert records to fill root leaf node */
    for (u = 0; u < (INSERT_SPLIT_ROOT_NREC - 1); u++) {
        record = u + 2;
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check up on B-tree */
    bt2_stat.depth    = 0;
    bt2_stat.nrecords = (INSERT_SPLIT_ROOT_NREC - 1);
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record = (hsize_t)33;
    if (check_node_depth(bt2, &record, (unsigned)0) < 0)
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Insert record to split root leaf node */
    record = INSERT_SPLIT_ROOT_NREC + 1;
    if (H5B2_insert(bt2, &record) < 0)
        FAIL_STACK_ERROR

    /* Check up on B-tree */
    bt2_stat.depth    = 1;
    bt2_stat.nrecords = INSERT_SPLIT_ROOT_NREC;
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record = (hsize_t)33;
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Insert a couple more records, on the left side of the B-tree */
    record = 0;
    if (H5B2_insert(bt2, &record) < 0)
        FAIL_STACK_ERROR
    record = 1;
    if (H5B2_insert(bt2, &record) < 0)
        FAIL_STACK_ERROR

    /* Check up on B-tree */
    bt2_stat.depth    = 1;
    bt2_stat.nrecords = (INSERT_SPLIT_ROOT_NREC + 2);
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record = (hsize_t)33;
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Iterate over B-tree to check records have been inserted correctly */
    idx = 0;
    if (H5B2_iterate(bt2, iter_cb, &idx) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the index is correct */
    if (idx != (INSERT_SPLIT_ROOT_NREC + 2))
        TEST_ERROR

    /* Attempt to find non-existant record in level-1 B-tree */
    /* (Should not be found, but not fail) */
    idx   = INSERT_SPLIT_ROOT_NREC + 10;
    found = FALSE;
    if (H5B2_find(bt2, &idx, &found, find_cb, &idx) < 0)
        TEST_ERROR
    if (found)
        TEST_ERROR

    /* Attempt to find existant record in root of level-1 B-tree */
    idx   = 33;
    found = FALSE;
    if (H5B2_find(bt2, &idx, &found, find_cb, &idx) < 0)
        FAIL_STACK_ERROR
    if (!found)
        TEST_ERROR

    /* Attempt to find existant record in leaf of level-1 B-tree */
    idx   = 56;
    found = FALSE;
    if (H5B2_find(bt2, &idx, &found, find_cb, &idx) < 0)
        FAIL_STACK_ERROR
    if (!found)
        TEST_ERROR

    /* Attempt to index non-existant record in level-1 B-tree */
    idx = 0;
    H5E_BEGIN_TRY
    {
        ret = H5B2_index(bt2, H5_ITER_INC, (hsize_t)(INSERT_SPLIT_ROOT_NREC + 2), find_cb, NULL);
    }
    H5E_END_TRY;
    /* Should fail */
    if (ret != FAIL)
        TEST_ERROR

    /* Attempt to index existing record in root of level-1 B-tree */
    idx = 33;
    if (H5B2_index(bt2, H5_ITER_INC, (hsize_t)33, find_cb, &idx) < 0)
        FAIL_STACK_ERROR

    /* Attempt to index existing record in left leaf of level-1 B-tree */
    idx = 0;
    if (H5B2_index(bt2, H5_ITER_INC, (hsize_t)0, find_cb, &idx) < 0)
        FAIL_STACK_ERROR

    /* Attempt to index existing record in right leaf of level-1 B-tree */
    idx = 50;
    if (H5B2_index(bt2, H5_ITER_INC, (hsize_t)50, find_cb, &idx) < 0)
        FAIL_STACK_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    if (H5Fclose(file) < 0)
        TEST_ERROR

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (bt2)
            H5B2_close(bt2);
        H5Fclose(file);
    }
    H5E_END_TRY;
    return 1;
} /* test_insert_split_root() */

/*-------------------------------------------------------------------------
 * Function:    test_insert_level1_2leaf_redistrib
 *
 * Purpose:    Basic tests for the B-tree v2 code.  This test inserts enough
 *              records to split the root node and force the tree to depth 1.
 *              It continues to add a more records to the each of the
 *              left and right leaf nodes after the split to force a 2 node
 *              redistribution
 *
 * Return:    Success:    0
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Tuesday, February  8, 2005
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_insert_level1_2leaf_redistrib(hid_t fapl, const H5B2_create_t *cparam, const bt2_test_param_t *tparam)
{
    hid_t       file = -1;   /* File ID */
    H5F_t *     f    = NULL; /* Internal file object pointer */
    H5B2_t *    bt2  = NULL; /* v2 B-tree wrapper */
    haddr_t     bt2_addr;    /* Address of B-tree created */
    hsize_t     record;      /* Record to insert into tree */
    H5B2_stat_t bt2_stat;    /* Statistics about B-tree created */
    unsigned    u;           /* Local index variable */

    /*
     * Test inserting many records into v2 B-tree
     */
    TESTING("B-tree insert: redistribute 2 leaves in level 1 B-tree (l->r)");

    /* Create the file for the test */
    if (create_file(&file, &f, fapl) < 0)
        TEST_ERROR

    /* Create the v2 B-tree & get its address */
    if (create_btree(f, cparam, &bt2, &bt2_addr) < 0)
        TEST_ERROR

    /* Insert enough records to force root to split into 2 leaves */
    for (u = 0; u < INSERT_SPLIT_ROOT_NREC; u++) {
        record = u + (INSERT_SPLIT_ROOT_NREC / 2) + 1;
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check up on B-tree */
    bt2_stat.depth    = 1;
    bt2_stat.nrecords = INSERT_SPLIT_ROOT_NREC;
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record = (hsize_t)INSERT_SPLIT_ROOT_NREC;
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Force redistribution from left node into right node */
    for (u = 0; u < (INSERT_SPLIT_ROOT_NREC / 2) + 1; u++) {
        record = u;
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check up on B-tree */
    bt2_stat.depth    = 1;
    bt2_stat.nrecords = (INSERT_SPLIT_ROOT_NREC + (INSERT_SPLIT_ROOT_NREC / 2) + 1);
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record = (hsize_t)((INSERT_SPLIT_ROOT_NREC / 2) + (INSERT_SPLIT_ROOT_NREC / 4) + 1);
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    PASSED();

    /*
     * Test inserting many records into v2 B-tree
     */
    TESTING("B-tree insert: redistribute 2 leaves in level 1 B-tree (r->l)");

    /* Create the v2 B-tree & get its address */
    if (create_btree(f, cparam, &bt2, &bt2_addr) < 0)
        TEST_ERROR

    /* Insert enough records to force root to split into 2 leaves */
    for (u = 0; u < INSERT_SPLIT_ROOT_NREC; u++) {
        record = u;
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check up on B-tree */
    bt2_stat.depth    = 1;
    bt2_stat.nrecords = INSERT_SPLIT_ROOT_NREC;
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record = (hsize_t)(INSERT_SPLIT_ROOT_NREC / 2);
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Force redistribution from left node into right node */
    for (u = 0; u < (INSERT_SPLIT_ROOT_NREC / 2) + 1; u++) {
        record = u + INSERT_SPLIT_ROOT_NREC;
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check up on B-tree */
    bt2_stat.depth    = 1;
    bt2_stat.nrecords = INSERT_SPLIT_ROOT_NREC + (INSERT_SPLIT_ROOT_NREC / 2) + 1;
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record = (hsize_t)((INSERT_SPLIT_ROOT_NREC / 2) + (INSERT_SPLIT_ROOT_NREC / 4) + 1);
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    /* Close file */
    if (H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (bt2)
            H5B2_close(bt2);
        H5Fclose(file);
    }
    H5E_END_TRY;
    return 1;
} /* test_insert_level1_2leaf_redistrib() */

/*-------------------------------------------------------------------------
 * Function:    test_insert_level1_side_split
 *
 * Purpose:    Basic tests for the B-tree v2 code.  This test inserts enough
 *              records to split the root node and force the tree to depth 1.
 *              It continues to add a more records to the each of the
 *              left and right leaf nodes after the split to force a 2 node
 *              split, adding another node to the B-tree
 *
 * Return:    Success:    0
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Tuesday, February  9, 2005
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_insert_level1_side_split(hid_t fapl, const H5B2_create_t *cparam, const bt2_test_param_t *tparam)
{
    hid_t       file = -1;   /* File ID */
    H5F_t *     f    = NULL; /* Internal file object pointer */
    H5B2_t *    bt2  = NULL; /* v2 B-tree wrapper */
    haddr_t     bt2_addr;    /* Address of B-tree created */
    hsize_t     record;      /* Record to insert into tree */
    H5B2_stat_t bt2_stat;    /* Statistics about B-tree created */
    unsigned    u;           /* Local index variable */

    /*
     * Test inserting many records into v2 B-tree
     */
    TESTING("B-tree insert: split side leaf into 2 leaves in level 1 B-tree (l->r)");

    /* Create the file for the test */
    if (create_file(&file, &f, fapl) < 0)
        TEST_ERROR

    /* Create the v2 B-tree & get its address */
    if (create_btree(f, cparam, &bt2, &bt2_addr) < 0)
        TEST_ERROR

    /* Insert enough records to force root to split into 2 leaves */
    for (u = 0; u < INSERT_SPLIT_ROOT_NREC; u++) {
        record = u + INSERT_SPLIT_ROOT_NREC;
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check up on B-tree */
    bt2_stat.depth    = 1;
    bt2_stat.nrecords = INSERT_SPLIT_ROOT_NREC;
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record = INSERT_SPLIT_ROOT_NREC + (INSERT_SPLIT_ROOT_NREC / 2);
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Force left node to split */
    for (u = 0; u < INSERT_SPLIT_ROOT_NREC; u++) {
        record = u;
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check up on B-tree */
    bt2_stat.depth    = 1;
    bt2_stat.nrecords = 2 * INSERT_SPLIT_ROOT_NREC;
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record = 31;
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record = 63;
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    PASSED();

    /*
     * Test inserting many records into v2 B-tree
     */
    TESTING("B-tree insert: split side leaf into 2 leaves in level 1 B-tree (r->l)");

    /* Create the v2 B-tree & get its address */
    if (create_btree(f, cparam, &bt2, &bt2_addr) < 0)
        TEST_ERROR

    /* Insert enough records to force root to split into 2 leaves */
    for (u = 0; u < INSERT_SPLIT_ROOT_NREC; u++) {
        record = u;
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check up on B-tree */
    bt2_stat.depth    = 1;
    bt2_stat.nrecords = INSERT_SPLIT_ROOT_NREC;
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record = (INSERT_SPLIT_ROOT_NREC / 2);
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Force right node to split */
    for (u = 0; u < INSERT_SPLIT_ROOT_NREC; u++) {
        record = u + INSERT_SPLIT_ROOT_NREC;
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check up on B-tree */
    bt2_stat.depth    = 1;
    bt2_stat.nrecords = 2 * INSERT_SPLIT_ROOT_NREC;
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record = 62;
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record = 94;
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    /* Close file */
    if (H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (bt2)
            H5B2_close(bt2);
        H5Fclose(file);
    }
    H5E_END_TRY;
    return 1;
} /* test_insert_level1_side_split() */

/*-------------------------------------------------------------------------
 * Function:    test_insert_level1_3leaf_redistrib
 *
 * Purpose:    Basic tests for the B-tree v2 code.  This test inserts enough
 *              records to split the root node and force the tree to depth 1.
 *              It continues to add a more records to the each of the
 *              left and right leaf nodes after the split to force a 2 node
 *              split, adding another node to the B-tree, then continues to
 *              add records until a 3 node redistribution occurs
 *
 * Return:    Success:    0
 *
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Thursday, February 10, 2005
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_insert_level1_3leaf_redistrib(hid_t fapl, const H5B2_create_t *cparam, const bt2_test_param_t *tparam)
{
    hid_t       file = -1;   /* File ID */
    H5F_t *     f    = NULL; /* Internal file object pointer */
    H5B2_t *    bt2  = NULL; /* v2 B-tree wrapper */
    haddr_t     bt2_addr;    /* Address of B-tree created */
    hsize_t     record;      /* Record to insert into tree */
    H5B2_stat_t bt2_stat;    /* Statistics about B-tree created */
    hsize_t     idx;         /* Index within B-tree, for iterator */
    unsigned    u;           /* Local index variable */

    /*
     * Test inserting many records into v2 B-tree
     */
    TESTING("B-tree insert: redistribute 3 leaves in level 1 B-tree");

    /* Create the file for the test */
    if (create_file(&file, &f, fapl) < 0)
        TEST_ERROR

    /* Create the v2 B-tree & get its address */
    if (create_btree(f, cparam, &bt2, &bt2_addr) < 0)
        TEST_ERROR

    /* Insert enough records to force root to split into 2 leaves */
    for (u = 0; u < INSERT_SPLIT_ROOT_NREC; u++) {
        record = u + (INSERT_SPLIT_ROOT_NREC + (INSERT_SPLIT_ROOT_NREC / 2) + 1);
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check up on B-tree */
    bt2_stat.depth    = 1;
    bt2_stat.nrecords = INSERT_SPLIT_ROOT_NREC;
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record = (2 * INSERT_SPLIT_ROOT_NREC);
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Force left node to split */
    for (u = 0; u < INSERT_SPLIT_ROOT_NREC; u++) {
        record = u;
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check up on B-tree */
    bt2_stat.depth    = 1;
    bt2_stat.nrecords = 2 * INSERT_SPLIT_ROOT_NREC;
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record = (INSERT_SPLIT_ROOT_NREC / 2);
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record = (INSERT_SPLIT_ROOT_NREC + (INSERT_SPLIT_ROOT_NREC / 2) + 1);
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Insert records to force middle node to redistribute */
    for (u = 0; u < ((INSERT_SPLIT_ROOT_NREC / 2) + 1); u++) {
        record = u + INSERT_SPLIT_ROOT_NREC;
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check up on B-tree */
    bt2_stat.depth    = 1;
    bt2_stat.nrecords = (2 * INSERT_SPLIT_ROOT_NREC) + (INSERT_SPLIT_ROOT_NREC / 2) + 1;
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record = 52;
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record = 105;
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Iterate over B-tree to check records have been inserted correctly */
    idx = 0;
    if (H5B2_iterate(bt2, iter_cb, &idx) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the index is correct */
    if (idx != ((INSERT_SPLIT_ROOT_NREC * 2) + (INSERT_SPLIT_ROOT_NREC / 2) + 1))
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    /* Close file */
    if (H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (bt2)
            H5B2_close(bt2);
        H5Fclose(file);
    }
    H5E_END_TRY;
    return 1;
} /* test_insert_level1_3leaf_redistrib() */

/*-------------------------------------------------------------------------
 * Function:    test_insert_level1_middle_split
 *
 * Purpose:    Basic tests for the B-tree v2 code.  This test inserts enough
 *              records to split the root node and force the tree to depth 1.
 *              It continues to add a more records to the each of the
 *              left and right leaf nodes after the split to force a 2 node
 *              split, adding another node to the B-tree, then continues to
 *              add records until a 3 node split occurs
 *
 * Return:    Success:    0
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Thursday, February 10, 2005
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_insert_level1_middle_split(hid_t fapl, const H5B2_create_t *cparam, const bt2_test_param_t *tparam)
{
    hid_t       file = -1;   /* File ID */
    H5F_t *     f    = NULL; /* Internal file object pointer */
    H5B2_t *    bt2  = NULL; /* v2 B-tree wrapper */
    haddr_t     bt2_addr;    /* Address of B-tree created */
    hsize_t     record;      /* Record to insert into tree */
    H5B2_stat_t bt2_stat;    /* Statistics about B-tree created */
    hsize_t     idx;         /* Index within B-tree, for iterator */
    unsigned    u;           /* Local index variable */

    /*
     * Test inserting many records into v2 B-tree
     */
    TESTING("B-tree insert: split middle leaf into 2 leaves in level 1 B-tree");

    /* Create the file for the test */
    if (create_file(&file, &f, fapl) < 0)
        TEST_ERROR

    /* Create the v2 B-tree & get its address */
    if (create_btree(f, cparam, &bt2, &bt2_addr) < 0)
        TEST_ERROR

    /* Insert enough records to force root to split into 2 leaves */
    for (u = 0; u < INSERT_SPLIT_ROOT_NREC; u++) {
        record = u + (INSERT_SPLIT_ROOT_NREC * 2);
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check up on B-tree */
    bt2_stat.depth    = 1;
    bt2_stat.nrecords = INSERT_SPLIT_ROOT_NREC;
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record = (2 * INSERT_SPLIT_ROOT_NREC) + (INSERT_SPLIT_ROOT_NREC / 2);
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Force split from left node into right node */
    for (u = 0; u < (INSERT_SPLIT_ROOT_NREC * 2); u++) {
        record = u;
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check up on B-tree */
    bt2_stat.depth    = 1;
    bt2_stat.nrecords = 3 * INSERT_SPLIT_ROOT_NREC;
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record = 62;
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record = 94;
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record = 126;
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Iterate over B-tree to check records have been inserted correctly */
    idx = 0;
    if (H5B2_iterate(bt2, iter_cb, &idx) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the index is correct */
    if (idx != (INSERT_SPLIT_ROOT_NREC * 3))
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    /* Close file */
    if (H5Fclose(file) < 0)
        STACK_ERROR

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (bt2)
            H5B2_close(bt2);
        H5Fclose(file);
    }
    H5E_END_TRY;
    return 1;
} /* test_insert_level1_middle_split() */

/*-------------------------------------------------------------------------
 * Function:    test_insert_make_level2
 *
 * Purpose:    Basic tests for the B-tree v2 code.  This test inserts enough
 *              records to make a level 2 B-tree
 *
 * Return:    Success:    0
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Friday, February 11, 2005
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_insert_make_level2(hid_t fapl, const H5B2_create_t *cparam, const bt2_test_param_t *tparam)
{
    hid_t       file = -1;   /* File ID */
    H5F_t *     f    = NULL; /* Internal file object pointer */
    H5B2_t *    bt2  = NULL; /* v2 B-tree wrapper */
    haddr_t     bt2_addr;    /* Address of B-tree created */
    hsize_t     record;      /* Record to insert into tree */
    H5B2_stat_t bt2_stat;    /* Statistics about B-tree created */
    hsize_t     idx;         /* Index within B-tree, for iterator */
    unsigned    u;           /* Local index variable */
    hbool_t     found;       /* Whether record was found */
    herr_t      ret;         /* Generic error return value */

    /*
     * Test inserting many records into v2 B-tree
     */
    TESTING("B-tree insert: make level 2 B-tree");

    /* Create the file for the test */
    if (create_file(&file, &f, fapl) < 0)
        TEST_ERROR

    /* Create the v2 B-tree & get its address */
    if (create_btree(f, cparam, &bt2, &bt2_addr) < 0)
        TEST_ERROR

    /* Insert enough records to force root to split into 2 internal nodes */
    for (u = 0; u < (INSERT_SPLIT_ROOT_NREC * 9); u++) {
        record = u + 2;
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */
    for (; u < ((INSERT_SPLIT_ROOT_NREC * 29) + 1); u++) {
        record = u + 4;
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check up on B-tree */
    bt2_stat.depth    = 2;
    bt2_stat.nrecords = (INSERT_SPLIT_ROOT_NREC * 29) + 1;
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record = 948;
    if (check_node_depth(bt2, &record, (unsigned)2) < 0)
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Add some extra records to left-most leaf */
    record = 0;
    if (H5B2_insert(bt2, &record) < 0)
        FAIL_STACK_ERROR
    record = 1;
    if (H5B2_insert(bt2, &record) < 0)
        FAIL_STACK_ERROR

    /* Add some extra records to middle leaf */
    record = (INSERT_SPLIT_ROOT_NREC * 9) + 2;
    if (H5B2_insert(bt2, &record) < 0)
        FAIL_STACK_ERROR
    record = (INSERT_SPLIT_ROOT_NREC * 9) + 3;
    if (H5B2_insert(bt2, &record) < 0)
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Iterate over B-tree to check records have been inserted correctly */
    idx = 0;
    if (H5B2_iterate(bt2, iter_cb, &idx) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the index is correct */
    if (idx != ((INSERT_SPLIT_ROOT_NREC * 29) + 5))
        TEST_ERROR

    /* Attempt to find non-existant record in level-2 B-tree */
    /* (Should not be found, but not fail) */
    idx   = INSERT_SPLIT_ROOT_NREC * 30;
    found = FALSE;
    if (H5B2_find(bt2, &idx, &found, find_cb, &idx) < 0)
        TEST_ERROR
    if (found)
        TEST_ERROR

    /* Attempt to find existant record in root of level-2 B-tree */
    idx   = 948;
    found = FALSE;
    if (H5B2_find(bt2, &idx, &found, find_cb, &idx) < 0)
        FAIL_STACK_ERROR
    if (!found)
        TEST_ERROR

    /* Check with B-tree */
    record = 948;
    if (check_node_depth(bt2, &record, (unsigned)2) < 0)
        TEST_ERROR

    /* Attempt to find existant record in internal node of level-2 B-tree */
    idx   = 505;
    found = FALSE;
    if (H5B2_find(bt2, &idx, &found, find_cb, &idx) < 0)
        FAIL_STACK_ERROR
    if (!found)
        TEST_ERROR

    /* Check with B-tree */
    record = 505;
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR

    /* Attempt to find existant record in leaf of level-2 B-tree */
    idx   = 555;
    found = FALSE;
    if (H5B2_find(bt2, &idx, &found, find_cb, &idx) < 0)
        FAIL_STACK_ERROR
    if (!found)
        TEST_ERROR

    /* Check with B-tree */
    record = 555;
    if (check_node_depth(bt2, &record, (unsigned)0) < 0)
        TEST_ERROR

    /* Attempt to index non-existant record in level-2 B-tree */
    idx = 0;
    H5E_BEGIN_TRY
    {
        ret = H5B2_index(bt2, H5_ITER_INC, (hsize_t)(INSERT_SPLIT_ROOT_NREC * 30), find_cb, NULL);
    }
    H5E_END_TRY;
    /* Should fail */
    if (ret != FAIL)
        TEST_ERROR

    /* Attempt to index existing record in root of level-2 B-tree */
    idx = 948;
    if (H5B2_index(bt2, H5_ITER_INC, (hsize_t)948, find_cb, &idx) < 0)
        FAIL_STACK_ERROR

    /* Attempt to index existing record in internal node of level-2 B-tree */
    idx = 505;
    if (H5B2_index(bt2, H5_ITER_INC, (hsize_t)505, find_cb, &idx) < 0)
        FAIL_STACK_ERROR

    /* Attempt to index existing record in leaf of level-2 B-tree */
    idx = 555;
    if (H5B2_index(bt2, H5_ITER_INC, (hsize_t)555, find_cb, &idx) < 0)
        FAIL_STACK_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    /* Close file */
    if (H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (bt2)
            H5B2_close(bt2);
        H5Fclose(file);
    }
    H5E_END_TRY;
    return 1;
} /* test_insert_make_level2() */

/*-------------------------------------------------------------------------
 * Function:    test_insert_level2_leaf_redistrib
 *
 * Purpose:    Basic tests for the B-tree v2 code.  This test inserts enough
 *              records to make a level 2 B-tree and then adds enough more
 *              records to force the leaves to redistribute
 *
 * Return:    Success:    0
 *
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Thursday, February 17, 2005
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_insert_level2_leaf_redistrib(hid_t fapl, const H5B2_create_t *cparam, const bt2_test_param_t *tparam)
{
    hid_t       file = -1;   /* File ID */
    H5F_t *     f    = NULL; /* Internal file object pointer */
    H5B2_t *    bt2  = NULL; /* v2 B-tree wrapper */
    haddr_t     bt2_addr;    /* Address of B-tree created */
    hsize_t     record;      /* Record to insert into tree */
    H5B2_stat_t bt2_stat;    /* Statistics about B-tree created */
    hsize_t     idx;         /* Index within B-tree, for iterator */
    unsigned    u;           /* Local index variable */

    /*
     * Test inserting many records into v2 B-tree
     */
    TESTING("B-tree insert: redistrib right-most leaf in level 2 B-tree");

    /* Create the file for the test */
    if (create_file(&file, &f, fapl) < 0)
        TEST_ERROR

    /* Create the v2 B-tree & get its address */
    if (create_btree(f, cparam, &bt2, &bt2_addr) < 0)
        TEST_ERROR

    /* Insert enough records to force root to split into 2 internal nodes */
    /* And fill rightmost leaf */
    for (u = 0; u < (INSERT_SPLIT_ROOT_NREC * 8); u++) {
        record = u + (INSERT_SPLIT_ROOT_NREC / 2) + 1;
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */
    for (; u < ((INSERT_SPLIT_ROOT_NREC * 29) + (INSERT_SPLIT_ROOT_NREC / 2)); u++) {
        record = u + INSERT_SPLIT_ROOT_NREC + 1;
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check up on B-tree */
    bt2_stat.depth    = 2;
    bt2_stat.nrecords = (INSERT_SPLIT_ROOT_NREC * 29) + (INSERT_SPLIT_ROOT_NREC / 2);
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record = 1008;
    if (check_node_depth(bt2, &record, (unsigned)2) < 0)
        TEST_ERROR
    record = 1859;
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record = 1921;
    if (check_node_depth(bt2, &record, (unsigned)0) < 0)
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Insert record to force redistribution of rightmost leaf */
    record = u + INSERT_SPLIT_ROOT_NREC + 1;
    if (H5B2_insert(bt2, &record) < 0)
        FAIL_STACK_ERROR

    /* Check up on B-tree */
    bt2_stat.depth    = 2;
    bt2_stat.nrecords = (INSERT_SPLIT_ROOT_NREC * 29) + (INSERT_SPLIT_ROOT_NREC / 2) + 1;
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record = 1008;
    if (check_node_depth(bt2, &record, (unsigned)2) < 0)
        TEST_ERROR
    record = 1875;
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record = 1922;
    if (check_node_depth(bt2, &record, (unsigned)0) < 0)
        TEST_ERROR

    PASSED();

    TESTING("B-tree insert: redistrib left-most leaf in level 2 B-tree");

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Check up on B-tree */
    bt2_stat.depth    = 2;
    bt2_stat.nrecords = (INSERT_SPLIT_ROOT_NREC * 29) + (INSERT_SPLIT_ROOT_NREC / 2) + 1;
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record = 1008;
    if (check_node_depth(bt2, &record, (unsigned)2) < 0)
        TEST_ERROR
    record = 94;
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record = 32;
    if (check_node_depth(bt2, &record, (unsigned)0) < 0)
        TEST_ERROR

    /* Add more records to left-most leaf, to force a 2->1 split and then a
     *  2 node redistribution on left leaf
     */
    for (u = 0; u < (INSERT_SPLIT_ROOT_NREC / 2) + 1; u++) {
        record = u;
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check up on B-tree */
    bt2_stat.depth    = 2;
    bt2_stat.nrecords = (INSERT_SPLIT_ROOT_NREC * 30) + 1;
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record = 1008;
    if (check_node_depth(bt2, &record, (unsigned)2) < 0)
        TEST_ERROR
    record = 47;
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record = 0;
    if (check_node_depth(bt2, &record, (unsigned)0) < 0)
        TEST_ERROR

    PASSED();

    TESTING("B-tree insert: redistrib middle leaf in level 2 B-tree");

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Check up on B-tree */
    bt2_stat.depth    = 2;
    bt2_stat.nrecords = (INSERT_SPLIT_ROOT_NREC * 30) + 1;
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record = 1008;
    if (check_node_depth(bt2, &record, (unsigned)2) < 0) /* Record in root node */
        TEST_ERROR
    record = 535;
    if (check_node_depth(bt2, &record, (unsigned)1) < 0) /* Record in middle node before insertion point */
        TEST_ERROR
    record = 630;
    if (check_node_depth(bt2, &record, (unsigned)1) < 0) /* Record in middle node after insertion point */
        TEST_ERROR
    record = 568;
    if (check_node_depth(bt2, &record, (unsigned)0) < 0) /* Record in leaf node just after insertion point */
        TEST_ERROR

    /* Add more records to middle leaf, to force a split and a 3 node redistribution on middle leaf */
    for (u = 0; u < (INSERT_SPLIT_ROOT_NREC / 2) + 1; u++) {
        record = u + (INSERT_SPLIT_ROOT_NREC * 8) + (INSERT_SPLIT_ROOT_NREC / 2) + 1;
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check up on B-tree */
    bt2_stat.depth    = 2;
    bt2_stat.nrecords = (INSERT_SPLIT_ROOT_NREC * 30) + (INSERT_SPLIT_ROOT_NREC / 2) + 2;
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record = 1008;
    if (check_node_depth(bt2, &record, (unsigned)2) < 0) /* Record in root node */
        TEST_ERROR
    record = 524;
    if (check_node_depth(bt2, &record, (unsigned)1) < 0) /* Record in middle node before insertion point */
        TEST_ERROR
    record = 577;
    if (check_node_depth(bt2, &record, (unsigned)1) < 0) /* Record in middle node after insertion point */
        TEST_ERROR
    record = 568;
    if (check_node_depth(bt2, &record, (unsigned)0) < 0) /* Record in leaf node just after insertion point */
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Iterate over B-tree to check records have been inserted correctly */
    idx = 0;
    if (H5B2_iterate(bt2, iter_cb, &idx) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the index is correct */
    if (idx != ((INSERT_SPLIT_ROOT_NREC * 30) + (INSERT_SPLIT_ROOT_NREC / 2) + 2))
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    /* Close file */
    if (H5Fclose(file) < 0)
        STACK_ERROR

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (bt2)
            H5B2_close(bt2);
        H5Fclose(file);
    }
    H5E_END_TRY;
    return 1;
} /* test_insert_level2_leaf_redistrib() */

/*-------------------------------------------------------------------------
 * Function:    test_insert_level2_leaf_split
 *
 * Purpose:    Basic tests for the B-tree v2 code.  This test inserts enough
 *              records to make a level 2 B-tree and then adds enough more
 *              records to force leaves to split.
 *
 * Return:    Success:    0
 *
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Thursday, February 17, 2005
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_insert_level2_leaf_split(hid_t fapl, const H5B2_create_t *cparam, const bt2_test_param_t *tparam)
{
    hid_t       file = -1;   /* File ID */
    H5F_t *     f    = NULL; /* Internal file object pointer */
    H5B2_t *    bt2  = NULL; /* v2 B-tree wrapper */
    haddr_t     bt2_addr;    /* Address of B-tree created */
    hsize_t     record;      /* Record to insert into tree */
    H5B2_stat_t bt2_stat;    /* Statistics about B-tree created */
    hsize_t     idx;         /* Index within B-tree, for iterator */
    unsigned    u;           /* Local index variable */

    /*
     * Test inserting many records into v2 B-tree
     */
    TESTING("B-tree insert: split right-most leaf in level 2 B-tree");

    /* Create the file for the test */
    if (create_file(&file, &f, fapl) < 0)
        TEST_ERROR

    /* Create the v2 B-tree & get its address */
    if (create_btree(f, cparam, &bt2, &bt2_addr) < 0)
        TEST_ERROR

    /* Insert enough records to force root to split into 2 internal nodes */
    for (u = 0; u < (INSERT_SPLIT_ROOT_NREC * 8); u++) {
        record = u + 1;
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */
    for (; u < ((INSERT_SPLIT_ROOT_NREC * 29) + (INSERT_SPLIT_ROOT_NREC / 2)); u++) {
        record = u + 2;
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check up on B-tree */
    bt2_stat.depth    = 2;
    bt2_stat.nrecords = (INSERT_SPLIT_ROOT_NREC * 29) + (INSERT_SPLIT_ROOT_NREC / 2);
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record = 946; /* Record in root node */
    if (check_node_depth(bt2, &record, (unsigned)2) < 0)
        TEST_ERROR
    record = 1797; /* Right-most record in right internal node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record = 1859; /* Right-most record in right-most leaf */
    if (check_node_depth(bt2, &record, (unsigned)0) < 0)
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Insert enough records to force right-most leaf to split */
    for (u = 0; u < ((INSERT_SPLIT_ROOT_NREC / 2) + 1); u++) {
        record = u + (INSERT_SPLIT_ROOT_NREC * 29) + (INSERT_SPLIT_ROOT_NREC / 2) + 2;
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check up on B-tree */
    bt2_stat.depth    = 2;
    bt2_stat.nrecords = INSERT_SPLIT_ROOT_NREC * 30;
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record = 946; /* Record in root node */
    if (check_node_depth(bt2, &record, (unsigned)2) < 0)
        TEST_ERROR
    record = 1828; /* Next-to-right-most record in right-most internal node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record = 1860; /* Right-most record in right-most internal node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record = 1891; /* Right-most record in right-most leaf */
    if (check_node_depth(bt2, &record, (unsigned)0) < 0)
        TEST_ERROR

    PASSED();

    TESTING("B-tree insert: split left-most leaf in level 2 B-tree");

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Check up on B-tree */
    bt2_stat.depth    = 2;
    bt2_stat.nrecords = INSERT_SPLIT_ROOT_NREC * 30;
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record = 946; /* Record in root node */
    if (check_node_depth(bt2, &record, (unsigned)2) < 0)
        TEST_ERROR
    record = 63; /* Left-most record in left-most internal node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record = 1; /* Left-most record in left-most leaf */
    if (check_node_depth(bt2, &record, (unsigned)0) < 0)
        TEST_ERROR

    /* Add another record to left-most leaf, to force a 1->2 node split on left leaf */
    record = 0;
    if (H5B2_insert(bt2, &record) < 0)
        FAIL_STACK_ERROR

    /* Check up on B-tree */
    bt2_stat.depth    = 2;
    bt2_stat.nrecords = (INSERT_SPLIT_ROOT_NREC * 30) + 1;
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record = 946; /* Record in root node */
    if (check_node_depth(bt2, &record, (unsigned)2) < 0)
        TEST_ERROR
    record = 63; /* Left-most record in left-most internal node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record = 32; /* Left-most record in left internal node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record = 0; /* Left-most record in left-most leaf */
    if (check_node_depth(bt2, &record, (unsigned)0) < 0)
        TEST_ERROR

    PASSED();

    TESTING("B-tree insert: split middle leaf in level 2 B-tree");

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Check up on B-tree */
    bt2_stat.depth    = 2;
    bt2_stat.nrecords = (INSERT_SPLIT_ROOT_NREC * 30) + 1;
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record = 946; /* Record in root node */
    if (check_node_depth(bt2, &record, (unsigned)2) < 0)
        TEST_ERROR
    record = 504; /* Record in internal node just before insertion point */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record = 568; /* Record in internal node just after insertion point */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record = 506; /* Record in leaf node just after insertion point */
    if (check_node_depth(bt2, &record, (unsigned)0) < 0)
        TEST_ERROR

    /* Add another record to middle leaf, to force a node split on middle leaf */
    record = (INSERT_SPLIT_ROOT_NREC * 8) + 1;
    if (H5B2_insert(bt2, &record) < 0)
        FAIL_STACK_ERROR

    /* Check up on B-tree */
    bt2_stat.depth    = 2;
    bt2_stat.nrecords = (INSERT_SPLIT_ROOT_NREC * 30) + 2;
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record = 946; /* Record in root node */
    if (check_node_depth(bt2, &record, (unsigned)2) < 0)
        TEST_ERROR
    record = 504; /* Left-most record of split in left internal node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record = 537; /* Middle record of split in left internal node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record = 568; /* Right-most record of split in left internal node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record = 506; /* Record in leaf node just after insertion point */
    if (check_node_depth(bt2, &record, (unsigned)0) < 0)
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Iterate over B-tree to check records have been inserted correctly */
    idx = 0;
    if (H5B2_iterate(bt2, iter_cb, &idx) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the index is correct */
    if (idx != ((INSERT_SPLIT_ROOT_NREC * 30) + 2))
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    /* Close file */
    if (H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (bt2)
            H5B2_close(bt2);
        H5Fclose(file);
    }
    H5E_END_TRY;
    return 1;
} /* test_insert_level2_leaf_split() */

/*-------------------------------------------------------------------------
 * Function:    test_insert_level2_2internal_redistrib
 *
 * Purpose:    Basic tests for the B-tree v2 code.  This test inserts enough
 *              records to make a level 2 B-tree and then adds enough more
 *              records to force the left-most and right-most internal nodes to
 *              redistribute.
 *
 * Return:    Success:    0
 *
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Friday, February 18, 2005
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_insert_level2_2internal_redistrib(hid_t fapl, const H5B2_create_t *cparam,
                                       const bt2_test_param_t *tparam)
{
    hid_t       file = -1;   /* File ID */
    H5F_t *     f    = NULL; /* Internal file object pointer */
    H5B2_t *    bt2  = NULL; /* v2 B-tree wrapper */
    haddr_t     bt2_addr;    /* Address of B-tree created */
    hsize_t     record;      /* Record to insert into tree */
    H5B2_stat_t bt2_stat;    /* Statistics about B-tree created */
    hsize_t     idx;         /* Index within B-tree, for iterator */
    unsigned    u;           /* Local index variable */

    /*
     * Test inserting many records into v2 B-tree
     */
    TESTING("B-tree insert: redist. 2 internal (r->l) in level 2 B-tree");

    /* Create the file for the test */
    if (create_file(&file, &f, fapl) < 0)
        TEST_ERROR

    /* Create the v2 B-tree & get its address */
    if (create_btree(f, cparam, &bt2, &bt2_addr) < 0)
        TEST_ERROR

    /* Insert enough records to force root to split into 2 internal nodes */
    /* And fill up right internal node, to just before to redistribute it */
    for (u = 0; u < (INSERT_SPLIT_ROOT_NREC * 44); u++) {
        record = u + (INSERT_SPLIT_ROOT_NREC * 6) - 4;
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check up on B-tree */
    bt2_stat.depth    = 2;
    bt2_stat.nrecords = INSERT_SPLIT_ROOT_NREC * 44;
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record = 1318; /* Record in root node */
    if (check_node_depth(bt2, &record, (unsigned)2) < 0)
        TEST_ERROR
    record = 3114; /* Right-most record in right internal node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record = 3145; /* Right-most record in right leaf node */
    if (check_node_depth(bt2, &record, (unsigned)0) < 0)
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Insert record to redistribute right-most internal node */
    record = u + (INSERT_SPLIT_ROOT_NREC * 6) - 4;
    if (H5B2_insert(bt2, &record) < 0)
        FAIL_STACK_ERROR

    /* Check up on B-tree */
    bt2_stat.depth    = 2;
    bt2_stat.nrecords = INSERT_SPLIT_ROOT_NREC * 44 + 1;
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record = 1822; /* Record in root node */
    if (check_node_depth(bt2, &record, (unsigned)2) < 0)
        TEST_ERROR
    record = 3114; /* Right-most record in right internal node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record = 3146; /* Right-most record in right leaf node */
    if (check_node_depth(bt2, &record, (unsigned)0) < 0)
        TEST_ERROR

    PASSED();

    TESTING("B-tree insert: redist. 2 internal (l->r) in level 2 B-tree");

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Check up on B-tree */
    bt2_stat.depth    = 2;
    bt2_stat.nrecords = INSERT_SPLIT_ROOT_NREC * 44 + 1;
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record = 1822; /* Record in root node */
    if (check_node_depth(bt2, &record, (unsigned)2) < 0)
        TEST_ERROR
    record = 436; /* Left-most record in left internal node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record = 374; /* Left-most record in left leaf node */
    if (check_node_depth(bt2, &record, (unsigned)0) < 0)
        TEST_ERROR

    /* Force left-most internal node to redistribute */
    for (u = 0; u < ((INSERT_SPLIT_ROOT_NREC * 6) - 4); u++) {
        record = u;
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check up on B-tree */
    bt2_stat.depth    = 2;
    bt2_stat.nrecords = (INSERT_SPLIT_ROOT_NREC * 50) - 3;
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record = 1570; /* Record in root node */
    if (check_node_depth(bt2, &record, (unsigned)2) < 0)
        TEST_ERROR
    record = 61; /* Left-most record in left internal node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record = 0; /* Left-most record in left leaf node */
    if (check_node_depth(bt2, &record, (unsigned)0) < 0)
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Iterate over B-tree to check records have been inserted correctly */
    idx = 0;
    if (H5B2_iterate(bt2, iter_cb, &idx) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the index is correct */
    if (idx != ((INSERT_SPLIT_ROOT_NREC * 50) - 3))
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    /* Close file */
    if (H5Fclose(file) < 0)
        TEST_ERROR

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (bt2)
            H5B2_close(bt2);
        H5Fclose(file);
    }
    H5E_END_TRY;
    return 1;
} /* test_insert_level2_2internal_redistrib() */

/*-------------------------------------------------------------------------
 * Function:    test_insert_level2_2internal_split
 *
 * Purpose:    Basic tests for the B-tree v2 code.  This test inserts enough
 *              records to make a level 2 B-tree and then adds enough more
 *              records to force the left-most and right-most internal nodes to
 *              split.
 *
 * Return:    Success:    0
 *
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Friday, February 18, 2005
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_insert_level2_2internal_split(hid_t fapl, const H5B2_create_t *cparam, const bt2_test_param_t *tparam)
{
    hid_t       file = -1;   /* File ID */
    H5F_t *     f    = NULL; /* Internal file object pointer */
    H5B2_t *    bt2  = NULL; /* v2 B-tree wrapper */
    haddr_t     bt2_addr;    /* Address of B-tree created */
    hsize_t     record;      /* Record to insert into tree */
    H5B2_stat_t bt2_stat;    /* Statistics about B-tree created */
    hsize_t     idx;         /* Index within B-tree, for iterator */
    unsigned    u;           /* Local index variable */

    /*
     * Test inserting many records into v2 B-tree
     */
    TESTING("B-tree insert: split side internal node to 2 in level 2 B-tree (r->l)");

    /* Create the file for the test */
    if (create_file(&file, &f, fapl) < 0)
        TEST_ERROR

    /* Create the v2 B-tree & get its address */
    if (create_btree(f, cparam, &bt2, &bt2_addr) < 0)
        TEST_ERROR

    /* Insert enough records to force root to split into 2 internal nodes */
    /* (And fill up two child internal nodes) */
    for (u = 0; u < (INSERT_SPLIT_ROOT_NREC * 59); u++) {
        record = u + (INSERT_SPLIT_ROOT_NREC * 14) - (INSERT_SPLIT_ROOT_NREC / 4) + 3;
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check up on B-tree */
    bt2_stat.depth    = 2;
    bt2_stat.nrecords = INSERT_SPLIT_ROOT_NREC * 59;
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record = 2759; /* Record in root node */
    if (check_node_depth(bt2, &record, (unsigned)2) < 0)
        TEST_ERROR
    record = 4555; /* Right-most record in right internal node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record = 4586; /* Right-most record in right leaf node */
    if (check_node_depth(bt2, &record, (unsigned)0) < 0)
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Insert record to split right-most internal node */
    record = u + (INSERT_SPLIT_ROOT_NREC * 14) - (INSERT_SPLIT_ROOT_NREC / 4) + 3;
    if (H5B2_insert(bt2, &record) < 0)
        FAIL_STACK_ERROR

    /* Check up on B-tree */
    bt2_stat.depth    = 2;
    bt2_stat.nrecords = (INSERT_SPLIT_ROOT_NREC * 59) + 1;
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record = 2759; /* Left record in root node */
    if (check_node_depth(bt2, &record, (unsigned)2) < 0)
        TEST_ERROR
    record = 3704; /* Right record in root node */
    if (check_node_depth(bt2, &record, (unsigned)2) < 0)
        TEST_ERROR
    record = 4555; /* Right-most record in right internal node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record = 4387; /* Right-most record in right leaf node */
    if (check_node_depth(bt2, &record, (unsigned)0) < 0)
        TEST_ERROR

    PASSED();

    TESTING("B-tree insert: split side internal node to 2 in level 2 B-tree (l->2)");

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Check up on B-tree */
    bt2_stat.depth    = 2;
    bt2_stat.nrecords = (INSERT_SPLIT_ROOT_NREC * 59) + 1;
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record = 2759; /* Left record in root node */
    if (check_node_depth(bt2, &record, (unsigned)2) < 0)
        TEST_ERROR
    record = 932; /* Left-most record in left internal node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record = 870; /* Left-most record in left leaf node */
    if (check_node_depth(bt2, &record, (unsigned)0) < 0)
        TEST_ERROR

    /* Force left-most internal node to split */
    for (u = 0; u < ((INSERT_SPLIT_ROOT_NREC * 14) - (INSERT_SPLIT_ROOT_NREC / 4) + 3); u++) {
        record = u;
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check up on B-tree */
    bt2_stat.depth    = 2;
    bt2_stat.nrecords = (INSERT_SPLIT_ROOT_NREC * 73) - (INSERT_SPLIT_ROOT_NREC / 4) + 4;
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record = 870; /* Left record in root node */
    if (check_node_depth(bt2, &record, (unsigned)2) < 0)
        TEST_ERROR
    record = 1814; /* Next-to-left-most record in root node */
    if (check_node_depth(bt2, &record, (unsigned)2) < 0)
        TEST_ERROR
    record = 61; /* Left-most record in left internal node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record = 0; /* Left-most record in left leaf node */
    if (check_node_depth(bt2, &record, (unsigned)0) < 0)
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Iterate over B-tree to check records have been inserted correctly */
    idx = 0;
    if (H5B2_iterate(bt2, iter_cb, &idx) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the index is correct */
    if (idx != ((INSERT_SPLIT_ROOT_NREC * 73) - (INSERT_SPLIT_ROOT_NREC / 4) + 4))
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    /* Close file */
    if (H5Fclose(file) < 0)
        STACK_ERROR

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (bt2)
            H5B2_close(bt2);
        H5Fclose(file);
    }
    H5E_END_TRY;
    return 1;
} /* test_insert_level2_2internal_split() */

/*-------------------------------------------------------------------------
 * Function:    test_insert_level2_3internal_redistrib
 *
 * Purpose:    Basic tests for the B-tree v2 code.  This test inserts enough
 *              records to make a level 2 B-tree and then adds enough more
 *              records to force the left-most and right-most internal nodes to
 *              split and more records to force a 3 node redistribution of the
 *              internal nodes.
 *
 * Return:    Success:    0
 *
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Saturday, February 19, 2005
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_insert_level2_3internal_redistrib(hid_t fapl, const H5B2_create_t *cparam,
                                       const bt2_test_param_t *tparam)
{
    hid_t       file = -1;   /* File ID */
    H5F_t *     f    = NULL; /* Internal file object pointer */
    H5B2_t *    bt2  = NULL; /* v2 B-tree wrapper */
    haddr_t     bt2_addr;    /* Address of B-tree created */
    hsize_t     record;      /* Record to insert into tree */
    H5B2_stat_t bt2_stat;    /* Statistics about B-tree created */
    hsize_t     idx;         /* Index within B-tree, for iterator */
    unsigned    u;           /* Local index variable */

    /*
     * Test inserting many records into v2 B-tree
     */
    TESTING("B-tree insert: redistrib 3 internals in level 2 B-tree");

    /* Create the file for the test */
    if (create_file(&file, &f, fapl) < 0)
        TEST_ERROR

    /* Create the v2 B-tree & get its address */
    if (create_btree(f, cparam, &bt2, &bt2_addr) < 0)
        TEST_ERROR

    /* Insert enough records to force root to split into 3 internal nodes */
    for (u = 0; u < (INSERT_SPLIT_ROOT_NREC * 36); u++) {
        record = u;
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */
    for (; u < ((INSERT_SPLIT_ROOT_NREC * 59) + 1); u++) {
        record = u + (INSERT_SPLIT_ROOT_NREC * 13) + ((3 * INSERT_SPLIT_ROOT_NREC) / 4) + 3;
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check up on B-tree */
    bt2_stat.depth    = 2;
    bt2_stat.nrecords = (INSERT_SPLIT_ROOT_NREC * 59) + 1;
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record = 1889; /* Left record in root node */
    if (check_node_depth(bt2, &record, (unsigned)2) < 0)
        TEST_ERROR
    record = 3703; /* Right record in root node */
    if (check_node_depth(bt2, &record, (unsigned)2) < 0)
        TEST_ERROR
    record = 2267; /* Record to left of insertion point in middle internal node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record = 3199; /* Record to right of insertion point in middle internal node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record = 3137; /* Record just above insertion point in leaf node */
    if (check_node_depth(bt2, &record, (unsigned)0) < 0)
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Insert records to fill up middle internal node */
    for (u = 0; u < ((INSERT_SPLIT_ROOT_NREC * 13) + ((3 * INSERT_SPLIT_ROOT_NREC) / 4) + 2); u++) {
        record = u + (INSERT_SPLIT_ROOT_NREC * 36);
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check up on B-tree */
    bt2_stat.depth    = 2;
    bt2_stat.nrecords = (INSERT_SPLIT_ROOT_NREC * 72) + ((3 * INSERT_SPLIT_ROOT_NREC) / 4) + 3;
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record = 1889; /* Left record in root node */
    if (check_node_depth(bt2, &record, (unsigned)2) < 0)
        TEST_ERROR
    record = 3703; /* Right record in root node */
    if (check_node_depth(bt2, &record, (unsigned)2) < 0)
        TEST_ERROR
    record = 3104; /* Record to left of insertion point in middle internal node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record = 3137; /* Record to right of insertion point in middle internal node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record = 3135; /* Record just above insertion point in leaf node */
    if (check_node_depth(bt2, &record, (unsigned)0) < 0)
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Insert another record, forcing the middle internal node to redistribute */
    record = u + (INSERT_SPLIT_ROOT_NREC * 36);
    if (H5B2_insert(bt2, &record) < 0)
        FAIL_STACK_ERROR

    /* Check up on B-tree */
    bt2_stat.depth    = 2;
    bt2_stat.nrecords = (INSERT_SPLIT_ROOT_NREC * 72) + ((3 * INSERT_SPLIT_ROOT_NREC) / 4) + 4;
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record = 1574; /* Left record in root node */
    if (check_node_depth(bt2, &record, (unsigned)2) < 0)
        TEST_ERROR
    record = 3104; /* Right record in root node */
    if (check_node_depth(bt2, &record, (unsigned)2) < 0)
        TEST_ERROR
#ifdef NONE
    record = 2862; /* Record to left of insertion point in right internal node (now) */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
#endif             /* NONE */
    record = 3137; /* Record to right of insertion point in right internal node (now) */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record = 3135; /* Record just above insertion point in leaf node */
    if (check_node_depth(bt2, &record, (unsigned)0) < 0)
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Iterate over B-tree to check records have been inserted correctly */
    idx = 0;
    if (H5B2_iterate(bt2, iter_cb, &idx) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the index is correct */
    if (idx != ((INSERT_SPLIT_ROOT_NREC * 72) + ((3 * INSERT_SPLIT_ROOT_NREC) / 4) + 4))
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    /* Close file */
    if (H5Fclose(file) < 0)
        STACK_ERROR

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (bt2)
            H5B2_close(bt2);
        H5Fclose(file);
    }
    H5E_END_TRY;
    return 1;
} /* test_insert_level2_3internal_redistrib() */

/*-------------------------------------------------------------------------
 * Function:    test_insert_level2_3internal_split
 *
 * Purpose:    Basic tests for the B-tree v2 code.  This test inserts enough
 *              records to make a level 2 B-tree and then adds enough more
 *              records to force the left-most and right-most internal nodes to
 *              split and more records to force a 3->4 node split of the
 *              internal nodes.
 *
 * Return:    Success:    0
 *
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Saturday, February 19, 2005
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_insert_level2_3internal_split(hid_t fapl, const H5B2_create_t *cparam, const bt2_test_param_t *tparam)
{
    hid_t       file = -1;   /* File ID */
    H5F_t *     f    = NULL; /* Internal file object pointer */
    H5B2_t *    bt2  = NULL; /* v2 B-tree wrapper */
    haddr_t     bt2_addr;    /* Address of B-tree created */
    hsize_t     record;      /* Record to insert into tree */
    H5B2_stat_t bt2_stat;    /* Statistics about B-tree created */
    hsize_t     idx;         /* Index within B-tree, for iterator */
    unsigned    u;           /* Local index variable */

    /*
     * Test inserting many records into v2 B-tree
     */
    TESTING("B-tree insert: split 3 internals to 4 in level 2 B-tree");

    /* Create the file for the test */
    if (create_file(&file, &f, fapl) < 0)
        TEST_ERROR

    /* Create the v2 B-tree & get its address */
    if (create_btree(f, cparam, &bt2, &bt2_addr) < 0)
        TEST_ERROR

    /* Insert enough records to force root to split into 3 internal nodes */
    /* (and fill right internal node) */
    for (u = 0; u < (INSERT_SPLIT_ROOT_NREC * 31); u++) {
        record = u;
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */
    for (; u < (INSERT_SPLIT_ROOT_NREC * 74); u++) {
        record = u + ((INSERT_SPLIT_ROOT_NREC * 13) + ((3 * INSERT_SPLIT_ROOT_NREC) / 4) + 3);
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Check up on B-tree */
    bt2_stat.depth    = 2;
    bt2_stat.nrecords = INSERT_SPLIT_ROOT_NREC * 74;
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record = 1889; /* Left record in root node */
    if (check_node_depth(bt2, &record, (unsigned)2) < 0)
        TEST_ERROR
    record = 3703; /* Right record in root node */
    if (check_node_depth(bt2, &record, (unsigned)2) < 0)
        TEST_ERROR
    record = 1952; /* Record to left of insertion point in middle internal node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record = 2884; /* Record to right of insertion point in middle internal node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record = 2822; /* Record just after insertion point in leaf node */
    if (check_node_depth(bt2, &record, (unsigned)0) < 0)
        TEST_ERROR

    /* Insert records to fill up middle internal node */
    for (u = 0; u < ((INSERT_SPLIT_ROOT_NREC * 13) + ((3 * INSERT_SPLIT_ROOT_NREC) / 4) + 2); u++) {
        record = u + (INSERT_SPLIT_ROOT_NREC * 31);
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check up on B-tree */
    bt2_stat.depth    = 2;
    bt2_stat.nrecords = (INSERT_SPLIT_ROOT_NREC * 87) + ((3 * INSERT_SPLIT_ROOT_NREC) / 4) + 2;
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record = 1889; /* Left record in root node */
    if (check_node_depth(bt2, &record, (unsigned)2) < 0)
        TEST_ERROR
    record = 3703; /* Right record in root node */
    if (check_node_depth(bt2, &record, (unsigned)2) < 0)
        TEST_ERROR
    record = 2789; /* Record to left of insertion point in middle internal node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record = 2822; /* Record to right of insertion point in middle internal node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record = 2823; /* Record just above insertion point in leaf node */
    if (check_node_depth(bt2, &record, (unsigned)0) < 0)
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Insert record to split middle internal node */
    record = u + (INSERT_SPLIT_ROOT_NREC * 31);
    if (H5B2_insert(bt2, &record) < 0)
        FAIL_STACK_ERROR

    /* Check up on B-tree */
    bt2_stat.depth    = 2;
    bt2_stat.nrecords = (INSERT_SPLIT_ROOT_NREC * 87) + ((3 * INSERT_SPLIT_ROOT_NREC) / 4) + 3;
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record = 1889; /* Left record in root node */
    if (check_node_depth(bt2, &record, (unsigned)2) < 0)
        TEST_ERROR
    record = 2789; /* Middle record in root node */
    if (check_node_depth(bt2, &record, (unsigned)2) < 0)
        TEST_ERROR
    record = 3703; /* Right record in root node */
    if (check_node_depth(bt2, &record, (unsigned)2) < 0)
        TEST_ERROR
#ifdef NONE
    record = 3049; /* Record to left of insertion point in middle internal node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
#endif             /* NONE */
    record = 2822; /* Record to right of insertion point in middle internal node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record = 2823; /* Record just above insertion point in leaf node */
    if (check_node_depth(bt2, &record, (unsigned)0) < 0)
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Iterate over B-tree to check records have been inserted correctly */
    idx = 0;
    if (H5B2_iterate(bt2, iter_cb, &idx) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the index is correct */
    if (idx != ((INSERT_SPLIT_ROOT_NREC * 87) + ((3 * INSERT_SPLIT_ROOT_NREC) / 4) + 3))
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    /* Close file */
    if (H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (bt2)
            H5B2_close(bt2);
        H5Fclose(file);
    }
    H5E_END_TRY;
    return 1;
} /* test_insert_level2_3internal_split() */

/*-------------------------------------------------------------------------
 * Function:    test_insert_lots
 *
 * Purpose:    Basic tests for the B-tree v2 code.  This test inserts many
 *              records in random order, enough to make at a level 4 B-tree.
 *
 * Return:    Success:    0
 *
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Saturday, February 19, 2005
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_insert_lots(hid_t fapl, const H5B2_create_t *cparam, const bt2_test_param_t *tparam)
{
    hid_t       file = -1;      /* File ID */
    char        filename[1024]; /* Filename to use */
    H5F_t *     f   = NULL;     /* Internal file object pointer */
    H5B2_t *    bt2 = NULL;     /* v2 B-tree wrapper */
    haddr_t     bt2_addr;       /* Address of B-tree created */
    hsize_t     record;         /* Record to insert into tree */
    hsize_t     idx;            /* Index within B-tree, for iterator */
    time_t      curr_time;      /* Current time, for seeding random number generator */
    hsize_t *   records;        /* Record #'s for random insertion */
    unsigned    u;              /* Local index variable */
    unsigned    swap_idx;       /* Location to swap with when shuffling */
    hsize_t     temp_rec;       /* Temporary record */
    H5B2_stat_t bt2_stat;       /* Statistics about B-tree created */
    hsize_t     nrec;           /* Number of records in B-tree */
    hbool_t     found;          /* Whether record was found */
    herr_t      ret;            /* Generic error return value */

    /* Initialize random number seed */
    curr_time = HDtime(NULL);
#if 0
curr_time=1109170019;
HDfprintf(stderr,"curr_time=%lu\n",(unsigned long)curr_time);
#endif
    HDsrandom((unsigned)curr_time);

    /*
     * Test inserting many records into v2 B-tree
     */
    TESTING("B-tree insert: create random level 4 B-tree");

    /* Allocate space for the records */
    if (NULL == (records = (hsize_t *)HDmalloc(sizeof(hsize_t) * INSERT_MANY)))
        TEST_ERROR

    /* Initialize record #'s */
    for (u = 0; u < INSERT_MANY; u++)
        records[u] = u;

    /* Shuffle record #'s */
    for (u = 0; u < INSERT_MANY; u++) {
        swap_idx          = ((unsigned)HDrandom() % (INSERT_MANY - u)) + u;
        temp_rec          = records[u];
        records[u]        = records[swap_idx];
        records[swap_idx] = temp_rec;
    } /* end for */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if (NULL == (f = (H5F_t *)H5VL_object(file)))
        STACK_ERROR

    /* Ignore metadata tags in the file's cache */
    if (H5AC_ignore_tags(f) < 0)
        STACK_ERROR

    /* Create the v2 B-tree & get its address */
    if (create_btree(f, cparam, &bt2, &bt2_addr) < 0)
        TEST_ERROR

    /* Insert random records */
    for (u = 0; u < INSERT_MANY; u++) {
        record = records[u];
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check up on B-tree */
    bt2_stat.depth    = 4;
    bt2_stat.nrecords = INSERT_MANY;
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    /* Close file */
    if (H5Fclose(file) < 0)
        STACK_ERROR

    /* Re-open the file */
    if ((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if (NULL == (f = (H5F_t *)H5VL_object(file)))
        FAIL_STACK_ERROR

    /* Ignore metadata tags in the file's cache */
    if (H5AC_ignore_tags(f) < 0)
        STACK_ERROR

    /* Re-open v2 B-tree */
    if (NULL == (bt2 = H5B2_open(f, bt2_addr, f)))
        FAIL_STACK_ERROR

    /* Check up on B-tree after re-open */
    bt2_stat.depth    = 4;
    bt2_stat.nrecords = INSERT_MANY;
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR

    /* Iterate over B-tree to check records have been inserted correctly */
    idx = 0;
    if (H5B2_iterate(bt2, iter_cb, &idx) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the index is correct */
    if (idx != INSERT_MANY)
        TEST_ERROR

    /* Attempt to find non-existant record in level-4 B-tree */
    /* (Should not be found, but not fail) */
    idx   = INSERT_MANY * 2;
    found = FALSE;
    if (H5B2_find(bt2, &idx, &found, find_cb, &idx) < 0)
        TEST_ERROR
    if (found)
        TEST_ERROR

    /* Find random records */
    for (u = 0; u < FIND_MANY; u++) {
        /* Pick random record */
        idx = (hsize_t)(HDrandom() % INSERT_MANY);

        /* Attempt to find existant record in root of level-4 B-tree */
        found = FALSE;
        if (H5B2_find(bt2, &idx, &found, find_cb, &idx) < 0)
            FAIL_STACK_ERROR
        if (!found)
            TEST_ERROR
    } /* end for */

    /* Attempt to index non-existant record in level-4 B-tree, in increasing & decreasing order */
    H5E_BEGIN_TRY
    {
        ret = H5B2_index(bt2, H5_ITER_INC, (hsize_t)(INSERT_MANY * 3), find_cb, NULL);
    }
    H5E_END_TRY;
    /* Should fail */
    if (ret != FAIL)
        TEST_ERROR
    H5E_BEGIN_TRY
    {
        ret = H5B2_index(bt2, H5_ITER_DEC, (hsize_t)(INSERT_MANY * 3), find_cb, NULL);
    }
    H5E_END_TRY;
    /* Should fail */
    if (ret != FAIL)
        TEST_ERROR

    /* Find random records */
    for (u = 0; u < FIND_MANY; u++) {
        /* Pick random record */
        idx = (hsize_t)(HDrandom() % INSERT_MANY);

        /* Attempt to find existant record in root of level-4 B-tree */
        /* (in increasing order) */
        if (H5B2_index(bt2, H5_ITER_INC, idx, find_cb, &idx) < 0)
            FAIL_STACK_ERROR

        /* Attempt to find existant record in root of level-4 B-tree */
        /* (in decreasing order) */
        if (H5B2_index(bt2, H5_ITER_DEC, idx, find_dec_cb, &idx) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    PASSED();

    TESTING("B-tree insert: attempt duplicate record in level 4 B-tree");

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    record = INSERT_MANY / 2;
    H5E_BEGIN_TRY
    {
        ret = H5B2_insert(bt2, &record);
    }
    H5E_END_TRY;
    /* Should fail */
    if (ret != FAIL)
        TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(bt2, &nrec) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the # of records is correct */
    if (nrec != INSERT_MANY)
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    /* Close file */
    if (H5Fclose(file) < 0)
        TEST_ERROR

    PASSED();

    HDfree(records);

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (bt2)
            H5B2_close(bt2);
        H5Fclose(file);
    }
    H5E_END_TRY;
    HDfree(records);
    return 1;
} /* test_insert_lots() */

/*-------------------------------------------------------------------------
 * Function:    test_update_basic
 *
 * Purpose:    Basic tests for the v2 B-tree update operation
 *
 * Return:    Success:    0
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Wednesday, December 23, 2015
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_update_basic(hid_t fapl, const H5B2_create_t *cparam, const bt2_test_param_t *tparam)
{
    hid_t           file = -1;   /* File ID */
    H5F_t *         f    = NULL; /* Internal file object pointer */
    H5B2_t *        bt2  = NULL; /* v2 B-tree wrapper */
    haddr_t         bt2_addr;    /* Address of B-tree created */
    H5B2_test_rec_t record;      /* Record to insert into tree */
    H5B2_test_rec_t modify;      /* Modified value */
    H5B2_test_rec_t find;        /* Record to find */
    hbool_t         found;       /* Whether record was found */
    herr_t          ret;         /* Generic error return value */

    /* Create the file for the test */
    if (create_file(&file, &f, fapl) < 0)
        TEST_ERROR

    /* Create the v2 B-tree & get its address */
    if (create_btree(f, cparam, &bt2, &bt2_addr) < 0)
        TEST_ERROR

    /*
     * Test insert record into empty v2 B-tree
     */
    TESTING("B-tree update: inserting first record in empty B-tree");

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    record.key = 42;
    record.val = 72;
    if (H5B2_update(bt2, &record, no_modify_cb, NULL) < 0)
        FAIL_STACK_ERROR

    /* Attempt to find non-existant record in B-tree with 1 record */
    /* (Should not be found, but not fail) */
    find.key = 10;
    find.val = (hsize_t)-1;
    found    = FALSE;
    if (H5B2_find(bt2, &find, &found, find_rec_cb, &find) < 0)
        FAIL_STACK_ERROR
    if (find.val != (hsize_t)-1)
        TEST_ERROR
    if (found)
        TEST_ERROR

    /* Try again with NULL 'op' */
    /* (Should not be found, but not fail) */
    found = FALSE;
    if (H5B2_find(bt2, &find, &found, NULL, NULL) < 0)
        FAIL_STACK_ERROR
    if (find.val != (hsize_t)-1)
        TEST_ERROR
    if (found)
        TEST_ERROR

    /* Attempt to find existant record in B-tree with 1 record */
    find.key = 42;
    found    = FALSE;
    if (H5B2_find(bt2, &find, &found, find_rec_cb, &find) < 0)
        FAIL_STACK_ERROR
    if (find.val != 72)
        TEST_ERROR
    if (!found)
        TEST_ERROR

    /* Try again with NULL 'op' */
    find.val = (hsize_t)-1;
    found    = FALSE;
    if (H5B2_find(bt2, &find, &found, NULL, NULL) < 0)
        FAIL_STACK_ERROR
    if (find.val != (hsize_t)-1)
        TEST_ERROR
    if (!found)
        TEST_ERROR

    /* Attempt to index non-existant record in B-tree with 1 record */
    H5E_BEGIN_TRY
    {
        ret = H5B2_index(bt2, H5_ITER_INC, (hsize_t)1, index_rec_cb, NULL);
    }
    H5E_END_TRY;
    /* Should fail */
    if (ret != FAIL)
        TEST_ERROR

    /* Attempt to index existing record in B-tree with 1 record */
    find.key = (hsize_t)-1;
    if (H5B2_index(bt2, H5_ITER_INC, (hsize_t)0, index_rec_cb, &find) < 0)
        FAIL_STACK_ERROR
    if (find.key != 42)
        TEST_ERROR
    if (find.val != 72)
        TEST_ERROR

    PASSED();

    /*
     * Test update only record into v2 B-tree
     */
    TESTING("B-tree update: update only record in B-tree");

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    modify.key = 42;
    modify.val = 43;
    if (H5B2_update(bt2, &modify, modify_rec_cb, &modify) < 0)
        FAIL_STACK_ERROR

    /* Attempt to find non-existant record in B-tree with 1 record */
    /* (Should not be found, but not fail) */
    find.key = 10;
    find.val = (hsize_t)-1;
    found    = FALSE;
    if (H5B2_find(bt2, &find, &found, find_rec_cb, &find) < 0)
        FAIL_STACK_ERROR
    if (find.val != (hsize_t)-1)
        TEST_ERROR
    if (found)
        TEST_ERROR

    /* Try again with NULL 'op' */
    /* (Should not be found, but not fail) */
    found = FALSE;
    if (H5B2_find(bt2, &find, &found, NULL, NULL) < 0)
        FAIL_STACK_ERROR
    if (find.val != (hsize_t)-1)
        TEST_ERROR
    if (found)
        TEST_ERROR

    /* Attempt to find modified record in B-tree with 1 record */
    find.key = 42;
    found    = FALSE;
    if (H5B2_find(bt2, &find, &found, find_rec_cb, &find) < 0)
        FAIL_STACK_ERROR
    if (find.val != 43)
        TEST_ERROR
    if (!found)
        TEST_ERROR

    /* Try again with NULL 'op' */
    find.val = (hsize_t)-1;
    found    = FALSE;
    if (H5B2_find(bt2, &find, &found, NULL, NULL) < 0)
        FAIL_STACK_ERROR
    if (find.val != (hsize_t)-1)
        TEST_ERROR
    if (!found)
        TEST_ERROR

    /* Attempt to index non-existant record in B-tree with 1 record */
    H5E_BEGIN_TRY
    {
        ret = H5B2_index(bt2, H5_ITER_INC, (hsize_t)1, index_rec_cb, NULL);
    }
    H5E_END_TRY;
    /* Should fail */
    if (ret != FAIL)
        TEST_ERROR

    /* Attempt to index existing record in B-tree with 1 record */
    find.key = (hsize_t)-1;
    find.val = (hsize_t)-1;
    if (H5B2_index(bt2, H5_ITER_INC, (hsize_t)0, index_rec_cb, &find) < 0)
        FAIL_STACK_ERROR
    if (find.key != 42)
        TEST_ERROR
    if (find.val != 43)
        TEST_ERROR

    PASSED();

    /*
     * Test inserting more records into v2 B-tree
     */
    TESTING("B-tree update: insert several records");

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /*
     * Test inserting second record into v2 B-tree, before all other records
     */
    record.key = 34;
    record.val = 11;
    if (H5B2_update(bt2, &record, no_modify_cb, NULL) < 0)
        FAIL_STACK_ERROR

    /*
     * Test inserting third record into v2 B-tree, after all other records
     */
    record.key = 56;
    record.val = 12;
    if (H5B2_update(bt2, &record, no_modify_cb, NULL) < 0)
        FAIL_STACK_ERROR

    /*
     * Test inserting fourth record into v2 B-tree, in the middle of other records
     */
    record.key = 38;
    record.val = 13;
    if (H5B2_update(bt2, &record, no_modify_cb, NULL) < 0)
        FAIL_STACK_ERROR

    /* Attempt to find non-existant record in level-0 B-tree with several records */
    /* (Should not be found, but not fail) */
    find.key = 10;
    find.val = (hsize_t)-1;
    found    = FALSE;
    if (H5B2_find(bt2, &find, &found, find_rec_cb, &find) < 0)
        TEST_ERROR
    if (find.val != (hsize_t)-1)
        TEST_ERROR
    if (found)
        TEST_ERROR

    /* Attempt to find existant record in level-0 B-tree with several records */
    find.key = 56;
    found    = FALSE;
    if (H5B2_find(bt2, &find, &found, find_rec_cb, &find) < 0)
        TEST_ERROR
    if (find.val != 12)
        TEST_ERROR
    if (!found)
        TEST_ERROR

    /* Attempt to index non-existant record in B-tree with several records */
    H5E_BEGIN_TRY
    {
        ret = H5B2_index(bt2, H5_ITER_INC, (hsize_t)4, index_rec_cb, NULL);
    }
    H5E_END_TRY;
    /* Should fail */
    if (ret != FAIL)
        TEST_ERROR

    /* Attempt to index existing record in B-tree with several records */
    find.key = (hsize_t)-1;
    find.val = (hsize_t)-1;
    if (H5B2_index(bt2, H5_ITER_INC, (hsize_t)0, index_rec_cb, &find) < 0)
        TEST_ERROR
    if (find.key != 34)
        TEST_ERROR
    if (find.val != 11)
        TEST_ERROR
    find.key = (hsize_t)-1;
    find.val = (hsize_t)-1;
    if (H5B2_index(bt2, H5_ITER_INC, (hsize_t)1, index_rec_cb, &find) < 0)
        TEST_ERROR
    if (find.key != 38)
        TEST_ERROR
    if (find.val != 13)
        TEST_ERROR
    find.key = (hsize_t)-1;
    find.val = (hsize_t)-1;
    if (H5B2_index(bt2, H5_ITER_INC, (hsize_t)2, index_rec_cb, &find) < 0)
        TEST_ERROR
    if (find.key != 42)
        TEST_ERROR
    if (find.val != 43)
        TEST_ERROR
    find.key = (hsize_t)-1;
    find.val = (hsize_t)-1;
    if (H5B2_index(bt2, H5_ITER_INC, (hsize_t)3, index_rec_cb, &find) < 0)
        TEST_ERROR
    if (find.key != 56)
        TEST_ERROR
    if (find.val != 12)
        TEST_ERROR

    PASSED();

    /*
     * Test updating all existing records in v2 B-tree
     */
    TESTING("B-tree update: update several records");

    record.key = 34;
    modify.key = 34;
    modify.val = 21;
    if (H5B2_update(bt2, &record, modify_rec_cb, &modify) < 0)
        FAIL_STACK_ERROR
    record.key = 38;
    modify.key = 38;
    modify.val = 23;
    if (H5B2_update(bt2, &record, modify_rec_cb, &modify) < 0)
        FAIL_STACK_ERROR
    record.key = 42;
    modify.key = 42;
    modify.val = 24;
    if (H5B2_update(bt2, &record, modify_rec_cb, &modify) < 0)
        FAIL_STACK_ERROR
    record.key = 56;
    modify.key = 56;
    modify.val = 22;
    if (H5B2_update(bt2, &record, modify_rec_cb, &modify) < 0)
        FAIL_STACK_ERROR

    /* Attempt to find non-existant record in level-0 B-tree with several records */
    /* (Should not be found, but not fail) */
    find.key = 41;
    find.val = (hsize_t)-1;
    found    = FALSE;
    if (H5B2_find(bt2, &find, &found, find_rec_cb, &find) < 0)
        TEST_ERROR
    if (find.val != (hsize_t)-1)
        TEST_ERROR
    if (found)
        TEST_ERROR

    /* Attempt to find existant record in level-0 B-tree with several record */
    find.key = 56;
    found    = FALSE;
    if (H5B2_find(bt2, &find, &found, find_rec_cb, &find) < 0)
        TEST_ERROR
    if (find.val != 22)
        TEST_ERROR
    if (!found)
        TEST_ERROR

    /* Attempt to index non-existant record in B-tree with several records */
    H5E_BEGIN_TRY
    {
        ret = H5B2_index(bt2, H5_ITER_INC, (hsize_t)4, index_rec_cb, NULL);
    }
    H5E_END_TRY;
    /* Should fail */
    if (ret != FAIL)
        TEST_ERROR

    /* Attempt to index existing record in B-tree with several records */
    find.key = (hsize_t)-1;
    find.val = (hsize_t)-1;
    if (H5B2_index(bt2, H5_ITER_INC, (hsize_t)0, index_rec_cb, &find) < 0)
        TEST_ERROR
    if (find.key != 34)
        TEST_ERROR
    if (find.val != 21)
        TEST_ERROR
    find.key = (hsize_t)-1;
    find.val = (hsize_t)-1;
    if (H5B2_index(bt2, H5_ITER_INC, (hsize_t)1, index_rec_cb, &find) < 0)
        TEST_ERROR
    if (find.key != 38)
        TEST_ERROR
    if (find.val != 23)
        TEST_ERROR
    find.key = (hsize_t)-1;
    find.val = (hsize_t)-1;
    if (H5B2_index(bt2, H5_ITER_INC, (hsize_t)2, index_rec_cb, &find) < 0)
        TEST_ERROR
    if (find.key != 42)
        TEST_ERROR
    if (find.val != 24)
        TEST_ERROR
    find.key = (hsize_t)-1;
    find.val = (hsize_t)-1;
    if (H5B2_index(bt2, H5_ITER_INC, (hsize_t)3, index_rec_cb, &find) < 0)
        TEST_ERROR
    if (find.key != 56)
        TEST_ERROR
    if (find.val != 22)
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    /* Close the file */
    if (H5Fclose(file) < 0)
        TEST_ERROR

    PASSED();

    /* All tests passed */
    return (0);

error:
    H5E_BEGIN_TRY
    {
        if (bt2)
            H5B2_close(bt2);
        H5Fclose(file);
    }
    H5E_END_TRY;
    return (1);
} /* test_update_basic() */

/*-------------------------------------------------------------------------
 * Function:    test_update_split_root
 *
 * Purpose:    Basic tests for the B-tree v2 code.  This test inserts enough
 *              records to split the root node and force the tree to depth 1.
 *              It also continues to add a few more records to each of the
 *              left and right leaf nodes after the split
 *
 * Return:    Success:    0
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Wednesday, December 23, 2015
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_update_split_root(hid_t fapl, const H5B2_create_t *cparam, const bt2_test_param_t *tparam)
{
    hid_t           file = -1;   /* File ID */
    H5F_t *         f    = NULL; /* Internal file object pointer */
    H5B2_t *        bt2  = NULL; /* v2 B-tree wrapper */
    haddr_t         bt2_addr;    /* Address of B-tree created */
    H5B2_test_rec_t record;      /* Record to insert into tree */
    H5B2_test_rec_t modify;      /* Modified value */
    H5B2_test_rec_t find;        /* Record to find */
    H5B2_test_rec_t idx;         /* Index within B-tree, for iterator */
    H5B2_stat_t     bt2_stat;    /* Statistics about B-tree created */
    unsigned        u;           /* Local index variable */
    hbool_t         found;       /* Whether record was found */
    herr_t          ret;         /* Generic error return value */

    /*
     * Test inserting enough records into v2 B-tree to split the root node
     */
    TESTING("B-tree update: split root");

    /* Create the file for the test */
    if (create_file(&file, &f, fapl) < 0)
        TEST_ERROR

    /* Create the v2 B-tree & get its address */
    if (create_btree(f, cparam, &bt2, &bt2_addr) < 0)
        TEST_ERROR

    /* Insert records to fill root leaf node */
    for (u = 0; u < (INSERT_SPLIT_ROOT_NREC_REC - 1); u++) {
        record.key = u + 2;
        record.val = u * 2 + 4;
        if (H5B2_update(bt2, &record, no_modify_cb, NULL) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check up on B-tree */
    bt2_stat.depth    = 0;
    bt2_stat.nrecords = (INSERT_SPLIT_ROOT_NREC_REC - 1);
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record.key = 33;
    if (check_node_depth(bt2, &record, (unsigned)0) < 0)
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Insert record to split root leaf node */
    record.key = INSERT_SPLIT_ROOT_NREC_REC + 1;
    record.val = (INSERT_SPLIT_ROOT_NREC_REC - 1) * 2 + 4;
    if (H5B2_update(bt2, &record, no_modify_cb, NULL) < 0)
        FAIL_STACK_ERROR

    /* Check up on B-tree */
    bt2_stat.depth    = 1;
    bt2_stat.nrecords = INSERT_SPLIT_ROOT_NREC_REC;
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record.key = 33;
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR

    /* Iterate over B-tree to check records have been inserted correctly */
    idx.key = 2;
    idx.val = 4;
    if (H5B2_iterate(bt2, iter_rec_cb, &idx) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the index is correct */
    if (idx.key != (INSERT_SPLIT_ROOT_NREC_REC + 2))
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Modify all records */
    for (u = 0; u < INSERT_SPLIT_ROOT_NREC_REC; u++) {
        record.key = u + 2;
        modify.key = u + 2;
        modify.val = u * 2 + 5;
        if (H5B2_update(bt2, &record, modify_rec_cb, &modify) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check up on B-tree */
    bt2_stat.depth    = 1;
    bt2_stat.nrecords = INSERT_SPLIT_ROOT_NREC_REC;
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record.key = 33;
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR

    /* Iterate over B-tree to check records have been inserted correctly */
    idx.key = 2;
    idx.val = 5;
    if (H5B2_iterate(bt2, iter_rec_cb, &idx) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the index is correct */
    if (idx.key != (INSERT_SPLIT_ROOT_NREC_REC + 2))
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Insert a couple more records, on the left side of the B-tree */
    record.key = 0;
    record.val = 1;
    if (H5B2_update(bt2, &record, no_modify_cb, NULL) < 0)
        FAIL_STACK_ERROR
    record.key = 1;
    record.val = 3;
    if (H5B2_update(bt2, &record, no_modify_cb, NULL) < 0)
        FAIL_STACK_ERROR

    /* Check up on B-tree */
    bt2_stat.depth    = 1;
    bt2_stat.nrecords = (INSERT_SPLIT_ROOT_NREC_REC + 2);
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record.key = 33;
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Iterate over B-tree to check records have been inserted correctly */
    idx.key = 0;
    idx.val = 1;
    if (H5B2_iterate(bt2, iter_rec_cb, &idx) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the index is correct */
    if (idx.key != (INSERT_SPLIT_ROOT_NREC_REC + 2))
        TEST_ERROR

    /* Attempt to find non-existant record in level-1 B-tree */
    /* (Should not be found, but not fail) */
    find.key = 800;
    find.val = (hsize_t)-1;
    found    = FALSE;
    if (H5B2_find(bt2, &find, &found, find_rec_cb, &find) < 0)
        TEST_ERROR
    if (find.val != (hsize_t)-1)
        TEST_ERROR
    if (found)
        TEST_ERROR

    /* Attempt to find existant record in root of level-1 B-tree */
    find.key = 33;
    find.val = (hsize_t)-1;
    found    = FALSE;
    if (H5B2_find(bt2, &find, &found, find_rec_cb, &find) < 0)
        FAIL_STACK_ERROR
    if (find.key != 33)
        TEST_ERROR
    if (find.val != 67)
        TEST_ERROR
    if (!found)
        TEST_ERROR

    /* Attempt to find existant record in leaf of level-1 B-tree */
    find.key = 56;
    find.val = (hsize_t)-1;
    found    = FALSE;
    if (H5B2_find(bt2, &find, &found, find_rec_cb, &find) < 0)
        FAIL_STACK_ERROR
    if (find.key != 56)
        TEST_ERROR
    if (find.val != 113)
        TEST_ERROR
    if (!found)
        TEST_ERROR

    /* Attempt to index non-existant record in level-1 B-tree */
    H5E_BEGIN_TRY
    {
        ret = H5B2_index(bt2, H5_ITER_INC, (hsize_t)(INSERT_SPLIT_ROOT_NREC_REC + 2), index_rec_cb, NULL);
    }
    H5E_END_TRY;
    /* Should fail */
    if (ret != FAIL)
        TEST_ERROR

    /* Attempt to index existing record in root of level-1 B-tree */
    find.key = (hsize_t)-1;
    find.val = (hsize_t)-1;
    if (H5B2_index(bt2, H5_ITER_INC, (hsize_t)33, index_rec_cb, &find) < 0)
        FAIL_STACK_ERROR
    if (find.key != 33)
        TEST_ERROR
    if (find.val != 67)
        TEST_ERROR

    /* Attempt to index existing record in left leaf of level-1 B-tree */
    find.key = (hsize_t)-1;
    find.val = (hsize_t)-1;
    if (H5B2_index(bt2, H5_ITER_INC, (hsize_t)0, index_rec_cb, &find) < 0)
        FAIL_STACK_ERROR
    if (find.key != 0)
        TEST_ERROR
    if (find.val != 1)
        TEST_ERROR

    /* Attempt to index existing record in right leaf of level-1 B-tree */
    find.key = (hsize_t)-1;
    find.val = (hsize_t)-1;
    if (H5B2_index(bt2, H5_ITER_INC, (hsize_t)50, index_rec_cb, &find) < 0)
        FAIL_STACK_ERROR
    if (find.key != 50)
        TEST_ERROR
    if (find.val != 101)
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    if (H5Fclose(file) < 0)
        TEST_ERROR

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (bt2)
            H5B2_close(bt2);
        H5Fclose(file);
    }
    H5E_END_TRY;
    return 1;
} /* test_update_split_root() */

/*-------------------------------------------------------------------------
 * Function:    test_update_level1_2leaf_redistrib
 *
 * Purpose:    Basic tests for the B-tree v2 code.  This test inserts enough
 *              records to split the root node and force the tree to depth 1.
 *              It continues to add a more records to the each of the
 *              left and right leaf nodes after the split to force a 2 node
 *              redistribution
 *
 * Return:    Success:    0
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Saturday, December 26, 2015
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_update_level1_2leaf_redistrib(hid_t fapl, const H5B2_create_t *cparam, const bt2_test_param_t *tparam)
{
    hid_t           file = -1;   /* File ID */
    H5F_t *         f    = NULL; /* Internal file object pointer */
    H5B2_t *        bt2  = NULL; /* v2 B-tree wrapper */
    haddr_t         bt2_addr;    /* Address of B-tree created */
    H5B2_test_rec_t record;      /* Record to insert into tree */
    H5B2_stat_t     bt2_stat;    /* Statistics about B-tree created */
    unsigned        u;           /* Local index variable */

    /*
     * Test inserting many records into v2 B-tree
     */
    TESTING("B-tree update: redistribute 2 leaves in level 1 B-tree (l->r)");

    /* Create the file for the test */
    if (create_file(&file, &f, fapl) < 0)
        TEST_ERROR

    /* Create the v2 B-tree & get its address */
    if (create_btree(f, cparam, &bt2, &bt2_addr) < 0)
        TEST_ERROR

    /* Insert enough records to force root to split into 2 leaves */
    for (u = 0; u < INSERT_SPLIT_ROOT_NREC_REC; u++) {
        record.key = u + (INSERT_SPLIT_ROOT_NREC_REC / 2) + 1;
        record.val = u + (INSERT_SPLIT_ROOT_NREC_REC / 2) + 10;
        if (H5B2_update(bt2, &record, no_modify_cb, NULL) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check up on B-tree */
    bt2_stat.depth    = 1;
    bt2_stat.nrecords = INSERT_SPLIT_ROOT_NREC_REC;
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record.key = INSERT_SPLIT_ROOT_NREC_REC;
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Force redistribution from left node into right node */
    for (u = 0; u < (INSERT_SPLIT_ROOT_NREC_REC / 2) + 1; u++) {
        record.key = u;
        record.val = u + 9;
        if (H5B2_update(bt2, &record, no_modify_cb, NULL) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check up on B-tree */
    bt2_stat.depth    = 1;
    bt2_stat.nrecords = (INSERT_SPLIT_ROOT_NREC_REC + (INSERT_SPLIT_ROOT_NREC_REC / 2) + 1);
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record.key = (INSERT_SPLIT_ROOT_NREC_REC / 2) + (INSERT_SPLIT_ROOT_NREC_REC / 4);
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    PASSED();

    /*
     * Test inserting many records into v2 B-tree
     */
    TESTING("B-tree update: redistribute 2 leaves in level 1 B-tree (r->l)");

    /* Create the v2 B-tree & get its address */
    if (create_btree(f, cparam, &bt2, &bt2_addr) < 0)
        TEST_ERROR

    /* Insert enough records to force root to split into 2 leaves */
    for (u = 0; u < INSERT_SPLIT_ROOT_NREC_REC; u++) {
        record.key = u;
        record.val = u + 9;
        if (H5B2_update(bt2, &record, no_modify_cb, NULL) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check up on B-tree */
    bt2_stat.depth    = 1;
    bt2_stat.nrecords = INSERT_SPLIT_ROOT_NREC_REC;
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record.key = (INSERT_SPLIT_ROOT_NREC_REC / 2) - 1;
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Force redistribution from left node into right node */
    for (u = 0; u < (INSERT_SPLIT_ROOT_NREC_REC / 2) + 1; u++) {
        record.key = u + INSERT_SPLIT_ROOT_NREC_REC;
        record.val = u + INSERT_SPLIT_ROOT_NREC_REC + 9;
        if (H5B2_update(bt2, &record, no_modify_cb, NULL) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check up on B-tree */
    bt2_stat.depth    = 1;
    bt2_stat.nrecords = INSERT_SPLIT_ROOT_NREC_REC + (INSERT_SPLIT_ROOT_NREC_REC / 2) + 1;
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record.key = (INSERT_SPLIT_ROOT_NREC_REC / 2) + (INSERT_SPLIT_ROOT_NREC_REC / 4) - 1;
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    /* Close file */
    if (H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (bt2)
            H5B2_close(bt2);
        H5Fclose(file);
    }
    H5E_END_TRY;
    return 1;
} /* test_update_level1_2leaf_redistrib() */

/*-------------------------------------------------------------------------
 * Function:    test_update_level1_side_split
 *
 * Purpose:    Basic tests for the B-tree v2 code.  This test inserts enough
 *              records to split the root node and force the tree to depth 1.
 *              It continues to add a more records to the each of the
 *              left and right leaf nodes after the split to force a 2 node
 *              split, adding another node to the B-tree
 *
 * Return:    Success:    0
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Saturday, December 26, 2015
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_update_level1_side_split(hid_t fapl, const H5B2_create_t *cparam, const bt2_test_param_t *tparam)
{
    hid_t           file = -1;   /* File ID */
    H5F_t *         f    = NULL; /* Internal file object pointer */
    H5B2_t *        bt2  = NULL; /* v2 B-tree wrapper */
    haddr_t         bt2_addr;    /* Address of B-tree created */
    H5B2_test_rec_t record;      /* Record to insert into tree */
    H5B2_stat_t     bt2_stat;    /* Statistics about B-tree created */
    unsigned        u;           /* Local index variable */

    /*
     * Test inserting many records into v2 B-tree
     */
    TESTING("B-tree update: split side leaf into 2 leaves in level 1 B-tree (l->r)");

    /* Create the file for the test */
    if (create_file(&file, &f, fapl) < 0)
        TEST_ERROR

    /* Create the v2 B-tree & get its address */
    if (create_btree(f, cparam, &bt2, &bt2_addr) < 0)
        TEST_ERROR

    /* Insert enough records to force root to split into 2 leaves */
    for (u = 0; u < INSERT_SPLIT_ROOT_NREC_REC; u++) {
        record.key = u + INSERT_SPLIT_ROOT_NREC_REC;
        record.val = u + INSERT_SPLIT_ROOT_NREC_REC + 10;
        if (H5B2_update(bt2, &record, no_modify_cb, NULL) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check up on B-tree */
    bt2_stat.depth    = 1;
    bt2_stat.nrecords = INSERT_SPLIT_ROOT_NREC_REC;
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record.key = INSERT_SPLIT_ROOT_NREC_REC + (INSERT_SPLIT_ROOT_NREC_REC / 2) - 1;
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Force left node to split */
    for (u = 0; u < INSERT_SPLIT_ROOT_NREC_REC; u++) {
        record.key = u;
        record.val = u + 10;
        if (H5B2_update(bt2, &record, no_modify_cb, NULL) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check up on B-tree */
    bt2_stat.depth    = 1;
    bt2_stat.nrecords = 2 * INSERT_SPLIT_ROOT_NREC_REC;
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record.key = 31;
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record.key = 64;
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    PASSED();

    /*
     * Test inserting many records into v2 B-tree
     */
    TESTING("B-tree update: split side leaf into 2 leaves in level 1 B-tree (r->l)");

    /* Create the v2 B-tree & get its address */
    if (create_btree(f, cparam, &bt2, &bt2_addr) < 0)
        TEST_ERROR

    /* Insert enough records to force root to split into 2 leaves */
    for (u = 0; u < INSERT_SPLIT_ROOT_NREC_REC; u++) {
        record.key = u;
        record.val = u + 10;
        if (H5B2_update(bt2, &record, no_modify_cb, NULL) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check up on B-tree */
    bt2_stat.depth    = 1;
    bt2_stat.nrecords = INSERT_SPLIT_ROOT_NREC_REC;
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record.key = (INSERT_SPLIT_ROOT_NREC_REC / 2) - 1;
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Force right node to split */
    for (u = 0; u < INSERT_SPLIT_ROOT_NREC_REC; u++) {
        record.key = u + INSERT_SPLIT_ROOT_NREC_REC;
        record.val = u + INSERT_SPLIT_ROOT_NREC_REC + 10;
        if (H5B2_update(bt2, &record, no_modify_cb, NULL) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check up on B-tree */
    bt2_stat.depth    = 1;
    bt2_stat.nrecords = 2 * INSERT_SPLIT_ROOT_NREC_REC;
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record.key = 63;
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record.key = 95;
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    /* Close file */
    if (H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (bt2)
            H5B2_close(bt2);
        H5Fclose(file);
    }
    H5E_END_TRY;
    return 1;
} /* test_update_level1_side_split() */

/*-------------------------------------------------------------------------
 * Function:    test_update_level1_3leaf_redistrib
 *
 * Purpose:    Basic tests for the B-tree v2 code.  This test inserts enough
 *              records to split the root node and force the tree to depth 1.
 *              It continues to add a more records to the each of the
 *              left and right leaf nodes after the split to force a 2 node
 *              split, adding another node to the B-tree, then continues to
 *              add records until a 3 node redistribution occurs
 *
 * Return:    Success:    0
 *
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Saturday, December 26, 2015
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_update_level1_3leaf_redistrib(hid_t fapl, const H5B2_create_t *cparam, const bt2_test_param_t *tparam)
{
    hid_t           file = -1;   /* File ID */
    H5F_t *         f    = NULL; /* Internal file object pointer */
    H5B2_t *        bt2  = NULL; /* v2 B-tree wrapper */
    haddr_t         bt2_addr;    /* Address of B-tree created */
    H5B2_test_rec_t record;      /* Record to insert into tree */
    H5B2_test_rec_t idx;         /* Index within B-tree, for iterator */
    H5B2_stat_t     bt2_stat;    /* Statistics about B-tree created */
    unsigned        u;           /* Local index variable */

    /*
     * Test inserting many records into v2 B-tree
     */
    TESTING("B-tree update: redistribute 3 leaves in level 1 B-tree");

    /* Create the file for the test */
    if (create_file(&file, &f, fapl) < 0)
        TEST_ERROR

    /* Create the v2 B-tree & get its address */
    if (create_btree(f, cparam, &bt2, &bt2_addr) < 0)
        TEST_ERROR

    /* Insert enough records to force root to split into 2 leaves */
    for (u = 0; u < INSERT_SPLIT_ROOT_NREC_REC; u++) {
        record.key = u + (INSERT_SPLIT_ROOT_NREC_REC + (INSERT_SPLIT_ROOT_NREC_REC / 2) + 1);
        record.val = record.key * 2;
        if (H5B2_update(bt2, &record, no_modify_cb, NULL) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check up on B-tree */
    bt2_stat.depth    = 1;
    bt2_stat.nrecords = INSERT_SPLIT_ROOT_NREC_REC;
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record.key = (2 * INSERT_SPLIT_ROOT_NREC_REC);
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Force left node to split */
    for (u = 0; u < INSERT_SPLIT_ROOT_NREC_REC; u++) {
        record.key = u;
        record.val = record.key * 2;
        if (H5B2_update(bt2, &record, no_modify_cb, NULL) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check up on B-tree */
    bt2_stat.depth    = 1;
    bt2_stat.nrecords = 2 * INSERT_SPLIT_ROOT_NREC_REC;
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record.key = (INSERT_SPLIT_ROOT_NREC_REC / 2) - 1;
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record.key = INSERT_SPLIT_ROOT_NREC_REC + (INSERT_SPLIT_ROOT_NREC_REC / 2) + 1;
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Insert records to force middle node to redistribute */
    for (u = 0; u < ((INSERT_SPLIT_ROOT_NREC_REC / 2) + 1); u++) {
        record.key = u + INSERT_SPLIT_ROOT_NREC_REC;
        record.val = record.key * 2;
        if (H5B2_update(bt2, &record, no_modify_cb, NULL) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check up on B-tree */
    bt2_stat.depth    = 1;
    bt2_stat.nrecords = (2 * INSERT_SPLIT_ROOT_NREC_REC) + (INSERT_SPLIT_ROOT_NREC_REC / 2) + 1;
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record.key = 52;
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record.key = 107;
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Iterate over B-tree to check records have been inserted correctly */
    idx.key = 0;
    idx.val = 0;
    if (H5B2_iterate(bt2, iter_rec_cb, &idx) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the index is correct */
    if (idx.key != ((INSERT_SPLIT_ROOT_NREC_REC * 2) + (INSERT_SPLIT_ROOT_NREC_REC / 2) + 1))
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    /* Close file */
    if (H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (bt2)
            H5B2_close(bt2);
        H5Fclose(file);
    }
    H5E_END_TRY;
    return 1;
} /* test_update_level1_3leaf_redistrib() */

/*-------------------------------------------------------------------------
 * Function:    test_update_level1_middle_split
 *
 * Purpose:    Basic tests for the B-tree v2 code.  This test inserts enough
 *              records to split the root node and force the tree to depth 1.
 *              It continues to add a more records to the each of the
 *              left and right leaf nodes after the split to force a 2 node
 *              split, adding another node to the B-tree, then continues to
 *              add records until a 3 node split occurs
 *
 * Return:    Success:    0
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Saturday, December 26, 2015
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_update_level1_middle_split(hid_t fapl, const H5B2_create_t *cparam, const bt2_test_param_t *tparam)
{
    hid_t           file = -1;   /* File ID */
    H5F_t *         f    = NULL; /* Internal file object pointer */
    H5B2_t *        bt2  = NULL; /* v2 B-tree wrapper */
    haddr_t         bt2_addr;    /* Address of B-tree created */
    H5B2_test_rec_t record;      /* Record to insert into tree */
    H5B2_stat_t     bt2_stat;    /* Statistics about B-tree created */
    H5B2_test_rec_t idx;         /* Index within B-tree, for iterator */
    unsigned        u;           /* Local index variable */

    /*
     * Test inserting many records into v2 B-tree
     */
    TESTING("B-tree update: split middle leaf into 2 leaves in level 1 B-tree");

    /* Create the file for the test */
    if (create_file(&file, &f, fapl) < 0)
        TEST_ERROR

    /* Create the v2 B-tree & get its address */
    if (create_btree(f, cparam, &bt2, &bt2_addr) < 0)
        TEST_ERROR

    /* Insert enough records to force root to split into 2 leaves */
    for (u = 0; u < INSERT_SPLIT_ROOT_NREC_REC; u++) {
        record.key = u + (INSERT_SPLIT_ROOT_NREC_REC * 2);
        record.val = record.key * 2;
        if (H5B2_update(bt2, &record, no_modify_cb, NULL) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check up on B-tree */
    bt2_stat.depth    = 1;
    bt2_stat.nrecords = INSERT_SPLIT_ROOT_NREC_REC;
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record.key = (2 * INSERT_SPLIT_ROOT_NREC_REC) + (INSERT_SPLIT_ROOT_NREC_REC / 2) - 1;
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Force split from left node into right node */
    for (u = 0; u < (INSERT_SPLIT_ROOT_NREC_REC * 2); u++) {
        record.key = u;
        record.val = record.key * 2;
        if (H5B2_update(bt2, &record, no_modify_cb, NULL) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check up on B-tree */
    bt2_stat.depth    = 1;
    bt2_stat.nrecords = 3 * INSERT_SPLIT_ROOT_NREC_REC;
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record.key = 63;
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record.key = 95;
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record.key = 128;
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Iterate over B-tree to check records have been inserted correctly */
    idx.key = 0;
    idx.val = 0;
    if (H5B2_iterate(bt2, iter_rec_cb, &idx) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the index is correct */
    if (idx.key != (INSERT_SPLIT_ROOT_NREC_REC * 3))
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    /* Close file */
    if (H5Fclose(file) < 0)
        STACK_ERROR

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (bt2)
            H5B2_close(bt2);
        H5Fclose(file);
    }
    H5E_END_TRY;
    return 1;
} /* test_update_level1_middle_split() */

/*-------------------------------------------------------------------------
 * Function:    test_update_make_level2
 *
 * Purpose:    Basic tests for the B-tree v2 code.  This test inserts enough
 *              records to make a level 2 B-tree
 *
 * Return:    Success:    0
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Saturday, December 26, 2015
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_update_make_level2(hid_t fapl, const H5B2_create_t *cparam, const bt2_test_param_t *tparam)
{
    hid_t           file = -1;   /* File ID */
    H5F_t *         f    = NULL; /* Internal file object pointer */
    H5B2_t *        bt2  = NULL; /* v2 B-tree wrapper */
    haddr_t         bt2_addr;    /* Address of B-tree created */
    H5B2_test_rec_t record;      /* Record to insert into tree */
    H5B2_test_rec_t find;        /* Record to find */
    H5B2_test_rec_t idx;         /* Index within B-tree, for iterator */
    H5B2_stat_t     bt2_stat;    /* Statistics about B-tree created */
    unsigned        u;           /* Local index variable */
    hbool_t         found;       /* Whether record was found */
    herr_t          ret;         /* Generic error return value */

    /*
     * Test inserting many records into v2 B-tree
     */
    TESTING("B-tree update: make level 2 B-tree (l->r)");

    /* Create the file for the test */
    if (create_file(&file, &f, fapl) < 0)
        TEST_ERROR

    /* Create the v2 B-tree & get its address */
    if (create_btree(f, cparam, &bt2, &bt2_addr) < 0)
        TEST_ERROR

    /* Insert enough records to force root to split into 2 internal nodes */
    for (u = 0; u < (INSERT_SPLIT_ROOT_NREC_REC * 9); u++) {
        record.key = u + 2; /* Leave a gap for later insertion */
        record.val = record.key * 2;
        if (H5B2_update(bt2, &record, no_modify_cb, NULL) < 0)
            FAIL_STACK_ERROR
    } /* end for */
    for (; u < (INSERT_SPLIT_ROOT_NREC_REC * 41); u++) {
        record.key = u + 4; /* Leave a gap for later insertion */
        record.val = record.key * 2;
        if (H5B2_update(bt2, &record, no_modify_cb, NULL) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check up on B-tree */
    bt2_stat.depth    = 2;
    bt2_stat.nrecords = (INSERT_SPLIT_ROOT_NREC_REC * 41);
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record.key = 1347;
    if (check_node_depth(bt2, &record, (unsigned)2) < 0)
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Add some extra records to left-most leaf */
    record.key = 0;
    record.val = record.key * 2;
    if (H5B2_update(bt2, &record, no_modify_cb, NULL) < 0)
        FAIL_STACK_ERROR
    record.key = 1;
    record.val = record.key * 2;
    if (H5B2_update(bt2, &record, no_modify_cb, NULL) < 0)
        FAIL_STACK_ERROR

    /* Add some extra records to middle leaf */
    record.key = (INSERT_SPLIT_ROOT_NREC_REC * 9) + 2;
    record.val = record.key * 2;
    if (H5B2_update(bt2, &record, no_modify_cb, NULL) < 0)
        FAIL_STACK_ERROR
    record.key = (INSERT_SPLIT_ROOT_NREC_REC * 9) + 3;
    record.val = record.key * 2;
    if (H5B2_update(bt2, &record, no_modify_cb, NULL) < 0)
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Iterate over B-tree to check records have been inserted correctly */
    idx.key = 0;
    idx.val = 0;
    if (H5B2_iterate(bt2, iter_rec_cb, &idx) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the index is correct */
    if (idx.key != ((INSERT_SPLIT_ROOT_NREC_REC * 41) + 4))
        TEST_ERROR

    /* Attempt to find non-existant record in level-2 B-tree */
    /* (Should not be found, but not fail) */
    find.key = INSERT_SPLIT_ROOT_NREC_REC * 42;
    find.val = (hsize_t)-1;
    found    = FALSE;
    if (H5B2_find(bt2, &find, &found, find_rec_cb, &find) < 0)
        TEST_ERROR
    if (find.val != (hsize_t)-1)
        TEST_ERROR
    if (found)
        TEST_ERROR

    /* Attempt to find existant record in root of level-2 B-tree */
    find.key = 1347;
    find.val = (hsize_t)-1;
    found    = FALSE;
    if (H5B2_find(bt2, &find, &found, find_rec_cb, &find) < 0)
        FAIL_STACK_ERROR
    if (find.val != (1347 * 2))
        TEST_ERROR
    if (!found)
        TEST_ERROR

    /* Check with B-tree */
    record.key = 1347;
    if (check_node_depth(bt2, &record, (unsigned)2) < 0)
        TEST_ERROR

    /* Attempt to find existant record in internal node of level-2 B-tree */
    find.key = 513;
    find.val = (hsize_t)-1;
    found    = FALSE;
    if (H5B2_find(bt2, &find, &found, find_rec_cb, &find) < 0)
        FAIL_STACK_ERROR
    if (find.val != (513 * 2))
        TEST_ERROR
    if (!found)
        TEST_ERROR

    /* Check with B-tree */
    record.key = 513;
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR

    /* Attempt to find existant record in leaf of level-2 B-tree */
    find.key = 555;
    find.val = (hsize_t)-1;
    found    = FALSE;
    if (H5B2_find(bt2, &find, &found, find_rec_cb, &find) < 0)
        FAIL_STACK_ERROR
    if (find.val != (555 * 2))
        TEST_ERROR
    if (!found)
        TEST_ERROR

    /* Check with B-tree */
    record.key = 555;
    if (check_node_depth(bt2, &record, (unsigned)0) < 0)
        TEST_ERROR

    /* Attempt to index non-existant record in level-2 B-tree */
    H5E_BEGIN_TRY
    {
        ret = H5B2_index(bt2, H5_ITER_INC, (hsize_t)(INSERT_SPLIT_ROOT_NREC_REC * 42), index_rec_cb, NULL);
    }
    H5E_END_TRY;
    /* Should fail */
    if (ret != FAIL)
        TEST_ERROR

    /* Attempt to index existing record in root of level-2 B-tree */
    find.key = (hsize_t)-1;
    find.val = (hsize_t)-1;
    if (H5B2_index(bt2, H5_ITER_INC, (hsize_t)1347, index_rec_cb, &find) < 0)
        FAIL_STACK_ERROR
    if (find.key != 1347)
        TEST_ERROR
    if (find.val != (1347 * 2))
        TEST_ERROR

    /* Attempt to index existing record in internal node of level-2 B-tree */
    find.key = (hsize_t)-1;
    find.val = (hsize_t)-1;
    if (H5B2_index(bt2, H5_ITER_INC, (hsize_t)513, index_rec_cb, &find) < 0)
        FAIL_STACK_ERROR
    if (find.key != 513)
        TEST_ERROR
    if (find.val != (513 * 2))
        TEST_ERROR

    /* Attempt to index existing record in leaf of level-2 B-tree */
    find.key = (hsize_t)-1;
    find.val = (hsize_t)-1;
    if (H5B2_index(bt2, H5_ITER_INC, (hsize_t)555, index_rec_cb, &find) < 0)
        FAIL_STACK_ERROR
    if (find.key != 555)
        TEST_ERROR
    if (find.val != (555 * 2))
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    PASSED();

    /*
     * Test inserting many records into v2 B-tree
     */
    TESTING("B-tree update: make level 2 B-tree (r->l)");

    /* Create the v2 B-tree & get its address */
    if (create_btree(f, cparam, &bt2, &bt2_addr) < 0)
        TEST_ERROR

    /* Insert enough records to force root to split into 2 internal nodes */
    for (u = 0; u < (INSERT_SPLIT_ROOT_NREC_REC * 9); u++) {
        record.key = ((INSERT_SPLIT_ROOT_NREC_REC * 41) + 1) - u;
        record.val = record.key * 2;
        if (H5B2_update(bt2, &record, no_modify_cb, NULL) < 0)
            FAIL_STACK_ERROR
    } /* end for */
    for (; u < (INSERT_SPLIT_ROOT_NREC_REC * 41); u++) {
        record.key = ((INSERT_SPLIT_ROOT_NREC_REC * 41) + 1) - (u + 2); /* Leave a gap for later insertion */
        record.val = record.key * 2;
        if (H5B2_update(bt2, &record, no_modify_cb, NULL) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check up on B-tree */
    bt2_stat.depth    = 2;
    bt2_stat.nrecords = (INSERT_SPLIT_ROOT_NREC_REC * 41);
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record.key = 1344;
    if (check_node_depth(bt2, &record, (unsigned)2) < 0)
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Add some extra records to right-most leaf */
    record.key = (INSERT_SPLIT_ROOT_NREC_REC * 41) + 2;
    record.val = record.key * 2;
    if (H5B2_update(bt2, &record, no_modify_cb, NULL) < 0)
        FAIL_STACK_ERROR
    record.key = (INSERT_SPLIT_ROOT_NREC_REC * 41) + 3;
    record.val = record.key * 2;
    if (H5B2_update(bt2, &record, no_modify_cb, NULL) < 0)
        FAIL_STACK_ERROR

    /* Add some extra records to middle leaf */
    record.key = ((INSERT_SPLIT_ROOT_NREC_REC * 41) - (INSERT_SPLIT_ROOT_NREC_REC * 9));
    record.val = record.key * 2;
    if (H5B2_update(bt2, &record, no_modify_cb, NULL) < 0)
        FAIL_STACK_ERROR
    record.key = ((INSERT_SPLIT_ROOT_NREC_REC * 41) - (INSERT_SPLIT_ROOT_NREC_REC * 9)) + 1;
    record.val = record.key * 2;
    if (H5B2_update(bt2, &record, no_modify_cb, NULL) < 0)
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Iterate over B-tree to check records have been inserted correctly */
    idx.key = 0;
    idx.val = 0;
    if (H5B2_iterate(bt2, iter_rec_cb, &idx) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the index is correct */
    if (idx.key != ((INSERT_SPLIT_ROOT_NREC_REC * 41) + 4))
        TEST_ERROR

    /* Attempt to find non-existant record in level-2 B-tree */
    /* (Should not be found, but not fail) */
    find.key = INSERT_SPLIT_ROOT_NREC_REC * 42;
    find.val = (hsize_t)-1;
    found    = FALSE;
    if (H5B2_find(bt2, &find, &found, find_rec_cb, &find) < 0)
        TEST_ERROR
    if (find.val != (hsize_t)-1)
        TEST_ERROR
    if (found)
        TEST_ERROR

    /* Attempt to find existant record in root of level-2 B-tree */
    find.key = 1344;
    find.val = (hsize_t)-1;
    found    = FALSE;
    if (H5B2_find(bt2, &find, &found, find_rec_cb, &find) < 0)
        FAIL_STACK_ERROR
    if (find.val != (1344 * 2))
        TEST_ERROR
    if (!found)
        TEST_ERROR

    /* Check with B-tree */
    record.key = 1344;
    if (check_node_depth(bt2, &record, (unsigned)2) < 0)
        TEST_ERROR

    /* Attempt to find existant record in internal node of level-2 B-tree */
    find.key = 512;
    find.val = (hsize_t)-1;
    found    = FALSE;
    if (H5B2_find(bt2, &find, &found, find_rec_cb, &find) < 0)
        FAIL_STACK_ERROR
    if (find.val != (512 * 2))
        TEST_ERROR
    if (!found)
        TEST_ERROR

    /* Check with B-tree */
    record.key = 512;
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR

    /* Attempt to find existant record in leaf of level-2 B-tree */
    find.key = 555;
    find.val = (hsize_t)-1;
    found    = FALSE;
    if (H5B2_find(bt2, &find, &found, find_rec_cb, &find) < 0)
        FAIL_STACK_ERROR
    if (find.val != (555 * 2))
        TEST_ERROR
    if (!found)
        TEST_ERROR

    /* Check with B-tree */
    record.key = 555;
    if (check_node_depth(bt2, &record, (unsigned)0) < 0)
        TEST_ERROR

    /* Attempt to index non-existant record in level-2 B-tree */
    H5E_BEGIN_TRY
    {
        ret = H5B2_index(bt2, H5_ITER_INC, (hsize_t)(INSERT_SPLIT_ROOT_NREC_REC * 42), index_rec_cb, NULL);
    }
    H5E_END_TRY;
    /* Should fail */
    if (ret != FAIL)
        TEST_ERROR

    /* Attempt to index existing record in root of level-2 B-tree */
    find.key = (hsize_t)-1;
    find.val = (hsize_t)-1;
    if (H5B2_index(bt2, H5_ITER_INC, (hsize_t)1344, index_rec_cb, &find) < 0)
        FAIL_STACK_ERROR
    if (find.key != 1344)
        TEST_ERROR
    if (find.val != (1344 * 2))
        TEST_ERROR

    /* Attempt to index existing record in internal node of level-2 B-tree */
    find.key = (hsize_t)-1;
    find.val = (hsize_t)-1;
    if (H5B2_index(bt2, H5_ITER_INC, (hsize_t)512, index_rec_cb, &find) < 0)
        FAIL_STACK_ERROR
    if (find.key != 512)
        TEST_ERROR
    if (find.val != (512 * 2))
        TEST_ERROR

    /* Attempt to index existing record in leaf of level-2 B-tree */
    find.key = (hsize_t)-1;
    find.val = (hsize_t)-1;
    if (H5B2_index(bt2, H5_ITER_INC, (hsize_t)555, index_rec_cb, &find) < 0)
        FAIL_STACK_ERROR
    if (find.key != 555)
        TEST_ERROR
    if (find.val != (555 * 2))
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    PASSED();

    /*
     * Test inserting many records into v2 B-tree
     */
    TESTING("B-tree update: make level 2 B-tree (l+r->middle)");

    /* Create the v2 B-tree & get its address */
    if (create_btree(f, cparam, &bt2, &bt2_addr) < 0)
        TEST_ERROR

    /* Insert enough records to force root to split into 2 internal nodes */
    for (u = 0; u < (INSERT_SPLIT_ROOT_NREC_REC * 9); u++) {
        record.key = ((INSERT_SPLIT_ROOT_NREC_REC * 41) + 3) - u;
        record.val = record.key * 2;
        if (H5B2_update(bt2, &record, no_modify_cb, NULL) < 0)
            FAIL_STACK_ERROR
    } /* end for */
    for (; u < (INSERT_SPLIT_ROOT_NREC_REC * 41); u++) {
        record.key = u - ((INSERT_SPLIT_ROOT_NREC_REC * 9) - 2); /* Leave a gap for later insertion */
        record.val = record.key * 2;
        if (H5B2_update(bt2, &record, no_modify_cb, NULL) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check up on B-tree */
    bt2_stat.depth    = 2;
    bt2_stat.nrecords = (INSERT_SPLIT_ROOT_NREC_REC * 41);
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR
    record.key = 1345;
    if (check_node_depth(bt2, &record, (unsigned)2) < 0)
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Add some extra records to right-most leaf */
    record.key = (INSERT_SPLIT_ROOT_NREC_REC * 41) + 4;
    record.val = record.key * 2;
    if (H5B2_update(bt2, &record, no_modify_cb, NULL) < 0)
        FAIL_STACK_ERROR
    record.key = (INSERT_SPLIT_ROOT_NREC_REC * 41) + 5;
    record.val = record.key * 2;
    if (H5B2_update(bt2, &record, no_modify_cb, NULL) < 0)
        FAIL_STACK_ERROR

    /* Add some extra records to middle leaf */
    record.key = ((INSERT_SPLIT_ROOT_NREC_REC * 41) - (INSERT_SPLIT_ROOT_NREC_REC * 9)) + 2;
    record.val = record.key * 2;
    if (H5B2_update(bt2, &record, no_modify_cb, NULL) < 0)
        FAIL_STACK_ERROR
    record.key = ((INSERT_SPLIT_ROOT_NREC_REC * 41) - (INSERT_SPLIT_ROOT_NREC_REC * 9)) + 3;
    record.val = record.key * 2;
    if (H5B2_update(bt2, &record, no_modify_cb, NULL) < 0)
        FAIL_STACK_ERROR

    /* Add some extra records to left-most leaf */
    record.key = 0;
    record.val = record.key * 2;
    if (H5B2_update(bt2, &record, no_modify_cb, NULL) < 0)
        FAIL_STACK_ERROR
    record.key = 1;
    record.val = record.key * 2;
    if (H5B2_update(bt2, &record, no_modify_cb, NULL) < 0)
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Iterate over B-tree to check records have been inserted correctly */
    idx.key = 0;
    idx.val = 0;
    if (H5B2_iterate(bt2, iter_rec_cb, &idx) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the index is correct */
    if (idx.key != ((INSERT_SPLIT_ROOT_NREC_REC * 41) + 6))
        TEST_ERROR

    /* Attempt to find non-existant record in level-2 B-tree */
    /* (Should not be found, but not fail) */
    find.key = INSERT_SPLIT_ROOT_NREC_REC * 42;
    find.val = (hsize_t)-1;
    found    = FALSE;
    if (H5B2_find(bt2, &find, &found, find_rec_cb, &find) < 0)
        TEST_ERROR
    if (find.val != (hsize_t)-1)
        TEST_ERROR
    if (found)
        TEST_ERROR

    /* Attempt to find existant record in root of level-2 B-tree */
    find.key = 1345;
    find.val = (hsize_t)-1;
    found    = FALSE;
    if (H5B2_find(bt2, &find, &found, find_rec_cb, &find) < 0)
        FAIL_STACK_ERROR
    if (find.val != (1345 * 2))
        TEST_ERROR
    if (!found)
        TEST_ERROR

    /* Check with B-tree */
    record.key = 1345;
    if (check_node_depth(bt2, &record, (unsigned)2) < 0)
        TEST_ERROR

    /* Attempt to find existant record in internal node of level-2 B-tree */
    find.key = 513;
    find.val = (hsize_t)-1;
    found    = FALSE;
    if (H5B2_find(bt2, &find, &found, find_rec_cb, &find) < 0)
        FAIL_STACK_ERROR
    if (find.val != (513 * 2))
        TEST_ERROR
    if (!found)
        TEST_ERROR

    /* Check with B-tree */
    record.key = 513;
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR

    /* Attempt to find existant record in leaf of level-2 B-tree */
    find.key = 555;
    find.val = (hsize_t)-1;
    found    = FALSE;
    if (H5B2_find(bt2, &find, &found, find_rec_cb, &find) < 0)
        FAIL_STACK_ERROR
    if (find.val != (555 * 2))
        TEST_ERROR
    if (!found)
        TEST_ERROR

    /* Check with B-tree */
    record.key = 555;
    if (check_node_depth(bt2, &record, (unsigned)0) < 0)
        TEST_ERROR

    /* Attempt to index non-existant record in level-2 B-tree */
    H5E_BEGIN_TRY
    {
        ret = H5B2_index(bt2, H5_ITER_INC, (hsize_t)(INSERT_SPLIT_ROOT_NREC_REC * 42), index_rec_cb, NULL);
    }
    H5E_END_TRY;
    /* Should fail */
    if (ret != FAIL)
        TEST_ERROR

    /* Attempt to index existing record in level-2 B-tree */
    find.key = (hsize_t)-1;
    find.val = (hsize_t)-1;
    if (H5B2_index(bt2, H5_ITER_INC, (hsize_t)1345, index_rec_cb, &find) < 0)
        FAIL_STACK_ERROR
    if (find.key != 1345)
        TEST_ERROR
    if (find.val != (1345 * 2))
        TEST_ERROR

    /* Attempt to index existing record in internal node of level-2 B-tree */
    find.key = (hsize_t)-1;
    find.val = (hsize_t)-1;
    if (H5B2_index(bt2, H5_ITER_INC, (hsize_t)513, index_rec_cb, &find) < 0)
        FAIL_STACK_ERROR
    if (find.key != 513)
        TEST_ERROR
    if (find.val != (513 * 2))
        TEST_ERROR

    /* Attempt to index existing record in leaf of level-2 B-tree */
    find.key = (hsize_t)-1;
    find.val = (hsize_t)-1;
    if (H5B2_index(bt2, H5_ITER_INC, (hsize_t)555, index_rec_cb, &find) < 0)
        FAIL_STACK_ERROR
    if (find.key != 555)
        TEST_ERROR
    if (find.val != (555 * 2))
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    /* Close file */
    if (H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (bt2)
            H5B2_close(bt2);
        H5Fclose(file);
    }
    H5E_END_TRY;
    return 1;
} /* test_update_make_level2() */

/*-------------------------------------------------------------------------
 * Function:    test_update_lots
 *
 * Purpose:    Basic tests for the B-tree v2 code.  This test inserts many
 *              records in random order, enough to make at a level 4 B-tree.
 *
 * Return:    Success:    0
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Sunday, December 27, 2015
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_update_lots(hid_t fapl, const H5B2_create_t *cparam, const bt2_test_param_t *tparam)
{
    hid_t            file = -1;   /* File ID */
    H5F_t *          f    = NULL; /* Internal file object pointer */
    H5B2_t *         bt2  = NULL; /* v2 B-tree wrapper */
    haddr_t          bt2_addr;    /* Address of B-tree created */
    time_t           curr_time;   /* Current time, for seeding random number generator */
    H5B2_test_rec_t *records;     /* Record #'s for random insertion */
    H5B2_test_rec_t  record;      /* Record to insert into tree */
    H5B2_test_rec_t  modify;      /* Modified value */
    H5B2_test_rec_t  find;        /* Record to find */
    H5B2_test_rec_t  iter;        /* Index within B-tree, for iterator */
    H5B2_stat_t      bt2_stat;    /* Statistics about B-tree created */
    hsize_t          nrec;        /* Number of records in B-tree */
    unsigned         u;           /* Local index variable */
    hbool_t          found;       /* Whether record was found */
    herr_t           ret;         /* Generic error return value */

    /* Initialize random number seed */
    curr_time = HDtime(NULL);
#if 0
curr_time = 1451342093;
HDfprintf(stderr, "curr_time = %lu\n", (unsigned long)curr_time);
#endif
    HDsrandom((unsigned)curr_time);

    /*
     * Test inserting many records into v2 B-tree
     */
    TESTING("B-tree update: create random level 4 B-tree");

    /* Allocate space for the records */
    if (NULL == (records = (H5B2_test_rec_t *)HDmalloc(sizeof(H5B2_test_rec_t) * INSERT_MANY_REC)))
        TEST_ERROR

    /* Initialize record #'s */
    for (u = 0; u < INSERT_MANY_REC; u++) {
        records[u].key = u;
        records[u].val = u * 2;
    } /* end for */

    /* Shuffle record #'s */
    for (u = 0; u < INSERT_MANY_REC; u++) {
        H5B2_test_rec_t temp_rec; /* Temporary record */
        unsigned        swap_idx; /* Location to swap with when shuffling */

        swap_idx          = ((unsigned)HDrandom() % (INSERT_MANY_REC - u)) + u;
        temp_rec          = records[u];
        records[u]        = records[swap_idx];
        records[swap_idx] = temp_rec;
    } /* end for */

    /* Create the file for the test */
    if (create_file(&file, &f, fapl) < 0)
        TEST_ERROR

    /* Create the v2 B-tree & get its address */
    if (create_btree(f, cparam, &bt2, &bt2_addr) < 0)
        TEST_ERROR

    /* Insert random records */
    for (u = 0; u < INSERT_MANY_REC; u++) {
        record = records[u];
        if (H5B2_update(bt2, &record, no_modify_cb, NULL) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check up on B-tree */
    bt2_stat.depth    = 4;
    bt2_stat.nrecords = INSERT_MANY_REC;
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    /* Close file */
    if (H5Fclose(file) < 0)
        STACK_ERROR
    file = -1;

    /* Re-open the file for the test */
    if (reopen_file(&file, &f, fapl) < 0)
        TEST_ERROR

    /* Re-open v2 B-tree */
    if (NULL == (bt2 = H5B2_open(f, bt2_addr, f)))
        FAIL_STACK_ERROR

    /* Check up on B-tree after re-open */
    bt2_stat.depth    = 4;
    bt2_stat.nrecords = INSERT_MANY_REC;
    if (check_stats(bt2, &bt2_stat) < 0)
        TEST_ERROR

    /* Iterate over B-tree to check records have been inserted correctly */
    iter.key = 0;
    iter.val = 0;
    if (H5B2_iterate(bt2, iter_rec_cb, &iter) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the index is correct */
    if (iter.key != INSERT_MANY_REC)
        TEST_ERROR

    /* Attempt to find non-existant record in level-4 B-tree */
    /* (Should not be found, but not fail) */
    find.key = INSERT_MANY_REC * 2;
    find.val = (hsize_t)-1;
    found    = FALSE;
    if (H5B2_find(bt2, &find, &found, find_rec_cb, &find) < 0)
        TEST_ERROR
    if (find.val != (hsize_t)-1)
        TEST_ERROR
    if (found)
        TEST_ERROR

    /* Find random records */
    for (u = 0; u < FIND_MANY_REC; u++) {
        /* Pick random record */
        find.key = (hsize_t)(HDrandom() % INSERT_MANY_REC);
        find.val = (hsize_t)-1;

        /* Attempt to find existant record in level-4 B-tree */
        found = FALSE;
        if (H5B2_find(bt2, &find, &found, find_rec_cb, &find) < 0)
            FAIL_STACK_ERROR
        if (find.val != (find.key * 2))
            TEST_ERROR
        if (!found)
            TEST_ERROR
    } /* end for */

    /* Attempt to index non-existant record in level-4 B-tree, in increasing & decreasing order */
    H5E_BEGIN_TRY
    {
        ret = H5B2_index(bt2, H5_ITER_INC, (hsize_t)(INSERT_MANY_REC * 3), find_rec_cb, NULL);
    }
    H5E_END_TRY;
    /* Should fail */
    if (ret != FAIL)
        TEST_ERROR
    H5E_BEGIN_TRY
    {
        ret = H5B2_index(bt2, H5_ITER_DEC, (hsize_t)(INSERT_MANY_REC * 3), find_rec_cb, NULL);
    }
    H5E_END_TRY;
    /* Should fail */
    if (ret != FAIL)
        TEST_ERROR

    /* Find random records */
    for (u = 0; u < FIND_MANY_REC; u++) {
        hsize_t idx; /* Record index */

        /* Pick random record */
        idx = (hsize_t)(HDrandom() % INSERT_MANY_REC);

        /* Reset find record */
        find.key = (hsize_t)-1;
        find.val = (hsize_t)-1;

        /* Attempt to find existant record in level-4 B-tree */
        /* (in increasing order) */
        if (H5B2_index(bt2, H5_ITER_INC, idx, index_rec_cb, &find) < 0)
            FAIL_STACK_ERROR
        if (find.key != idx)
            TEST_ERROR
        if (find.val != (find.key * 2))
            TEST_ERROR

        /* Reset find record */
        find.key = (hsize_t)-1;
        find.val = (hsize_t)-1;

        /* Attempt to find existant record in level-4 B-tree */
        /* (in decreasing order) */
        if (H5B2_index(bt2, H5_ITER_DEC, idx, index_rec_cb, &find) < 0)
            FAIL_STACK_ERROR
        if (find.key != (INSERT_MANY_REC - (idx + 1)))
            TEST_ERROR
        if (find.val != (find.key * 2))
            TEST_ERROR
    } /* end for */

    PASSED();

    TESTING("B-tree update: update record in level 4 B-tree");

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    record.key = INSERT_MANY_REC / 2;
    modify.key = INSERT_MANY_REC / 2;
    modify.val = record.key * 3;
    if (H5B2_update(bt2, &record, modify_rec_cb, &modify) < 0)
        FAIL_STACK_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(bt2, &nrec) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the # of records is correct */
    if (nrec != INSERT_MANY_REC)
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    /* Close file */
    if (H5Fclose(file) < 0)
        TEST_ERROR

    PASSED();

    HDfree(records);

    return 0;

error:
    HDfprintf(stderr, "curr_time = %lu\n", (unsigned long)curr_time);
    H5E_BEGIN_TRY
    {
        if (bt2)
            H5B2_close(bt2);
        H5Fclose(file);
    }
    H5E_END_TRY;
    HDfree(records);

    return 1;
} /* test_update_lots() */

/*-------------------------------------------------------------------------
 * Function:    test_remove_basic
 *
 * Purpose:    Basic tests for the B-tree v2 code
 *
 * Return:    Success:    0
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Friday, February 25, 2005
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_remove_basic(hid_t fapl, const H5B2_create_t *cparam, const bt2_test_param_t *tparam)
{
    hid_t   file = -1;   /* File ID */
    H5F_t * f    = NULL; /* Internal file object pointer */
    H5B2_t *bt2  = NULL; /* v2 B-tree wrapper */
    haddr_t bt2_addr;    /* Address of B-tree created */
    hsize_t record;      /* Record to insert into tree */
    hsize_t rrecord;     /* Record to remove from tree */
    hsize_t nrec;        /* Number of records in B-tree */
    haddr_t root_addr;   /* Address of root of B-tree created */
    herr_t  ret;         /* Generic error return value */

    /* Record removal tests */
    TESTING("B-tree remove: record from empty B-tree");

    /* Create the file for the test */
    if (create_file(&file, &f, fapl) < 0)
        TEST_ERROR

    /* Create the v2 B-tree & get its address */
    if (create_btree(f, cparam, &bt2, &bt2_addr) < 0)
        TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(bt2, &nrec) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the # of records is correct */
    if (nrec != 0)
        TEST_ERROR

    /* Attempt to remove a record from a B-tree with no records */
    record = 0;
    H5E_BEGIN_TRY
    {
        ret = H5B2_remove(bt2, &record, NULL, NULL);
    }
    H5E_END_TRY;
    /* Should fail */
    if (ret != FAIL)
        TEST_ERROR

    PASSED();

    TESTING("B-tree remove: non-existant record from 1 record B-tree");

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Insert one record into B-tree */
    record = 42;
    if (H5B2_insert(bt2, &record) < 0)
        FAIL_STACK_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(bt2, &nrec) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the # of records is correct */
    if (nrec != 1)
        TEST_ERROR

    /* Query the address of the root node in the B-tree */
    if (H5B2__get_root_addr_test(bt2, &root_addr) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the address of the root node is defined */
    if (!H5F_addr_defined(root_addr))
        TEST_ERROR

    /* Attempt to remove a non-existant record from a B-tree with 1 record */
    record = 0;
    H5E_BEGIN_TRY
    {
        ret = H5B2_remove(bt2, &record, NULL, NULL);
    }
    H5E_END_TRY;
    /* Should fail */
    if (ret != FAIL)
        TEST_ERROR

    PASSED();

    /* Attempt to remove a record from a B-tree with 1 record */
    TESTING("B-tree remove: existant record from 1 record B-tree");

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    record  = 42;
    rrecord = 0;
    if (H5B2_remove(bt2, &record, remove_cb, &rrecord) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the record value is correct */
    if (rrecord != 42)
        TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(bt2, &nrec) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the # of records is correct */
    if (nrec != 0)
        TEST_ERROR

    /* Query the address of the root node in the B-tree */
    if (H5B2__get_root_addr_test(bt2, &root_addr) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the root node has been freed */
    if (H5F_addr_defined(root_addr))
        TEST_ERROR

    PASSED();

    /* Attempt to insert records into B-tree which had records removed */
    TESTING("B-tree remove: adding records to B-tree after removal");

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Insert several records into B-tree again */
    record = 42;
    if (H5B2_insert(bt2, &record) < 0)
        FAIL_STACK_ERROR
    record = 34;
    if (H5B2_insert(bt2, &record) < 0)
        FAIL_STACK_ERROR
    record = 56;
    if (H5B2_insert(bt2, &record) < 0)
        FAIL_STACK_ERROR
    record = 38;
    if (H5B2_insert(bt2, &record) < 0)
        FAIL_STACK_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(bt2, &nrec) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the # of records is correct */
    if (nrec != 4)
        TEST_ERROR

    PASSED();

    /* Attempt to remove a non-existant record from a level-0 B-tree with mult. record */
    TESTING("B-tree remove: non-existant record from level-0 B-tree");

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    record = 0;
    H5E_BEGIN_TRY
    {
        ret = H5B2_remove(bt2, &record, NULL, NULL);
    }
    H5E_END_TRY;
    /* Should fail */
    if (ret != FAIL)
        TEST_ERROR

    PASSED();

    /* Attempt to remove a record from a level-0 B-tree with mult. record */
    TESTING("B-tree remove: mult. existant records from level-0 B-tree");

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    record  = 42;
    rrecord = 0;
    if (H5B2_remove(bt2, &record, remove_cb, &rrecord) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the record value is correct */
    if (rrecord != 42)
        TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(bt2, &nrec) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the # of records is correct */
    if (nrec != 3)
        TEST_ERROR

    /* Query the address of the root node in the B-tree */
    if (H5B2__get_root_addr_test(bt2, &root_addr) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the root node has not been freed */
    if (!H5F_addr_defined(root_addr))
        TEST_ERROR

    record  = 34;
    rrecord = 0;
    if (H5B2_remove(bt2, &record, remove_cb, &rrecord) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the record value is correct */
    if (rrecord != 34)
        TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(bt2, &nrec) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the # of records is correct */
    if (nrec != 2)
        TEST_ERROR

    /* Query the address of the root node in the B-tree */
    if (H5B2__get_root_addr_test(bt2, &root_addr) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the root node has not been freed */
    if (!H5F_addr_defined(root_addr))
        TEST_ERROR

    record  = 56;
    rrecord = 0;
    if (H5B2_remove(bt2, &record, remove_cb, &rrecord) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the record value is correct */
    if (rrecord != 56)
        TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(bt2, &nrec) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the # of records is correct */
    if (nrec != 1)
        TEST_ERROR

    /* Query the address of the root node in the B-tree */
    if (H5B2__get_root_addr_test(bt2, &root_addr) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the root node has not been freed */
    if (!H5F_addr_defined(root_addr))
        TEST_ERROR

    record  = 38;
    rrecord = 0;
    if (H5B2_remove(bt2, &record, remove_cb, &rrecord) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the record value is correct */
    if (rrecord != 38)
        TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(bt2, &nrec) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the # of records is correct */
    if (nrec != 0)
        TEST_ERROR

    /* Query the address of the root node in the B-tree */
    if (H5B2__get_root_addr_test(bt2, &root_addr) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the root node has been freed */
    if (H5F_addr_defined(root_addr))
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    /* Close file */
    if (H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (bt2)
            H5B2_close(bt2);
        H5Fclose(file);
    }
    H5E_END_TRY;
    return 1;
} /* test_remove_basic() */

/*-------------------------------------------------------------------------
 * Function:    test_remove_level1_noredistrib
 *
 * Purpose:    Basic tests for the B-tree v2 code
 *
 * Return:    Success:    0
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Friday, February 25, 2005
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_remove_level1_noredistrib(hid_t fapl, const H5B2_create_t *cparam, const bt2_test_param_t *tparam)
{
    hid_t    file = -1;   /* File ID */
    H5F_t *  f    = NULL; /* Internal file object pointer */
    H5B2_t * bt2  = NULL; /* v2 B-tree wrapper */
    haddr_t  bt2_addr;    /* Address of B-tree created */
    hsize_t  record;      /* Record to insert into tree */
    hsize_t  rrecord;     /* Record to remove from tree */
    hsize_t  nrec;        /* Number of records in B-tree */
    haddr_t  root_addr;   /* Address of root of B-tree created */
    unsigned u;           /* Local index variable */
    herr_t   ret;         /* Generic error return value */

    /* B-tree record removal tests */
    TESTING("B-tree remove: non-existant record from level-1 B-tree");

    /* Create the file for the test */
    if (create_file(&file, &f, fapl) < 0)
        TEST_ERROR

    /* Create the v2 B-tree & get its address */
    if (create_btree(f, cparam, &bt2, &bt2_addr) < 0)
        TEST_ERROR

    /* Create level-1 B-tree with 3 leaves */
    for (u = 0; u < (INSERT_SPLIT_ROOT_NREC * 2); u++) {
        record = u;
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(bt2, &nrec) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the # of records is correct */
    if (nrec != (INSERT_SPLIT_ROOT_NREC * 2))
        TEST_ERROR

    /* Query the address of the root node in the B-tree */
    if (H5B2__get_root_addr_test(bt2, &root_addr) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the address of the root node is defined */
    if (!H5F_addr_defined(root_addr))
        TEST_ERROR

    /* Attempt to remove a non-existant record from a B-tree with 1 record */
    record = (INSERT_SPLIT_ROOT_NREC * 2) + 1;
    H5E_BEGIN_TRY
    {
        ret = H5B2_remove(bt2, &record, NULL, NULL);
    }
    H5E_END_TRY;
    /* Should fail */
    if (ret != FAIL)
        TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(bt2, &nrec) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the # of records is correct */
    if (nrec != (INSERT_SPLIT_ROOT_NREC * 2))
        TEST_ERROR

    PASSED();

    /* Attempt to remove a record from right leaf of a level-1 B-tree with noredistribution */
    TESTING("B-tree remove: record from right leaf of level-1 B-tree");

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Check up on B-tree */
    record = 62; /* Left record in root node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record = 94; /* Right record in root node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record = (INSERT_SPLIT_ROOT_NREC * 2) - 2;
    if (check_node_depth(bt2, &record, (unsigned)0) < 0)
        TEST_ERROR

    rrecord = 0;
    if (H5B2_remove(bt2, &record, remove_cb, &rrecord) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the record value is correct */
    if (rrecord != ((INSERT_SPLIT_ROOT_NREC * 2) - 2))
        TEST_ERROR

    /* Make certain that the leaf nodes didn't redistribute */
    record = 62; /* Left record in root node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record = 94; /* Right record in root node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(bt2, &nrec) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the # of records is correct */
    if (nrec != ((INSERT_SPLIT_ROOT_NREC * 2) - 1))
        TEST_ERROR

    PASSED();

    /* Attempt to remove a record from left leaf of a level-1 B-tree with noredistribution */
    TESTING("B-tree remove: record from left leaf of level-1 B-tree");

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Check up on B-tree */
    record = 0;
    if (check_node_depth(bt2, &record, (unsigned)0) < 0)
        TEST_ERROR

    rrecord = 1;
    if (H5B2_remove(bt2, &record, remove_cb, &rrecord) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the record value is correct */
    if (rrecord != 0)
        TEST_ERROR

    /* Make certain that the leaf nodes didn't redistribute */
    record = 62; /* Left record in root node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record = 94; /* Right record in root node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(bt2, &nrec) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the # of records is correct */
    if (nrec != ((INSERT_SPLIT_ROOT_NREC * 2) - 2))
        TEST_ERROR

    PASSED();

    /* Attempt to remove a record from middle leaf of a level-1 B-tree with noredistribution */
    TESTING("B-tree remove: record from middle leaf of level-1 B-tree");

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Check up on B-tree */
    record = INSERT_SPLIT_ROOT_NREC;
    if (check_node_depth(bt2, &record, (unsigned)0) < 0)
        TEST_ERROR

    rrecord = 0;
    if (H5B2_remove(bt2, &record, remove_cb, &rrecord) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the record value is correct */
    if (rrecord != INSERT_SPLIT_ROOT_NREC)
        TEST_ERROR

    /* Make certain that the leaf nodes didn't redistribute */
    record = 62; /* Left record in root node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record = 94; /* Right record in root node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(bt2, &nrec) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the # of records is correct */
    if (nrec != ((INSERT_SPLIT_ROOT_NREC * 2) - 3))
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    /* Close file */
    if (H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (bt2)
            H5B2_close(bt2);
        H5Fclose(file);
    }
    H5E_END_TRY;
    return 1;
} /* test_remove_level1_noredistrib() */

/*-------------------------------------------------------------------------
 * Function:    test_remove_level1_redistrib
 *
 * Purpose:    Basic tests for the B-tree v2 code
 *
 * Return:    Success:    0
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Friday, March  4, 2005
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_remove_level1_redistrib(hid_t fapl, const H5B2_create_t *cparam, const bt2_test_param_t *tparam)
{
    hid_t    file = -1;   /* File ID */
    H5F_t *  f    = NULL; /* Internal file object pointer */
    H5B2_t * bt2  = NULL; /* v2 B-tree wrapper */
    haddr_t  bt2_addr;    /* Address of B-tree created */
    hsize_t  record;      /* Record to insert into tree */
    hsize_t  rrecord;     /* Record to remove from tree */
    hsize_t  nrec;        /* Number of records in B-tree */
    haddr_t  root_addr;   /* Address of root of B-tree created */
    unsigned u;           /* Local index variable */

    /* More complex record removals */
    TESTING("B-tree remove: redistribute 2 leaves in level-1 B-tree (r->l)");

    /* Create the file for the test */
    if (create_file(&file, &f, fapl) < 0)
        TEST_ERROR

    /* Create the v2 B-tree & get its address */
    if (create_btree(f, cparam, &bt2, &bt2_addr) < 0)
        TEST_ERROR

    /* Create level-1 B-tree with 3 leaves */
    for (u = 0; u < (INSERT_SPLIT_ROOT_NREC * 2); u++) {
        record = u;
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check record values in root of B-tree */
    record = 62; /* Left record in root node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record = 94; /* Right record in root node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(bt2, &nrec) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the # of records is correct */
    if (nrec != (INSERT_SPLIT_ROOT_NREC * 2))
        TEST_ERROR

    /* Query the address of the root node in the B-tree */
    if (H5B2__get_root_addr_test(bt2, &root_addr) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the address of the root node is defined */
    if (!H5F_addr_defined(root_addr))
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Attempt to remove enough records from right leaf of a level-1 B-tree to force redistribution */
    for (u = 0; u < 8; u++) {
        record  = (INSERT_SPLIT_ROOT_NREC * 2) - (u + 1);
        rrecord = HSIZET_MAX;
        if (H5B2_remove(bt2, &record, remove_cb, &rrecord) < 0)
            FAIL_STACK_ERROR

        /* Make certain that the record value is correct */
        if (rrecord != ((INSERT_SPLIT_ROOT_NREC * 2) - (u + 1)))
            TEST_ERROR

        /* Query the number of records in the B-tree */
        if (H5B2_get_nrec(bt2, &nrec) < 0)
            FAIL_STACK_ERROR

        /* Make certain that the # of records is correct */
        if (nrec != ((INSERT_SPLIT_ROOT_NREC * 2) - (u + 1)))
            TEST_ERROR
    } /* end for */

    /* Check record values in root of B-tree */
    record = 62; /* Left record in root node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record = 90; /* Right record in root node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR

    PASSED();

    /* Attempt to remove enough records from left leaf of a level-1 B-tree to force redistribution */
    TESTING("B-tree remove: redistribute 2 leaves in level-1 B-tree (l->r)");

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    for (u = 0; u < 39; u++) {
        record  = u;
        rrecord = HSIZET_MAX;
        if (H5B2_remove(bt2, &record, remove_cb, &rrecord) < 0)
            FAIL_STACK_ERROR

        /* Make certain that the record value is correct */
        if (rrecord != u)
            TEST_ERROR

        /* Query the number of records in the B-tree */
        if (H5B2_get_nrec(bt2, &nrec) < 0)
            FAIL_STACK_ERROR

        /* Make certain that the # of records is correct */
        if (nrec != (((INSERT_SPLIT_ROOT_NREC * 2) - 8) - (u + 1)))
            TEST_ERROR
    } /* end for */

    /* Check record values in root of B-tree */
    record = 64; /* Left record in root node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record = 90; /* Right record in root node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR

    PASSED();

    /* Attempt to remove enough records from middle leaf of a level-1 B-tree to force redistribution */
    TESTING("B-tree remove: redistribute 3 leaves in level-1 B-tree");

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    for (u = 0; u < 2; u++) {
        record  = INSERT_SPLIT_ROOT_NREC + 2 + u;
        rrecord = HSIZET_MAX;
        if (H5B2_remove(bt2, &record, remove_cb, &rrecord) < 0)
            FAIL_STACK_ERROR

        /* Make certain that the record value is correct */
        if (rrecord != (INSERT_SPLIT_ROOT_NREC + 2 + u))
            TEST_ERROR

        /* Query the number of records in the B-tree */
        if (H5B2_get_nrec(bt2, &nrec) < 0)
            FAIL_STACK_ERROR

        /* Make certain that the # of records is correct */
        if (nrec != ((((INSERT_SPLIT_ROOT_NREC * 2) - 47)) - (u + 1)))
            TEST_ERROR
    } /* end for */

    /* Check record values in root of B-tree */
    record = 64; /* Left record in root node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record = 91; /* Right record in root node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    /* Close file */
    if (H5Fclose(file) < 0)
        TEST_ERROR

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (bt2)
            H5B2_close(bt2);
        H5Fclose(file);
    }
    H5E_END_TRY;
    return 1;
} /* test_remove_level1_redistrib() */

/*-------------------------------------------------------------------------
 * Function:    test_remove_level1_2leaf_merge
 *
 * Purpose:    Basic tests for the B-tree v2 code
 *
 * Return:    Success:    0
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Friday, March  4, 2005
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_remove_level1_2leaf_merge(hid_t fapl, const H5B2_create_t *cparam, const bt2_test_param_t *tparam)
{
    hid_t                 file = -1;   /* File ID */
    H5F_t *               f    = NULL; /* Internal file object pointer */
    H5B2_t *              bt2  = NULL; /* v2 B-tree wrapper */
    haddr_t               bt2_addr;    /* Address of B-tree created */
    hsize_t               record;      /* Record to insert into tree */
    hsize_t               rrecord;     /* Record to remove from tree */
    hsize_t               nrec;        /* Number of records in B-tree */
    haddr_t               root_addr;   /* Address of root of B-tree created */
    H5B2_node_info_test_t ninfo;       /* B-tree node info */
    unsigned              u;           /* Local index variable */

    TESTING("B-tree remove: merge 2 leaves to 1 in level-1 B-tree (r->l)");

    /* Create the file for the test */
    if (create_file(&file, &f, fapl) < 0)
        TEST_ERROR

    /* Create the v2 B-tree & get its address */
    if (create_btree(f, cparam, &bt2, &bt2_addr) < 0)
        TEST_ERROR

    /* Create level-1 B-tree with 3 leaves */
    for (u = 0; u < (INSERT_SPLIT_ROOT_NREC * 2); u++) {
        record = u;
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check record values in root of B-tree */
    record = 62; /* Left record in root node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    ninfo.depth = 1;
    ninfo.nrec  = 2;
    record      = 94; /* Right record in root node */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(bt2, &nrec) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the # of records is correct */
    if (nrec != (INSERT_SPLIT_ROOT_NREC * 2))
        TEST_ERROR

    /* Query the address of the root node in the B-tree */
    if (H5B2__get_root_addr_test(bt2, &root_addr) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the address of the root node is defined */
    if (!H5F_addr_defined(root_addr))
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Attempt to remove enough records from right leaf of a level-1 B-tree to force redistribution */
    for (u = 0; u < (INSERT_SPLIT_ROOT_NREC / 4); u++) {
        record  = (INSERT_SPLIT_ROOT_NREC * 2) - (u + 1);
        rrecord = HSIZET_MAX;
        if (H5B2_remove(bt2, &record, remove_cb, &rrecord) < 0)
            FAIL_STACK_ERROR

        /* Make certain that the record value is correct */
        if (rrecord != ((INSERT_SPLIT_ROOT_NREC * 2) - (u + 1)))
            TEST_ERROR

        /* Query the number of records in the B-tree */
        if (H5B2_get_nrec(bt2, &nrec) < 0)
            FAIL_STACK_ERROR

        /* Make certain that the # of records is correct */
        if (nrec != ((INSERT_SPLIT_ROOT_NREC * 2) - (u + 1)))
            TEST_ERROR
    } /* end for */

    /* Check record values in root of B-tree */
    ninfo.depth = 1;
    ninfo.nrec  = 1;
    record      = 62; /* Left record in root node */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    PASSED();

    /* Attempt to remove enough records from left leaf of a level-1 B-tree to force redistribution */
    TESTING("B-tree remove: merge 2 leaves to 1 in level-1 B-tree (l->r)");

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Fill B-tree back up */
    for (u = 0; u < (INSERT_SPLIT_ROOT_NREC / 4); u++) {
        record = (INSERT_SPLIT_ROOT_NREC * 2) - (u + 1);
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check record values in root of B-tree */
    record = 62; /* Left record in root node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    ninfo.depth = 1;
    ninfo.nrec  = 2;
    record      = 94; /* Right record in root node */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    /* Remove records */
    for (u = 0; u < ((3 * INSERT_SPLIT_ROOT_NREC) / 4) - 1; u++) {
        record  = u;
        rrecord = HSIZET_MAX;
        if (H5B2_remove(bt2, &record, remove_cb, &rrecord) < 0)
            FAIL_STACK_ERROR

        /* Make certain that the record value is correct */
        if (rrecord != u)
            TEST_ERROR

        /* Query the number of records in the B-tree */
        if (H5B2_get_nrec(bt2, &nrec) < 0)
            FAIL_STACK_ERROR

        /* Make certain that the # of records is correct */
        if (nrec != ((INSERT_SPLIT_ROOT_NREC * 2) - (u + 1)))
            TEST_ERROR
    } /* end for */

    /* Check record values in root of B-tree */
    ninfo.depth = 1;
    ninfo.nrec  = 1;
    record      = 94; /* Left record in root node */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    /* Close file */
    if (H5Fclose(file) < 0)
        TEST_ERROR

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (bt2)
            H5B2_close(bt2);
        H5Fclose(file);
    }
    H5E_END_TRY;
    return 1;
} /* test_remove_level1_2leaf_merge() */

/*-------------------------------------------------------------------------
 * Function:    test_remove_level1_3leaf_merge
 *
 * Purpose:    Basic tests for the B-tree v2 code
 *
 * Return:    Success:    0
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Friday, March  4, 2005
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_remove_level1_3leaf_merge(hid_t fapl, const H5B2_create_t *cparam, const bt2_test_param_t *tparam)
{
    hid_t                 file = -1;   /* File ID */
    H5F_t *               f    = NULL; /* Internal file object pointer */
    H5B2_t *              bt2  = NULL; /* v2 B-tree wrapper */
    haddr_t               bt2_addr;    /* Address of B-tree created */
    hsize_t               record;      /* Record to insert into tree */
    hsize_t               rrecord;     /* Record to remove from tree */
    hsize_t               nrec;        /* Number of records in B-tree */
    haddr_t               root_addr;   /* Address of root of B-tree created */
    H5B2_node_info_test_t ninfo;       /* B-tree node info */
    unsigned              u;           /* Local index variable */

    TESTING("B-tree remove: merge 3 leaves to 2 in level-1 B-tree");

    /* Create the file for the test */
    if (create_file(&file, &f, fapl) < 0)
        TEST_ERROR

    /* Create the v2 B-tree & get its address */
    if (create_btree(f, cparam, &bt2, &bt2_addr) < 0)
        TEST_ERROR

    /* Create level-1 B-tree with 3 leaves */
    for (u = 0; u < (INSERT_SPLIT_ROOT_NREC * 2); u++) {
        record = u;
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check record values in root of B-tree */
    record = 62; /* Left record in root node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    ninfo.depth = 1;
    ninfo.nrec  = 2;
    record      = 94; /* Right record in root node */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(bt2, &nrec) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the # of records is correct */
    if (nrec != (INSERT_SPLIT_ROOT_NREC * 2))
        TEST_ERROR

    /* Query the address of the root node in the B-tree */
    if (H5B2__get_root_addr_test(bt2, &root_addr) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the address of the root node is defined */
    if (!H5F_addr_defined(root_addr))
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Attempt to remove enough records from middle leaf of a level-1 B-tree to force merge */
    for (u = 0; u < ((5 * INSERT_SPLIT_ROOT_NREC) / 6) - 1; u++) {
        record  = ((3 * INSERT_SPLIT_ROOT_NREC) / 2) - (u + 1);
        rrecord = HSIZET_MAX;
        if (H5B2_remove(bt2, &record, remove_cb, &rrecord) < 0)
            FAIL_STACK_ERROR

        /* Make certain that the record value is correct */
        if (rrecord != (((3 * INSERT_SPLIT_ROOT_NREC) / 2) - (u + 1)))
            TEST_ERROR

        /* Query the number of records in the B-tree */
        if (H5B2_get_nrec(bt2, &nrec) < 0)
            FAIL_STACK_ERROR

        /* Make certain that the # of records is correct */
        if (nrec != ((INSERT_SPLIT_ROOT_NREC * 2) - (u + 1)))
            TEST_ERROR
    } /* end for */

    /* Check record values in root of B-tree */
    ninfo.depth = 1;
    ninfo.nrec  = 1;
    record      = 37; /* Only record in root node */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    /* Close file */
    if (H5Fclose(file) < 0)
        TEST_ERROR

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (bt2)
            H5B2_close(bt2);
        H5Fclose(file);
    }
    H5E_END_TRY;
    return 1;
} /* test_remove_level1_3leaf_merge() */

/*-------------------------------------------------------------------------
 * Function:    test_remove_level1_promote
 *
 * Purpose:    Basic tests for the B-tree v2 code
 *
 * Return:    Success:    0
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Friday, March  4, 2005
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_remove_level1_promote(hid_t fapl, const H5B2_create_t *cparam, const bt2_test_param_t *tparam)
{
    hid_t                 file = -1;   /* File ID */
    H5F_t *               f    = NULL; /* Internal file object pointer */
    H5B2_t *              bt2  = NULL; /* v2 B-tree wrapper */
    haddr_t               bt2_addr;    /* Address of B-tree created */
    hsize_t               record;      /* Record to insert into tree */
    hsize_t               rrecord;     /* Record to remove from tree */
    hsize_t               nrec;        /* Number of records in B-tree */
    haddr_t               root_addr;   /* Address of root of B-tree created */
    H5B2_node_info_test_t ninfo;       /* B-tree node info */
    unsigned              u;           /* Local index variable */

    TESTING("B-tree remove: promote from right leaf of level-1 B-tree");

    /* Create the file for the test */
    if (create_file(&file, &f, fapl) < 0)
        TEST_ERROR

    /* Create the v2 B-tree & get its address */
    if (create_btree(f, cparam, &bt2, &bt2_addr) < 0)
        TEST_ERROR

    /* Create level-1 B-tree with 5 leaves */
    for (u = 0; u < (INSERT_SPLIT_ROOT_NREC * 4); u++) {
        record = u;
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check record values in root of B-tree */
    record = 62; /* Left-most record in root node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record = 125; /* Center-Left record in root node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record = 188; /* Center-Right record in root node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    ninfo.depth = 1;
    ninfo.nrec  = 4;
    record      = 220; /* Right-most record in root node */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(bt2, &nrec) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the # of records is correct */
    if (nrec != (INSERT_SPLIT_ROOT_NREC * 4))
        TEST_ERROR

    /* Query the address of the root node in the B-tree */
    if (H5B2__get_root_addr_test(bt2, &root_addr) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the address of the root node is defined */
    if (!H5F_addr_defined(root_addr))
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Attempt to remove record from root node of a level-1 B-tree to force promotion from right leaf */
    record  = 220;
    rrecord = HSIZET_MAX;
    if (H5B2_remove(bt2, &record, remove_cb, &rrecord) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the record value is correct */
    if (rrecord != 220)
        TEST_ERROR

    /* Check record values in root of B-tree */
    record = 62; /* Left-most record in root node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record = 125; /* Center-Left record in root node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record = 188; /* Center-Right record in root node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    ninfo.depth = 1;
    ninfo.nrec  = 4;
    record      = 221; /* Right-most record in root node */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(bt2, &nrec) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the # of records is correct */
    if (nrec != (INSERT_SPLIT_ROOT_NREC * 4) - 1)
        TEST_ERROR

    PASSED();

    /* Attempt to remove record from root node of a level-1 B-tree to force promotion from left leaf */
    /* (Note: current algorithm doesn't actually ever promote from left leaf.
     *  It would be useful to update the B-tree routines to always choose
     *  to promote a record from the node with more children. - QAK)
     */
    TESTING("B-tree remove: promote from left leaf of level-1 B-tree");

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    record  = 62;
    rrecord = HSIZET_MAX;
    if (H5B2_remove(bt2, &record, remove_cb, &rrecord) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the record value is correct */
    if (rrecord != 62)
        TEST_ERROR

    /* Check record values in root of B-tree */
    record = 63; /* Left-most record in root node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record = 125; /* Center-Left record in root node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record = 188; /* Center-Right record in root node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    ninfo.depth = 1;
    ninfo.nrec  = 4;
    record      = 221; /* Right-most record in root node */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(bt2, &nrec) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the # of records is correct */
    if (nrec != (INSERT_SPLIT_ROOT_NREC * 4) - 2)
        TEST_ERROR

    PASSED();

    /* Attempt to remove record from root node of a level-1 B-tree to force promotion from middle leaf */
    TESTING("B-tree remove: promote from middle leaf of level-1 B-tree");

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    record  = 125;
    rrecord = HSIZET_MAX;
    if (H5B2_remove(bt2, &record, remove_cb, &rrecord) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the record value is correct */
    if (rrecord != 125)
        TEST_ERROR

    /* Check record values in root of B-tree */
    record = 63; /* Left-most record in root node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record = 126; /* Center-Left record in root node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    record = 188; /* Center-Right record in root node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    ninfo.depth = 1;
    ninfo.nrec  = 4;
    record      = 221; /* Right-most record in root node */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(bt2, &nrec) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the # of records is correct */
    if (nrec != (INSERT_SPLIT_ROOT_NREC * 4) - 3)
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    /* Close file */
    if (H5Fclose(file) < 0)
        TEST_ERROR

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (bt2)
            H5B2_close(bt2);
        H5Fclose(file);
    }
    H5E_END_TRY;
    return 1;
} /* test_remove_level1_promote() */

/*-------------------------------------------------------------------------
 * Function:    test_remove_level1_promote_2leaf_redistrib
 *
 * Purpose:    Basic tests for the B-tree v2 code
 *
 * Return:    Success:    0
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Friday, March  4, 2005
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_remove_level1_promote_2leaf_redistrib(hid_t fapl, const H5B2_create_t *cparam,
                                           const bt2_test_param_t *tparam)
{
    hid_t                 file = -1;   /* File ID */
    H5F_t *               f    = NULL; /* Internal file object pointer */
    H5B2_t *              bt2  = NULL; /* v2 B-tree wrapper */
    haddr_t               bt2_addr;    /* Address of B-tree created */
    hsize_t               record;      /* Record to insert into tree */
    hsize_t               rrecord;     /* Record to remove from tree */
    hsize_t               nrec;        /* Number of records in B-tree */
    haddr_t               root_addr;   /* Address of root of B-tree created */
    H5B2_node_info_test_t ninfo;       /* B-tree node info */
    unsigned              u;           /* Local index variable */

    TESTING("B-tree remove: promote from leaf of level-1 B-tree w/2 node redistrib");

    /* Create the file for the test */
    if (create_file(&file, &f, fapl) < 0)
        TEST_ERROR

    /* Create the v2 B-tree & get its address */
    if (create_btree(f, cparam, &bt2, &bt2_addr) < 0)
        TEST_ERROR

    /* Create level-1 B-tree with 3 leaves */
    for (u = 0; u < (INSERT_SPLIT_ROOT_NREC * 2); u++) {
        record = u;
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check record values in root of B-tree */
    record = 62; /* Left-most record in root node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    ninfo.depth = 1;
    ninfo.nrec  = 2;
    record      = 94; /* Right record in root node */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(bt2, &nrec) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the # of records is correct */
    if (nrec != (INSERT_SPLIT_ROOT_NREC * 2))
        TEST_ERROR

    /* Query the address of the root node in the B-tree */
    if (H5B2__get_root_addr_test(bt2, &root_addr) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the address of the root node is defined */
    if (!H5F_addr_defined(root_addr))
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Attempt to remove record from root node of a level-1 B-tree to force promotion from right leaf */

    /* Remove records from right leaf until its ready to redistribute */
    for (u = 0; u < 7; u++) {
        record  = (INSERT_SPLIT_ROOT_NREC * 2) - (u + 1);
        rrecord = HSIZET_MAX;
        if (H5B2_remove(bt2, &record, remove_cb, &rrecord) < 0)
            FAIL_STACK_ERROR

        /* Make certain that the record value is correct */
        if (rrecord != ((INSERT_SPLIT_ROOT_NREC * 2) - (u + 1)))
            TEST_ERROR

        /* Query the number of records in the B-tree */
        if (H5B2_get_nrec(bt2, &nrec) < 0)
            FAIL_STACK_ERROR

        /* Make certain that the # of records is correct */
        if (nrec != ((INSERT_SPLIT_ROOT_NREC * 2) - (u + 1)))
            TEST_ERROR
    } /* end for */

    record  = 94;
    rrecord = HSIZET_MAX;
    if (H5B2_remove(bt2, &record, remove_cb, &rrecord) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the record value is correct */
    if (rrecord != 94)
        TEST_ERROR

    /* Check record values in root of B-tree */
    record = 62; /* Left-most record in root node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    ninfo.depth = 1;
    ninfo.nrec  = 2;
    record      = 90; /* Right record in root node */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(bt2, &nrec) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the # of records is correct */
    if (nrec != (INSERT_SPLIT_ROOT_NREC * 2) - 8)
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    /* Close file */
    if (H5Fclose(file) < 0)
        TEST_ERROR

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (bt2)
            H5B2_close(bt2);
        H5Fclose(file);
    }
    H5E_END_TRY;
    return 1;
} /* test_remove_level1_promote_2leaf_redistrib() */

/*-------------------------------------------------------------------------
 * Function:    test_remove_level1_promote_3leaf_redistrib
 *
 * Purpose:    Basic tests for the B-tree v2 code
 *
 * Return:    Success:    0
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Friday, March  4, 2005
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_remove_level1_promote_3leaf_redistrib(hid_t fapl, const H5B2_create_t *cparam,
                                           const bt2_test_param_t *tparam)
{
    hid_t                 file = -1;   /* File ID */
    H5F_t *               f    = NULL; /* Internal file object pointer */
    H5B2_t *              bt2  = NULL; /* v2 B-tree wrapper */
    haddr_t               bt2_addr;    /* Address of B-tree created */
    hsize_t               record;      /* Record to insert into tree */
    hsize_t               rrecord;     /* Record to remove from tree */
    hsize_t               nrec;        /* Number of records in B-tree */
    haddr_t               root_addr;   /* Address of root of B-tree created */
    H5B2_node_info_test_t ninfo;       /* B-tree node info */
    unsigned              u;           /* Local index variable */

    TESTING("B-tree remove: promote from leaf of level-1 B-tree w/3 node redistrib");

    /* Create the file for the test */
    if (create_file(&file, &f, fapl) < 0)
        TEST_ERROR

    /* Create the v2 B-tree & get its address */
    if (create_btree(f, cparam, &bt2, &bt2_addr) < 0)
        TEST_ERROR

    /* Create level-1 B-tree with 3 leaves */
    for (u = 0; u < (INSERT_SPLIT_ROOT_NREC * 2); u++) {
        record = u;
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check record values in root of B-tree */
    record = 62; /* Left-most record in root node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    ninfo.depth = 1;
    ninfo.nrec  = 2;
    record      = 94; /* Right record in root node */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(bt2, &nrec) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the # of records is correct */
    if (nrec != (INSERT_SPLIT_ROOT_NREC * 2))
        TEST_ERROR

    /* Query the address of the root node in the B-tree */
    if (H5B2__get_root_addr_test(bt2, &root_addr) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the address of the root node is defined */
    if (!H5F_addr_defined(root_addr))
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Attempt to remove record from root node of a level-1 B-tree to force promotion from middle leaf */

    /* Remove records from right leaf until its ready to redistribute */
    for (u = 0; u < 7; u++) {
        record  = 63 + u;
        rrecord = HSIZET_MAX;
        if (H5B2_remove(bt2, &record, remove_cb, &rrecord) < 0)
            FAIL_STACK_ERROR

        /* Make certain that the record value is correct */
        if (rrecord != (63 + u))
            TEST_ERROR

        /* Query the number of records in the B-tree */
        if (H5B2_get_nrec(bt2, &nrec) < 0)
            FAIL_STACK_ERROR

        /* Make certain that the # of records is correct */
        if (nrec != ((INSERT_SPLIT_ROOT_NREC * 2) - (u + 1)))
            TEST_ERROR
    } /* end for */

    record  = 62;
    rrecord = HSIZET_MAX;
    if (H5B2_remove(bt2, &record, remove_cb, &rrecord) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the record value is correct */
    if (rrecord != 62)
        TEST_ERROR

    /* Check record values in root of B-tree */
    record = 39; /* Left record in root node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    ninfo.depth = 1;
    ninfo.nrec  = 2;
    record      = 86; /* Right record in root node */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(bt2, &nrec) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the # of records is correct */
    if (nrec != (INSERT_SPLIT_ROOT_NREC * 2) - 8)
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    /* Close file */
    if (H5Fclose(file) < 0)
        TEST_ERROR

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (bt2)
            H5B2_close(bt2);
        H5Fclose(file);
    }
    H5E_END_TRY;
    return 1;
} /* test_remove_level1_promote_3leaf_redistrib() */

/*-------------------------------------------------------------------------
 * Function:    test_remove_level1_promote_2leaf_merge
 *
 * Purpose:    Basic tests for the B-tree v2 code
 *
 * Return:    Success:    0
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Friday, March  4, 2005
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_remove_level1_promote_2leaf_merge(hid_t fapl, const H5B2_create_t *cparam,
                                       const bt2_test_param_t *tparam)
{
    hid_t                 file = -1;   /* File ID */
    H5F_t *               f    = NULL; /* Internal file object pointer */
    H5B2_t *              bt2  = NULL; /* v2 B-tree wrapper */
    haddr_t               bt2_addr;    /* Address of B-tree created */
    hsize_t               record;      /* Record to insert into tree */
    hsize_t               rrecord;     /* Record to remove from tree */
    hsize_t               nrec;        /* Number of records in B-tree */
    haddr_t               root_addr;   /* Address of root of B-tree created */
    H5B2_node_info_test_t ninfo;       /* B-tree node info */
    unsigned              u;           /* Local index variable */

    TESTING("B-tree remove: promote from leaf of level-1 B-tree w/2->1 merge");

    /* Create the file for the test */
    if (create_file(&file, &f, fapl) < 0)
        TEST_ERROR

    /* Create the v2 B-tree & get its address */
    if (create_btree(f, cparam, &bt2, &bt2_addr) < 0)
        TEST_ERROR

    /* Create level-1 B-tree with 3 leaves */
    for (u = 0; u < (INSERT_SPLIT_ROOT_NREC * 2); u++) {
        record = u;
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check record values in root of B-tree */
    record = 62; /* Left-most record in root node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    ninfo.depth = 1;
    ninfo.nrec  = 2;
    record      = 94; /* Right record in root node */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(bt2, &nrec) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the # of records is correct */
    if (nrec != (INSERT_SPLIT_ROOT_NREC * 2))
        TEST_ERROR

    /* Query the address of the root node in the B-tree */
    if (H5B2__get_root_addr_test(bt2, &root_addr) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the address of the root node is defined */
    if (!H5F_addr_defined(root_addr))
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Attempt to remove record from root node of a level-1 B-tree to force promotion from right leaf */

    /* Remove records from right leaf until its ready to merge */
    for (u = 0; u < 14; u++) {
        record  = (INSERT_SPLIT_ROOT_NREC * 2) - (u + 1);
        rrecord = HSIZET_MAX;
        if (H5B2_remove(bt2, &record, remove_cb, &rrecord) < 0)
            FAIL_STACK_ERROR

        /* Make certain that the record value is correct */
        if (rrecord != ((INSERT_SPLIT_ROOT_NREC * 2) - (u + 1)))
            TEST_ERROR

        /* Query the number of records in the B-tree */
        if (H5B2_get_nrec(bt2, &nrec) < 0)
            FAIL_STACK_ERROR

        /* Make certain that the # of records is correct */
        if (nrec != ((INSERT_SPLIT_ROOT_NREC * 2) - (u + 1)))
            TEST_ERROR
    } /* end for */

    record  = 87;
    rrecord = HSIZET_MAX;
    if (H5B2_remove(bt2, &record, remove_cb, &rrecord) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the record value is correct */
    if (rrecord != 87)
        TEST_ERROR

    /* Check record values in root of B-tree */
    ninfo.depth = 1;
    ninfo.nrec  = 1;
    record      = 62; /* Middle record in root node */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(bt2, &nrec) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the # of records is correct */
    if (nrec != (INSERT_SPLIT_ROOT_NREC * 2) - 15)
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    /* Close file */
    if (H5Fclose(file) < 0)
        TEST_ERROR

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (bt2)
            H5B2_close(bt2);
        H5Fclose(file);
    }
    H5E_END_TRY;
    return 1;
} /* test_remove_level1_promote_2leaf_merge() */

/*-------------------------------------------------------------------------
 * Function:    test_remove_level1_promote_3leaf_merge
 *
 * Purpose:    Basic tests for the B-tree v2 code
 *
 * Return:    Success:    0
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Friday, March  4, 2005
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_remove_level1_promote_3leaf_merge(hid_t fapl, const H5B2_create_t *cparam,
                                       const bt2_test_param_t *tparam)
{
    hid_t                 file = -1;   /* File ID */
    H5F_t *               f    = NULL; /* Internal file object pointer */
    H5B2_t *              bt2  = NULL; /* v2 B-tree wrapper */
    haddr_t               bt2_addr;    /* Address of B-tree created */
    hsize_t               record;      /* Record to insert into tree */
    hsize_t               rrecord;     /* Record to remove from tree */
    hsize_t               nrec;        /* Number of records in B-tree */
    haddr_t               root_addr;   /* Address of root of B-tree created */
    H5B2_node_info_test_t ninfo;       /* B-tree node info */
    unsigned              u;           /* Local index variable */

    TESTING("B-tree remove: promote from leaf of level-1 B-tree w/3->2 merge");

    /* Create the file for the test */
    if (create_file(&file, &f, fapl) < 0)
        TEST_ERROR

    /* Create the v2 B-tree & get its address */
    if (create_btree(f, cparam, &bt2, &bt2_addr) < 0)
        TEST_ERROR

    /* Create level-2 B-tree with 3 leaves */
    for (u = 0; u < (INSERT_SPLIT_ROOT_NREC * 2); u++) {
        record = u;
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check record values in root of B-tree */
    record = 62; /* Left-most record in root node */
    if (check_node_depth(bt2, &record, (unsigned)1) < 0)
        TEST_ERROR
    ninfo.depth = 1;
    ninfo.nrec  = 2;
    record      = 94; /* Right record in root node */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(bt2, &nrec) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the # of records is correct */
    if (nrec != (INSERT_SPLIT_ROOT_NREC * 2))
        TEST_ERROR

    /* Query the address of the root node in the B-tree */
    if (H5B2__get_root_addr_test(bt2, &root_addr) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the address of the root node is defined */
    if (!H5F_addr_defined(root_addr))
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Attempt to remove record from root node of a level-1 B-tree to force promotion from middle leaf */

    /* Remove records from middle leaf until it's ready to merge */
    for (u = 0; u < 50; u++) {
        record  = ((3 * INSERT_SPLIT_ROOT_NREC) / 2) - (u + 1);
        rrecord = HSIZET_MAX;
        if (H5B2_remove(bt2, &record, remove_cb, &rrecord) < 0)
            FAIL_STACK_ERROR

        /* Make certain that the record value is correct */
        if (rrecord != (((3 * INSERT_SPLIT_ROOT_NREC) / 2) - (u + 1)))
            TEST_ERROR

        /* Query the number of records in the B-tree */
        if (H5B2_get_nrec(bt2, &nrec) < 0)
            FAIL_STACK_ERROR

        /* Make certain that the # of records is correct */
        if (nrec != ((INSERT_SPLIT_ROOT_NREC * 2) - (u + 1)))
            TEST_ERROR
    } /* end for */

    record  = 25;
    rrecord = HSIZET_MAX;
    if (H5B2_remove(bt2, &record, remove_cb, &rrecord) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the record value is correct */
    if (rrecord != 25)
        TEST_ERROR

    /* Check record values in root of B-tree */
    ninfo.depth = 1;
    ninfo.nrec  = 1;
    record      = 37; /* Right record in root node */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(bt2, &nrec) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the # of records is correct */
    if (nrec != (INSERT_SPLIT_ROOT_NREC * 2) - 51)
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    /* Close file */
    if (H5Fclose(file) < 0)
        TEST_ERROR

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (bt2)
            H5B2_close(bt2);
        H5Fclose(file);
    }
    H5E_END_TRY;
    return 1;
} /* test_remove_level1_promote_3leaf_merge() */

/*-------------------------------------------------------------------------
 * Function:    test_remove_level1_collapse
 *
 * Purpose:    Basic tests for the B-tree v2 code
 *
 * Return:    Success:    0
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Friday, March  4, 2005
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_remove_level1_collapse(hid_t fapl, const H5B2_create_t *cparam, const bt2_test_param_t *tparam)
{
    hid_t                 file = -1;   /* File ID */
    H5F_t *               f    = NULL; /* Internal file object pointer */
    H5B2_t *              bt2  = NULL; /* v2 B-tree wrapper */
    haddr_t               bt2_addr;    /* Address of B-tree created */
    hsize_t               record;      /* Record to insert into tree */
    hsize_t               rrecord;     /* Record to remove from tree */
    hsize_t               nrec;        /* Number of records in B-tree */
    haddr_t               root_addr;   /* Address of root of B-tree created */
    H5B2_node_info_test_t ninfo;       /* B-tree node info */
    unsigned              u;           /* Local index variable */

    TESTING("B-tree remove: collapse level-1 B-tree back to level-0");

    /* Create the file for the test */
    if (create_file(&file, &f, fapl) < 0)
        TEST_ERROR

    /* Create the v2 B-tree & get its address */
    if (create_btree(f, cparam, &bt2, &bt2_addr) < 0)
        TEST_ERROR

    /* Create level-1 B-tree with 2 leaves */
    for (u = 0; u < INSERT_SPLIT_ROOT_NREC; u++) {
        record = u;
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check record values in root of B-tree */
    ninfo.depth = 1;
    ninfo.nrec  = 1;
    record      = 31; /* Middle record in root node */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(bt2, &nrec) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the # of records is correct */
    if (nrec != INSERT_SPLIT_ROOT_NREC)
        TEST_ERROR

    /* Query the address of the root node in the B-tree */
    if (H5B2__get_root_addr_test(bt2, &root_addr) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the address of the root node is defined */
    if (!H5F_addr_defined(root_addr))
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Attempt to remove records from B-tree to force a single leaf for the B-tree */
    for (u = 0; u < 14; u++) {
        record  = INSERT_SPLIT_ROOT_NREC - (u + 1);
        rrecord = HSIZET_MAX;
        if (H5B2_remove(bt2, &record, remove_cb, &rrecord) < 0)
            FAIL_STACK_ERROR

        /* Make certain that the record value is correct */
        if (rrecord != (INSERT_SPLIT_ROOT_NREC - (u + 1)))
            TEST_ERROR

        /* Query the number of records in the B-tree */
        if (H5B2_get_nrec(bt2, &nrec) < 0)
            FAIL_STACK_ERROR

        /* Make certain that the # of records is correct */
        if (nrec != (INSERT_SPLIT_ROOT_NREC - (u + 1)))
            TEST_ERROR
    } /* end for */

    /* Check record values in root of B-tree */
    ninfo.depth = 0;
    ninfo.nrec  = (uint16_t)(INSERT_SPLIT_ROOT_NREC - u);
    record      = 31; /* Middle record in root node */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(bt2, &nrec) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the # of records is correct */
    if (nrec != (INSERT_SPLIT_ROOT_NREC - u))
        TEST_ERROR

    /* Query the address of the root node in the B-tree */
    if (H5B2__get_root_addr_test(bt2, &root_addr) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the address of the root node is defined */
    if (!H5F_addr_defined(root_addr))
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    /* Close file */
    if (H5Fclose(file) < 0)
        TEST_ERROR

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (bt2)
            H5B2_close(bt2);
        H5Fclose(file);
    }
    H5E_END_TRY;
    return 1;
} /* test_remove_level1_collapse() */

/*-------------------------------------------------------------------------
 * Function:    test_remove_level2_promote
 *
 * Purpose:    Basic tests for the B-tree v2 code
 *
 * Return:    Success:    0
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Friday, March  4, 2005
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_remove_level2_promote(hid_t fapl, const H5B2_create_t *cparam, const bt2_test_param_t *tparam)
{
    hid_t                 file = -1;   /* File ID */
    H5F_t *               f    = NULL; /* Internal file object pointer */
    H5B2_t *              bt2  = NULL; /* v2 B-tree wrapper */
    haddr_t               bt2_addr;    /* Address of B-tree created */
    hsize_t               record;      /* Record to insert into tree */
    hsize_t               rrecord;     /* Record to remove from tree */
    hsize_t               nrec;        /* Number of records in B-tree */
    haddr_t               root_addr;   /* Address of root of B-tree created */
    H5B2_node_info_test_t ninfo;       /* B-tree node info */
    unsigned              u;           /* Local index variable */

    TESTING("B-tree remove: promote from right internal of level-2 B-tree");

    /* Create the file for the test */
    if (create_file(&file, &f, fapl) < 0)
        TEST_ERROR

    /* Create the v2 B-tree & get its address */
    if (create_btree(f, cparam, &bt2, &bt2_addr) < 0)
        TEST_ERROR

    /* Create level-2 B-tree with 3 internal nodes */
    for (u = 0; u < ((INSERT_SPLIT_ROOT_NREC * 59) + 1); u++) {
        record = u;
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check record values in root of B-tree */
    record = 1889; /* Left record in root node */
    if (check_node_depth(bt2, &record, (unsigned)2) < 0)
        TEST_ERROR
    ninfo.depth = 2;
    ninfo.nrec  = 2;
    record      = 2834; /* Right record in root node */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(bt2, &nrec) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the # of records is correct */
    if (nrec != ((INSERT_SPLIT_ROOT_NREC * 59) + 1))
        TEST_ERROR

    /* Query the address of the root node in the B-tree */
    if (H5B2__get_root_addr_test(bt2, &root_addr) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the address of the root node is defined */
    if (!H5F_addr_defined(root_addr))
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Check information about record in right internal node */
    ninfo.depth = 1;
    ninfo.nrec  = 14;
    record      = 2960; /* Record in right internal node */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    /* Attempt to remove record from right internal node of a level-2 B-tree to force promotion */
    record  = 2960;
    rrecord = HSIZET_MAX;
    if (H5B2_remove(bt2, &record, remove_cb, &rrecord) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the record value is correct */
    if (rrecord != 2960)
        TEST_ERROR

    /* Check information about record promoted into right internal node */
    ninfo.depth = 1;
    ninfo.nrec  = 14;
    record      = 2961; /* Record in right internal node */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(bt2, &nrec) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the # of records is correct */
    if (nrec != (INSERT_SPLIT_ROOT_NREC * 59))
        TEST_ERROR

    PASSED();

    /* Attempt to remove record from left internal node of a level-2 B-tree to force promotion */
    TESTING("B-tree remove: promote from left internal of level-2 B-tree");

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Check information about record in left internal node */
    ninfo.depth = 1;
    ninfo.nrec  = 29;
    record      = 1133; /* Record in left internal node */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    record  = 1133;
    rrecord = HSIZET_MAX;
    if (H5B2_remove(bt2, &record, remove_cb, &rrecord) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the record value is correct */
    if (rrecord != 1133)
        TEST_ERROR

    /* Check information about record in left internal node */
    ninfo.depth = 1;
    ninfo.nrec  = 29;
    record      = 1134; /* Record in left internal node */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(bt2, &nrec) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the # of records is correct */
    if (nrec != (INSERT_SPLIT_ROOT_NREC * 59) - 1)
        TEST_ERROR

    PASSED();

    /* Attempt to remove record from middle internal node of a level-2 B-tree to force promotion */
    TESTING("B-tree remove: promote from middle internal of level-2 B-tree");

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Check information about record in middle internal node */
    ninfo.depth = 1;
    ninfo.nrec  = 14;
    record      = 2267; /* Record in middle internal node */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    record  = 2267;
    rrecord = HSIZET_MAX;
    if (H5B2_remove(bt2, &record, remove_cb, &rrecord) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the record value is correct */
    if (rrecord != 2267)
        TEST_ERROR

    /* Check information about record in middle internal node */
    ninfo.depth = 1;
    ninfo.nrec  = 14;
    record      = 2268; /* Record in middle internal node */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(bt2, &nrec) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the # of records is correct */
    if (nrec != (INSERT_SPLIT_ROOT_NREC * 59) - 2)
        TEST_ERROR

    PASSED();

    /* Attempt to remove record from root node of a level-2 B-tree to force promotion */
    TESTING("B-tree remove: promote record from root of level-2 B-tree");

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Check information about record in root node */
    ninfo.depth = 2;
    ninfo.nrec  = 2;
    record      = 1889; /* Left record in root node */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    record  = 1889;
    rrecord = HSIZET_MAX;
    if (H5B2_remove(bt2, &record, remove_cb, &rrecord) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the record value is correct */
    if (rrecord != 1889)
        TEST_ERROR

    /* Check information about record in root node */
    ninfo.depth = 2;
    ninfo.nrec  = 2;
    record      = 1890; /* Left record in root node */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(bt2, &nrec) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the # of records is correct */
    if (nrec != (INSERT_SPLIT_ROOT_NREC * 59) - 3)
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Check information about record in root node */
    ninfo.depth = 2;
    ninfo.nrec  = 2;
    record      = 2834; /* Right record in root node */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    record  = 2834;
    rrecord = HSIZET_MAX;
    if (H5B2_remove(bt2, &record, remove_cb, &rrecord) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the record value is correct */
    if (rrecord != 2834)
        TEST_ERROR

    /* Check information about record in root node */
    ninfo.depth = 2;
    ninfo.nrec  = 2;
    record      = 2835; /* Right record in root node */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(bt2, &nrec) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the # of records is correct */
    if (nrec != (INSERT_SPLIT_ROOT_NREC * 59) - 4)
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    /* Close file */
    if (H5Fclose(file) < 0)
        TEST_ERROR

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (bt2)
            H5B2_close(bt2);
        H5Fclose(file);
    }
    H5E_END_TRY;
    return 1;
} /* test_remove_level2_promote() */

/*-------------------------------------------------------------------------
 * Function:    test_remove_level2_promote_2internal_redistrib
 *
 * Purpose:    Basic tests for the B-tree v2 code
 *
 * Return:    Success:    0
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Monday, March  7, 2005
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_remove_level2_promote_2internal_redistrib(hid_t fapl, const H5B2_create_t *cparam,
                                               const bt2_test_param_t *tparam)
{
    hid_t                 file = -1;   /* File ID */
    H5F_t *               f    = NULL; /* Internal file object pointer */
    H5B2_t *              bt2  = NULL; /* v2 B-tree wrapper */
    haddr_t               bt2_addr;    /* Address of B-tree created */
    hsize_t               record;      /* Record to insert into tree */
    hsize_t               rrecord;     /* Record to remove from tree */
    hsize_t               nrec;        /* Number of records in B-tree */
    haddr_t               root_addr;   /* Address of root of B-tree created */
    H5B2_node_info_test_t ninfo;       /* B-tree node info */
    unsigned              u;           /* Local index variable */

    TESTING("B-tree remove: promote from right internal of level-2 B-tree w/redistrib");

    /* Create the file for the test */
    if (create_file(&file, &f, fapl) < 0)
        TEST_ERROR

    /* Create the v2 B-tree & get its address */
    if (create_btree(f, cparam, &bt2, &bt2_addr) < 0)
        TEST_ERROR

    /* Create level-2 B-tree with 3 internal nodes */
    for (u = 0; u < ((INSERT_SPLIT_ROOT_NREC * 59) + 1); u++) {
        record = u;
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check record values in root of B-tree */
    record = 1889; /* Left record in root node */
    if (check_node_depth(bt2, &record, (unsigned)2) < 0)
        TEST_ERROR
    ninfo.depth = 2;
    ninfo.nrec  = 2;
    record      = 2834; /* Right record in root node */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(bt2, &nrec) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the # of records is correct */
    if (nrec != ((INSERT_SPLIT_ROOT_NREC * 59) + 1))
        TEST_ERROR

    /* Query the address of the root node in the B-tree */
    if (H5B2__get_root_addr_test(bt2, &root_addr) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the address of the root node is defined */
    if (!H5F_addr_defined(root_addr))
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Check status of B-tree */
    ninfo.depth = 1;
    ninfo.nrec  = 14;
    record      = 3685; /* Right-most record in root node */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    /* Attempt to remove record from right internal node of a level-2 B-tree to force promotion
     * w/redistribution */
    for (u = 0; u < 8; u++) {
        record  = ((INSERT_SPLIT_ROOT_NREC * 59) + 1) - (u + 1);
        rrecord = HSIZET_MAX;
        if (H5B2_remove(bt2, &record, remove_cb, &rrecord) < 0)
            FAIL_STACK_ERROR

        /* Make certain that the record value is correct */
        if (rrecord != (((INSERT_SPLIT_ROOT_NREC * 59) + 1) - (u + 1)))
            TEST_ERROR

        /* Query the number of records in the B-tree */
        if (H5B2_get_nrec(bt2, &nrec) < 0)
            FAIL_STACK_ERROR

        /* Make certain that the # of records is correct */
        if (nrec != (((INSERT_SPLIT_ROOT_NREC * 59) + 1) - (u + 1)))
            TEST_ERROR
    } /* end for */

    record  = 3685;
    rrecord = HSIZET_MAX;
    if (H5B2_remove(bt2, &record, remove_cb, &rrecord) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the record value is correct */
    if (rrecord != 3685)
        TEST_ERROR

    /* Check status of B-tree */
    ninfo.depth = 1;
    ninfo.nrec  = 14;
    record      = 3681; /* Right-most record in root node */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(bt2, &nrec) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the # of records is correct */
    if (nrec != (INSERT_SPLIT_ROOT_NREC * 59) - 8)
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    /* Close file */
    if (H5Fclose(file) < 0)
        TEST_ERROR

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (bt2)
            H5B2_close(bt2);
        H5Fclose(file);
    }
    H5E_END_TRY;
    return 1;
} /* test_remove_level2_promote_2internal_redistrib() */

/*-------------------------------------------------------------------------
 * Function:    test_remove_level2_3promote_internal_redistrib
 *
 * Purpose:    Basic tests for the B-tree v2 code
 *
 * Return:    Success:    0
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Monday, March  7, 2005
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_remove_level2_promote_3internal_redistrib(hid_t fapl, const H5B2_create_t *cparam,
                                               const bt2_test_param_t *tparam)
{
    hid_t                 file = -1;   /* File ID */
    H5F_t *               f    = NULL; /* Internal file object pointer */
    H5B2_t *              bt2  = NULL; /* v2 B-tree wrapper */
    haddr_t               bt2_addr;    /* Address of B-tree created */
    hsize_t               record;      /* Record to insert into tree */
    hsize_t               rrecord;     /* Record to remove from tree */
    hsize_t               nrec;        /* Number of records in B-tree */
    haddr_t               root_addr;   /* Address of root of B-tree created */
    H5B2_node_info_test_t ninfo;       /* B-tree node info */
    unsigned              u;           /* Local index variable */

    TESTING("B-tree remove: promote from left internal of level-2 B-tree w/redistrib");

    /* Create the file for the test */
    if (create_file(&file, &f, fapl) < 0)
        TEST_ERROR

    /* Create the v2 B-tree & get its address */
    if (create_btree(f, cparam, &bt2, &bt2_addr) < 0)
        TEST_ERROR

    /* Create level-2 B-tree with 3 internal nodes */
    for (u = 0; u < ((INSERT_SPLIT_ROOT_NREC * 59) + 1); u++) {
        record = u;
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check record values in root of B-tree */
    record = 1889; /* Left record in root node */
    if (check_node_depth(bt2, &record, (unsigned)2) < 0)
        TEST_ERROR
    ninfo.depth = 2;
    ninfo.nrec  = 2;
    record      = 2834; /* Right record in root node */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(bt2, &nrec) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the # of records is correct */
    if (nrec != ((INSERT_SPLIT_ROOT_NREC * 59) + 1))
        TEST_ERROR

    /* Query the address of the root node in the B-tree */
    if (H5B2__get_root_addr_test(bt2, &root_addr) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the address of the root node is defined */
    if (!H5F_addr_defined(root_addr))
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Check status of B-tree */
    ninfo.depth = 1;
    ninfo.nrec  = 29;
    record      = 62; /* Left-most record in left node */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    /* Attempt to remove record from left internal node of a level-2 B-tree to force promotion
     * w/redistribution */
    for (u = 0; u < 38; u++) {
        record  = 63 + u;
        rrecord = HSIZET_MAX;
        if (H5B2_remove(bt2, &record, remove_cb, &rrecord) < 0)
            FAIL_STACK_ERROR

        /* Make certain that the record value is correct */
        if (rrecord != (63 + u))
            TEST_ERROR

        /* Query the number of records in the B-tree */
        if (H5B2_get_nrec(bt2, &nrec) < 0)
            FAIL_STACK_ERROR

        /* Make certain that the # of records is correct */
        if (nrec != (((INSERT_SPLIT_ROOT_NREC * 59) + 1) - (u + 1)))
            TEST_ERROR
    } /* end for */

    record  = 62;
    rrecord = HSIZET_MAX;
    if (H5B2_remove(bt2, &record, remove_cb, &rrecord) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the record value is correct */
    if (rrecord != 62)
        TEST_ERROR

    /* Check status of B-tree */
    ninfo.depth = 1;
    ninfo.nrec  = 29;
    record      = 49; /* Left-most record in left node */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(bt2, &nrec) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the # of records is correct */
    if (nrec != (INSERT_SPLIT_ROOT_NREC * 59) - 38)
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    /* Close file */
    if (H5Fclose(file) < 0)
        TEST_ERROR

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (bt2)
            H5B2_close(bt2);
        H5Fclose(file);
    }
    H5E_END_TRY;
    return 1;
} /* test_remove_level2_promote_3internal_redistrib() */

/*-------------------------------------------------------------------------
 * Function:    test_remove_level2_promote_2internal_merge
 *
 * Purpose:    Basic tests for the B-tree v2 code
 *
 * Return:    Success:    0
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Monday, March  7, 2005
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_remove_level2_promote_2internal_merge(hid_t fapl, const H5B2_create_t *cparam,
                                           const bt2_test_param_t *tparam)
{
    hid_t                 file = -1;   /* File ID */
    H5F_t *               f    = NULL; /* Internal file object pointer */
    H5B2_t *              bt2  = NULL; /* v2 B-tree wrapper */
    haddr_t               bt2_addr;    /* Address of B-tree created */
    hsize_t               record;      /* Record to insert into tree */
    hsize_t               rrecord;     /* Record to remove from tree */
    hsize_t               nrec;        /* Number of records in B-tree */
    haddr_t               root_addr;   /* Address of root of B-tree created */
    H5B2_node_info_test_t ninfo;       /* B-tree node info */
    unsigned              u;           /* Local index variable */

    TESTING("B-tree remove: promote from right internal of level-2 B-tree w/merge");

    /* Create the file for the test */
    if (create_file(&file, &f, fapl) < 0)
        TEST_ERROR

    /* Create the v2 B-tree & get its address */
    if (create_btree(f, cparam, &bt2, &bt2_addr) < 0)
        TEST_ERROR

    /* Create level-2 B-tree with 3 internal nodes */
    for (u = 0; u < ((INSERT_SPLIT_ROOT_NREC * 59) + 1); u++) {
        record = u;
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check record values in root of B-tree */
    record = 1889; /* Left record in root node */
    if (check_node_depth(bt2, &record, (unsigned)2) < 0)
        TEST_ERROR
    ninfo.depth = 2;
    ninfo.nrec  = 2;
    record      = 2834; /* Right record in root node */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(bt2, &nrec) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the # of records is correct */
    if (nrec != ((INSERT_SPLIT_ROOT_NREC * 59) + 1))
        TEST_ERROR

    /* Query the address of the root node in the B-tree */
    if (H5B2__get_root_addr_test(bt2, &root_addr) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the address of the root node is defined */
    if (!H5F_addr_defined(root_addr))
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Check information about record in right internal node */
    ninfo.depth = 1;
    ninfo.nrec  = 14;
    record      = 3685; /* Right-most record in right internal node */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    /* Attempt to remove record from right internal node of a level-2 B-tree to force promotion
     * w/redistribution */
    for (u = 0; u < 15; u++) {
        record  = ((INSERT_SPLIT_ROOT_NREC * 59) + 1) - (u + 1);
        rrecord = HSIZET_MAX;
        if (H5B2_remove(bt2, &record, remove_cb, &rrecord) < 0)
            FAIL_STACK_ERROR

        /* Make certain that the record value is correct */
        if (rrecord != (((INSERT_SPLIT_ROOT_NREC * 59) + 1) - (u + 1)))
            TEST_ERROR

        /* Query the number of records in the B-tree */
        if (H5B2_get_nrec(bt2, &nrec) < 0)
            FAIL_STACK_ERROR

        /* Make certain that the # of records is correct */
        if (nrec != (((INSERT_SPLIT_ROOT_NREC * 59) + 1) - (u + 1)))
            TEST_ERROR
    } /* end for */

    /* Force merge by promoting current right-most record */
    record  = 3678;
    rrecord = HSIZET_MAX;
    if (H5B2_remove(bt2, &record, remove_cb, &rrecord) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the record value is correct */
    if (rrecord != 3678)
        TEST_ERROR

    /* Check information about record in right internal node */
    ninfo.depth = 1;
    ninfo.nrec  = 13;
    record      = 3653; /* Right-most record in right internal node (now) */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(bt2, &nrec) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the # of records is correct */
    if (nrec != (INSERT_SPLIT_ROOT_NREC * 59) - 15)
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    /* Close file */
    if (H5Fclose(file) < 0)
        TEST_ERROR

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (bt2)
            H5B2_close(bt2);
        H5Fclose(file);
    }
    H5E_END_TRY;
    return 1;
} /* test_remove_level2_promote_2internal_merge() */

/*-------------------------------------------------------------------------
 * Function:    test_remove_level2_3promote_internal_merge
 *
 * Purpose:    Basic tests for the B-tree v2 code
 *
 * Return:    Success:    0
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Monday, March  7, 2005
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_remove_level2_promote_3internal_merge(hid_t fapl, const H5B2_create_t *cparam,
                                           const bt2_test_param_t *tparam)
{
    hid_t                 file = -1;   /* File ID */
    H5F_t *               f    = NULL; /* Internal file object pointer */
    H5B2_t *              bt2  = NULL; /* v2 B-tree wrapper */
    haddr_t               bt2_addr;    /* Address of B-tree created */
    hsize_t               record;      /* Record to insert into tree */
    hsize_t               rrecord;     /* Record to remove from tree */
    hsize_t               nrec;        /* Number of records in B-tree */
    haddr_t               root_addr;   /* Address of root of B-tree created */
    H5B2_node_info_test_t ninfo;       /* B-tree node info */
    unsigned              u;           /* Local index variable */

    TESTING("B-tree remove: promote from middle internal of level-2 B-tree w/merge");

    /* Create the file for the test */
    if (create_file(&file, &f, fapl) < 0)
        TEST_ERROR

    /* Create the v2 B-tree & get its address */
    if (create_btree(f, cparam, &bt2, &bt2_addr) < 0)
        TEST_ERROR

    /* Create level-2 B-tree with 3 internal nodes */
    for (u = 0; u < ((INSERT_SPLIT_ROOT_NREC * 59) + 1); u++) {
        record = u;
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check record values in root of B-tree */
    record = 1889; /* Left record in root node */
    if (check_node_depth(bt2, &record, (unsigned)2) < 0)
        TEST_ERROR
    ninfo.depth = 2;
    ninfo.nrec  = 2;
    record      = 2834; /* Right record in root node */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(bt2, &nrec) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the # of records is correct */
    if (nrec != ((INSERT_SPLIT_ROOT_NREC * 59) + 1))
        TEST_ERROR

    /* Query the address of the root node in the B-tree */
    if (H5B2__get_root_addr_test(bt2, &root_addr) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the address of the root node is defined */
    if (!H5F_addr_defined(root_addr))
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Check information about record in left internal node */
    ninfo.depth = 1;
    ninfo.nrec  = 29;
    record      = 62; /* Left-most record in left internal node */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    /* Attempt to remove record from left internal node of a level-2 B-tree to force promotion
     * w/redistribution */
    for (u = 0; u < 112; u++) {
        record  = 48 + u;
        rrecord = HSIZET_MAX;
        if (H5B2_remove(bt2, &record, remove_cb, &rrecord) < 0)
            FAIL_STACK_ERROR

        /* Make certain that the record value is correct */
        if (rrecord != (48 + u))
            TEST_ERROR

        /* Query the number of records in the B-tree */
        if (H5B2_get_nrec(bt2, &nrec) < 0)
            FAIL_STACK_ERROR

        /* Make certain that the # of records is correct */
        if (nrec != (((INSERT_SPLIT_ROOT_NREC * 59) + 1) - (u + 1)))
            TEST_ERROR
    } /* end for */

    /* Force merge of left-most internal nodes by promotion */
    record  = 25;
    rrecord = HSIZET_MAX;
    if (H5B2_remove(bt2, &record, remove_cb, &rrecord) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the record value is correct */
    if (rrecord != 25)
        TEST_ERROR

    /* Check information about record in left internal node */
    ninfo.depth = 1;
    ninfo.nrec  = 28;
    record      = 37; /* Left-most record in left internal node (now) */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(bt2, &nrec) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the # of records is correct */
    if (nrec != (INSERT_SPLIT_ROOT_NREC * 59) - 112)
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    /* Close file */
    if (H5Fclose(file) < 0)
        TEST_ERROR

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (bt2)
            H5B2_close(bt2);
        H5Fclose(file);
    }
    H5E_END_TRY;
    return 1;
} /* test_remove_level2_promote_3internal_merge() */

/*-------------------------------------------------------------------------
 * Function:    test_remove_level2_2internal_merge_left
 *
 * Purpose:    Basic tests for the B-tree v2 code
 *
 * Return:    Success:    0
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Tuesday, March  8, 2005
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_remove_level2_2internal_merge_left(hid_t fapl, const H5B2_create_t *cparam,
                                        const bt2_test_param_t *tparam)
{
    hid_t                 file = -1;   /* File ID */
    H5F_t *               f    = NULL; /* Internal file object pointer */
    H5B2_t *              bt2  = NULL; /* v2 B-tree wrapper */
    haddr_t               bt2_addr;    /* Address of B-tree created */
    hsize_t               record;      /* Record to insert into tree */
    hsize_t               rrecord;     /* Record to remove from tree */
    hsize_t               nrec;        /* Number of records in B-tree */
    haddr_t               root_addr;   /* Address of root of B-tree created */
    H5B2_node_info_test_t ninfo;       /* B-tree node info */
    unsigned              u;           /* Local index variable */

    TESTING("B-tree remove: merge 2 internal nodes to 1 in level-2 B-tree (l->r)");

    /* Create the file for the test */
    if (create_file(&file, &f, fapl) < 0)
        TEST_ERROR

    /* Create the v2 B-tree & get its address */
    if (create_btree(f, cparam, &bt2, &bt2_addr) < 0)
        TEST_ERROR

    /* Create level-2 B-tree with 3 internal nodes */
    for (u = 0; u < ((INSERT_SPLIT_ROOT_NREC * 59) + 1); u++) {
        record = u;
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check record values in root of B-tree */
    record = 1889; /* Left record in root node */
    if (check_node_depth(bt2, &record, (unsigned)2) < 0)
        TEST_ERROR
    ninfo.depth = 2;
    ninfo.nrec  = 2;
    record      = 2834; /* Right record in root node */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(bt2, &nrec) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the # of records is correct */
    if (nrec != ((INSERT_SPLIT_ROOT_NREC * 59) + 1))
        TEST_ERROR

    /* Query the address of the root node in the B-tree */
    if (H5B2__get_root_addr_test(bt2, &root_addr) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the address of the root node is defined */
    if (!H5F_addr_defined(root_addr))
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Attempt to remove records from a level-2 B-tree to force 2 internal nodes to merge */
    for (u = 0; u < ((INSERT_SPLIT_ROOT_NREC * 21) + 15); u++) {
        record  = u;
        rrecord = HSIZET_MAX;
        if (H5B2_remove(bt2, &record, remove_cb, &rrecord) < 0)
            FAIL_STACK_ERROR

        /* Make certain that the record value is correct */
        if (rrecord != u)
            TEST_ERROR

        /* Query the number of records in the B-tree */
        if (H5B2_get_nrec(bt2, &nrec) < 0)
            FAIL_STACK_ERROR

        /* Make certain that the # of records is correct */
        if (nrec != (((INSERT_SPLIT_ROOT_NREC * 59) + 1) - (u + 1)))
            TEST_ERROR
    } /* end for */

    /* Check status of B-tree */
    ninfo.depth = 2;
    ninfo.nrec  = 1;
    record      = 2834; /* Middle record in root node */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    /* Close file */
    if (H5Fclose(file) < 0)
        TEST_ERROR

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (bt2)
            H5B2_close(bt2);
        H5Fclose(file);
    }
    H5E_END_TRY;
    return 1;
} /* test_remove_level2_2internal_merge_left() */

/*-------------------------------------------------------------------------
 * Function:    test_remove_level2_2internal_merge_right
 *
 * Purpose:    Basic tests for the B-tree v2 code
 *
 * Return:    Success:    0
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Tuesday, March  8, 2005
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_remove_level2_2internal_merge_right(hid_t fapl, const H5B2_create_t *cparam,
                                         const bt2_test_param_t *tparam)
{
    hid_t                 file = -1;   /* File ID */
    H5F_t *               f    = NULL; /* Internal file object pointer */
    H5B2_t *              bt2  = NULL; /* v2 B-tree wrapper */
    haddr_t               bt2_addr;    /* Address of B-tree created */
    hsize_t               record;      /* Record to insert into tree */
    hsize_t               rrecord;     /* Record to remove from tree */
    hsize_t               nrec;        /* Number of records in B-tree */
    haddr_t               root_addr;   /* Address of root of B-tree created */
    H5B2_node_info_test_t ninfo;       /* B-tree node info */
    unsigned              u;           /* Local index variable */

    TESTING("B-tree remove: merge 2 internal nodes to 1 in level-2 B-tree (r->l)");

    /* Create the file for the test */
    if (create_file(&file, &f, fapl) < 0)
        TEST_ERROR

    /* Create the v2 B-tree & get its address */
    if (create_btree(f, cparam, &bt2, &bt2_addr) < 0)
        TEST_ERROR

    /* Create level-2 B-tree with 3 internal nodes */
    for (u = 0; u < ((INSERT_SPLIT_ROOT_NREC * 59) + 1); u++) {
        record = u;
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check record values in root of B-tree */
    record = 1889; /* Left record in root node */
    if (check_node_depth(bt2, &record, (unsigned)2) < 0)
        TEST_ERROR
    ninfo.depth = 2;
    ninfo.nrec  = 2;
    record      = 2834; /* Right record in root node */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(bt2, &nrec) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the # of records is correct */
    if (nrec != ((INSERT_SPLIT_ROOT_NREC * 59) + 1))
        TEST_ERROR

    /* Query the address of the root node in the B-tree */
    if (H5B2__get_root_addr_test(bt2, &root_addr) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the address of the root node is defined */
    if (!H5F_addr_defined(root_addr))
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Attempt to remove records from a level-2 B-tree to force 2 internal nodes to merge */
    for (u = 0; u < ((INSERT_SPLIT_ROOT_NREC * 5) + 17); u++) {
        record  = ((INSERT_SPLIT_ROOT_NREC * 59) + 1) - (u + 1);
        rrecord = HSIZET_MAX;
        if (H5B2_remove(bt2, &record, remove_cb, &rrecord) < 0)
            FAIL_STACK_ERROR

        /* Make certain that the record value is correct */
        if (rrecord != (((INSERT_SPLIT_ROOT_NREC * 59) + 1) - (u + 1)))
            TEST_ERROR

        /* Query the number of records in the B-tree */
        if (H5B2_get_nrec(bt2, &nrec) < 0)
            FAIL_STACK_ERROR

        /* Make certain that the # of records is correct */
        if (nrec != (((INSERT_SPLIT_ROOT_NREC * 59) + 1) - (u + 1)))
            TEST_ERROR
    } /* end for */

    /* Check status of B-tree */
    ninfo.depth = 2;
    ninfo.nrec  = 1;
    record      = 1889; /* Middle record in root node */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    /* Close file */
    if (H5Fclose(file) < 0)
        TEST_ERROR

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (bt2)
            H5B2_close(bt2);
        H5Fclose(file);
    }
    H5E_END_TRY;
    return 1;
} /* test_remove_level2_2internal_merge_right() */

/*-------------------------------------------------------------------------
 * Function:    test_remove_level2_3internal_merge
 *
 * Purpose:    Basic tests for the B-tree v2 code
 *
 * Return:    Success:    0
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Tuesday, March  8, 2005
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_remove_level2_3internal_merge(hid_t fapl, const H5B2_create_t *cparam, const bt2_test_param_t *tparam)
{
    hid_t                 file = -1;   /* File ID */
    H5F_t *               f    = NULL; /* Internal file object pointer */
    H5B2_t *              bt2  = NULL; /* v2 B-tree wrapper */
    haddr_t               bt2_addr;    /* Address of B-tree created */
    hsize_t               record;      /* Record to insert into tree */
    hsize_t               rrecord;     /* Record to remove from tree */
    hsize_t               nrec;        /* Number of records in B-tree */
    haddr_t               root_addr;   /* Address of root of B-tree created */
    H5B2_node_info_test_t ninfo;       /* B-tree node info */
    unsigned              u;           /* Local index variable */

    TESTING("B-tree remove: merge 3 internal nodes to 2 in level-2 B-tree");

    /* Create the file for the test */
    if (create_file(&file, &f, fapl) < 0)
        TEST_ERROR

    /* Create the v2 B-tree & get its address */
    if (create_btree(f, cparam, &bt2, &bt2_addr) < 0)
        TEST_ERROR

    /* Create level-2 B-tree with 3 internal nodes */
    for (u = 0; u < ((INSERT_SPLIT_ROOT_NREC * 59) + 1); u++) {
        record = u;
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check record values in root of B-tree */
    record = 1889; /* Left record in root node */
    if (check_node_depth(bt2, &record, (unsigned)2) < 0)
        TEST_ERROR
    ninfo.depth = 2;
    ninfo.nrec  = 2;
    record      = 2834; /* Right record in root node */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(bt2, &nrec) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the # of records is correct */
    if (nrec != ((INSERT_SPLIT_ROOT_NREC * 59) + 1))
        TEST_ERROR

    /* Query the address of the root node in the B-tree */
    if (H5B2__get_root_addr_test(bt2, &root_addr) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the address of the root node is defined */
    if (!H5F_addr_defined(root_addr))
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Attempt to remove record from middle internal node of a level-2 B-tree to force promotion
     * w/redistribution */
    for (u = 0; u < ((INSERT_SPLIT_ROOT_NREC * 23) + 15); u++) {
        record  = (INSERT_SPLIT_ROOT_NREC * 20) + u;
        rrecord = HSIZET_MAX;
        if (H5B2_remove(bt2, &record, remove_cb, &rrecord) < 0)
            FAIL_STACK_ERROR

        /* Make certain that the record value is correct */
        if (rrecord != ((INSERT_SPLIT_ROOT_NREC * 20) + u))
            TEST_ERROR

        /* Query the number of records in the B-tree */
        if (H5B2_get_nrec(bt2, &nrec) < 0)
            FAIL_STACK_ERROR

        /* Make certain that the # of records is correct */
        if (nrec != (((INSERT_SPLIT_ROOT_NREC * 59) + 1) - (u + 1)))
            TEST_ERROR
    } /* end for */

    /* Check status of B-tree */
    ninfo.depth = 2;
    ninfo.nrec  = 1;
    record      = 1196; /* Right record in root node */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    /* Close file */
    if (H5Fclose(file) < 0)
        TEST_ERROR

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (bt2)
            H5B2_close(bt2);
        H5Fclose(file);
    }
    H5E_END_TRY;
    return 1;
} /* test_remove_level2_3internal_merge() */

/*-------------------------------------------------------------------------
 * Function:    test_remove_level2_collapse_right
 *
 * Purpose:    Basic tests for the B-tree v2 code
 *
 * Return:    Success:    0
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Tuesday, March  8, 2005
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_remove_level2_collapse_right(hid_t fapl, const H5B2_create_t *cparam, const bt2_test_param_t *tparam)
{
    hid_t                 file = -1;   /* File ID */
    H5F_t *               f    = NULL; /* Internal file object pointer */
    H5B2_t *              bt2  = NULL; /* v2 B-tree wrapper */
    haddr_t               bt2_addr;    /* Address of B-tree created */
    hsize_t               record;      /* Record to insert into tree */
    hsize_t               rrecord;     /* Record to remove from tree */
    hsize_t               nrec;        /* Number of records in B-tree */
    haddr_t               root_addr;   /* Address of root of B-tree created */
    H5B2_stat_t           bt2_stat;    /* Statistics about B-tree created */
    H5B2_node_info_test_t ninfo;       /* B-tree node info */
    unsigned              u;           /* Local index variable */

    TESTING("B-tree remove: collapse level-2 B-tree back to level-1 (r->l)");

    /* Create the file for the test */
    if (create_file(&file, &f, fapl) < 0)
        TEST_ERROR

    /* Create the v2 B-tree & get its address */
    if (create_btree(f, cparam, &bt2, &bt2_addr) < 0)
        TEST_ERROR

    /* Create level-2 B-tree with 3 internal nodes */
    for (u = 0; u < ((INSERT_SPLIT_ROOT_NREC * 59) + 1); u++) {
        record = u;
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check record values in root of B-tree */
    record = 1889; /* Left record in root node */
    if (check_node_depth(bt2, &record, (unsigned)2) < 0)
        TEST_ERROR
    ninfo.depth = 2;
    ninfo.nrec  = 2;
    record      = 2834; /* Right record in root node */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(bt2, &nrec) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the # of records is correct */
    if (nrec != ((INSERT_SPLIT_ROOT_NREC * 59) + 1))
        TEST_ERROR

    /* Query the address of the root node in the B-tree */
    if (H5B2__get_root_addr_test(bt2, &root_addr) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the address of the root node is defined */
    if (!H5F_addr_defined(root_addr))
        TEST_ERROR

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Attempt to remove records from a level-2 B-tree to force back to level-1 */
    for (u = 0; u < (INSERT_SPLIT_ROOT_NREC * 34) + 17; u++) {
        record  = ((INSERT_SPLIT_ROOT_NREC * 59) + 1) - (u + 1);
        rrecord = HSIZET_MAX;
        if (H5B2_remove(bt2, &record, remove_cb, &rrecord) < 0)
            FAIL_STACK_ERROR

        /* Make certain that the record value is correct */
        if (record != (((INSERT_SPLIT_ROOT_NREC * 59) + 1) - (u + 1)))
            TEST_ERROR

        /* Query the number of records in the B-tree */
        if (H5B2_get_nrec(bt2, &nrec) < 0)
            FAIL_STACK_ERROR

        /* Make certain that the # of records is correct */
        if (nrec != (((INSERT_SPLIT_ROOT_NREC * 59) + 1) - (u + 1)))
            TEST_ERROR
    } /* end for */

    /* Check up on B-tree */
    if (H5B2_stat_info(bt2, &bt2_stat) < 0)
        FAIL_STACK_ERROR
    if (bt2_stat.depth != 1)
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    /* Close file */
    if (H5Fclose(file) < 0)
        TEST_ERROR

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (bt2)
            H5B2_close(bt2);
        H5Fclose(file);
    }
    H5E_END_TRY;
    return 1;
} /* test_remove_level2_collapse_right() */

/*-------------------------------------------------------------------------
 * Function:    gen_l4_btree2
 *
 * Purpose:    Generate a level-4 v2 B-tree for testing.
 *
 * Return:    Success:    0
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Tuesday, October 14, 2008
 *
 *-------------------------------------------------------------------------
 */
static unsigned
gen_l4_btree2(const char *filename, hid_t fapl, const H5B2_create_t *cparam, haddr_t *bt2_addr,
              const hsize_t *records)
{
    hid_t       file = -1;   /* File ID */
    H5F_t *     f    = NULL; /* Internal file object pointer */
    H5B2_t *    bt2  = NULL; /* v2 B-tree wrapper */
    hsize_t     record;      /* Record to insert into tree */
    unsigned    u;           /* Local index variable */
    H5B2_stat_t bt2_stat;    /* Statistics about B-tree created */

    /* Create the file to work on */
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        STACK_ERROR

    /* Get a pointer to the internal file object */
    if (NULL == (f = (H5F_t *)H5VL_object(file)))
        STACK_ERROR

    /* Ignore metadata tags in the file's cache */
    if (H5AC_ignore_tags(f) < 0)
        STACK_ERROR

    /* Create the v2 B-tree & get its address */
    if (create_btree(f, cparam, &bt2, bt2_addr) < 0)
        TEST_ERROR

    /* Insert random records */
    for (u = 0; u < INSERT_MANY; u++) {
        record = records[u];
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check up on B-tree */
    if (H5B2_stat_info(bt2, &bt2_stat) < 0)
        FAIL_STACK_ERROR
    if (bt2_stat.depth != 4)
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    /* Close file */
    if (H5Fclose(file) < 0)
        STACK_ERROR

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (bt2)
            H5B2_close(bt2);
        H5Fclose(file);
    }
    H5E_END_TRY;

    return 1;
} /* gen_l4_btree2() */

/*-------------------------------------------------------------------------
 * Function:    test_remove_lots
 *
 * Purpose:    Basic tests for the B-tree v2 code.  This test inserts many
 *              records in random order, enough to make at a level 4 B-tree
 *              and then removes them all, by record and by index.
 *
 * Return:    Success:    0
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Tuesday, March  8, 2005
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_remove_lots(const char *env_h5_drvr, hid_t fapl, const H5B2_create_t *cparam)
{
    hid_t     file = -1;        /* File ID */
    char      filename[1024];   /* Filename to use */
    H5F_t *   f  = NULL;        /* Internal file object pointer */
    int       fd = -1;          /* File descriptor */
    h5_stat_t sb;               /* Stat buffer for file */
    void *    file_data = NULL; /* Copy of file data */
    H5B2_t *  bt2       = NULL; /* v2 B-tree wrapper */
    haddr_t   bt2_addr;         /* Address of B-tree created */
    hsize_t   record;           /* Record to insert into tree */
    hsize_t   rrecord;          /* Record to remove from tree */
    haddr_t   root_addr;        /* Address of root of B-tree created */
    time_t    curr_time;        /* Current time, for seeding random number generator */
    hsize_t * records;          /* Record #'s for random insertion */
    unsigned  u;                /* Local index variable */
    unsigned  rem_idx;          /* Location to remove */
    hsize_t   nrec;             /* Number of records in B-tree */
    hbool_t   single_file_vfd;  /* Whether VFD used stores data in a single file */

    /* Initialize random number seed */
    curr_time = HDtime(NULL);
#if 0
curr_time = 1163537969;
HDfprintf(stderr, "curr_time = %lu\n", (unsigned long)curr_time);
#endif
    HDsrandom((unsigned)curr_time);

    /*
     * Test removing many records into v2 B-tree
     */

    /* Allocate space for the records */
    if (NULL == (records = (hsize_t *)HDmalloc(sizeof(hsize_t) * INSERT_MANY)))
        TEST_ERROR

    /* Initialize record #'s */
    for (u = 0; u < INSERT_MANY; u++)
        records[u] = u;

    /* Shuffle record #'s */
    for (u = 0; u < INSERT_MANY; u++) {
        hsize_t  temp_rec; /* Temporary record */
        unsigned swap_idx; /* Location to swap with when shuffling */

        swap_idx          = ((unsigned)HDrandom() % (INSERT_MANY - u)) + u;
        temp_rec          = records[u];
        records[u]        = records[swap_idx];
        records[swap_idx] = temp_rec;
    } /* end for */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Generate the v2 B-tree to test */
    if (gen_l4_btree2(filename, fapl, cparam, &bt2_addr, records))
        TEST_ERROR

    /* Check for VFD which stores data in multiple files */
    single_file_vfd = (hbool_t)(HDstrcmp(env_h5_drvr, "split") != 0 && HDstrcmp(env_h5_drvr, "multi") != 0 &&
                                HDstrcmp(env_h5_drvr, "family") != 0);
    if (single_file_vfd) {
        /* Make a copy of the file in memory, in order to speed up deletion testing */

        /* Open the file just created */
        if ((fd = HDopen(filename, O_RDONLY)) < 0)
            TEST_ERROR

        /* Retrieve the file's size */
        if (HDfstat(fd, &sb) < 0)
            TEST_ERROR

        /* Allocate space for the file data */
        if (NULL == (file_data = HDmalloc((size_t)sb.st_size)))
            TEST_ERROR

        /* Read file's data into memory */
        if (HDread(fd, file_data, (size_t)sb.st_size) < (ssize_t)sb.st_size)
            TEST_ERROR

        /* Close the file */
        if (HDclose(fd) < 0)
            TEST_ERROR
        fd = -1;
    } /* end if */

    /* Print banner for this test */
    TESTING("B-tree remove: create random level 4 B-tree and delete all records in random order");

    /* Re-open the file */
    if ((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if (NULL == (f = (H5F_t *)H5VL_object(file)))
        FAIL_STACK_ERROR

    /* Ignore metadata tags in the file's cache */
    if (H5AC_ignore_tags(f) < 0)
        STACK_ERROR

    /* Re-shuffle record #'s */
    for (u = 0; u < INSERT_MANY; u++) {
        hsize_t  temp_rec; /* Temporary record */
        unsigned swap_idx; /* Location to swap with when shuffling */

        swap_idx          = ((unsigned)HDrandom() % (INSERT_MANY - u)) + u;
        temp_rec          = records[u];
        records[u]        = records[swap_idx];
        records[swap_idx] = temp_rec;
    } /* end for */

    /* Re-open v2 B-tree */
    if (NULL == (bt2 = H5B2_open(f, bt2_addr, f)))
        FAIL_STACK_ERROR

    /* Remove all records */
    for (u = 0; u < INSERT_MANY; u++) {
        record  = records[u];
        rrecord = HSIZET_MAX;
        if (H5B2_remove(bt2, &record, remove_cb, &rrecord) < 0)
            FAIL_STACK_ERROR

        /* Make certain that the record value is correct */
        if (rrecord != records[u])
            TEST_ERROR

        /* Query the number of records in the B-tree */
        if (H5B2_get_nrec(bt2, &nrec) < 0)
            FAIL_STACK_ERROR

        /* Make certain that the # of records is correct */
        if (nrec != (INSERT_MANY - (u + 1)))
            TEST_ERROR
    } /* end for */

    /* Query the address of the root node in the B-tree */
    if (H5B2__get_root_addr_test(bt2, &root_addr) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the address of the root node is defined */
    if (H5F_addr_defined(root_addr))
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    /* Close file */
    if (H5Fclose(file) < 0)
        STACK_ERROR

    PASSED();

    /* Check for VFD which stores data in multiple files */
    if (single_file_vfd) {
        /* Re-write the file's data with the copy in memory */

        /* Open the file just created */
        if ((fd = HDopen(filename, O_RDWR | O_CREAT | O_TRUNC, H5_POSIX_CREATE_MODE_RW)) < 0)
            TEST_ERROR

        /* Write file's data from memory */
        if (HDwrite(fd, file_data, (size_t)sb.st_size) < (ssize_t)sb.st_size)
            TEST_ERROR

        /* Close the file */
        if (HDclose(fd) < 0)
            TEST_ERROR
        fd = -1;
    } /* end if */
    else {
        /* Re-generate the v2 B-tree to test */
        if (gen_l4_btree2(filename, fapl, cparam, &bt2_addr, records))
            TEST_ERROR
    } /* end else */

    /* Print banner for this test */
    TESTING("B-tree remove: create random level 4 B-tree and delete all records by index, in random order");

    /* Re-open the file */
    if ((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if (NULL == (f = (H5F_t *)H5VL_object(file)))
        FAIL_STACK_ERROR

    /* Ignore metadata tags in the file's cache */
    if (H5AC_ignore_tags(f) < 0)
        STACK_ERROR

    /* Re-open v2 B-tree */
    if (NULL == (bt2 = H5B2_open(f, bt2_addr, f)))
        FAIL_STACK_ERROR

    /* Remove all records */
    for (u = 0; u < INSERT_MANY; u++) {
        /* Pick a record index to remove from randomly */
        rem_idx = ((unsigned)HDrandom() % (INSERT_MANY - u));
        rrecord = HSIZET_MAX;

        /* Remove random record */
        if (H5B2_remove_by_idx(bt2, H5_ITER_INC, (hsize_t)rem_idx, remove_cb, &rrecord) < 0)
            FAIL_STACK_ERROR

        /* Make certain that the record value is correct */
        if (rrecord >= INSERT_MANY)
            TEST_ERROR

        /* Query the number of records in the B-tree */
        if (H5B2_get_nrec(bt2, &nrec) < 0)
            FAIL_STACK_ERROR

        /* Make certain that the # of records is correct */
        if (nrec != (INSERT_MANY - (u + 1)))
            TEST_ERROR
    } /* end for */

    /* Query the address of the root node in the B-tree */
    if (H5B2__get_root_addr_test(bt2, &root_addr) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the address of the root node is defined */
    if (H5F_addr_defined(root_addr))
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    /* Close file */
    if (H5Fclose(file) < 0)
        STACK_ERROR

    PASSED();

    /* Check for VFD which stores data in multiple files */
    if (single_file_vfd) {
        /* Re-write the file's data with the copy in memory */

        /* Open the file just created */
        if ((fd = HDopen(filename, O_RDWR | O_CREAT | O_TRUNC, H5_POSIX_CREATE_MODE_RW)) < 0)
            TEST_ERROR

        /* Write file's data from memory */
        if (HDwrite(fd, file_data, (size_t)sb.st_size) < (ssize_t)sb.st_size)
            TEST_ERROR

        /* Close the file */
        if (HDclose(fd) < 0)
            TEST_ERROR
        fd = -1;
    } /* end if */
    else {
        /* Re-generate the v2 B-tree to test */
        if (gen_l4_btree2(filename, fapl, cparam, &bt2_addr, records))
            TEST_ERROR
    } /* end else */

    /* Print banner for this test */
    TESTING(
        "B-tree remove: create random level 4 B-tree and delete all records by index, in increasing order");

    /* Re-open the file */
    if ((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if (NULL == (f = (H5F_t *)H5VL_object(file)))
        FAIL_STACK_ERROR

    /* Ignore metadata tags in the file's cache */
    if (H5AC_ignore_tags(f) < 0)
        STACK_ERROR

    /* Re-open v2 B-tree */
    if (NULL == (bt2 = H5B2_open(f, bt2_addr, f)))
        FAIL_STACK_ERROR

    /* Remove all records */
    for (u = 0; u < INSERT_MANY; u++) {
        /* Remove first record */
        rrecord = HSIZET_MAX;
        if (H5B2_remove_by_idx(bt2, H5_ITER_INC, (hsize_t)0, remove_cb, &rrecord) < 0)
            FAIL_STACK_ERROR

        /* Make certain that the record value is correct */
        if (rrecord != u)
            TEST_ERROR

        /* Query the number of records in the B-tree */
        if (H5B2_get_nrec(bt2, &nrec) < 0)
            FAIL_STACK_ERROR

        /* Make certain that the # of records is correct */
        if (nrec != (INSERT_MANY - (u + 1)))
            TEST_ERROR
    } /* end for */

    /* Query the address of the root node in the B-tree */
    if (H5B2__get_root_addr_test(bt2, &root_addr) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the address of the root node is defined */
    if (H5F_addr_defined(root_addr))
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    /* Close file */
    if (H5Fclose(file) < 0)
        STACK_ERROR

    PASSED();

    /* Check for VFD which stores data in multiple files */
    if (single_file_vfd) {
        /* Re-write the file's data with the copy in memory */

        /* Open the file just created */
        if ((fd = HDopen(filename, O_RDWR | O_CREAT | O_TRUNC, H5_POSIX_CREATE_MODE_RW)) < 0)
            TEST_ERROR

        /* Write file's data from memory */
        if (HDwrite(fd, file_data, (size_t)sb.st_size) < (ssize_t)sb.st_size)
            TEST_ERROR

        /* Close the file */
        if (HDclose(fd) < 0)
            TEST_ERROR
        fd = -1;
    } /* end if */
    else {
        /* Re-generate the v2 B-tree to test */
        if (gen_l4_btree2(filename, fapl, cparam, &bt2_addr, records))
            TEST_ERROR
    } /* end else */

    /* Print banner for this test */
    TESTING(
        "B-tree remove: create random level 4 B-tree and delete all records by index, in decreasing order");

    /* Re-open the file */
    if ((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if (NULL == (f = (H5F_t *)H5VL_object(file)))
        FAIL_STACK_ERROR

    /* Ignore metadata tags in the file's cache */
    if (H5AC_ignore_tags(f) < 0)
        STACK_ERROR

    /* Re-open v2 B-tree */
    if (NULL == (bt2 = H5B2_open(f, bt2_addr, f)))
        FAIL_STACK_ERROR

    /* Remove all records */
    for (u = 0; u < INSERT_MANY; u++) {
        /* Remove last record */
        rrecord = HSIZET_MAX;
        if (H5B2_remove_by_idx(bt2, H5_ITER_DEC, (hsize_t)0, remove_cb, &rrecord) < 0)
            FAIL_STACK_ERROR

        /* Make certain that the record value is correct */
        if (rrecord != (INSERT_MANY - (u + 1)))
            TEST_ERROR

        /* Query the number of records in the B-tree */
        if (H5B2_get_nrec(bt2, &nrec) < 0)
            FAIL_STACK_ERROR

        /* Make certain that the # of records is correct */
        if (nrec != (INSERT_MANY - (u + 1)))
            TEST_ERROR
    } /* end for */

    /* Query the address of the root node in the B-tree */
    if (H5B2__get_root_addr_test(bt2, &root_addr) < 0)
        FAIL_STACK_ERROR

    /* Make certain that the address of the root node is defined */
    if (H5F_addr_defined(root_addr))
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    /* Close file */
    if (H5Fclose(file) < 0)
        STACK_ERROR

    PASSED();

    if (records)
        HDfree(records);
    if (file_data)
        HDfree(file_data);

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (bt2)
            H5B2_close(bt2);
        H5Fclose(file);
    }
    H5E_END_TRY;

    if (fd > 0)
        HDclose(fd);
    if (records)
        HDfree(records);
    if (file_data)
        HDfree(file_data);

    return 1;
} /* test_remove_lots() */

/*-------------------------------------------------------------------------
 * Function:    test_find_neighbor
 *
 * Purpose:    Basic tests for the B-tree v2 code.  This test exercises
 *              code to find nearest neighbors to a given value in a B-tree.
 *
 * Return:    Success:    0
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Tuesday, March  8, 2005
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_find_neighbor(hid_t fapl, const H5B2_create_t *cparam, const bt2_test_param_t *tparam)
{
    hid_t                 file = -1;   /* File ID */
    H5F_t *               f    = NULL; /* Internal file object pointer */
    H5B2_t *              bt2  = NULL; /* v2 B-tree wrapper */
    haddr_t               bt2_addr;    /* Address of B-tree created */
    hsize_t               record;      /* Record to insert into tree */
    hsize_t               search;      /* Search value */
    hsize_t *             records;     /* Record #'s for random insertion */
    unsigned              u;           /* Local index variable */
    H5B2_node_info_test_t ninfo;       /* B-tree node info */
    herr_t                ret;         /* Generic error return value */

    /* Allocate space for the records */
    if (NULL == (records = (hsize_t *)HDmalloc(sizeof(hsize_t) * FIND_NEIGHBOR)))
        TEST_ERROR

    /* Initialize record #'s */
    for (u = 0; u < FIND_NEIGHBOR; u++)
        records[u] = u * 2;

    /*
     * Test nearest neighbor for '<' cases
     */
    TESTING("B-tree find: nearest neighbor less than a value");

    /* Create the file for the test */
    if (create_file(&file, &f, fapl) < 0)
        TEST_ERROR

    /* Create the v2 B-tree & get its address */
    if (create_btree(f, cparam, &bt2, &bt2_addr) < 0)
        TEST_ERROR

    /* Insert records */
    for (u = 0; u < FIND_NEIGHBOR; u++) {
        record = records[u];
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Attempt to find record B-tree less than a value */
    search = 0;
    H5E_BEGIN_TRY
    {
        ret = H5B2_neighbor(bt2, H5B2_COMPARE_LESS, &search, neighbor_cb, &record);
    }
    H5E_END_TRY;
    /* Should fail */
    if (ret != FAIL)
        TEST_ERROR

    search = 1;
    if (H5B2_neighbor(bt2, H5B2_COMPARE_LESS, &search, neighbor_cb, &record) < 0)
        FAIL_STACK_ERROR
    if (record != 0)
        TEST_ERROR

    search = 2;
    if (H5B2_neighbor(bt2, H5B2_COMPARE_LESS, &search, neighbor_cb, &record) < 0)
        FAIL_STACK_ERROR
    if (record != 0)
        TEST_ERROR

    search = 3;
    if (H5B2_neighbor(bt2, H5B2_COMPARE_LESS, &search, neighbor_cb, &record) < 0)
        FAIL_STACK_ERROR
    if (record != 2)
        TEST_ERROR

    search = 4;
    if (H5B2_neighbor(bt2, H5B2_COMPARE_LESS, &search, neighbor_cb, &record) < 0)
        FAIL_STACK_ERROR
    if (record != 2)
        TEST_ERROR

    /* Check status of B-tree */
    ninfo.depth = 1;
    ninfo.nrec  = 14;
    record      = 250; /* Record in left internal node */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    /* Neighbor is in internal node */
    search = 251;
    if (H5B2_neighbor(bt2, H5B2_COMPARE_LESS, &search, neighbor_cb, &record) < 0)
        FAIL_STACK_ERROR
    if (record != 250)
        TEST_ERROR

    /* Check status of B-tree */
    ninfo.depth = 2;
    ninfo.nrec  = 1;
    record      = 1888; /* Record in root node */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    /* Neighbor is in root node */
    search = 1889;
    if (H5B2_neighbor(bt2, H5B2_COMPARE_LESS, &search, neighbor_cb, &record) < 0)
        FAIL_STACK_ERROR
    if (record != 1888)
        TEST_ERROR

    search = (FIND_NEIGHBOR * 2) + 1;
    if (H5B2_neighbor(bt2, H5B2_COMPARE_LESS, &search, neighbor_cb, &record) < 0)
        FAIL_STACK_ERROR
    if (record != ((FIND_NEIGHBOR - 1) * 2))
        TEST_ERROR

    PASSED();

    /*
     * Test nearest neighbor for '>' cases
     */
    TESTING("B-tree find: nearest neighbor greater than a value");

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Attempt to find record B-tree less than a value */
    search = (FIND_NEIGHBOR * 2) + 1;
    H5E_BEGIN_TRY
    {
        ret = H5B2_neighbor(bt2, H5B2_COMPARE_GREATER, &search, neighbor_cb, &record);
    }
    H5E_END_TRY;
    /* Should fail */
    if (ret != FAIL)
        TEST_ERROR

    search = 0;
    if (H5B2_neighbor(bt2, H5B2_COMPARE_GREATER, &search, neighbor_cb, &record) < 0)
        FAIL_STACK_ERROR
    if (record != 2)
        TEST_ERROR

    search = 1;
    if (H5B2_neighbor(bt2, H5B2_COMPARE_GREATER, &search, neighbor_cb, &record) < 0)
        FAIL_STACK_ERROR
    if (record != 2)
        TEST_ERROR

    search = 2;
    if (H5B2_neighbor(bt2, H5B2_COMPARE_GREATER, &search, neighbor_cb, &record) < 0)
        FAIL_STACK_ERROR
    if (record != 4)
        TEST_ERROR

    search = 3;
    if (H5B2_neighbor(bt2, H5B2_COMPARE_GREATER, &search, neighbor_cb, &record) < 0)
        FAIL_STACK_ERROR
    if (record != 4)
        TEST_ERROR

    /* Check status of B-tree */
    ninfo.depth = 1;
    ninfo.nrec  = 16;
    record      = 2896; /* Record in right internal node */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    /* Neighbor is in internal node */
    search = 2895;
    if (H5B2_neighbor(bt2, H5B2_COMPARE_GREATER, &search, neighbor_cb, &record) < 0)
        FAIL_STACK_ERROR
    if (record != 2896)
        TEST_ERROR

    /* Neighbor is in root node */
    search = 1887;
    if (H5B2_neighbor(bt2, H5B2_COMPARE_GREATER, &search, neighbor_cb, &record) < 0)
        FAIL_STACK_ERROR
    if (record != 1888)
        TEST_ERROR

    search = ((FIND_NEIGHBOR - 1) * 2) - 1;
    if (H5B2_neighbor(bt2, H5B2_COMPARE_GREATER, &search, neighbor_cb, &record) < 0)
        FAIL_STACK_ERROR
    if (record != ((FIND_NEIGHBOR - 1) * 2))
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    /* Close file */
    if (H5Fclose(file) < 0)
        STACK_ERROR

    PASSED();

    HDfree(records);

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (bt2)
            H5B2_close(bt2);
        H5Fclose(file);
    }
    H5E_END_TRY;
    HDfree(records);
    return 1;
} /* test_find_neighbor() */

/*-------------------------------------------------------------------------
 * Function:    test_delete
 *
 * Purpose:    Basic tests for the B-tree v2 code.  This test exercises
 *              code to delete a B-tree from a file
 *
 * Return:    Success:    0
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Wednesday, March  9, 2005
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_delete(hid_t fapl, const H5B2_create_t *cparam)
{
    hid_t          file = -1;      /* File ID */
    char           filename[1024]; /* Filename to use */
    H5F_t *        f = NULL;       /* Internal file object pointer */
    h5_stat_size_t empty_size;     /* Size of an empty file */
    h5_stat_size_t file_size;      /* Size of each file created */
    H5B2_t *       bt2 = NULL;     /* v2 B-tree wrapper */
    haddr_t        bt2_addr;       /* Address of B-tree created */
    hsize_t        record;         /* Record to insert into tree */
    H5B2_stat_t    bt2_stat;       /* Statistics about B-tree created */
    unsigned       u;              /* Local index variable */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Attempt to delete empty B-tree */
    TESTING("B-tree delete: delete empty B-tree");

    /* Create empty file for size comparisons later */

    /* Create the file to work on */
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        STACK_ERROR

    /* Close file */
    if (H5Fclose(file) < 0)
        STACK_ERROR

    /* Get the size of an empty file */
    if ((empty_size = h5_get_file_size(filename, fapl)) < 0)
        TEST_ERROR

    /* Create the file to work on */
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        STACK_ERROR

    /* Get a pointer to the internal file object */
    if (NULL == (f = (H5F_t *)H5VL_object(file)))
        STACK_ERROR

    /* Ignore metadata tags in the file's cache */
    if (H5AC_ignore_tags(f) < 0)
        STACK_ERROR

    /* Create the v2 B-tree & get its address */
    if (create_btree(f, cparam, &bt2, &bt2_addr) < 0)
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    /*
     * Delete v2 B-tree
     */
    if (H5B2_delete(f, bt2_addr, f, NULL, NULL) < 0)
        FAIL_STACK_ERROR

    /* Close the file */
    if (H5Fclose(file) < 0)
        STACK_ERROR

    /* Get the size of the file */
    if ((file_size = h5_get_file_size(filename, fapl)) < 0)
        TEST_ERROR

    /* Verify the file is correct size */
    if (file_size != empty_size)
        TEST_ERROR

    PASSED();

    /* Attempt to delete level-0 B-tree */
    TESTING("B-tree delete: delete level-0 B-tree");

    /* Create the file to work on */
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        STACK_ERROR

    /* Get a pointer to the internal file object */
    if (NULL == (f = (H5F_t *)H5VL_object(file)))
        STACK_ERROR

    /* Ignore metadata tags in the file's cache */
    if (H5AC_ignore_tags(f) < 0)
        STACK_ERROR

    /* Create the v2 B-tree & get its address */
    if (create_btree(f, cparam, &bt2, &bt2_addr) < 0)
        TEST_ERROR

    /* Insert records */
    for (u = 0; u < DELETE_SMALL; u++) {
        record = u;
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check up on B-tree */
    if (H5B2_stat_info(bt2, &bt2_stat) < 0)
        FAIL_STACK_ERROR
    if (bt2_stat.depth != 0)
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    /*
     * Delete v2 B-tree
     */
    if (H5B2_delete(f, bt2_addr, f, NULL, NULL) < 0)
        FAIL_STACK_ERROR

    /* Close file */
    if (H5Fclose(file) < 0)
        STACK_ERROR

    /* Get the size of the file */
    if ((file_size = h5_get_file_size(filename, fapl)) < 0)
        TEST_ERROR

    /* Verify the file is correct size */
    if (file_size != empty_size)
        TEST_ERROR

    PASSED();

    /* Attempt to delete level-1 B-tree */
    TESTING("B-tree delete: delete level-1 B-tree");

    /* Create the file to work on */
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        STACK_ERROR

    /* Get a pointer to the internal file object */
    if (NULL == (f = (H5F_t *)H5VL_object(file)))
        STACK_ERROR

    /* Ignore metadata tags in the file's cache */
    if (H5AC_ignore_tags(f) < 0)
        STACK_ERROR

    /* Create the v2 B-tree & get its address */
    if (create_btree(f, cparam, &bt2, &bt2_addr) < 0)
        TEST_ERROR

    /* Insert records */
    for (u = 0; u < DELETE_MEDIUM; u++) {
        record = u;
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check up on B-tree */
    if (H5B2_stat_info(bt2, &bt2_stat) < 0)
        FAIL_STACK_ERROR
    if (bt2_stat.depth != 1)
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    /*
     * Delete v2 B-tree
     */
    if (H5B2_delete(f, bt2_addr, f, NULL, NULL) < 0)
        FAIL_STACK_ERROR

    /* Close file */
    if (H5Fclose(file) < 0)
        STACK_ERROR

    /* Get the size of the file */
    if ((file_size = h5_get_file_size(filename, fapl)) < 0)
        TEST_ERROR

    /* Verify the file is correct size */
    if (file_size != empty_size)
        TEST_ERROR

    PASSED();

    /* Attempt to delete level-2 B-tree */
    TESTING("B-tree delete: delete level-2 B-tree");

    /* Create the file to work on */
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        STACK_ERROR

    /* Get a pointer to the internal file object */
    if (NULL == (f = (H5F_t *)H5VL_object(file)))
        STACK_ERROR

    /* Ignore metadata tags in the file's cache */
    if (H5AC_ignore_tags(f) < 0)
        STACK_ERROR

    /* Create the v2 B-tree & get its address */
    if (create_btree(f, cparam, &bt2, &bt2_addr) < 0)
        TEST_ERROR

    /* Insert records */
    for (u = 0; u < DELETE_LARGE; u++) {
        record = u;
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check up on B-tree */
    if (H5B2_stat_info(bt2, &bt2_stat) < 0)
        FAIL_STACK_ERROR
    if (bt2_stat.depth != 2)
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    /*
     * Delete v2 B-tree
     */
    if (H5B2_delete(f, bt2_addr, f, NULL, NULL) < 0)
        FAIL_STACK_ERROR

    /* Close file */
    if (H5Fclose(file) < 0)
        STACK_ERROR

    /* Get the size of the file */
    if ((file_size = h5_get_file_size(filename, fapl)) < 0)
        TEST_ERROR

    /* Verify the file is correct size */
    if (file_size != empty_size)
        TEST_ERROR

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (bt2)
            H5B2_close(bt2);
        H5Fclose(file);
    }
    H5E_END_TRY;
    return 1;
} /* test_delete() */

/*-------------------------------------------------------------------------
 * Function:    test_modify
 *
 * Purpose:    Basic tests for the B-tree v2 code.  This test exercises
 *              code to modify an existing record in the B-tree
 *
 * Return:    Success:    0
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Friday, March 10, 2005
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_modify(hid_t fapl, const H5B2_create_t *cparam, const bt2_test_param_t *tparam)
{
    hid_t                 file = -1;   /* File ID */
    H5F_t *               f    = NULL; /* Internal file object pointer */
    H5B2_t *              bt2  = NULL; /* v2 B-tree wrapper */
    haddr_t               bt2_addr;    /* Address of B-tree created */
    hsize_t               record;      /* Record to insert into tree */
    hsize_t               modify;      /* Modified value */
    hsize_t               found;       /* Found value */
    H5B2_stat_t           bt2_stat;    /* Statistics about B-tree created */
    H5B2_node_info_test_t ninfo;       /* B-tree node info */
    unsigned              u;           /* Local index variable */
    hbool_t               rec_found;   /* Whether record was found */
    herr_t                ret;         /* Generic error return value */

    /*
     * Test modifying records
     */
    TESTING("B-tree modify: attempt to modify non-existant record");

    /* Create the file for the test */
    if (create_file(&file, &f, fapl) < 0)
        TEST_ERROR

    /* Create the v2 B-tree & get its address */
    if (create_btree(f, cparam, &bt2, &bt2_addr) < 0)
        TEST_ERROR

    /* Create level-2 B-tree with 3 internal nodes */
    for (u = 0; u < ((INSERT_SPLIT_ROOT_NREC * 59) + 1); u++) {
        record = u * 5;
        if (H5B2_insert(bt2, &record) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Check up on B-tree */
    if (H5B2_stat_info(bt2, &bt2_stat) < 0)
        FAIL_STACK_ERROR
    if (bt2_stat.depth != 2)
        TEST_ERROR

    /* Attempt to modify a non-existant record */
    record = 3;
    modify = 4;
    H5E_BEGIN_TRY
    {
        ret = H5B2_modify(bt2, &record, modify_cb, &modify);
    }
    H5E_END_TRY;
    /* Should fail */
    if (ret != FAIL)
        TEST_ERROR

    PASSED();

    TESTING("B-tree modify: modify record in leaf node");

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Check status of B-tree */
    ninfo.depth = 0;
    ninfo.nrec  = 62;
    record      = 4330; /* Record in leaf node */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    /* Attempt to modify a record in a leaf node */
    record = 4330;
    modify = 4331;
    if (H5B2_modify(bt2, &record, modify_cb, &modify) < 0)
        FAIL_STACK_ERROR

    /* Check status of B-tree */
    ninfo.depth = 0;
    ninfo.nrec  = 62;
    record      = 4331; /* Record in leaf node */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    /* Attempt to find modified record */
    record    = 4331;
    found     = 4331;
    rec_found = FALSE;
    if (H5B2_find(bt2, &record, &rec_found, find_cb, &found) < 0)
        FAIL_STACK_ERROR
    if (found != 4331)
        TEST_ERROR
    if (!rec_found)
        TEST_ERROR

    /* Attempt to find original record */
    record = 4330;
    found  = HSIZET_MAX;
    H5E_BEGIN_TRY
    {
        ret = H5B2_modify(bt2, &record, modify_cb, &modify);
    }
    H5E_END_TRY;
    /* Should fail */
    if (ret != FAIL)
        TEST_ERROR

    PASSED();

    TESTING("B-tree modify: modify record in internal node");

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Check status of B-tree */
    ninfo.depth = 1;
    ninfo.nrec  = 29;
    record      = 5350; /* Record in internal node */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    /* Attempt to modify a record in an internal node */
    record = 5350;
    modify = 5352;
    if (H5B2_modify(bt2, &record, modify_cb, &modify) < 0)
        FAIL_STACK_ERROR

    /* Check status of B-tree */
    ninfo.depth = 1;
    ninfo.nrec  = 29;
    record      = 5352; /* Record in internal node */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    /* Attempt to find modified record */
    record    = 5352;
    found     = 5352;
    rec_found = FALSE;
    if (H5B2_find(bt2, &record, &rec_found, find_cb, &found) < 0)
        STACK_ERROR
    if (found != 5352)
        TEST_ERROR
    if (!rec_found)
        TEST_ERROR

    /* Attempt to find original record */
    record = 5350;
    found  = 5350;
    H5E_BEGIN_TRY
    {
        ret = H5B2_modify(bt2, &record, modify_cb, &modify);
    }
    H5E_END_TRY;
    /* Should fail */
    if (ret != FAIL)
        TEST_ERROR

    PASSED();

    TESTING("B-tree modify: modify record in root node");

    /* Check for closing & re-opening the B-tree */
    if (reopen_btree(f, &bt2, bt2_addr, tparam) < 0)
        TEST_ERROR

    /* Check status of B-tree */
    ninfo.depth = 2;
    ninfo.nrec  = 2;
    record      = 9445; /* Record in root node */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    /* Attempt to modify a record in a root node */
    record = 9445;
    modify = 9448;
    if (H5B2_modify(bt2, &record, modify_cb, &modify) < 0)
        FAIL_STACK_ERROR

    /* Check status of B-tree */
    ninfo.depth = 2;
    ninfo.nrec  = 2;
    record      = 9448; /* Record in root node */
    if (check_node_info(bt2, record, &ninfo) < 0)
        TEST_ERROR

    /* Attempt to find modified record */
    record    = 9448;
    found     = 9448;
    rec_found = FALSE;
    if (H5B2_find(bt2, &record, &rec_found, find_cb, &found) < 0)
        STACK_ERROR
    if (found != 9448)
        TEST_ERROR
    if (!rec_found)
        TEST_ERROR

    /* Attempt to find original record */
    record = 9445;
    found  = 9445;
    H5E_BEGIN_TRY
    {
        ret = H5B2_modify(bt2, &record, modify_cb, &modify);
    }
    H5E_END_TRY;
    /* Should fail */
    if (ret != FAIL)
        TEST_ERROR

    /* Close the v2 B-tree */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    /* Close file */
    if (H5Fclose(file) < 0)
        TEST_ERROR

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (bt2)
            H5B2_close(bt2);
        H5Fclose(file);
    }
    H5E_END_TRY;
    return 1;
} /* test_modify() */

/*-------------------------------------------------------------------------
 * Function:    test_open_twice_diff
 *
 * Purpose:    Open a v2 B-tree twice, through different "top" file
 *              handles, with an intermediate file open that takes the "shared"
 *              file handle from the first B-tree's file pointer.
 *
 * Return:    Success:    0
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Friday, December 18, 2015
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_open_twice_diff(hid_t fapl, const H5B2_create_t *cparam)
{
    char    filename[1024];     /* Filename to use */
    char    filename_tmp[1024]; /* Temporary file name */
    hid_t   file   = -1;        /* File ID */
    hid_t   file2  = -1;        /* File ID */
    hid_t   file0  = -1;        /* File ID */
    hid_t   file00 = -1;        /* File ID */
    H5F_t * f      = NULL;      /* Internal file object pointer */
    H5F_t * f2     = NULL;      /* Internal file object pointer */
    H5B2_t *bt2    = NULL;      /* v2 B-tree wrapper */
    H5B2_t *bt2_2  = NULL;      /* Second v2 B-tree wrapper */
    haddr_t bt2_addr;           /* Address of B-tree created */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /*
     * Display testing message
     */
    TESTING("open B-tree twice, through different file handles");

    /* Create the file to work on */
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if (NULL == (f = (H5F_t *)H5VL_object(file)))
        FAIL_STACK_ERROR

    /* Ignore metadata tags in the file's cache */
    if (H5AC_ignore_tags(f) < 0)
        FAIL_STACK_ERROR

    /* Create the v2 B-tree & get its address */
    if (create_btree(f, cparam, &bt2, &bt2_addr) < 0)
        TEST_ERROR

    /* Re-open v2 B-tree */
    if (NULL == (bt2_2 = H5B2_open(f, bt2_addr, f)))
        FAIL_STACK_ERROR

    /* Close the second v2 B-tree wrapper */
    if (H5B2_close(bt2_2) < 0)
        FAIL_STACK_ERROR
    bt2_2 = NULL;

    /* Re-open the file */
    /* (So that there is something holding the file open when the extensible
     *  array is closed)
     */
    if ((file0 = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Close the first v2 B-tree wrapper */
    if (H5B2_close(bt2) < 0)
        FAIL_STACK_ERROR
    bt2 = NULL;

    /* Close the file */
    /* (close before second file, to detect error on internal B-tree header's
     *  shared file information)
     */
    if (H5Fclose(file) < 0)
        FAIL_STACK_ERROR
    file = -1;

    /* Open a different file */
    /* (This re-allocates the 'top' file pointer and assigns it a different
     *  'shared' file pointer, making the file pointer in the fixed array's
     *  header stale)
     */
    h5_fixname(FILENAME[1], fapl, filename_tmp, sizeof(filename_tmp));
    if ((file00 = H5Fcreate(filename_tmp, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Re-open the file with the v2 B-tree array */
    if ((file2 = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if (NULL == (f2 = (H5F_t *)H5VL_object(file2)))
        FAIL_STACK_ERROR

    /* Open the B-tree through the second file handle */
    if (NULL == (bt2_2 = H5B2_open(f2, bt2_addr, f2)))
        FAIL_STACK_ERROR

    /* Close the extra file handles */
    if (H5Fclose(file0) < 0)
        FAIL_STACK_ERROR
    if (H5Fclose(file00) < 0)
        FAIL_STACK_ERROR

    /* Close the second v2 B-tree */
    if (H5B2_close(bt2_2) < 0)
        FAIL_STACK_ERROR
    bt2_2 = NULL;

    /* Close file */
    if (H5Fclose(file2) < 0)
        FAIL_STACK_ERROR

    /* All tests passed */
    PASSED();

    /* All tests passed */
    return (0);

error:
    H5E_BEGIN_TRY
    {
        if (bt2)
            H5B2_close(bt2);
        if (bt2)
            H5B2_close(bt2_2);
        H5Fclose(file);
        H5Fclose(file2);
        H5Fclose(file0);
        H5Fclose(file00);
    }
    H5E_END_TRY;
    return (1);
} /* test_open_twice_diff() */

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:    Test the B-tree v2 code
 *
 * Return:    Success:
 *
 *        Failure:
 *
 * Programmer:    Quincey Koziol
 *              Tuesday, February  1, 2005
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    H5B2_create_t    cparam, cparam2; /* Creation parameters for v2 B-tree */
    bt2_test_param_t tparam;          /* Test parameters for v2 B-tree */
    hid_t            fapl    = -1;    /* File access property list for data files */
    unsigned         nerrors = 0;     /* Cumulative error count */
    unsigned         reopen;          /* Whether to reopen B-tree during tests */
    int              ExpressMode;
    const char *     envval         = NULL;
    hbool_t          api_ctx_pushed = FALSE; /* Whether API context pushed */

    envval = HDgetenv("HDF5_DRIVER");
    if (envval == NULL)
        envval = "nomatch";

    /* Reset library */
    h5_reset();
    fapl        = h5_fileaccess();
    ExpressMode = GetTestExpress();
    if (ExpressMode > 1)
        HDprintf("***Express test mode on.  Some tests may be skipped\n");

    /* Initialize v2 B-tree creation parameters */
    init_cparam(&cparam, &cparam2);

    /* Push API context */
    if (H5CX_push() < 0)
        FAIL_STACK_ERROR
    api_ctx_pushed = TRUE;

    /* Loop over re-opening B-tree during tests */
    for (reopen = FALSE; reopen <= TRUE; reopen++) {
        if (reopen) {
            HDfprintf(stdout, "Testing with reopening B-tree:\n");
            tparam.reopen_btree = TRUE;
        } /* end if */
        else {
            HDfprintf(stdout, "Testing without reopening B-tree:\n");
            tparam.reopen_btree = FALSE;
        } /* end else */

        /* Test B-tree record insertion */
        /* (Iteration, find & index routines exercised in these routines as well) */
        nerrors += test_insert_basic(fapl, &cparam, &tparam);
        nerrors += test_insert_split_root(fapl, &cparam, &tparam);
        nerrors += test_insert_level1_2leaf_redistrib(fapl, &cparam, &tparam);
        nerrors += test_insert_level1_side_split(fapl, &cparam, &tparam);
        nerrors += test_insert_level1_3leaf_redistrib(fapl, &cparam, &tparam);
        nerrors += test_insert_level1_middle_split(fapl, &cparam, &tparam);
        nerrors += test_insert_make_level2(fapl, &cparam, &tparam);
        nerrors += test_insert_level2_leaf_redistrib(fapl, &cparam, &tparam);
        nerrors += test_insert_level2_leaf_split(fapl, &cparam, &tparam);
        nerrors += test_insert_level2_2internal_redistrib(fapl, &cparam, &tparam);
        nerrors += test_insert_level2_2internal_split(fapl, &cparam, &tparam);
        nerrors += test_insert_level2_3internal_redistrib(fapl, &cparam, &tparam);
        nerrors += test_insert_level2_3internal_split(fapl, &cparam, &tparam);
        if (ExpressMode > 1)
            HDprintf("***Express test mode on.  test_insert_lots skipped\n");
        else
            nerrors += test_insert_lots(fapl, &cparam, &tparam);

        /* Test B-tree record update (ie. insert/modify) */
        /* (Iteration, find & index routines exercised in these routines as well) */
        nerrors += test_update_basic(fapl, &cparam2, &tparam);
        nerrors += test_update_split_root(fapl, &cparam2, &tparam);
        nerrors += test_update_level1_2leaf_redistrib(fapl, &cparam2, &tparam);
        nerrors += test_update_level1_side_split(fapl, &cparam2, &tparam);
        nerrors += test_update_level1_3leaf_redistrib(fapl, &cparam2, &tparam);
        nerrors += test_update_level1_middle_split(fapl, &cparam2, &tparam);
        nerrors += test_update_make_level2(fapl, &cparam2, &tparam);
        if (ExpressMode > 1)
            HDprintf("***Express test mode on.  test_update_lots skipped\n");
        else
            nerrors += test_update_lots(fapl, &cparam2, &tparam);

        /* Test B-tree record removal */
        /* Querying the number of records routine also tested in these routines as well */
        nerrors += test_remove_basic(fapl, &cparam, &tparam);
        nerrors += test_remove_level1_noredistrib(fapl, &cparam, &tparam);
        nerrors += test_remove_level1_redistrib(fapl, &cparam, &tparam);
        nerrors += test_remove_level1_2leaf_merge(fapl, &cparam, &tparam);
        nerrors += test_remove_level1_3leaf_merge(fapl, &cparam, &tparam);
        nerrors += test_remove_level1_promote(fapl, &cparam, &tparam);
        nerrors += test_remove_level1_promote_2leaf_redistrib(fapl, &cparam, &tparam);
        nerrors += test_remove_level1_promote_3leaf_redistrib(fapl, &cparam, &tparam);
        nerrors += test_remove_level1_promote_2leaf_merge(fapl, &cparam, &tparam);
        nerrors += test_remove_level1_promote_3leaf_merge(fapl, &cparam, &tparam);
        nerrors += test_remove_level1_collapse(fapl, &cparam, &tparam);
        nerrors += test_remove_level2_promote(fapl, &cparam, &tparam);
        nerrors += test_remove_level2_promote_2internal_redistrib(fapl, &cparam, &tparam);
        nerrors += test_remove_level2_promote_3internal_redistrib(fapl, &cparam, &tparam);
        nerrors += test_remove_level2_promote_2internal_merge(fapl, &cparam, &tparam);
        nerrors += test_remove_level2_promote_3internal_merge(fapl, &cparam, &tparam);
        nerrors += test_remove_level2_2internal_merge_left(fapl, &cparam, &tparam);
        nerrors += test_remove_level2_2internal_merge_right(fapl, &cparam, &tparam);
        nerrors += test_remove_level2_3internal_merge(fapl, &cparam, &tparam);
        nerrors += test_remove_level2_collapse_right(fapl, &cparam, &tparam);
        if (ExpressMode > 1)
            HDprintf("***Express test mode on.  test_remove_lots skipped\n");
        else
            nerrors += test_remove_lots(envval, fapl, &cparam);

        /* Test more complex B-tree queries */
        nerrors += test_find_neighbor(fapl, &cparam, &tparam);

        /* Test deleting B-trees */
        nerrors += test_delete(fapl, &cparam);

        /* Test modifying B-tree records */
        nerrors += test_modify(fapl, &cparam, &tparam);
    } /* end for */

    /* Test opening B-trees twice */
    nerrors += test_open_twice_diff(fapl, &cparam);

    /* Verify symbol table messages are cached */
    nerrors += (h5_verify_cached_stabs(FILENAME, fapl) < 0 ? 1 : 0);

    /* Pop API context */
    if (api_ctx_pushed && H5CX_pop(FALSE) < 0)
        FAIL_STACK_ERROR
    api_ctx_pushed = FALSE;

    if (nerrors)
        goto error;

    HDputs("All v2 B-tree tests passed.");

    h5_cleanup(FILENAME, fapl);

    return 0;

error:
    HDputs("*** TESTS FAILED ***");

    H5E_BEGIN_TRY
    {
        H5Pclose(fapl);
    }
    H5E_END_TRY;

    if (api_ctx_pushed)
        H5CX_pop(FALSE);

    return 1;
} /* end main() */
