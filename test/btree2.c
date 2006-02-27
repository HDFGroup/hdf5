/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
 * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* Programmer:  Quincey Koziol <koziol@ncsa.uiuc.edu>
 *              Tuesday, February  1, 2005
 */
#include "h5test.h"

/*
 * This file needs to access private datatypes from the H5B2 package.
 * This file also needs to access the v2 B-tree testing code.
 */
#define H5B2_PACKAGE
#define H5B2_TESTING
#include "H5B2pkg.h"

/* Other private headers that this test requires */
#include "H5Iprivate.h"

const char *FILENAME[] = {
    "btree2",
    NULL
};

#define INSERT_SPLIT_ROOT_NREC  80
#define INSERT_MANY             (320*1000)
#define FIND_MANY               (INSERT_MANY/100)
#define FIND_NEIGHBOR           1000
#define DELETE_SMALL            10
#define DELETE_MEDIUM           200
#define DELETE_LARGE            1000


/*-------------------------------------------------------------------------
 * Function:	iter_cb
 *
 * Purpose:	v2 B-tree iterator callback
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Wednesday, February 16, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
iter_cb(const void *_record, void *_op_data)
{
    const hsize_t *record = (const hsize_t *)_record;
    hsize_t *idx = (hsize_t *)_op_data;

    if(*record != *idx)
        return(H5B2_ITER_ERROR);

    (*idx)++;
    return(H5B2_ITER_CONT);
} /* end iter_cb() */


/*-------------------------------------------------------------------------
 * Function:	find_cb
 *
 * Purpose:	v2 B-tree find callback
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Thursday, February 24, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
find_cb(const void *_record, void *_op_data)
{
    const hsize_t *record = (const hsize_t *)_record;
    hsize_t *search = (hsize_t *)_op_data;

    if(*record != *search)
        return(-1);

    return(0);
} /* end find_cb() */


/*-------------------------------------------------------------------------
 * Function:	neighbor_cb
 *
 * Purpose:	v2 B-tree neighbor callback
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, March  8, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
neighbor_cb(const void *_record, void *_op_data)
{
    const hsize_t *record = (const hsize_t *)_record;
    hsize_t *search = (hsize_t *)_op_data;

    *search = *record;

    return(0);
} /* end neighbor_cb() */


/*-------------------------------------------------------------------------
 * Function:	modify_cb
 *
 * Purpose:	v2 B-tree modify callback
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Friday, March 10, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
modify_cb(void *_record, void *_op_data, hbool_t *changed)
{
    hsize_t *record = (hsize_t *)_record;
    hsize_t *modify = (hsize_t *)_op_data;

    *record = *modify;
    *changed = TRUE;

    return(0);
} /* end modify_cb() */


/*-------------------------------------------------------------------------
 * Function:	test_insert_basic
 *
 * Purpose:	Basic tests for the B-tree v2 code
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Thursday, February  3, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_insert_basic(hid_t fapl)
{
    hid_t	file = -1;              /* File ID */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    hsize_t     record;                 /* Record to insert into tree */
    hsize_t     idx;                    /* Index within B-tree, for iterator */
    haddr_t     bt2_addr;               /* Address of B-tree created */
    herr_t      ret;                    /* Generic error return value */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /*
     * Test v2 B-tree creation
     */
    TESTING("B-tree creation");
    if(H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(bt2_addr))
        FAIL_STACK_ERROR
    PASSED();

    /* Attempt to iterate over a B-tree with no records */
    idx = 0;
    if(H5B2_iterate(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, iter_cb, &idx) < 0)
        FAIL_STACK_ERROR
    /* Make certain that the index hasn't changed */
    if(idx != 0)
        TEST_ERROR

    /* Attempt to find record in B-tree with no records */
    idx = 0;
    H5E_BEGIN_TRY {
	ret = H5B2_find(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &idx, find_cb, NULL);
    } H5E_END_TRY;
    /* Should fail */
    if(ret != FAIL)
        TEST_ERROR

    /* Attempt to index record in B-tree with no records */
    idx = 0;
    H5E_BEGIN_TRY {
	ret = H5B2_index(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, (hsize_t)0, find_cb, NULL);
    } H5E_END_TRY;
    /* Should fail */
    if(ret != FAIL)
        TEST_ERROR

    /*
     * Test inserting record into v2 B-tree
     */
    TESTING("B-tree insert: several records");
    record = 42;
    if(H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record) < 0)
        FAIL_STACK_ERROR

    /* Attempt to find non-existant record in B-tree with 1 record */
    idx = 41;
    H5E_BEGIN_TRY {
	ret = H5B2_find(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &idx, find_cb, &idx);
    } H5E_END_TRY;
    /* Should fail */
    if(ret != FAIL)
        TEST_ERROR

    /* Try again with NULL 'op' */
    H5E_BEGIN_TRY {
	ret = H5B2_find(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &idx, NULL, NULL);
    } H5E_END_TRY;
    /* Should fail */
    if(ret != FAIL)
        TEST_ERROR

    /* Attempt to find existant record in B-tree with 1 record */
    idx = 42;
    if(H5B2_find(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &idx, find_cb, &idx)<0)
        TEST_ERROR

    /* Try again with NULL 'op' */
    if(H5B2_find(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &idx, NULL, NULL)<0)
        TEST_ERROR

    /* Attempt to index non-existant record in B-tree with 1 record */
    idx = 0;
    H5E_BEGIN_TRY {
	ret = H5B2_index(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, (hsize_t)1, find_cb, NULL);
    } H5E_END_TRY;
    /* Should fail */
    if(ret != FAIL)
        TEST_ERROR

    /* Attempt to index existing record in B-tree with 1 record */
    idx = 42;
    if(H5B2_index(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, (hsize_t)0, find_cb, &idx)<0)
        TEST_ERROR

    /*
     * Test inserting second record into v2 B-tree, before all other records
     */
    record = 34;
    if(H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record) < 0)
        FAIL_STACK_ERROR

    /*
     * Test inserting third record into v2 B-tree, after all other records
     */
    record = 56;
    if(H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record) < 0)
        FAIL_STACK_ERROR

    /*
     * Test inserting fourth record into v2 B-tree, in the middle of other records
     */
    record = 38;
    if(H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record) < 0)
        FAIL_STACK_ERROR

    /* Attempt to find non-existant record in level-0 B-tree with several records */
    idx = 41;
    H5E_BEGIN_TRY {
	ret = H5B2_find(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &idx, find_cb, &idx);
    } H5E_END_TRY;
    /* Should fail */
    if(ret != FAIL)
        TEST_ERROR

    /* Attempt to find existant record in level-0 B-tree with several record */
    idx = 56;
    if(H5B2_find(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &idx, find_cb, &idx)<0)
        TEST_ERROR

    /* Attempt to index non-existant record in B-tree with several records */
    idx = 0;
    H5E_BEGIN_TRY {
	ret = H5B2_index(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, (hsize_t)4, find_cb, NULL);
    } H5E_END_TRY;
    /* Should fail */
    if(ret != FAIL)
        TEST_ERROR

    /* Attempt to index existing record in B-tree with several records */
    idx = 34;
    if(H5B2_index(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, (hsize_t)0, find_cb, &idx)<0)
        TEST_ERROR
    idx = 38;
    if(H5B2_index(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, (hsize_t)1, find_cb, &idx)<0)
        TEST_ERROR
    idx = 42;
    if(H5B2_index(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, (hsize_t)2, find_cb, &idx)<0)
        TEST_ERROR
    idx = 56;
    if(H5B2_index(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, (hsize_t)3, find_cb, &idx)<0)
        TEST_ERROR

    PASSED();

    /* Close the file */
    if(H5Fclose(file)<0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_insert_basic() */


/*-------------------------------------------------------------------------
 * Function:	test_insert_split_root
 *
 * Purpose:	Basic tests for the B-tree v2 code.  This test inserts enough
 *              records to split the root node and force the tree to depth 1.
 *              It also continues to add a few more records to each of the
 *              left and right leaf nodes after the split
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Thursday, February  3, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_insert_split_root(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    hsize_t     record;                 /* Record to insert into tree */
    hsize_t     idx;                    /* Index within B-tree, for iterator */
    haddr_t     bt2_addr;               /* Address of B-tree created */
    unsigned    u;                      /* Local index variable */
    herr_t      ret;                    /* Generic error return value */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR

    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Test v2 B-tree creation
     */
    if (H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Test inserting many records into v2 B-tree
     */
    TESTING("B-tree insert: split root");

    for(u=0; u<INSERT_SPLIT_ROOT_NREC; u++) {
        record=u+2;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }
    record=0;
    if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    }
    record=1;
    if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    }

    /* Iterate over B-tree to check records have been inserted correctly */
    idx = 0;
    if(H5B2_iterate(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, iter_cb, &idx)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /* Make certain that the index is correct */
    if(idx != (INSERT_SPLIT_ROOT_NREC+2)) TEST_ERROR

    /* Attempt to find non-existant record in level-1 B-tree */
    idx = INSERT_SPLIT_ROOT_NREC + 10;
    H5E_BEGIN_TRY {
	ret = H5B2_find(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &idx, find_cb, &idx);
    } H5E_END_TRY;
    /* Should fail */
    if(ret != FAIL) TEST_ERROR

    /* Attempt to find existant record in root of level-1 B-tree */
    idx = 33;
    if(H5B2_find(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &idx, find_cb, &idx)<0) TEST_ERROR

    /* Attempt to find existant record in leaf of level-1 B-tree */
    idx = 56;
    if(H5B2_find(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &idx, find_cb, &idx)<0) TEST_ERROR

    /* Attempt to index non-existant record in level-1 B-tree */
    idx = 0;
    H5E_BEGIN_TRY {
	ret = H5B2_index(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, (hsize_t)(INSERT_SPLIT_ROOT_NREC+2), find_cb, NULL);
    } H5E_END_TRY;
    /* Should fail */
    if(ret != FAIL) TEST_ERROR

    /* Attempt to index existing record in root of level-1 B-tree */
    idx = 33;
    if(H5B2_index(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, (hsize_t)33, find_cb, &idx)<0) TEST_ERROR

    /* Attempt to index existing record in left leaf of level-1 B-tree */
    idx = 0;
    if(H5B2_index(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, (hsize_t)0, find_cb, &idx)<0) TEST_ERROR

    /* Attempt to index existing record in right leaf of level-1 B-tree */
    idx = 50;
    if(H5B2_index(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, (hsize_t)50, find_cb, &idx)<0) TEST_ERROR

    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_insert_split_root() */


/*-------------------------------------------------------------------------
 * Function:	test_insert_level1_2leaf_redistrib
 *
 * Purpose:	Basic tests for the B-tree v2 code.  This test inserts enough
 *              records to split the root node and force the tree to depth 1.
 *              It continues to add a more records to the each of the
 *              left and right leaf nodes after the split to force a 2 node
 *              redistribution
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, February  8, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_insert_level1_2leaf_redistrib(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    hsize_t     record;                 /* Record to insert into tree */
    haddr_t     bt2_addr;               /* Address of B-tree created */
    unsigned    u;                      /* Local index variable */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR

    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Create v2 B-tree
     */
    if (H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Test inserting many records into v2 B-tree
     */
    TESTING("B-tree insert: redistribute 2 leaves in level 1 B-tree (l->r)");

    /* Insert enough records to force root to split into 2 leaves */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC; u++) {
        record=u+INSERT_SPLIT_ROOT_NREC/2;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    /* Force redistribution from left node into right node */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC/2; u++) {
        record=u;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }
    PASSED();

    /*
     * Create v2 B-tree
     */
    if (H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Test inserting many records into v2 B-tree
     */
    TESTING("B-tree insert: redistribute 2 leaves in level 1 B-tree (r->l)");

    /* Insert enough records to force root to split into 2 leaves */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC; u++) {
        record=u;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    /* Force redistribution from left node into right node */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC/2; u++) {
        record=u+INSERT_SPLIT_ROOT_NREC;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }
    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_insert_level1_2leaf_redistrib() */


/*-------------------------------------------------------------------------
 * Function:	test_insert_level1_2leaf_split
 *
 * Purpose:	Basic tests for the B-tree v2 code.  This test inserts enough
 *              records to split the root node and force the tree to depth 1.
 *              It continues to add a more records to the each of the
 *              left and right leaf nodes after the split to force a 2 node
 *              split, adding another node to the B-tree
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, February  9, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_insert_level1_2leaf_split(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    hsize_t     record;                 /* Record to insert into tree */
    haddr_t     bt2_addr;               /* Address of B-tree created */
    unsigned    u;                      /* Local index variable */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR

    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Create v2 B-tree
     */
    if (H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Test inserting many records into v2 B-tree
     */
    TESTING("B-tree insert: split 2 leaves to 3 in level 1 B-tree (l->r)");

    /* Insert enough records to force root to split into 2 leaves */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC; u++) {
        record=u+INSERT_SPLIT_ROOT_NREC;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    /* Force split from left node into right node */
    for(u=0; u<(3*INSERT_SPLIT_ROOT_NREC)/4; u++) {
        record=u;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }
    PASSED();

    /*
     * Create v2 B-tree
     */
    if (H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Test inserting many records into v2 B-tree
     */
    TESTING("B-tree insert: split 2 leaves to 3 in level 1 B-tree (r->l)");

    /* Insert enough records to force root to split into 2 leaves */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC; u++) {
        record=u;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    /* Force redistribution from left node into right node */
    for(u=0; u<(3*INSERT_SPLIT_ROOT_NREC)/4; u++) {
        record=u+INSERT_SPLIT_ROOT_NREC;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }
    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_insert_level1_2leaf_split() */


/*-------------------------------------------------------------------------
 * Function:	test_insert_level1_3leaf_redistrib
 *
 * Purpose:	Basic tests for the B-tree v2 code.  This test inserts enough
 *              records to split the root node and force the tree to depth 1.
 *              It continues to add a more records to the each of the
 *              left and right leaf nodes after the split to force a 2 node
 *              split, adding another node to the B-tree, then continues to
 *              add records until a 3 node redistribution occurs
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Thursday, February 10, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_insert_level1_3leaf_redistrib(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    hsize_t     record;                 /* Record to insert into tree */
    haddr_t     bt2_addr;               /* Address of B-tree created */
    hsize_t     idx;                    /* Index within B-tree, for iterator */
    unsigned    u;                      /* Local index variable */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR

    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Create v2 B-tree
     */
    if (H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Test inserting many records into v2 B-tree
     */
    TESTING("B-tree insert: redistribute 3 leaves in level 1 B-tree");

    /* Insert enough records to force root to split into 2 leaves */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC; u++) {
        record=u+INSERT_SPLIT_ROOT_NREC;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    /* Force split from left node into right node */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC; u++) {
        record=u;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    /* Iterate over B-tree to check records have been inserted correctly */
    idx = 0;
    if(H5B2_iterate(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, iter_cb, &idx)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /* Make certain that the index is correct */
    if(idx != (INSERT_SPLIT_ROOT_NREC*2)) TEST_ERROR

    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_insert_level1_3leaf_redistrib() */


/*-------------------------------------------------------------------------
 * Function:	test_insert_level1_3leaf_split
 *
 * Purpose:	Basic tests for the B-tree v2 code.  This test inserts enough
 *              records to split the root node and force the tree to depth 1.
 *              It continues to add a more records to the each of the
 *              left and right leaf nodes after the split to force a 2 node
 *              split, adding another node to the B-tree, then continues to
 *              add records until a 3 node split occurs
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Thursday, February 10, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_insert_level1_3leaf_split(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    hsize_t     record;                 /* Record to insert into tree */
    haddr_t     bt2_addr;               /* Address of B-tree created */
    unsigned    u;                      /* Local index variable */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR

    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Create v2 B-tree
     */
    if (H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Test inserting many records into v2 B-tree
     */
    TESTING("B-tree insert: split 3 leaves to 4 in level 1 B-tree");

    /* Insert enough records to force root to split into 2 leaves */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC; u++) {
        record=u+INSERT_SPLIT_ROOT_NREC*2;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    /* Force split from left node into right node */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC*2; u++) {
        record=u;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }
    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_insert_level1_3leaf_split() */


/*-------------------------------------------------------------------------
 * Function:	test_insert_make_level2
 *
 * Purpose:	Basic tests for the B-tree v2 code.  This test inserts enough
 *              records to make a level 2 B-tree
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Friday, February 11, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_insert_make_level2(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    hsize_t     record;                 /* Record to insert into tree */
    haddr_t     bt2_addr;               /* Address of B-tree created */
    hsize_t     idx;                    /* Index within B-tree, for iterator */
    unsigned    u;                      /* Local index variable */
    herr_t      ret;                    /* Generic error return value */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR

    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Create v2 B-tree
     */
    if (H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Test inserting many records into v2 B-tree
     */
    TESTING("B-tree insert: make level 2 B-tree");

    /* Insert enough records to force root to split into 2 leaves */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC*5; u++) {
        record=u+2;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }
    for(; u<INSERT_SPLIT_ROOT_NREC*11; u++) {
        record=u+4;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    /* Add some extra records to left-most leaf */
    record=0;
    if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    }
    record=1;
    if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    }

    /* Add some extra records to middle leaf */
    record=(INSERT_SPLIT_ROOT_NREC*5)+2;
    if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    }
    record=(INSERT_SPLIT_ROOT_NREC*5)+3;
    if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    }


    /* Iterate over B-tree to check records have been inserted correctly */
    idx = 0;
    if(H5B2_iterate(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, iter_cb, &idx)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /* Make certain that the index is correct */
    if(idx != (INSERT_SPLIT_ROOT_NREC*11)+4) TEST_ERROR

    /* Attempt to find non-existant record in level-1 B-tree */
    idx = INSERT_SPLIT_ROOT_NREC*12;
    H5E_BEGIN_TRY {
	ret = H5B2_find(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &idx, find_cb, &idx);
    } H5E_END_TRY;
    /* Should fail */
    if(ret != FAIL) TEST_ERROR

    /* Attempt to find existant record in root of level-2 B-tree */
    idx = 433;
    if(H5B2_find(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &idx, find_cb, &idx)<0) TEST_ERROR

    /* Attempt to find existant record in internal node of level-2 B-tree */
    idx = 259;
    if(H5B2_find(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &idx, find_cb, &idx)<0) TEST_ERROR

    /* Attempt to find existant record in leaf of level-2 B-tree */
    idx = 346;
    if(H5B2_find(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &idx, find_cb, &idx)<0) TEST_ERROR

    /* Attempt to index non-existant record in level-1 B-tree */
    idx = 0;
    H5E_BEGIN_TRY {
	ret = H5B2_index(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, (hsize_t)(INSERT_SPLIT_ROOT_NREC*12), find_cb, NULL);
    } H5E_END_TRY;
    /* Should fail */
    if(ret != FAIL) TEST_ERROR

    /* Attempt to index existing record in root of level-2 B-tree */
    idx = 433;
    if(H5B2_index(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, (hsize_t)433, find_cb, &idx)<0) TEST_ERROR

    /* Attempt to index existing record in internal node of level-2 B-tree */
    idx = 734;
    if(H5B2_index(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, (hsize_t)734, find_cb, &idx)<0) TEST_ERROR

    /* Attempt to index existing record in leaf of level-2 B-tree */
    idx = 883;
    if(H5B2_index(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, (hsize_t)883, find_cb, &idx)<0) TEST_ERROR

    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_insert_make_level2() */


/*-------------------------------------------------------------------------
 * Function:	test_insert_level2_leaf_redistrib
 *
 * Purpose:	Basic tests for the B-tree v2 code.  This test inserts enough
 *              records to make a level 2 B-tree and then adds enough more
 *              records to force the leaves to redistribute
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Thursday, February 17, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_insert_level2_leaf_redistrib(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    hsize_t     record;                 /* Record to insert into tree */
    haddr_t     bt2_addr;               /* Address of B-tree created */
    hsize_t     idx;                    /* Index within B-tree, for iterator */
    unsigned    u;                      /* Local index variable */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR

    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Create v2 B-tree
     */
    if (H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Test inserting many records into v2 B-tree
     */
    TESTING("B-tree insert: redistrib right-most leaf in level 2 B-tree");

    /* Insert enough records to force root to split into 2 internal nodes */
    /* Also redistributes right leaf */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC*4; u++) {
        record=u+(INSERT_SPLIT_ROOT_NREC/2);
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }
    for(; u<INSERT_SPLIT_ROOT_NREC*11; u++) {
        record=u+INSERT_SPLIT_ROOT_NREC;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    PASSED();

    TESTING("B-tree insert: redistrib left-most leaf in level 2 B-tree");

    /* Add more records to left-most leaf, to force a 2 node redistribution on left leaf */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC/2; u++) {
        record=u;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    PASSED();

    TESTING("B-tree insert: redistrib middle leaf in level 2 B-tree");

    /* Add more records to middle leaf, to force a 3 node redistribution on middle leaf */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC/2; u++) {
        record=u+(INSERT_SPLIT_ROOT_NREC/2)+(INSERT_SPLIT_ROOT_NREC*4);
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    /* Iterate over B-tree to check records have been inserted correctly */
    idx = 0;
    if(H5B2_iterate(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, iter_cb, &idx)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /* Make certain that the index is correct */
    if(idx != (INSERT_SPLIT_ROOT_NREC*12)) TEST_ERROR

    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_insert_level2_leaf_redistrib() */


/*-------------------------------------------------------------------------
 * Function:	test_insert_level2_leaf_split
 *
 * Purpose:	Basic tests for the B-tree v2 code.  This test inserts enough
 *              records to make a level 2 B-tree and then adds enough more
 *              records to force leaves to split.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Thursday, February 17, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_insert_level2_leaf_split(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    hsize_t     record;                 /* Record to insert into tree */
    haddr_t     bt2_addr;               /* Address of B-tree created */
    hsize_t     idx;                    /* Index within B-tree, for iterator */
    unsigned    u;                      /* Local index variable */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR

    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Create v2 B-tree
     */
    if (H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Test inserting many records into v2 B-tree
     */
    TESTING("B-tree insert: split right-most leaf in level 2 B-tree");

    /* Insert enough records to force root to split into 2 internal nodes */
    /* Also splits right leaf */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC*4; u++) {
        record=u+INSERT_SPLIT_ROOT_NREC;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }
    for(; u<INSERT_SPLIT_ROOT_NREC*12; u++) {
        record=u+(INSERT_SPLIT_ROOT_NREC*2);
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    PASSED();

    TESTING("B-tree insert: split left-most leaf in level 2 B-tree");

    /* Add more records to left-most leaf, to force a 2->3 node split on left leaf */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC; u++) {
        record=u;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    PASSED();

    TESTING("B-tree insert: split middle leaf in level 2 B-tree");

    /* Add more records to middle leaf, to force a 3->4 node split on middle leaf */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC; u++) {
        record=u+(INSERT_SPLIT_ROOT_NREC*5);
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    /* Iterate over B-tree to check records have been inserted correctly */
    idx = 0;
    if(H5B2_iterate(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, iter_cb, &idx)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /* Make certain that the index is correct */
    if(idx != (INSERT_SPLIT_ROOT_NREC*14)) TEST_ERROR

    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_insert_level2_leaf_split() */


/*-------------------------------------------------------------------------
 * Function:	test_insert_level2_2internal_redistrib
 *
 * Purpose:	Basic tests for the B-tree v2 code.  This test inserts enough
 *              records to make a level 2 B-tree and then adds enough more
 *              records to force the left-most and right-most internal nodes to
 *              redistribute.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Friday, February 18, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_insert_level2_2internal_redistrib(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    hsize_t     record;                 /* Record to insert into tree */
    haddr_t     bt2_addr;               /* Address of B-tree created */
    hsize_t     idx;                    /* Index within B-tree, for iterator */
    unsigned    u;                      /* Local index variable */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR

    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Create v2 B-tree
     */
    if (H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Test inserting many records into v2 B-tree
     */
    TESTING("B-tree insert: redist. 2 internal (r->l) in level 2 B-tree");

    /* Insert enough records to force root to split into 2 internal nodes */
    /* Also forces right-most internal node to redistribute */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC*16; u++) {
        record=u+(INSERT_SPLIT_ROOT_NREC*3);
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    PASSED();

    TESTING("B-tree insert: redist. 2 internal (l->r) in level 2 B-tree");

    /* Force left-most internal node to redistribute */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC*3; u++) {
        record=u;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    /* Iterate over B-tree to check records have been inserted correctly */
    idx = 0;
    if(H5B2_iterate(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, iter_cb, &idx)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /* Make certain that the index is correct */
    if(idx != (INSERT_SPLIT_ROOT_NREC*19)) TEST_ERROR

    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_insert_level2_2internal_redistrib() */


/*-------------------------------------------------------------------------
 * Function:	test_insert_level2_2internal_split
 *
 * Purpose:	Basic tests for the B-tree v2 code.  This test inserts enough
 *              records to make a level 2 B-tree and then adds enough more
 *              records to force the left-most and right-most internal nodes to
 *              split.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Friday, February 18, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_insert_level2_2internal_split(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    hsize_t     record;                 /* Record to insert into tree */
    haddr_t     bt2_addr;               /* Address of B-tree created */
    hsize_t     idx;                    /* Index within B-tree, for iterator */
    unsigned    u;                      /* Local index variable */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR

    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Create v2 B-tree
     */
    if (H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Test inserting many records into v2 B-tree
     */
    TESTING("B-tree insert: split 2 internals to 3 in level 2 B-tree (r->l)");

    /* Insert enough records to force root to split into 2 internal nodes */
    /* Also forces right-most internal node to split */
    for(u=0; u<(INSERT_SPLIT_ROOT_NREC*21); u++) {
        record=u+(INSERT_SPLIT_ROOT_NREC*8);
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    PASSED();

    TESTING("B-tree insert: split 2 internals to 3 in level 2 B-tree (l->r)");

    /* Force left-most internal node to split */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC*8; u++) {
        record=u;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    /* Iterate over B-tree to check records have been inserted correctly */
    idx = 0;
    if(H5B2_iterate(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, iter_cb, &idx)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /* Make certain that the index is correct */
    if(idx != (INSERT_SPLIT_ROOT_NREC*29)) TEST_ERROR

    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_insert_level2_2internal_split() */


/*-------------------------------------------------------------------------
 * Function:	test_insert_level2_3internal_redistrib
 *
 * Purpose:	Basic tests for the B-tree v2 code.  This test inserts enough
 *              records to make a level 2 B-tree and then adds enough more
 *              records to force the left-most and right-most internal nodes to
 *              split and more records to force a 3 node redistribution of the
 *              internal nodes.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Saturday, February 19, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_insert_level2_3internal_redistrib(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    hsize_t     record;                 /* Record to insert into tree */
    haddr_t     bt2_addr;               /* Address of B-tree created */
    hsize_t     idx;                    /* Index within B-tree, for iterator */
    unsigned    u;                      /* Local index variable */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR

    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Create v2 B-tree
     */
    if (H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Test inserting many records into v2 B-tree
     */
    TESTING("B-tree insert: redistrib 3 internals in level 2 B-tree");

    /* Insert enough records to force root to split into 2 internal nodes */
    /* Also forces right-most internal node to split */
    for(u=0; u<(INSERT_SPLIT_ROOT_NREC*21); u++) {
        record=u+(INSERT_SPLIT_ROOT_NREC*13);
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    /* Force left-most internal node to split */
    /* Force middle node to perform redistribution */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC*13; u++) {
        record=u;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    /* Iterate over B-tree to check records have been inserted correctly */
    idx = 0;
    if(H5B2_iterate(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, iter_cb, &idx)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /* Make certain that the index is correct */
    if(idx != (INSERT_SPLIT_ROOT_NREC*34)) TEST_ERROR

    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_insert_level2_3internal_redistrib() */


/*-------------------------------------------------------------------------
 * Function:	test_insert_level2_3internal_split
 *
 * Purpose:	Basic tests for the B-tree v2 code.  This test inserts enough
 *              records to make a level 2 B-tree and then adds enough more
 *              records to force the left-most and right-most internal nodes to
 *              split and more records to force a 3->4 node split of the
 *              internal nodes.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Saturday, February 19, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_insert_level2_3internal_split(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    hsize_t     record;                 /* Record to insert into tree */
    haddr_t     bt2_addr;               /* Address of B-tree created */
    hsize_t     idx;                    /* Index within B-tree, for iterator */
    unsigned    u;                      /* Local index variable */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR

    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Create v2 B-tree
     */
    if (H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Test inserting many records into v2 B-tree
     */
    TESTING("B-tree insert: split 3 internals to 4 in level 2 B-tree");

    /* Insert enough records to force root to split into 2 internal nodes */
    /* Also forces right-most internal node to split */
    for(u=0; u<(INSERT_SPLIT_ROOT_NREC*21); u++) {
        record=u+(INSERT_SPLIT_ROOT_NREC*20);
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    /* Force left-most internal node to split */
    /* Force middle node to split */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC*20; u++) {
        record=u;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    /* Iterate over B-tree to check records have been inserted correctly */
    idx = 0;
    if(H5B2_iterate(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, iter_cb, &idx)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /* Make certain that the index is correct */
    if(idx != (INSERT_SPLIT_ROOT_NREC*41)) TEST_ERROR

    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_insert_level2_3internal_split() */


/*-------------------------------------------------------------------------
 * Function:	test_insert_lots
 *
 * Purpose:	Basic tests for the B-tree v2 code.  This test inserts many
 *              records in random order, enough to make at a level 4 B-tree.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Saturday, February 19, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_insert_lots(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    hsize_t     record;                 /* Record to insert into tree */
    haddr_t     bt2_addr;               /* Address of B-tree created */
    hsize_t     idx;                    /* Index within B-tree, for iterator */
    time_t      curr_time;              /* Current time, for seeding random number generator */
    hsize_t     *records;               /* Record #'s for random insertion */
    unsigned    u;                      /* Local index variable */
    unsigned    swap_idx;               /* Location to swap with when shuffling */
    hsize_t     temp_rec;               /* Temporary record */
    hsize_t     nrec;                   /* Number of records in B-tree */
    herr_t      ret;                    /* Generic error return value */

    /* Initialize random number seed */
    curr_time=HDtime(NULL);
#ifdef QAK
curr_time=1109170019;
HDfprintf(stderr,"curr_time=%lu\n",(unsigned long)curr_time);
#endif /* QAK */
    HDsrandom((unsigned long)curr_time);

    /* Allocate space for the records */
    if((records = HDmalloc(sizeof(hsize_t)*INSERT_MANY))==NULL) TEST_ERROR

    /* Initialize record #'s */
    for(u=0; u<INSERT_MANY; u++)
        records[u] = u;

    /* Shuffle record #'s */
    for(u=0; u<INSERT_MANY; u++) {
        swap_idx = (unsigned)(HDrandom()%(INSERT_MANY-u))+u;
        temp_rec = records[u];
        records[u] = records[swap_idx];
        records[swap_idx] = temp_rec;
    } /* end for */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR

    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Create v2 B-tree
     */
    if (H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Test inserting many records into v2 B-tree
     */
    TESTING("B-tree insert: create random level 4 B-tree");

    /* Insert random records */
    for(u=0; u<INSERT_MANY; u++) {
        record=records[u];
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
#ifdef QAK
HDfprintf(stderr,"curr_time=%lu\n",(unsigned long)curr_time);
#endif /* QAK */
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    /* Iterate over B-tree to check records have been inserted correctly */
    idx = 0;
    if(H5B2_iterate(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, iter_cb, &idx)<0) {
#ifdef QAK
HDfprintf(stderr,"curr_time=%lu\n",(unsigned long)curr_time);
#endif /* QAK */
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /* Make certain that the index is correct */
    if(idx != INSERT_MANY) TEST_ERROR

    /* Attempt to find non-existant record in level-4 B-tree */
    idx = INSERT_MANY*2;
    H5E_BEGIN_TRY {
	ret = H5B2_find(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &idx, find_cb, &idx);
    } H5E_END_TRY;
    /* Should fail */
    if(ret != FAIL) TEST_ERROR

    /* Find random records */
    for(u=0; u<FIND_MANY; u++) {
        /* Pick random record */
        idx = (hsize_t)(HDrandom()%INSERT_MANY);

        /* Attempt to find existant record in root of level-4 B-tree */
        if(H5B2_find(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &idx, find_cb, &idx)<0) TEST_ERROR
    } /* end for */

    /* Attempt to index non-existant record in level-4 B-tree */
    idx = 0;
    H5E_BEGIN_TRY {
	ret = H5B2_index(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, (hsize_t)(INSERT_MANY*3), find_cb, NULL);
    } H5E_END_TRY;
    /* Should fail */
    if(ret != FAIL) TEST_ERROR

    /* Find random records */
    for(u=0; u<FIND_MANY; u++) {
        /* Pick random record */
        idx = (hsize_t)(HDrandom()%INSERT_MANY);

        /* Attempt to find existant record in root of level-4 B-tree */
        if(H5B2_index(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, idx, find_cb, &idx)<0) TEST_ERROR
    } /* end for */

    PASSED();

    TESTING("B-tree insert: attempt duplicate record in level 4 B-tree");

    record=INSERT_MANY/2;
    H5E_BEGIN_TRY {
        ret = H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record);
    } H5E_END_TRY;
    /* Should fail */
    if(ret != FAIL) TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the # of records is correct */
    if(nrec != INSERT_MANY) TEST_ERROR

    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR

    HDfree(records);

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    HDfree(records);
    return 1;
} /* test_insert_lots() */


/*-------------------------------------------------------------------------
 * Function:	test_remove_basic
 *
 * Purpose:	Basic tests for the B-tree v2 code
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Friday, February 25, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_remove_basic(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    hsize_t     record;                 /* Record to insert into tree */
    hsize_t     nrec;                   /* Number of records in B-tree */
    haddr_t     bt2_addr;               /* Address of B-tree created */
    haddr_t     root_addr;              /* Address of root of B-tree created */
    herr_t      ret;                    /* Generic error return value */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR

    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	TEST_ERROR
    }

    /*
     * Test v2 B-tree creation
     */
    if (H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the # of records is correct */
    if(nrec != 0) TEST_ERROR

    /* Attempt to remove a record from a B-tree with no records */
    TESTING("B-tree remove: record from empty B-tree");
    record = 0;
    H5E_BEGIN_TRY {
	ret = H5B2_remove(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record);
    } H5E_END_TRY;
    /* Should fail */
    if(ret != FAIL) TEST_ERROR

    PASSED();

    /* Insert one record into B-tree */
    record=42;
    if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the # of records is correct */
    if(nrec != 1) TEST_ERROR

    /* Query the address of the root node in the B-tree */
    if (H5B2_get_root_addr_test(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &root_addr)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the address of the root node is defined */
    if(!H5F_addr_defined(root_addr)) TEST_ERROR

    /* Attempt to remove a non-existant record from a B-tree with 1 record */
    TESTING("B-tree remove: non-existant record from 1 record B-tree");
    record = 0;
    H5E_BEGIN_TRY {
	ret = H5B2_remove(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record);
    } H5E_END_TRY;
    /* Should fail */
    if(ret != FAIL) TEST_ERROR

    PASSED();

    /* Attempt to remove a record from a B-tree with 1 record */
    TESTING("B-tree remove: existant record from 1 record B-tree");
    record = 42;
    if(H5B2_remove(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the record value is correct */
    if(record != 42) TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the # of records is correct */
    if(nrec != 0) TEST_ERROR

    /* Query the address of the root node in the B-tree */
    if (H5B2_get_root_addr_test(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &root_addr)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the root node has been freed */
    if(H5F_addr_defined(root_addr)) TEST_ERROR

    PASSED();

    /* Attempt to insert records into B-tree which had records removed */
    TESTING("B-tree remove: adding records to B-tree after removal");
    /* Insert several records into B-tree again */
    record=42;
    if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */
    record=34;
    if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }
    record=56;
    if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }
    record=38;
    if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the # of records is correct */
    if(nrec != 4) TEST_ERROR

    PASSED();

    /* Attempt to remove a non-existant record from a level-0 B-tree with mult. record */
    TESTING("B-tree remove: non-existant record from level-0 B-tree");
    record = 0;
    H5E_BEGIN_TRY {
	ret = H5B2_remove(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record);
    } H5E_END_TRY;
    /* Should fail */
    if(ret != FAIL) TEST_ERROR

    PASSED();

    /* Attempt to remove a record from a level-0 B-tree with mult. record */
    TESTING("B-tree remove: mult. existant records from level-0 B-tree");
    record = 42;
    if(H5B2_remove(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the record value is correct */
    if(record != 42) TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the # of records is correct */
    if(nrec != 3) TEST_ERROR

    /* Query the address of the root node in the B-tree */
    if (H5B2_get_root_addr_test(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &root_addr)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the root node has not been freed */
    if(!H5F_addr_defined(root_addr)) TEST_ERROR

    record = 34;
    if(H5B2_remove(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the record value is correct */
    if(record != 34) TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the # of records is correct */
    if(nrec != 2) TEST_ERROR

    /* Query the address of the root node in the B-tree */
    if (H5B2_get_root_addr_test(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &root_addr)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the root node has not been freed */
    if(!H5F_addr_defined(root_addr)) TEST_ERROR

    record = 56;
    if(H5B2_remove(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the record value is correct */
    if(record != 56) TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the # of records is correct */
    if(nrec != 1) TEST_ERROR

    /* Query the address of the root node in the B-tree */
    if (H5B2_get_root_addr_test(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &root_addr)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the root node has not been freed */
    if(!H5F_addr_defined(root_addr)) TEST_ERROR

    record = 38;
    if(H5B2_remove(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the record value is correct */
    if(record != 38) TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the # of records is correct */
    if(nrec != 0) TEST_ERROR

    /* Query the address of the root node in the B-tree */
    if (H5B2_get_root_addr_test(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &root_addr)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the root node has been freed */
    if(H5F_addr_defined(root_addr)) TEST_ERROR

    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_remove_basic() */


/*-------------------------------------------------------------------------
 * Function:	test_remove_level1_noredistrib
 *
 * Purpose:	Basic tests for the B-tree v2 code
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Friday, February 25, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_remove_level1_noredistrib(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    hsize_t     record;                 /* Record to insert into tree */
    hsize_t     nrec;                   /* Number of records in B-tree */
    haddr_t     bt2_addr;               /* Address of B-tree created */
    haddr_t     root_addr;              /* Address of root of B-tree created */
    unsigned    u;                      /* Local index variable */
    herr_t      ret;                    /* Generic error return value */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR

    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	TEST_ERROR
    }

    /*
     * Test v2 B-tree creation
     */
    if (H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /* Create level-1 B-tree with 3 leaves */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC*2; u++) {
        record=u;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the # of records is correct */
    if(nrec != (INSERT_SPLIT_ROOT_NREC*2)) TEST_ERROR

    /* Query the address of the root node in the B-tree */
    if (H5B2_get_root_addr_test(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &root_addr)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the address of the root node is defined */
    if(!H5F_addr_defined(root_addr)) TEST_ERROR

    /* Attempt to remove a non-existant record from a B-tree with 1 record */
    TESTING("B-tree remove: non-existant record from level-1 B-tree");
    record = (INSERT_SPLIT_ROOT_NREC*2)+1;
    H5E_BEGIN_TRY {
	ret = H5B2_remove(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record);
    } H5E_END_TRY;
    /* Should fail */
    if(ret != FAIL) TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the # of records is correct */
    if(nrec != (INSERT_SPLIT_ROOT_NREC*2)) TEST_ERROR

    PASSED();

    /* Attempt to remove a record from right leaf of a level-1 B-tree with noredistribution */
    TESTING("B-tree remove: record from right leaf of level-1 B-tree");
    record = (INSERT_SPLIT_ROOT_NREC*2)-2;
    if(H5B2_remove(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the record value is correct */
    if(record != ((INSERT_SPLIT_ROOT_NREC*2)-2)) TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the # of records is correct */
    if(nrec != ((INSERT_SPLIT_ROOT_NREC*2)-1)) TEST_ERROR

    PASSED();

    /* Attempt to remove a record from left leaf of a level-1 B-tree with noredistribution */
    TESTING("B-tree remove: record from left leaf of level-1 B-tree");
    record = 0;
    if(H5B2_remove(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the record value is correct */
    if(record != 0) TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the # of records is correct */
    if(nrec != ((INSERT_SPLIT_ROOT_NREC*2)-2)) TEST_ERROR

    PASSED();

    /* Attempt to remove a record from middle leaf of a level-1 B-tree with noredistribution */
    TESTING("B-tree remove: record from middle leaf of level-1 B-tree");
    record = INSERT_SPLIT_ROOT_NREC;
    if(H5B2_remove(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the record value is correct */
    if(record != INSERT_SPLIT_ROOT_NREC) TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the # of records is correct */
    if(nrec != ((INSERT_SPLIT_ROOT_NREC*2)-3)) TEST_ERROR

    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_remove_level1_noredistrib() */


/*-------------------------------------------------------------------------
 * Function:	test_remove_level1_redistrib
 *
 * Purpose:	Basic tests for the B-tree v2 code
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Friday, March  4, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_remove_level1_redistrib(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    hsize_t     record;                 /* Record to insert into tree */
    hsize_t     nrec;                   /* Number of records in B-tree */
    haddr_t     bt2_addr;               /* Address of B-tree created */
    haddr_t     root_addr;              /* Address of root of B-tree created */
    unsigned    u;                      /* Local index variable */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR

    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	TEST_ERROR
    }

    /*
     * Test v2 B-tree creation
     */
    if (H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /* Create level-1 B-tree with 3 leaves */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC*2; u++) {
        record=u;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the # of records is correct */
    if(nrec != (INSERT_SPLIT_ROOT_NREC*2)) TEST_ERROR

    /* Query the address of the root node in the B-tree */
    if (H5B2_get_root_addr_test(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &root_addr)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the address of the root node is defined */
    if(!H5F_addr_defined(root_addr)) TEST_ERROR

    /* Attempt to remove enough records from right leaf of a level-1 B-tree to force redistribution */
    TESTING("B-tree remove: redistribute 2 leaves in level-1 B-tree (r->l)");
    for(u=0; u < (INSERT_SPLIT_ROOT_NREC/2); u++) {
        record = (INSERT_SPLIT_ROOT_NREC*2)-(u+1);
        if(H5B2_remove(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        } /* end if */

        /* Make certain that the record value is correct */
        if(record != ((INSERT_SPLIT_ROOT_NREC*2)-(u+1))) TEST_ERROR

        /* Query the number of records in the B-tree */
        if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        } /* end if */

        /* Make certain that the # of records is correct */
        if(nrec != ((INSERT_SPLIT_ROOT_NREC*2)-(u+1))) TEST_ERROR
    } /* end for */

    PASSED();

    /* Attempt to remove enough records from left leaf of a level-1 B-tree to force redistribution */
    TESTING("B-tree remove: redistribute 2 leaves in level-1 B-tree (l->r)");
    for(u=0; u < (INSERT_SPLIT_ROOT_NREC/4); u++) {
        record = u;
        if(H5B2_remove(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        } /* end if */

        /* Make certain that the record value is correct */
        if(record != u) TEST_ERROR

        /* Query the number of records in the B-tree */
        if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        } /* end if */

        /* Make certain that the # of records is correct */
        if(nrec != (((INSERT_SPLIT_ROOT_NREC*2)-(INSERT_SPLIT_ROOT_NREC/2))-(u+1))) TEST_ERROR
    } /* end for */

    PASSED();

    /* Attempt to remove enough records from middle leaf of a level-1 B-tree to force redistribution */
    TESTING("B-tree remove: redistribute 3 leaves in level-1 B-tree");
    for(u=0; u < (INSERT_SPLIT_ROOT_NREC/8); u++) {
        record = ((INSERT_SPLIT_ROOT_NREC/4)*3) + u;
        if(H5B2_remove(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        } /* end if */

        /* Make certain that the record value is correct */
        if(record != (((INSERT_SPLIT_ROOT_NREC/4)*3) + u)) TEST_ERROR

        /* Query the number of records in the B-tree */
        if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        } /* end if */

        /* Make certain that the # of records is correct */
        if(nrec != (((INSERT_SPLIT_ROOT_NREC*2)-(3*(INSERT_SPLIT_ROOT_NREC/4)))-(u+1))) TEST_ERROR
    } /* end for */

    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_remove_level1_redistrib() */


/*-------------------------------------------------------------------------
 * Function:	test_remove_level1_2leaf_merge
 *
 * Purpose:	Basic tests for the B-tree v2 code
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Friday, March  4, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_remove_level1_2leaf_merge(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    hsize_t     record;                 /* Record to insert into tree */
    hsize_t     nrec;                   /* Number of records in B-tree */
    haddr_t     bt2_addr;               /* Address of B-tree created */
    haddr_t     root_addr;              /* Address of root of B-tree created */
    unsigned    u;                      /* Local index variable */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR

    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	TEST_ERROR
    }

    /*
     * Test v2 B-tree creation
     */
    if (H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /* Create level-1 B-tree with 3 leaves */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC*2; u++) {
        record=u;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the # of records is correct */
    if(nrec != (INSERT_SPLIT_ROOT_NREC*2)) TEST_ERROR

    /* Query the address of the root node in the B-tree */
    if (H5B2_get_root_addr_test(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &root_addr)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the address of the root node is defined */
    if(!H5F_addr_defined(root_addr)) TEST_ERROR

    /* Attempt to remove enough records from right leaf of a level-1 B-tree to force redistribution */
    TESTING("B-tree remove: merge 2 leaves to 1 in level-1 B-tree (r->l)");
    for(u=0; u < INSERT_SPLIT_ROOT_NREC; u++) {
        record = (INSERT_SPLIT_ROOT_NREC*2)-(u+1);
        if(H5B2_remove(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        } /* end if */

        /* Make certain that the record value is correct */
        if(record != ((INSERT_SPLIT_ROOT_NREC*2)-(u+1))) TEST_ERROR

        /* Query the number of records in the B-tree */
        if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        } /* end if */

        /* Make certain that the # of records is correct */
        if(nrec != ((INSERT_SPLIT_ROOT_NREC*2)-(u+1))) TEST_ERROR
    } /* end for */

    PASSED();

    /* Attempt to remove enough records from left leaf of a level-1 B-tree to force redistribution */
    TESTING("B-tree remove: merge 2 leaves to 1 in level-1 B-tree (l->r)");

    /* Fill B-tree back up */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC; u++) {
        record=INSERT_SPLIT_ROOT_NREC+u;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    /* Remove records */
    for(u=0; u < INSERT_SPLIT_ROOT_NREC; u++) {
        record = u;
        if(H5B2_remove(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        } /* end if */

        /* Make certain that the record value is correct */
        if(record != u) TEST_ERROR

        /* Query the number of records in the B-tree */
        if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        } /* end if */

        /* Make certain that the # of records is correct */
        if(nrec != ((INSERT_SPLIT_ROOT_NREC*2)-(u+1))) TEST_ERROR
    } /* end for */

    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_remove_level1_2leaf_merge() */


/*-------------------------------------------------------------------------
 * Function:	test_remove_level1_3leaf_merge
 *
 * Purpose:	Basic tests for the B-tree v2 code
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Friday, March  4, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_remove_level1_3leaf_merge(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    hsize_t     record;                 /* Record to insert into tree */
    hsize_t     nrec;                   /* Number of records in B-tree */
    haddr_t     bt2_addr;               /* Address of B-tree created */
    haddr_t     root_addr;              /* Address of root of B-tree created */
    unsigned    u;                      /* Local index variable */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR

    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	TEST_ERROR
    }

    /*
     * Test v2 B-tree creation
     */
    if (H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /* Create level-1 B-tree with 3 leaves */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC*2; u++) {
        record=u;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the # of records is correct */
    if(nrec != (INSERT_SPLIT_ROOT_NREC*2)) TEST_ERROR

    /* Query the address of the root node in the B-tree */
    if (H5B2_get_root_addr_test(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &root_addr)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the address of the root node is defined */
    if(!H5F_addr_defined(root_addr)) TEST_ERROR

    /* Attempt to remove enough records from middle leaf of a level-1 B-tree to force merge */
    TESTING("B-tree remove: merge 3 leaves to 2 in level-1 B-tree");
    for(u=0; u < INSERT_SPLIT_ROOT_NREC+2; u++) {
        record = (INSERT_SPLIT_ROOT_NREC/2)+3+u;
        if(H5B2_remove(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        } /* end if */

        /* Make certain that the record value is correct */
        if(record != ((INSERT_SPLIT_ROOT_NREC/2)+3+u)) TEST_ERROR

        /* Query the number of records in the B-tree */
        if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        } /* end if */

        /* Make certain that the # of records is correct */
        if(nrec != ((INSERT_SPLIT_ROOT_NREC*2)-(u+1))) TEST_ERROR
    } /* end for */

    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_remove_level1_3leaf_merge() */


/*-------------------------------------------------------------------------
 * Function:	test_remove_level1_promote
 *
 * Purpose:	Basic tests for the B-tree v2 code
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Friday, March  4, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_remove_level1_promote(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    hsize_t     record;                 /* Record to insert into tree */
    hsize_t     nrec;                   /* Number of records in B-tree */
    haddr_t     bt2_addr;               /* Address of B-tree created */
    haddr_t     root_addr;              /* Address of root of B-tree created */
    unsigned    u;                      /* Local index variable */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR

    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	TEST_ERROR
    }

    /*
     * Test v2 B-tree creation
     */
    if (H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /* Create level-1 B-tree with 5 leaves */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC*3; u++) {
        record=u;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the # of records is correct */
    if(nrec != (INSERT_SPLIT_ROOT_NREC*3)) TEST_ERROR

    /* Query the address of the root node in the B-tree */
    if (H5B2_get_root_addr_test(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &root_addr)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the address of the root node is defined */
    if(!H5F_addr_defined(root_addr)) TEST_ERROR

    /* Attempt to remove record from root node of a level-1 B-tree to force promotion from right leaf */
    TESTING("B-tree remove: promote from right leaf of level-1 B-tree");
    record = 181;
    if(H5B2_remove(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    } /* end if */

    /* Make certain that the record value is correct */
    if(record != 181) TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the # of records is correct */
    if(nrec != (INSERT_SPLIT_ROOT_NREC*3)-1) TEST_ERROR

    PASSED();

    /* Attempt to remove record from root node of a level-1 B-tree to force promotion from left leaf */
    TESTING("B-tree remove: promote from left leaf of level-1 B-tree");
    record = 42;
    if(H5B2_remove(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    } /* end if */

    /* Make certain that the record value is correct */
    if(record != 42) TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the # of records is correct */
    if(nrec != (INSERT_SPLIT_ROOT_NREC*3)-2) TEST_ERROR

    PASSED();

    /* Attempt to remove record from root node of a level-1 B-tree to force promotion from middle leaf */
    TESTING("B-tree remove: promote from middle leaf of level-1 B-tree");
    record = 85;
    if(H5B2_remove(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    } /* end if */

    /* Make certain that the record value is correct */
    if(record != 85) TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the # of records is correct */
    if(nrec != (INSERT_SPLIT_ROOT_NREC*3)-3) TEST_ERROR

    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_remove_level1_promote() */


/*-------------------------------------------------------------------------
 * Function:	test_remove_level1_promote_2leaf_redistrib
 *
 * Purpose:	Basic tests for the B-tree v2 code
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Friday, March  4, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_remove_level1_promote_2leaf_redistrib(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    hsize_t     record;                 /* Record to insert into tree */
    hsize_t     nrec;                   /* Number of records in B-tree */
    haddr_t     bt2_addr;               /* Address of B-tree created */
    haddr_t     root_addr;              /* Address of root of B-tree created */
    unsigned    u;                      /* Local index variable */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR

    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	TEST_ERROR
    }

    /*
     * Test v2 B-tree creation
     */
    if (H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /* Create level-1 B-tree with 3 leaves */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC*2; u++) {
        record=u;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the # of records is correct */
    if(nrec != (INSERT_SPLIT_ROOT_NREC*2)) TEST_ERROR

    /* Query the address of the root node in the B-tree */
    if (H5B2_get_root_addr_test(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &root_addr)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the address of the root node is defined */
    if(!H5F_addr_defined(root_addr)) TEST_ERROR

    /* Attempt to remove record from root node of a level-1 B-tree to force promotion from right leaf */
    TESTING("B-tree remove: promote from leaf of level-1 B-tree w/2 node redistrib");

    /* Remove records from right leaf until its ready to redistribute */
    for(u=0; u < 33; u++) {
        record = (INSERT_SPLIT_ROOT_NREC*2)-(u+1);
        if(H5B2_remove(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        } /* end if */

        /* Make certain that the record value is correct */
        if(record != ((INSERT_SPLIT_ROOT_NREC*2)-(u+1))) TEST_ERROR

        /* Query the number of records in the B-tree */
        if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        } /* end if */

        /* Make certain that the # of records is correct */
        if(nrec != ((INSERT_SPLIT_ROOT_NREC*2)-(u+1))) TEST_ERROR
    } /* end for */

    record = 101;
    if(H5B2_remove(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    } /* end if */

    /* Make certain that the record value is correct */
    if(record != 101) TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the # of records is correct */
    if(nrec != (INSERT_SPLIT_ROOT_NREC*2)-34) TEST_ERROR

    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_remove_level1_promote_2leaf_redistrib() */


/*-------------------------------------------------------------------------
 * Function:	test_remove_level1_promote_3leaf_redistrib
 *
 * Purpose:	Basic tests for the B-tree v2 code
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Friday, March  4, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_remove_level1_promote_3leaf_redistrib(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    hsize_t     record;                 /* Record to insert into tree */
    hsize_t     nrec;                   /* Number of records in B-tree */
    haddr_t     bt2_addr;               /* Address of B-tree created */
    haddr_t     root_addr;              /* Address of root of B-tree created */
    unsigned    u;                      /* Local index variable */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR

    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	TEST_ERROR
    }

    /*
     * Test v2 B-tree creation
     */
    if (H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /* Create level-1 B-tree with 3 leaves */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC*2; u++) {
        record=u;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the # of records is correct */
    if(nrec != (INSERT_SPLIT_ROOT_NREC*2)) TEST_ERROR

    /* Query the address of the root node in the B-tree */
    if (H5B2_get_root_addr_test(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &root_addr)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the address of the root node is defined */
    if(!H5F_addr_defined(root_addr)) TEST_ERROR

    /* Attempt to remove record from root node of a level-1 B-tree to force promotion from middle leaf */
    TESTING("B-tree remove: promote from leaf of level-1 B-tree w/3 node redistrib");

    /* Remove records from right leaf until its ready to redistribute */
    for(u=0; u < 33; u++) {
        record = 43 + u;
        if(H5B2_remove(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        } /* end if */

        /* Make certain that the record value is correct */
        if(record != (43 + u)) TEST_ERROR

        /* Query the number of records in the B-tree */
        if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        } /* end if */

        /* Make certain that the # of records is correct */
        if(nrec != ((INSERT_SPLIT_ROOT_NREC*2)-(u+1))) TEST_ERROR
    } /* end for */

    record = 42;
    if(H5B2_remove(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    } /* end if */

    /* Make certain that the record value is correct */
    if(record != 42) TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the # of records is correct */
    if(nrec != (INSERT_SPLIT_ROOT_NREC*2)-34) TEST_ERROR

    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_remove_level1_promote_3leaf_redistrib() */


/*-------------------------------------------------------------------------
 * Function:	test_remove_level1_promote_2leaf_merge
 *
 * Purpose:	Basic tests for the B-tree v2 code
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Friday, March  4, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_remove_level1_promote_2leaf_merge(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    hsize_t     record;                 /* Record to insert into tree */
    hsize_t     nrec;                   /* Number of records in B-tree */
    haddr_t     bt2_addr;               /* Address of B-tree created */
    haddr_t     root_addr;              /* Address of root of B-tree created */
    unsigned    u;                      /* Local index variable */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR

    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	TEST_ERROR
    }

    /*
     * Test v2 B-tree creation
     */
    if (H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /* Create level-1 B-tree with 3 leaves */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC*2; u++) {
        record=u;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the # of records is correct */
    if(nrec != (INSERT_SPLIT_ROOT_NREC*2)) TEST_ERROR

    /* Query the address of the root node in the B-tree */
    if (H5B2_get_root_addr_test(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &root_addr)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the address of the root node is defined */
    if(!H5F_addr_defined(root_addr)) TEST_ERROR

    /* Attempt to remove record from root node of a level-1 B-tree to force promotion from right leaf */
    TESTING("B-tree remove: promote from leaf of level-1 B-tree w/2->1 merge");

    /* Remove records from right leaf until its ready to redistribute */
    for(u=0; u < 66; u++) {
        record = (INSERT_SPLIT_ROOT_NREC*2)-(u+1);
        if(H5B2_remove(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        } /* end if */

        /* Make certain that the record value is correct */
        if(record != ((INSERT_SPLIT_ROOT_NREC*2)-(u+1))) TEST_ERROR

        /* Query the number of records in the B-tree */
        if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        } /* end if */

        /* Make certain that the # of records is correct */
        if(nrec != ((INSERT_SPLIT_ROOT_NREC*2)-(u+1))) TEST_ERROR
    } /* end for */

    record = 68;
    if(H5B2_remove(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    } /* end if */

    /* Make certain that the record value is correct */
    if(record != 68) TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the # of records is correct */
    if(nrec != (INSERT_SPLIT_ROOT_NREC*2)-67) TEST_ERROR

    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_remove_level1_promote_2leaf_merge() */


/*-------------------------------------------------------------------------
 * Function:	test_remove_level1_promote_3leaf_merge
 *
 * Purpose:	Basic tests for the B-tree v2 code
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Friday, March  4, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_remove_level1_promote_3leaf_merge(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    hsize_t     record;                 /* Record to insert into tree */
    hsize_t     nrec;                   /* Number of records in B-tree */
    haddr_t     bt2_addr;               /* Address of B-tree created */
    haddr_t     root_addr;              /* Address of root of B-tree created */
    unsigned    u;                      /* Local index variable */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR

    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	TEST_ERROR
    }

    /*
     * Test v2 B-tree creation
     */
    if (H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /* Create level-1 B-tree with 3 leaves */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC*2; u++) {
        record=u;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the # of records is correct */
    if(nrec != (INSERT_SPLIT_ROOT_NREC*2)) TEST_ERROR

    /* Query the address of the root node in the B-tree */
    if (H5B2_get_root_addr_test(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &root_addr)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the address of the root node is defined */
    if(!H5F_addr_defined(root_addr)) TEST_ERROR

    /* Attempt to remove record from root node of a level-1 B-tree to force promotion from middle leaf */
    TESTING("B-tree remove: promote from leaf of level-1 B-tree w/3->2 merge");

    /* Remove records from right leaf until its ready to redistribute */
    for(u=0; u < 81; u++) {
        record = 43 + u;
        if(H5B2_remove(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        } /* end if */

        /* Make certain that the record value is correct */
        if(record != (43 + u)) TEST_ERROR

        /* Query the number of records in the B-tree */
        if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        } /* end if */

        /* Make certain that the # of records is correct */
        if(nrec != ((INSERT_SPLIT_ROOT_NREC*2)-(u+1))) TEST_ERROR
    } /* end for */

    record = 26;
    if(H5B2_remove(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    } /* end if */

    /* Make certain that the record value is correct */
    if(record != 26) TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the # of records is correct */
    if(nrec != (INSERT_SPLIT_ROOT_NREC*2)-82) TEST_ERROR

    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_remove_level1_promote_3leaf_merge() */


/*-------------------------------------------------------------------------
 * Function:	test_remove_level1_collapse
 *
 * Purpose:	Basic tests for the B-tree v2 code
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Friday, March  4, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_remove_level1_collapse(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    hsize_t     record;                 /* Record to insert into tree */
    hsize_t     nrec;                   /* Number of records in B-tree */
    haddr_t     bt2_addr;               /* Address of B-tree created */
    haddr_t     root_addr;              /* Address of root of B-tree created */
    unsigned    u;                      /* Local index variable */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR

    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	TEST_ERROR
    }

    /*
     * Test v2 B-tree creation
     */
    if (H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /* Create level-1 B-tree with 2 leaves */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC; u++) {
        record=u;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the # of records is correct */
    if(nrec != (INSERT_SPLIT_ROOT_NREC)) TEST_ERROR

    /* Query the address of the root node in the B-tree */
    if (H5B2_get_root_addr_test(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &root_addr)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the address of the root node is defined */
    if(!H5F_addr_defined(root_addr)) TEST_ERROR

    /* Attempt to remove records from B-tree to force a single leaf for the B-tree */
    TESTING("B-tree remove: collapse level-1 B-tree back to level-0");
    for(u=0; u < 30; u++) {
        record = INSERT_SPLIT_ROOT_NREC - (u+1);
        if(H5B2_remove(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        } /* end if */

        /* Make certain that the record value is correct */
        if(record != (INSERT_SPLIT_ROOT_NREC - (u+1))) TEST_ERROR

        /* Query the number of records in the B-tree */
        if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        } /* end if */

        /* Make certain that the # of records is correct */
        if(nrec != (INSERT_SPLIT_ROOT_NREC-(u+1))) TEST_ERROR
    } /* end for */

    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_remove_level1_collapse() */


/*-------------------------------------------------------------------------
 * Function:	test_remove_level2_promote
 *
 * Purpose:	Basic tests for the B-tree v2 code
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Friday, March  4, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_remove_level2_promote(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    hsize_t     record;                 /* Record to insert into tree */
    hsize_t     nrec;                   /* Number of records in B-tree */
    haddr_t     bt2_addr;               /* Address of B-tree created */
    haddr_t     root_addr;              /* Address of root of B-tree created */
    unsigned    u;                      /* Local index variable */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR

    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	TEST_ERROR
    }

    /*
     * Test v2 B-tree creation
     */
    if (H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /* Create level-2 B-tree with 3 internal nodes */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC*21; u++) {
        record=u;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the # of records is correct */
    if(nrec != (INSERT_SPLIT_ROOT_NREC*21)) TEST_ERROR

    /* Query the address of the root node in the B-tree */
    if (H5B2_get_root_addr_test(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &root_addr)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the address of the root node is defined */
    if(!H5F_addr_defined(root_addr)) TEST_ERROR

    /* Attempt to remove record from right internal node of a level-2 B-tree to force promotion */
    TESTING("B-tree remove: promote from right internal of level-2 B-tree");
    record = 1632;
    if(H5B2_remove(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    } /* end if */

    /* Make certain that the record value is correct */
    if(record != 1632) TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the # of records is correct */
    if(nrec != (INSERT_SPLIT_ROOT_NREC*21)-1) TEST_ERROR

    PASSED();

    /* Attempt to remove record from left internal node of a level-2 B-tree to force promotion */
    TESTING("B-tree remove: promote from left internal of level-2 B-tree");
    record = 1117;
    if(H5B2_remove(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    } /* end if */

    /* Make certain that the record value is correct */
    if(record != 1117) TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the # of records is correct */
    if(nrec != (INSERT_SPLIT_ROOT_NREC*21)-2) TEST_ERROR

    PASSED();

    /* Attempt to remove record from middle internal node of a level-2 B-tree to force promotion */
    TESTING("B-tree remove: promote from middle internal of level-2 B-tree");
    record = 1246;
    if(H5B2_remove(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    } /* end if */

    /* Make certain that the record value is correct */
    if(record != 1246) TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the # of records is correct */
    if(nrec != (INSERT_SPLIT_ROOT_NREC*21)-3) TEST_ERROR

    PASSED();

    /* Attempt to remove record from root node of a level-2 B-tree to force promotion */
    TESTING("B-tree remove: promote from root of level-2 B-tree");
    record = 1074;
    if(H5B2_remove(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    } /* end if */

    /* Make certain that the record value is correct */
    if(record != 1074) TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the # of records is correct */
    if(nrec != (INSERT_SPLIT_ROOT_NREC*21)-4) TEST_ERROR

    record = 558;
    if(H5B2_remove(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    } /* end if */

    /* Make certain that the record value is correct */
    if(record != 558) TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the # of records is correct */
    if(nrec != (INSERT_SPLIT_ROOT_NREC*21)-5) TEST_ERROR

    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_remove_level2_promote() */


/*-------------------------------------------------------------------------
 * Function:	test_remove_level2_promote_2internal_redistrib
 *
 * Purpose:	Basic tests for the B-tree v2 code
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, March  7, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_remove_level2_promote_2internal_redistrib(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    hsize_t     record;                 /* Record to insert into tree */
    hsize_t     nrec;                   /* Number of records in B-tree */
    haddr_t     bt2_addr;               /* Address of B-tree created */
    haddr_t     root_addr;              /* Address of root of B-tree created */
    unsigned    u;                      /* Local index variable */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR

    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	TEST_ERROR
    }

    /*
     * Test v2 B-tree creation
     */
    if (H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /* Create level-2 B-tree with 3 internal nodes */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC*21; u++) {
        record=u;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the # of records is correct */
    if(nrec != (INSERT_SPLIT_ROOT_NREC*21)) TEST_ERROR

    /* Query the address of the root node in the B-tree */
    if (H5B2_get_root_addr_test(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &root_addr)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the address of the root node is defined */
    if(!H5F_addr_defined(root_addr)) TEST_ERROR

    /* Attempt to remove record from right internal node of a level-2 B-tree to force promotion w/redistribution */
    TESTING("B-tree remove: promote from right internal of level-2 B-tree w/redistrib");
    for(u=0; u < 22; u++) {
        record = (INSERT_SPLIT_ROOT_NREC*21) - (u+1);
        if(H5B2_remove(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        } /* end if */

        /* Make certain that the record value is correct */
        if(record != ((INSERT_SPLIT_ROOT_NREC*21) - (u+1))) TEST_ERROR

        /* Query the number of records in the B-tree */
        if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        } /* end if */

        /* Make certain that the # of records is correct */
        if(nrec != ((INSERT_SPLIT_ROOT_NREC*21)-(u+1))) TEST_ERROR
    } /* end for */

    record = 1632;
    if(H5B2_remove(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    } /* end if */

    /* Make certain that the record value is correct */
    if(record != 1632) TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the # of records is correct */
    if(nrec != (INSERT_SPLIT_ROOT_NREC*21)-23) TEST_ERROR

    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_remove_level2_promote_2internal_redistrib() */


/*-------------------------------------------------------------------------
 * Function:	test_remove_level2_3promote_internal_redistrib
 *
 * Purpose:	Basic tests for the B-tree v2 code
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, March  7, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_remove_level2_promote_3internal_redistrib(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    hsize_t     record;                 /* Record to insert into tree */
    hsize_t     nrec;                   /* Number of records in B-tree */
    haddr_t     bt2_addr;               /* Address of B-tree created */
    haddr_t     root_addr;              /* Address of root of B-tree created */
    unsigned    u;                      /* Local index variable */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR

    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	TEST_ERROR
    }

    /*
     * Test v2 B-tree creation
     */
    if (H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /* Create level-2 B-tree with 3 internal nodes */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC*21; u++) {
        record=u;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the # of records is correct */
    if(nrec != (INSERT_SPLIT_ROOT_NREC*21)) TEST_ERROR

    /* Query the address of the root node in the B-tree */
    if (H5B2_get_root_addr_test(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &root_addr)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the address of the root node is defined */
    if(!H5F_addr_defined(root_addr)) TEST_ERROR

    /* Attempt to remove record from right internal node of a level-2 B-tree to force promotion w/redistribution */
    TESTING("B-tree remove: promote from right internal of level-2 B-tree w/redistrib");
    for(u=0; u < 17; u++) {
        record = 43 + u;
        if(H5B2_remove(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        } /* end if */

        /* Make certain that the record value is correct */
        if(record != (43 + u)) TEST_ERROR

        /* Query the number of records in the B-tree */
        if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        } /* end if */

        /* Make certain that the # of records is correct */
        if(nrec != ((INSERT_SPLIT_ROOT_NREC*21)-(u+1))) TEST_ERROR
    } /* end for */

    record = 42;
    if(H5B2_remove(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    } /* end if */

    /* Make certain that the record value is correct */
    if(record != 42) TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the # of records is correct */
    if(nrec != (INSERT_SPLIT_ROOT_NREC*21)-18) TEST_ERROR

    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_remove_level2_promote_3internal_redistrib() */


/*-------------------------------------------------------------------------
 * Function:	test_remove_level2_promote_2internal_merge
 *
 * Purpose:	Basic tests for the B-tree v2 code
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, March  7, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_remove_level2_promote_2internal_merge(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    hsize_t     record;                 /* Record to insert into tree */
    hsize_t     nrec;                   /* Number of records in B-tree */
    haddr_t     bt2_addr;               /* Address of B-tree created */
    haddr_t     root_addr;              /* Address of root of B-tree created */
    unsigned    u;                      /* Local index variable */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR

    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	TEST_ERROR
    }

    /*
     * Test v2 B-tree creation
     */
    if (H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /* Create level-2 B-tree with 3 internal nodes */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC*21; u++) {
        record=u;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the # of records is correct */
    if(nrec != (INSERT_SPLIT_ROOT_NREC*21)) TEST_ERROR

    /* Query the address of the root node in the B-tree */
    if (H5B2_get_root_addr_test(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &root_addr)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the address of the root node is defined */
    if(!H5F_addr_defined(root_addr)) TEST_ERROR

    /* Attempt to remove record from right internal node of a level-2 B-tree to force promotion w/redistribution */
    TESTING("B-tree remove: promote from right internal of level-2 B-tree w/redistrib");
    for(u=0; u < 38; u++) {
        record = (INSERT_SPLIT_ROOT_NREC*21) - (u+1);
        if(H5B2_remove(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        } /* end if */

        /* Make certain that the record value is correct */
        if(record != ((INSERT_SPLIT_ROOT_NREC*21) - (u+1))) TEST_ERROR

        /* Query the number of records in the B-tree */
        if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        } /* end if */

        /* Make certain that the # of records is correct */
        if(nrec != ((INSERT_SPLIT_ROOT_NREC*21)-(u+1))) TEST_ERROR
    } /* end for */

    record = 1616;
    if(H5B2_remove(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    } /* end if */

    /* Make certain that the record value is correct */
    if(record != 1616) TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the # of records is correct */
    if(nrec != (INSERT_SPLIT_ROOT_NREC*21)-39) TEST_ERROR

    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_remove_level2_promote_2internal_merge() */


/*-------------------------------------------------------------------------
 * Function:	test_remove_level2_3promote_internal_merge
 *
 * Purpose:	Basic tests for the B-tree v2 code
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, March  7, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_remove_level2_promote_3internal_merge(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    hsize_t     record;                 /* Record to insert into tree */
    hsize_t     nrec;                   /* Number of records in B-tree */
    haddr_t     bt2_addr;               /* Address of B-tree created */
    haddr_t     root_addr;              /* Address of root of B-tree created */
    unsigned    u;                      /* Local index variable */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR

    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	TEST_ERROR
    }

    /*
     * Test v2 B-tree creation
     */
    if (H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /* Create level-2 B-tree with 3 internal nodes */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC*21; u++) {
        record=u;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the # of records is correct */
    if(nrec != (INSERT_SPLIT_ROOT_NREC*21)) TEST_ERROR

    /* Query the address of the root node in the B-tree */
    if (H5B2_get_root_addr_test(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &root_addr)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the address of the root node is defined */
    if(!H5F_addr_defined(root_addr)) TEST_ERROR

    /* Attempt to remove record from right internal node of a level-2 B-tree to force promotion w/redistribution */
    TESTING("B-tree remove: promote from right internal of level-2 B-tree w/redistrib");
    for(u=0; u < 49; u++) {
        record = 43 + u;
        if(H5B2_remove(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        } /* end if */

        /* Make certain that the record value is correct */
        if(record != (43 + u)) TEST_ERROR

        /* Query the number of records in the B-tree */
        if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        } /* end if */

        /* Make certain that the # of records is correct */
        if(nrec != ((INSERT_SPLIT_ROOT_NREC*21)-(u+1))) TEST_ERROR
    } /* end for */

    record = 26;
    if(H5B2_remove(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    } /* end if */

    /* Make certain that the record value is correct */
    if(record != 26) TEST_ERROR

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the # of records is correct */
    if(nrec != (INSERT_SPLIT_ROOT_NREC*21)-50) TEST_ERROR

    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_remove_level2_promote_3internal_merge() */


/*-------------------------------------------------------------------------
 * Function:	test_remove_level2_2internal_merge_left
 *
 * Purpose:	Basic tests for the B-tree v2 code
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, March  8, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_remove_level2_2internal_merge_left(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    hsize_t     record;                 /* Record to insert into tree */
    hsize_t     nrec;                   /* Number of records in B-tree */
    haddr_t     bt2_addr;               /* Address of B-tree created */
    haddr_t     root_addr;              /* Address of root of B-tree created */
    unsigned    u;                      /* Local index variable */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR

    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	TEST_ERROR
    }

    /*
     * Test v2 B-tree creation
     */
    if (H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /* Create level-2 B-tree with 3 internal nodes */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC*21; u++) {
        record=u;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the # of records is correct */
    if(nrec != (INSERT_SPLIT_ROOT_NREC*21)) TEST_ERROR

    /* Query the address of the root node in the B-tree */
    if (H5B2_get_root_addr_test(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &root_addr)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the address of the root node is defined */
    if(!H5F_addr_defined(root_addr)) TEST_ERROR

    /* Attempt to remove records from a level-2 B-tree to force 2 internal nodes to merge */
    TESTING("B-tree remove: merge 2 internal nodes to 1 in level-2 B-tree (l->r)");
    for(u=0; u < (INSERT_SPLIT_ROOT_NREC*5); u++) {
        record = u;
        if(H5B2_remove(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        } /* end if */

        /* Make certain that the record value is correct */
        if(record != u) TEST_ERROR

        /* Query the number of records in the B-tree */
        if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        } /* end if */

        /* Make certain that the # of records is correct */
        if(nrec != ((INSERT_SPLIT_ROOT_NREC*21)-(u+1))) TEST_ERROR
    } /* end for */

    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_remove_level2_2internal_merge_left() */


/*-------------------------------------------------------------------------
 * Function:	test_remove_level2_2internal_merge_right
 *
 * Purpose:	Basic tests for the B-tree v2 code
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, March  8, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_remove_level2_2internal_merge_right(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    hsize_t     record;                 /* Record to insert into tree */
    hsize_t     nrec;                   /* Number of records in B-tree */
    haddr_t     bt2_addr;               /* Address of B-tree created */
    haddr_t     root_addr;              /* Address of root of B-tree created */
    unsigned    u;                      /* Local index variable */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR

    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	TEST_ERROR
    }

    /*
     * Test v2 B-tree creation
     */
    if (H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /* Create level-2 B-tree with 3 internal nodes */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC*21; u++) {
        record=u;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the # of records is correct */
    if(nrec != (INSERT_SPLIT_ROOT_NREC*21)) TEST_ERROR

    /* Query the address of the root node in the B-tree */
    if (H5B2_get_root_addr_test(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &root_addr)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the address of the root node is defined */
    if(!H5F_addr_defined(root_addr)) TEST_ERROR

    /* Attempt to remove records from a level-2 B-tree to force 2 internal nodes to merge */
    TESTING("B-tree remove: merge 2 internal nodes to 1 in level-2 B-tree (r->l)");
    for(u=0; u < (INSERT_SPLIT_ROOT_NREC*6); u++) {
        record = (INSERT_SPLIT_ROOT_NREC*21) - (u + 1);
        if(H5B2_remove(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        } /* end if */

        /* Make certain that the record value is correct */
        if(record != ((INSERT_SPLIT_ROOT_NREC*21) - (u + 1))) TEST_ERROR

        /* Query the number of records in the B-tree */
        if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        } /* end if */

        /* Make certain that the # of records is correct */
        if(nrec != ((INSERT_SPLIT_ROOT_NREC*21)-(u+1))) TEST_ERROR
    } /* end for */

    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_remove_level2_2internal_merge_right() */


/*-------------------------------------------------------------------------
 * Function:	test_remove_level2_3internal_merge
 *
 * Purpose:	Basic tests for the B-tree v2 code
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, March  8, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_remove_level2_3internal_merge(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    hsize_t     record;                 /* Record to insert into tree */
    hsize_t     nrec;                   /* Number of records in B-tree */
    haddr_t     bt2_addr;               /* Address of B-tree created */
    haddr_t     root_addr;              /* Address of root of B-tree created */
    unsigned    u;                      /* Local index variable */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR

    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	TEST_ERROR
    }

    /*
     * Test v2 B-tree creation
     */
    if (H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /* Create level-2 B-tree with 3 internal nodes */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC*21; u++) {
        record=u;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the # of records is correct */
    if(nrec != (INSERT_SPLIT_ROOT_NREC*21)) TEST_ERROR

    /* Query the address of the root node in the B-tree */
    if (H5B2_get_root_addr_test(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &root_addr)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the address of the root node is defined */
    if(!H5F_addr_defined(root_addr)) TEST_ERROR

    /* Attempt to remove record from right internal node of a level-2 B-tree to force promotion w/redistribution */
    TESTING("B-tree remove: merge 3 internal nodes to 2 in level-2 B-tree");
    for(u=0; u < (INSERT_SPLIT_ROOT_NREC*8); u++) {
        record = 559 + u;
        if(H5B2_remove(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        } /* end if */

        /* Make certain that the record value is correct */
        if(record != (559 + u)) TEST_ERROR

        /* Query the number of records in the B-tree */
        if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        } /* end if */

        /* Make certain that the # of records is correct */
        if(nrec != ((INSERT_SPLIT_ROOT_NREC*21)-(u+1))) TEST_ERROR
    } /* end for */

    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_remove_level2_3internal_merge() */


/*-------------------------------------------------------------------------
 * Function:	test_remove_level2_collapse_right
 *
 * Purpose:	Basic tests for the B-tree v2 code
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, March  8, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_remove_level2_collapse_right(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    hsize_t     record;                 /* Record to insert into tree */
    hsize_t     nrec;                   /* Number of records in B-tree */
    haddr_t     bt2_addr;               /* Address of B-tree created */
    haddr_t     root_addr;              /* Address of root of B-tree created */
    unsigned    u;                      /* Local index variable */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR

    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	TEST_ERROR
    }

    /*
     * Test v2 B-tree creation
     */
    if (H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /* Create level-2 B-tree with 3 internal nodes */
    for(u=0; u<INSERT_SPLIT_ROOT_NREC*21; u++) {
        record=u;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    /* Query the number of records in the B-tree */
    if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the # of records is correct */
    if(nrec != (INSERT_SPLIT_ROOT_NREC*21)) TEST_ERROR

    /* Query the address of the root node in the B-tree */
    if (H5B2_get_root_addr_test(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &root_addr)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Make certain that the address of the root node is defined */
    if(!H5F_addr_defined(root_addr)) TEST_ERROR

    /* Attempt to remove records from a level-2 B-tree to force back to level-1 */
    TESTING("B-tree remove: collapse level-1 B-tree back to level-0 (r->l)");
    for(u=0; u < (INSERT_SPLIT_ROOT_NREC*12); u++) {
        record = (INSERT_SPLIT_ROOT_NREC*21)-(u+1);
        if(H5B2_remove(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        } /* end if */

        /* Make certain that the record value is correct */
        if(record != ((INSERT_SPLIT_ROOT_NREC*21)-(u+1))) TEST_ERROR

        /* Query the number of records in the B-tree */
        if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        } /* end if */

        /* Make certain that the # of records is correct */
        if(nrec != ((INSERT_SPLIT_ROOT_NREC*21)-(u+1))) TEST_ERROR
    } /* end for */

    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_remove_level2_collapse_right() */


/*-------------------------------------------------------------------------
 * Function:	test_remove_lots
 *
 * Purpose:	Basic tests for the B-tree v2 code.  This test inserts many
 *              records in random order, enough to make at a level 4 B-tree
 *              and then removes them all.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, March  8, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_remove_lots(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    hsize_t     record;                 /* Record to insert into tree */
    haddr_t     bt2_addr;               /* Address of B-tree created */
    time_t      curr_time;              /* Current time, for seeding random number generator */
    hsize_t     *records;               /* Record #'s for random insertion */
    unsigned    u;                      /* Local index variable */
    unsigned    swap_idx;               /* Location to swap with when shuffling */
    hsize_t     temp_rec;               /* Temporary record */
    hsize_t     nrec;                   /* Number of records in B-tree */

    /* Initialize random number seed */
    curr_time=HDtime(NULL);
#ifdef QAK
curr_time=1109170019;
HDfprintf(stderr,"curr_time=%lu\n",(unsigned long)curr_time);
#endif /* QAK */
    HDsrandom((unsigned long)curr_time);

    /* Allocate space for the records */
    if((records = HDmalloc(sizeof(hsize_t)*INSERT_MANY))==NULL) TEST_ERROR

    /* Initialize record #'s */
    for(u=0; u<INSERT_MANY; u++)
        records[u] = u;

    /* Shuffle record #'s */
    for(u=0; u<INSERT_MANY; u++) {
        swap_idx = (unsigned)(HDrandom()%(INSERT_MANY-u))+u;
        temp_rec = records[u];
        records[u] = records[swap_idx];
        records[swap_idx] = temp_rec;
    } /* end for */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR

    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Create v2 B-tree
     */
    if (H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Test inserting many records into v2 B-tree
     */
    TESTING("B-tree remove: create random level 4 B-tree and delete all records");

    /* Insert random records */
    for(u=0; u<INSERT_MANY; u++) {
        record=records[u];
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    } /* end for */

    /* Re-shuffle record #'s */
    for(u=0; u<INSERT_MANY; u++) {
        swap_idx = (unsigned)(HDrandom()%(INSERT_MANY-u))+u;
        temp_rec = records[u];
        records[u] = records[swap_idx];
        records[swap_idx] = temp_rec;
    } /* end for */

    /* Remove all records */
    for(u=0; u<INSERT_MANY; u++) {
        record=records[u];
        if(H5B2_remove(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
#ifdef QAK
HDfprintf(stderr,"curr_time=%lu\n",(unsigned long)curr_time);
#endif /* QAK */
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        } /* end if */

        /* Make certain that the record value is correct */
        if(record != records[u]) TEST_ERROR

        /* Query the number of records in the B-tree */
        if (H5B2_get_nrec(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &nrec)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        } /* end if */

        /* Make certain that the # of records is correct */
        if(nrec != (INSERT_MANY-(u+1))) TEST_ERROR
    } /* end for */

    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR

    HDfree(records);

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    HDfree(records);
    return 1;
} /* test_remove_lots() */


/*-------------------------------------------------------------------------
 * Function:	test_find_neighbor
 *
 * Purpose:	Basic tests for the B-tree v2 code.  This test exercises
 *              code to find nearest neighbors to a given value in a B-tree.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, March  8, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_find_neighbor(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    hsize_t     record;                 /* Record to insert into tree */
    haddr_t     bt2_addr;               /* Address of B-tree created */
    hsize_t     search;                 /* Search value */
    hsize_t     *records;               /* Record #'s for random insertion */
    unsigned    u;                      /* Local index variable */
    herr_t      ret;                    /* Generic error return value */

    /* Allocate space for the records */
    if((records = HDmalloc(sizeof(hsize_t)*FIND_NEIGHBOR))==NULL) TEST_ERROR

    /* Initialize record #'s */
    for(u=0; u<FIND_NEIGHBOR; u++)
        records[u] = u*2;

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR

    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Create v2 B-tree
     */
    if (H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /* Insert records */
    for(u=0; u<FIND_NEIGHBOR; u++) {
        record=records[u];
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    } /* end for */

    /*
     * Test nearest neighbor for '<' cases
     */
    TESTING("B-tree find: nearest neighbor less than a value");

    /* Attempt to find record B-tree less than a value */
    search = 0;
    H5E_BEGIN_TRY {
	ret = H5B2_neighbor(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, H5B2_COMPARE_LESS, &search, neighbor_cb, &record);
    } H5E_END_TRY;
    /* Should fail */
    if(ret != FAIL) TEST_ERROR

    search = 1;
    if(H5B2_neighbor(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, H5B2_COMPARE_LESS, &search, neighbor_cb, &record)<0) TEST_ERROR
    if(record != 0) TEST_ERROR

    search = 2;
    if(H5B2_neighbor(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, H5B2_COMPARE_LESS, &search, neighbor_cb, &record)<0) TEST_ERROR
    if(record != 0) TEST_ERROR

    search = 3;
    if(H5B2_neighbor(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, H5B2_COMPARE_LESS, &search, neighbor_cb, &record)<0) TEST_ERROR
    if(record != 2) TEST_ERROR

    search = 4;
    if(H5B2_neighbor(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, H5B2_COMPARE_LESS, &search, neighbor_cb, &record)<0) TEST_ERROR
    if(record != 2) TEST_ERROR

    /* Neighbor is in internal node */
    search = 85;
    if(H5B2_neighbor(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, H5B2_COMPARE_LESS, &search, neighbor_cb, &record)<0) TEST_ERROR
    if(record != 84) TEST_ERROR

    /* Neighbor is in root node */
    search = 859;
    if(H5B2_neighbor(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, H5B2_COMPARE_LESS, &search, neighbor_cb, &record)<0) TEST_ERROR
    if(record != 858) TEST_ERROR

    search = (FIND_NEIGHBOR * 2) + 1;
    if(H5B2_neighbor(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, H5B2_COMPARE_LESS, &search, neighbor_cb, &record)<0) TEST_ERROR
    if(record != ((FIND_NEIGHBOR-1)*2)) TEST_ERROR

    PASSED();

    /*
     * Test nearest neighbor for '>' cases
     */
    TESTING("B-tree find: nearest neighbor greater than a value");

    /* Attempt to find record B-tree less than a value */
    search = (FIND_NEIGHBOR * 2) + 1;
    H5E_BEGIN_TRY {
	ret = H5B2_neighbor(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, H5B2_COMPARE_GREATER, &search, neighbor_cb, &record);
    } H5E_END_TRY;
    /* Should fail */
    if(ret != FAIL) TEST_ERROR

    search = 0;
    if(H5B2_neighbor(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, H5B2_COMPARE_GREATER, &search, neighbor_cb, &record)<0) TEST_ERROR
    if(record != 2) TEST_ERROR

    search = 1;
    if(H5B2_neighbor(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, H5B2_COMPARE_GREATER, &search, neighbor_cb, &record)<0) TEST_ERROR
    if(record != 2) TEST_ERROR

    search = 2;
    if(H5B2_neighbor(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, H5B2_COMPARE_GREATER, &search, neighbor_cb, &record)<0) TEST_ERROR
    if(record != 4) TEST_ERROR

    search = 3;
    if(H5B2_neighbor(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, H5B2_COMPARE_GREATER, &search, neighbor_cb, &record)<0) TEST_ERROR
    if(record != 4) TEST_ERROR

    /* Neighbor is in internal node */
    search = 599;
    if(H5B2_neighbor(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, H5B2_COMPARE_GREATER, &search, neighbor_cb, &record)<0) TEST_ERROR
    if(record != 600) TEST_ERROR

    /* Neighbor is in root node */
    search = 857;
    if(H5B2_neighbor(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, H5B2_COMPARE_GREATER, &search, neighbor_cb, &record)<0) TEST_ERROR
    if(record != 858) TEST_ERROR

    search = ((FIND_NEIGHBOR-1) * 2) - 1;
    if(H5B2_neighbor(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, H5B2_COMPARE_GREATER, &search, neighbor_cb, &record)<0) TEST_ERROR
    if(record != ((FIND_NEIGHBOR-1)*2)) TEST_ERROR

    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR

    HDfree(records);

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    HDfree(records);
    return 1;
} /* test_find_neighbor() */


/*-------------------------------------------------------------------------
 * Function:	test_delete
 *
 * Purpose:	Basic tests for the B-tree v2 code.  This test exercises
 *              code to delete a B-tree from a file
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Wednesday, March  9, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_delete(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    off_t       empty_size;             /* Size of an empty file */
    off_t       file_size;              /* Size of each file created */
    hsize_t     record;                 /* Record to insert into tree */
    haddr_t     bt2_addr;               /* Address of B-tree created */
    unsigned    u;                      /* Local index variable */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

/* Create empty file for size comparisons later */

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR

    /* Close file */
    if(H5Fclose(file)<0) TEST_ERROR

    /* Get the size of an empty file */
    if((empty_size=h5_get_file_size(filename))==0) TEST_ERROR

    /* Attempt to delete empty B-tree */
    TESTING("B-tree delete: delete empty B-tree");

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR

    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /*
     * Create v2 B-tree
     */
    if (H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /*
     * Delete v2 B-tree
     */
    if (H5B2_delete(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    if (H5Fclose(file)<0) TEST_ERROR

    /* Get the size of the file */
    if((file_size=h5_get_file_size(filename))==0) TEST_ERROR

    /* Verify the file is correct size */
    if(file_size!=empty_size) TEST_ERROR

    PASSED();

    /* Attempt to delete level-0 B-tree */
    TESTING("B-tree delete: delete level-0 B-tree");

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR

    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /*
     * Create v2 B-tree
     */
    if (H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Insert records */
    for(u=0; u<DELETE_SMALL; u++) {
        record=u;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    } /* end for */

    /*
     * Delete v2 B-tree
     */
    if (H5B2_delete(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    if (H5Fclose(file)<0) TEST_ERROR

    /* Get the size of the file */
    if((file_size=h5_get_file_size(filename))==0) TEST_ERROR

    /* Verify the file is correct size */
    if(file_size!=empty_size) TEST_ERROR

    PASSED();

    /* Attempt to delete level-1 B-tree */
    TESTING("B-tree delete: delete level-1 B-tree");

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR

    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /*
     * Create v2 B-tree
     */
    if (H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Insert records */
    for(u=0; u<DELETE_MEDIUM; u++) {
        record=u;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    } /* end for */

    /*
     * Delete v2 B-tree
     */
    if (H5B2_delete(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    if (H5Fclose(file)<0) TEST_ERROR

    /* Get the size of the file */
    if((file_size=h5_get_file_size(filename))==0) TEST_ERROR

    /* Verify the file is correct size */
    if(file_size!=empty_size) TEST_ERROR

    PASSED();

    /* Attempt to delete level-2 B-tree */
    TESTING("B-tree delete: delete level-2 B-tree");

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR

    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /*
     * Create v2 B-tree
     */
    if (H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Insert records */
    for(u=0; u<DELETE_LARGE; u++) {
        record=u;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        }
    } /* end for */

    /*
     * Delete v2 B-tree
     */
    if (H5B2_delete(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    if (H5Fclose(file)<0) TEST_ERROR

    /* Get the size of the file */
    if((file_size=h5_get_file_size(filename))==0) TEST_ERROR

    /* Verify the file is correct size */
    if(file_size!=empty_size) TEST_ERROR

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_delete() */


/*-------------------------------------------------------------------------
 * Function:	test_modify
 *
 * Purpose:	Basic tests for the B-tree v2 code.  This test exercises
 *              code to modify an existing record in the B-tree
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Friday, March 10, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_modify(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    hsize_t     record;                 /* Record to insert into tree */
    haddr_t     bt2_addr;               /* Address of B-tree created */
    hsize_t     modify;                 /* Modified value */
    hsize_t     found;                  /* Found value */
    unsigned    u;                      /* Local index variable */
    herr_t      ret;                    /* Generic error return value */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR

    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /*
     * Create v2 B-tree
     */
    if (H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /* Insert records */
    for(u=0; u<100; u++) {
        record=u*5;
        if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        } /* end if */
    } /* end for */

    /*
     * Test modifying records
     */
    TESTING("B-tree modify: attempt to modify non-existant record");

    /* Attempt to modify a non-existant record */
    record = 3;
    modify = 4;
    H5E_BEGIN_TRY {
	ret = H5B2_modify(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record, modify_cb, &modify);
    } H5E_END_TRY;
    /* Should fail */
    if(ret != FAIL) TEST_ERROR

    PASSED();

    TESTING("B-tree modify: modify record in leaf node");

    /* Attempt to modify a record in a leaf node */
    record = 130;
    modify = 131;
    if (H5B2_modify(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record, modify_cb, &modify) < 0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    } /* end if */

    /* Attempt to find modified record */
    record = 131;
    found = 131;
    if(H5B2_find(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record, find_cb, &found)<0) TEST_ERROR
    if(found != 131) TEST_ERROR

    /* Attempt to find original record */
    record = 130;
    found = 130;
    H5E_BEGIN_TRY {
	ret = H5B2_modify(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record, modify_cb, &modify);
    } H5E_END_TRY;
    /* Should fail */
    if(ret != FAIL) TEST_ERROR

    PASSED();

    TESTING("B-tree modify: modify record in internal node");

    /* Attempt to modify a record in an internal node */
    record = 235;
    modify = 237;
    if (H5B2_modify(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record, modify_cb, &modify) < 0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    } /* end if */

    /* Attempt to find modified record */
    record = 237;
    found = 237;
    if(H5B2_find(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record, find_cb, &found)<0) TEST_ERROR
    if(found != 237) TEST_ERROR

    /* Attempt to find original record */
    record = 235;
    found = 235;
    H5E_BEGIN_TRY {
	ret = H5B2_modify(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record, modify_cb, &modify);
    } H5E_END_TRY;
    /* Should fail */
    if(ret != FAIL) TEST_ERROR

    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_modify() */


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Test the B-tree v2 code
 *
 * Return:	Success:
 *
 *		Failure:
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, February  1, 2005
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t	fapl = -1;              /* File access property list for data files */
    unsigned	nerrors = 0;            /* Cumulative error count */

    /* Reset library */
    h5_reset();
    fapl = h5_fileaccess();

    /* Test B-tree record insertion */
    /* Iteration, find & index routines tested in these routines as well */
    nerrors += test_insert_basic(fapl);
    nerrors += test_insert_split_root(fapl);
    nerrors += test_insert_level1_2leaf_redistrib(fapl);
    nerrors += test_insert_level1_2leaf_split(fapl);
    nerrors += test_insert_level1_3leaf_redistrib(fapl);
    nerrors += test_insert_level1_3leaf_split(fapl);
    nerrors += test_insert_make_level2(fapl);
    nerrors += test_insert_level2_leaf_redistrib(fapl);
    nerrors += test_insert_level2_leaf_split(fapl);
    nerrors += test_insert_level2_2internal_redistrib(fapl);
    nerrors += test_insert_level2_2internal_split(fapl);
    nerrors += test_insert_level2_3internal_redistrib(fapl);
    nerrors += test_insert_level2_3internal_split(fapl);
    nerrors += test_insert_lots(fapl);

    /* Test B-tree record removal */
    /* Querying the number of records routine also tested in these routines as well */
    nerrors += test_remove_basic(fapl);
    nerrors += test_remove_level1_noredistrib(fapl);
    nerrors += test_remove_level1_redistrib(fapl);
    nerrors += test_remove_level1_2leaf_merge(fapl);
    nerrors += test_remove_level1_3leaf_merge(fapl);
    nerrors += test_remove_level1_promote(fapl);
    nerrors += test_remove_level1_promote_2leaf_redistrib(fapl);
    nerrors += test_remove_level1_promote_3leaf_redistrib(fapl);
    nerrors += test_remove_level1_promote_2leaf_merge(fapl);
    nerrors += test_remove_level1_promote_3leaf_merge(fapl);
    nerrors += test_remove_level1_collapse(fapl);
    nerrors += test_remove_level2_promote(fapl);
    nerrors += test_remove_level2_promote_2internal_redistrib(fapl);
    nerrors += test_remove_level2_promote_3internal_redistrib(fapl);
    nerrors += test_remove_level2_promote_2internal_merge(fapl);
    nerrors += test_remove_level2_promote_3internal_merge(fapl);
    nerrors += test_remove_level2_2internal_merge_left(fapl);
    nerrors += test_remove_level2_2internal_merge_right(fapl);
    nerrors += test_remove_level2_3internal_merge(fapl);
    nerrors += test_remove_level2_collapse_right(fapl);
    nerrors += test_remove_lots(fapl);

    /* Test more complex B-tree queries */
    nerrors += test_find_neighbor(fapl);

    /* Test deleting B-trees */
    nerrors += test_delete(fapl);

    /* Test modifying B-tree records */
    nerrors += test_modify(fapl);

    if(nerrors)
        goto error;
    puts("All v2 B-tree tests passed.");
    h5_cleanup(FILENAME, fapl);
    return 0;

error:
    puts("*** TESTS FAILED ***");
    H5E_BEGIN_TRY {
	H5Pclose(fapl);
    } H5E_END_TRY;
    return 1;
} /* end main() */

