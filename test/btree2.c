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

#define INSERT_SPLIT_ROOT_NREC    80


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
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_insert_basic(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    hsize_t     record;                 /* Record to insert into tree */
    haddr_t     bt2_addr;               /* Address of B-tree created */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;
	
    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	TEST_ERROR;
    }

    /*
     * Test v2 B-tree creation
     */
    TESTING("B-tree creation");
    if (H5B2_create(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, 512, 8, 100, 40, &bt2_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }
    PASSED();

    /*
     * Test inserting record into v2 B-tree 
     */
    TESTING("B-tree insert");
    record=42;
    if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Test inserting second record into v2 B-tree, before all other records
     */
    record=34;
    if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Test inserting third record into v2 B-tree, after all other records
     */
    record=56;
    if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }

    /*
     * Test inserting fourth record into v2 B-tree, in the middle of other records
     */
    record=38;
    if (H5B2_insert(f, H5P_DATASET_XFER_DEFAULT, H5B2_TEST, bt2_addr, &record)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    }
    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR;

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
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
 * Modifications:
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
    haddr_t     bt2_addr;               /* Address of B-tree created */
    unsigned    u;                      /* Local index variable */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;
	
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
    TESTING("B-tree many - split root");

    for(u=0; u<INSERT_SPLIT_ROOT_NREC; u++) {
        record=u+10;
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

    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR;

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
 * Modifications:
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
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;
	
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
    TESTING("B-tree many - redistribute 2 leaves in level 1 B-tree (l->r)");

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
    TESTING("B-tree many - redistribute 2 leaves in level 1 B-tree (r->l)");

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

    if (H5Fclose(file)<0) TEST_ERROR;

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_insert_level1_2leaf_redistrib() */


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
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t	fapl=-1;
    int		nerrors=0;

    /* Reset library */
    h5_reset();
    fapl = h5_fileaccess();

    /* Test basic B-tree insertion */
    nerrors += test_insert_basic(fapl);
    nerrors += test_insert_split_root(fapl);
    nerrors += test_insert_level1_2leaf_redistrib(fapl);

    if (nerrors) goto error;
    puts("All v2 B-tree tests passed.");
#ifndef QAK
    h5_cleanup(FILENAME, fapl);
#else /* QAK */
HDfprintf(stderr,"Uncomment cleanup!\n");
#endif /* QAK */
    return 0;

error:
    puts("*** TESTS FAILED ***");
    H5E_BEGIN_TRY {
	H5Pclose(fapl);
    } H5E_END_TRY;
    return 1;
}

