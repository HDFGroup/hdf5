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
 *              Wednesday, March  9, 2005
 */
#include "h5test.h"

/*
 * This file needs to access private datatypes from the H5BT package.
 * This file also needs to access the block tracker testing code.
 */
#define H5BT_PACKAGE
#define H5BT_TESTING
#include "H5BTpkg.h"

/* Other private headers that this test requires */
#include "H5Iprivate.h"

const char *FILENAME[] = {
    "blocktrack",
    NULL
};

#define INSERT_MANY     100


/*-------------------------------------------------------------------------
 * Function:	test_create
 *
 * Purpose:	Basic tests for the block tracker code
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Wednesday, March  9, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_create(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    haddr_t     bt_addr;                /* Address of block tracker created */
    hsize_t     tot_size;               /* Total size of blocks tracked */
    hsize_t     max_size;               /* Max. size of blocks tracked */
    uint32_t    max_count;              /* Ref. count of max. size of blocks tracked */
    hbool_t     max_valid;              /* Is max. size valid over all blocks? */
    hsize_t     min_size;               /* Min. size of blocks tracked */
    uint32_t    min_count;              /* Ref. count of min. size of blocks tracked */
    hbool_t     min_valid;              /* Is min. size valid over all blocks? */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;
	
    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	TEST_ERROR;
    } /* end if */

    /*
     * Test block tracker creation
     */
    TESTING("block tracker creation");
    if (H5BT_create(f, H5P_DATASET_XFER_DEFAULT, &bt_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    if (H5BT_get_total_size(f, H5P_DATASET_XFER_DEFAULT, bt_addr, &tot_size)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */
    /* Make certain that the size is correct */
    if(tot_size != 0) TEST_ERROR;

    if (H5BT_get_max_info(f, H5P_DATASET_XFER_DEFAULT, bt_addr, &max_size, &max_count, &max_valid)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */
    /* Make certain that the max. info is correct */
    if(max_size != 0) TEST_ERROR;
    if(max_count != 0) TEST_ERROR;
    if(max_valid != 0) TEST_ERROR;

    if (H5BT_get_min_info(f, H5P_DATASET_XFER_DEFAULT, bt_addr, &min_size, &min_count, &min_valid)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */
    /* Make certain that the max. info is correct */
    if(min_size != HSIZET_MAX) TEST_ERROR;
    if(min_count != 0) TEST_ERROR;
    if(min_valid != 0) TEST_ERROR;

    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR;

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_create() */


/*-------------------------------------------------------------------------
 * Function:	test_insert_one
 *
 * Purpose:	Basic tests for the block tracker code
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Thursday, March 10, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_insert_one(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    haddr_t     bt_addr;                /* Address of block tracker created */
    hsize_t     tot_size;               /* Total size of blocks tracked */
    hsize_t     max_size;               /* Max. size of blocks tracked */
    uint32_t    max_count;              /* Ref. count of max. size of blocks tracked */
    hbool_t     max_valid;              /* Is max. size valid over all blocks? */
    hsize_t     min_size;               /* Min. size of blocks tracked */
    uint32_t    min_count;              /* Ref. count of min. size of blocks tracked */
    hbool_t     min_valid;              /* Is min. size valid over all blocks? */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;
	
    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	TEST_ERROR;
    } /* end if */

    /*
     * Test inserting one block
     */
    TESTING("insert 1 block");
    if (H5BT_create(f, H5P_DATASET_XFER_DEFAULT, &bt_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    if (H5BT_insert(f, H5P_DATASET_XFER_DEFAULT, bt_addr, (haddr_t)10, (hsize_t)20)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    if (H5BT_get_total_size(f, H5P_DATASET_XFER_DEFAULT, bt_addr, &tot_size)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */
    /* Make certain that the size is correct */
    if(tot_size != 20) TEST_ERROR;

    if (H5BT_get_max_info(f, H5P_DATASET_XFER_DEFAULT, bt_addr, &max_size, &max_count, &max_valid)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */
    /* Make certain that the max. info is correct */
    if(max_size != 20) TEST_ERROR;
    if(max_count != 1) TEST_ERROR;
    if(max_valid != 1) TEST_ERROR;

    if (H5BT_get_min_info(f, H5P_DATASET_XFER_DEFAULT, bt_addr, &min_size, &min_count, &min_valid)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */
    /* Make certain that the max. info is correct */
    if(min_size != 20) TEST_ERROR;
    if(min_count != 1) TEST_ERROR;
    if(min_valid != 1) TEST_ERROR;

    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR;

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_insert_one() */


/*-------------------------------------------------------------------------
 * Function:	test_insert_few
 *
 * Purpose:	Basic tests for the block tracker code
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Thursday, March 10, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_insert_few(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    haddr_t     bt_addr;                /* Address of block tracker created */
    hsize_t     tot_size;               /* Total size of blocks tracked */
    hsize_t     max_size;               /* Max. size of blocks tracked */
    uint32_t    max_count;              /* Ref. count of max. size of blocks tracked */
    hbool_t     max_valid;              /* Is max. size valid over all blocks? */
    hsize_t     min_size;               /* Min. size of blocks tracked */
    uint32_t    min_count;              /* Ref. count of min. size of blocks tracked */
    hbool_t     min_valid;              /* Is min. size valid over all blocks? */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;
	
    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	TEST_ERROR;
    } /* end if */

    /*
     * Test inserting one block
     */
    TESTING("insert several blocks");
    if (H5BT_create(f, H5P_DATASET_XFER_DEFAULT, &bt_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    if (H5BT_insert(f, H5P_DATASET_XFER_DEFAULT, bt_addr, (haddr_t)10, (hsize_t)20)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    if (H5BT_get_total_size(f, H5P_DATASET_XFER_DEFAULT, bt_addr, &tot_size)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */
    /* Make certain that the size is correct */
    if(tot_size != 20) TEST_ERROR;

    if (H5BT_get_max_info(f, H5P_DATASET_XFER_DEFAULT, bt_addr, &max_size, &max_count, &max_valid)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */
    /* Make certain that the max. info is correct */
    if(max_size != 20) TEST_ERROR;
    if(max_count != 1) TEST_ERROR;
    if(max_valid != 1) TEST_ERROR;

    if (H5BT_get_min_info(f, H5P_DATASET_XFER_DEFAULT, bt_addr, &min_size, &min_count, &min_valid)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */
    /* Make certain that the min. info is correct */
    if(min_size != 20) TEST_ERROR;
    if(min_count != 1) TEST_ERROR;
    if(min_valid != 1) TEST_ERROR;

    if (H5BT_insert(f, H5P_DATASET_XFER_DEFAULT, bt_addr, (haddr_t)50, (hsize_t)30)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    if (H5BT_get_total_size(f, H5P_DATASET_XFER_DEFAULT, bt_addr, &tot_size)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */
    /* Make certain that the size is correct */
    if(tot_size != 50) TEST_ERROR;

    if (H5BT_get_max_info(f, H5P_DATASET_XFER_DEFAULT, bt_addr, &max_size, &max_count, &max_valid)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */
    /* Make certain that the max. info is correct */
    if(max_size != 30) TEST_ERROR;
    if(max_count != 1) TEST_ERROR;
    if(max_valid != 1) TEST_ERROR;

    if (H5BT_get_min_info(f, H5P_DATASET_XFER_DEFAULT, bt_addr, &min_size, &min_count, &min_valid)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */
    /* Make certain that the min. info is correct */
    if(min_size != 20) TEST_ERROR;
    if(min_count != 1) TEST_ERROR;
    if(min_valid != 1) TEST_ERROR;

    if (H5BT_insert(f, H5P_DATASET_XFER_DEFAULT, bt_addr, (haddr_t)90, (hsize_t)30)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    if (H5BT_get_total_size(f, H5P_DATASET_XFER_DEFAULT, bt_addr, &tot_size)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */
    /* Make certain that the size is correct */
    if(tot_size != 80) TEST_ERROR;

    if (H5BT_get_max_info(f, H5P_DATASET_XFER_DEFAULT, bt_addr, &max_size, &max_count, &max_valid)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */
    /* Make certain that the max. info is correct */
    if(max_size != 30) TEST_ERROR;
    if(max_count != 2) TEST_ERROR;
    if(max_valid != 1) TEST_ERROR;

    if (H5BT_get_min_info(f, H5P_DATASET_XFER_DEFAULT, bt_addr, &min_size, &min_count, &min_valid)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */
    /* Make certain that the min. info is correct */
    if(min_size != 20) TEST_ERROR;
    if(min_count != 1) TEST_ERROR;
    if(min_valid != 1) TEST_ERROR;

    if (H5BT_insert(f, H5P_DATASET_XFER_DEFAULT, bt_addr, (haddr_t)130, (hsize_t)20)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    if (H5BT_get_total_size(f, H5P_DATASET_XFER_DEFAULT, bt_addr, &tot_size)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */
    /* Make certain that the size is correct */
    if(tot_size != 100) TEST_ERROR;

    if (H5BT_get_max_info(f, H5P_DATASET_XFER_DEFAULT, bt_addr, &max_size, &max_count, &max_valid)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */
    /* Make certain that the max. info is correct */
    if(max_size != 30) TEST_ERROR;
    if(max_count != 2) TEST_ERROR;
    if(max_valid != 1) TEST_ERROR;

    if (H5BT_get_min_info(f, H5P_DATASET_XFER_DEFAULT, bt_addr, &min_size, &min_count, &min_valid)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */
    /* Make certain that the min. info is correct */
    if(min_size != 20) TEST_ERROR;
    if(min_count != 2) TEST_ERROR;
    if(min_valid != 1) TEST_ERROR;

    if (H5BT_insert(f, H5P_DATASET_XFER_DEFAULT, bt_addr, (haddr_t)160, (hsize_t)10)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    if (H5BT_get_total_size(f, H5P_DATASET_XFER_DEFAULT, bt_addr, &tot_size)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */
    /* Make certain that the size is correct */
    if(tot_size != 110) TEST_ERROR;

    if (H5BT_get_max_info(f, H5P_DATASET_XFER_DEFAULT, bt_addr, &max_size, &max_count, &max_valid)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */
    /* Make certain that the max. info is correct */
    if(max_size != 30) TEST_ERROR;
    if(max_count != 2) TEST_ERROR;
    if(max_valid != 1) TEST_ERROR;

    if (H5BT_get_min_info(f, H5P_DATASET_XFER_DEFAULT, bt_addr, &min_size, &min_count, &min_valid)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */
    /* Make certain that the min. info is correct */
    if(min_size != 10) TEST_ERROR;
    if(min_count != 1) TEST_ERROR;
    if(min_valid != 1) TEST_ERROR;

    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR;

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_insert_one() */


/*-------------------------------------------------------------------------
 * Function:	test_insert_many
 *
 * Purpose:	Basic tests for the block tracker code
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Thursday, March 10, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_insert_many(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    haddr_t     bt_addr;                /* Address of block tracker created */
    unsigned    u;                      /* Local index variable */
    hsize_t     tot_size;               /* Total size of blocks tracked */
    hsize_t     max_size;               /* Max. size of blocks tracked */
    uint32_t    max_count;              /* Ref. count of max. size of blocks tracked */
    hbool_t     max_valid;              /* Is max. size valid over all blocks? */
    hsize_t     min_size;               /* Min. size of blocks tracked */
    uint32_t    min_count;              /* Ref. count of min. size of blocks tracked */
    hbool_t     min_valid;              /* Is min. size valid over all blocks? */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;
	
    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	TEST_ERROR;
    } /* end if */

    if (H5BT_create(f, H5P_DATASET_XFER_DEFAULT, &bt_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /*
     * Test inserting many, non-overlapping, non-mergeable blocks
     */
    TESTING("insert many blocks");
    for(u = 0; u < INSERT_MANY; u++) {
        if (H5BT_insert(f, H5P_DATASET_XFER_DEFAULT, bt_addr, (haddr_t)(u*50), (hsize_t)20)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        } /* end if */

        if (H5BT_get_total_size(f, H5P_DATASET_XFER_DEFAULT, bt_addr, &tot_size)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        } /* end if */
        /* Make certain that the size is correct */
        if(tot_size != ((u+1)*20)) TEST_ERROR;

        if (H5BT_get_max_info(f, H5P_DATASET_XFER_DEFAULT, bt_addr, &max_size, &max_count, &max_valid)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        } /* end if */
        /* Make certain that the max. info is correct */
        if(max_size != 20) TEST_ERROR;
        if(max_count != (u+1)) TEST_ERROR;
        if(max_valid != 1) TEST_ERROR;

        if (H5BT_get_min_info(f, H5P_DATASET_XFER_DEFAULT, bt_addr, &min_size, &min_count, &min_valid)<0) {
            H5_FAILED();
            H5Eprint_stack(H5E_DEFAULT, stdout);
            goto error;
        } /* end if */
        /* Make certain that the min. info is correct */
        if(min_size != 20) TEST_ERROR;
        if(min_count != (u+1)) TEST_ERROR;
        if(min_valid != 1) TEST_ERROR;

    } /* end for */

    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR;

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_insert_many() */


/*-------------------------------------------------------------------------
 * Function:	test_insert_overlap
 *
 * Purpose:	Basic tests for the block tracker code
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Thursday, March 10, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_insert_overlap(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    haddr_t     bt_addr;                /* Address of block tracker created */
    herr_t      ret;                    /* Generic return value */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;
	
    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	TEST_ERROR;
    } /* end if */

    if (H5BT_create(f, H5P_DATASET_XFER_DEFAULT, &bt_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Insert first block */
    if (H5BT_insert(f, H5P_DATASET_XFER_DEFAULT, bt_addr, (haddr_t)10, (hsize_t)20)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /*
     * Test inserting overlapping blocks
     */
    TESTING("insert overlapping blocks");

    /* Insert same block again (should fail) */
    H5E_BEGIN_TRY {
        ret = H5BT_insert(f, H5P_DATASET_XFER_DEFAULT, bt_addr, (haddr_t)10, (hsize_t)20);
    } H5E_END_TRY;
    if (ret != FAIL) TEST_ERROR;

    /* Insert block of different size at same address (should fail) */
    H5E_BEGIN_TRY {
        ret = H5BT_insert(f, H5P_DATASET_XFER_DEFAULT, bt_addr, (haddr_t)10, (hsize_t)10);
    } H5E_END_TRY;
    if (ret != FAIL) TEST_ERROR;

    /* Insert block which overlaps beginning of existing block (should fail) */
    H5E_BEGIN_TRY {
        ret = H5BT_insert(f, H5P_DATASET_XFER_DEFAULT, bt_addr, (haddr_t)5, (hsize_t)10);
    } H5E_END_TRY;
    if (ret != FAIL) TEST_ERROR;

    /* Insert block which overlaps end of existing block (should fail) */
    H5E_BEGIN_TRY {
        ret = H5BT_insert(f, H5P_DATASET_XFER_DEFAULT, bt_addr, (haddr_t)25, (hsize_t)10);
    } H5E_END_TRY;
    if (ret != FAIL) TEST_ERROR;

    /* Insert block which includes existing block (should fail) */
    H5E_BEGIN_TRY {
        ret = H5BT_insert(f, H5P_DATASET_XFER_DEFAULT, bt_addr, (haddr_t)5, (hsize_t)30);
    } H5E_END_TRY;
    if (ret != FAIL) TEST_ERROR;

    /* Insert block which is inside existing block (should fail) */
    H5E_BEGIN_TRY {
        ret = H5BT_insert(f, H5P_DATASET_XFER_DEFAULT, bt_addr, (haddr_t)15, (hsize_t)10);
    } H5E_END_TRY;
    if (ret != FAIL) TEST_ERROR;

    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR;

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_insert_overlap() */


/*-------------------------------------------------------------------------
 * Function:	test_insert_merge
 *
 * Purpose:	Basic tests for the block tracker code
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Thursday, March 10, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_insert_merge(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    haddr_t     bt_addr;                /* Address of block tracker created */
    hsize_t     tot_size;               /* Total size of blocks tracked */
    hsize_t     max_size;               /* Max. size of blocks tracked */
    uint32_t    max_count;              /* Ref. count of max. size of blocks tracked */
    hbool_t     max_valid;              /* Is max. size valid over all blocks? */
    hsize_t     min_size;               /* Min. size of blocks tracked */
    uint32_t    min_count;              /* Ref. count of min. size of blocks tracked */
    hbool_t     min_valid;              /* Is min. size valid over all blocks? */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;
	
    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	TEST_ERROR;
    } /* end if */

    if (H5BT_create(f, H5P_DATASET_XFER_DEFAULT, &bt_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Insert first block */
    if (H5BT_insert(f, H5P_DATASET_XFER_DEFAULT, bt_addr, (haddr_t)10, (hsize_t)20)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /*
     * Test inserting blocks which should merge into existing block(s)
     */
    TESTING("insert block which merges with existing upper block");

    /* Insert block which should merge with beginning of existing block */
    if (H5BT_insert(f, H5P_DATASET_XFER_DEFAULT, bt_addr, (haddr_t)8, (hsize_t)2)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    if (H5BT_get_total_size(f, H5P_DATASET_XFER_DEFAULT, bt_addr, &tot_size)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    } /* end if */
    /* Make certain that the size is correct */
    if(tot_size != 22) TEST_ERROR;

    if (H5BT_get_max_info(f, H5P_DATASET_XFER_DEFAULT, bt_addr, &max_size, &max_count, &max_valid)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    } /* end if */
    /* Make certain that the max. info is correct */
    if(max_size != 22) TEST_ERROR;
    if(max_count != 1) TEST_ERROR;
    if(max_valid != 1) TEST_ERROR;

    if (H5BT_get_min_info(f, H5P_DATASET_XFER_DEFAULT, bt_addr, &min_size, &min_count, &min_valid)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    } /* end if */
    /* Make certain that the min. info is correct */
    if(min_size != 22) TEST_ERROR;
    if(min_count != 1) TEST_ERROR;
    if(min_valid != 0) TEST_ERROR;

    PASSED();

    /*
     * Test inserting blocks which should merge into existing block(s)
     */
    TESTING("insert block which merges with existing lower block");

    /* Insert block which should merge with end of existing block */
    if (H5BT_insert(f, H5P_DATASET_XFER_DEFAULT, bt_addr, (haddr_t)30, (hsize_t)4)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    if (H5BT_get_total_size(f, H5P_DATASET_XFER_DEFAULT, bt_addr, &tot_size)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    } /* end if */
    /* Make certain that the size is correct */
    if(tot_size != 26) TEST_ERROR;

    if (H5BT_get_max_info(f, H5P_DATASET_XFER_DEFAULT, bt_addr, &max_size, &max_count, &max_valid)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    } /* end if */
    /* Make certain that the max. info is correct */
    if(max_size != 26) TEST_ERROR;
    if(max_count != 1) TEST_ERROR;
    if(max_valid != 1) TEST_ERROR;

    if (H5BT_get_min_info(f, H5P_DATASET_XFER_DEFAULT, bt_addr, &min_size, &min_count, &min_valid)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    } /* end if */
    /* Make certain that the min. info is correct */
    if(min_size != 26) TEST_ERROR;
    if(min_count != 1) TEST_ERROR;
    if(min_valid != 0) TEST_ERROR;

    PASSED();

    /*
     * Test inserting blocks which should merge into existing block(s)
     */
    TESTING("insert block which merges with existing upper & lower blocks");

    /* Insert block to merge with */
    if (H5BT_insert(f, H5P_DATASET_XFER_DEFAULT, bt_addr, (haddr_t)40, (hsize_t)10) < 0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Insert block which should merge with end of existing block */
    if (H5BT_insert(f, H5P_DATASET_XFER_DEFAULT, bt_addr, (haddr_t)34, (hsize_t)6) < 0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    if (H5BT_get_total_size(f, H5P_DATASET_XFER_DEFAULT, bt_addr, &tot_size)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    } /* end if */
    /* Make certain that the size is correct */
    if(tot_size != 42) TEST_ERROR;

    if (H5BT_get_max_info(f, H5P_DATASET_XFER_DEFAULT, bt_addr, &max_size, &max_count, &max_valid)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    } /* end if */
    /* Make certain that the max. info is correct */
    if(max_size != 42) TEST_ERROR;
    if(max_count != 1) TEST_ERROR;
    if(max_valid != 1) TEST_ERROR;

    if (H5BT_get_min_info(f, H5P_DATASET_XFER_DEFAULT, bt_addr, &min_size, &min_count, &min_valid)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    } /* end if */
    /* Make certain that the min. info is correct */
    if(min_size != 42) TEST_ERROR;
    if(min_count != 1) TEST_ERROR;
    if(min_valid != 0) TEST_ERROR;

    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR;

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_insert_merge() */


/*-------------------------------------------------------------------------
 * Function:	test_remove_whole
 *
 * Purpose:	Basic tests for the block tracker code
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Friday, March 11, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_remove_whole(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    haddr_t     bt_addr;                /* Address of block tracker created */
    hsize_t     tot_size;               /* Total size of blocks tracked */
    hsize_t     max_size;               /* Max. size of blocks tracked */
    uint32_t    max_count;              /* Ref. count of max. size of blocks tracked */
    hbool_t     max_valid;              /* Is max. size valid over all blocks? */
    hsize_t     min_size;               /* Min. size of blocks tracked */
    uint32_t    min_count;              /* Ref. count of min. size of blocks tracked */
    hbool_t     min_valid;              /* Is min. size valid over all blocks? */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;
	
    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	TEST_ERROR;
    } /* end if */

    if (H5BT_create(f, H5P_DATASET_XFER_DEFAULT, &bt_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /* Insert several blocks */
    if (H5BT_insert(f, H5P_DATASET_XFER_DEFAULT, bt_addr, (haddr_t)50, (hsize_t)20)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    } /* end if */

    if (H5BT_insert(f, H5P_DATASET_XFER_DEFAULT, bt_addr, (haddr_t)100, (hsize_t)15)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    } /* end if */

    if (H5BT_insert(f, H5P_DATASET_XFER_DEFAULT, bt_addr, (haddr_t)150, (hsize_t)15)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    } /* end if */

    if (H5BT_insert(f, H5P_DATASET_XFER_DEFAULT, bt_addr, (haddr_t)200, (hsize_t)35)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    } /* end if */

    if (H5BT_insert(f, H5P_DATASET_XFER_DEFAULT, bt_addr, (haddr_t)250, (hsize_t)35)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    } /* end if */

    /*
     * Test removing blocks
     */
    TESTING("remove entire block, in middle of block size range");
    if (H5BT_remove(f, H5P_DATASET_XFER_DEFAULT, bt_addr, (haddr_t)50, (hsize_t)20)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    } /* end if */

    if (H5BT_get_total_size(f, H5P_DATASET_XFER_DEFAULT, bt_addr, &tot_size)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    } /* end if */
    /* Make certain that the size is correct */
    if(tot_size != 100) TEST_ERROR;

    if (H5BT_get_max_info(f, H5P_DATASET_XFER_DEFAULT, bt_addr, &max_size, &max_count, &max_valid)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    } /* end if */
    /* Make certain that the max. info is correct */
    if(max_size != 35) TEST_ERROR;
    if(max_count != 2) TEST_ERROR;
    if(max_valid != 1) TEST_ERROR;

    if (H5BT_get_min_info(f, H5P_DATASET_XFER_DEFAULT, bt_addr, &min_size, &min_count, &min_valid)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    } /* end if */
    /* Make certain that the min. info is correct */
    if(min_size != 15) TEST_ERROR;
    if(min_count != 2) TEST_ERROR;
    if(min_valid != 1) TEST_ERROR;

    PASSED();

    TESTING("remove entire block, at bottom of block size range");

    /* Remove first block at min. size */
    if (H5BT_remove(f, H5P_DATASET_XFER_DEFAULT, bt_addr, (haddr_t)100, (hsize_t)15)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    } /* end if */

    if (H5BT_get_total_size(f, H5P_DATASET_XFER_DEFAULT, bt_addr, &tot_size)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    } /* end if */
    /* Make certain that the size is correct */
    if(tot_size != 85) TEST_ERROR;

    if (H5BT_get_max_info(f, H5P_DATASET_XFER_DEFAULT, bt_addr, &max_size, &max_count, &max_valid)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    } /* end if */
    /* Make certain that the max. info is correct */
    if(max_size != 35) TEST_ERROR;
    if(max_count != 2) TEST_ERROR;
    if(max_valid != 1) TEST_ERROR;

    if (H5BT_get_min_info(f, H5P_DATASET_XFER_DEFAULT, bt_addr, &min_size, &min_count, &min_valid)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    } /* end if */
    /* Make certain that the min. info is correct */
    if(min_size != 15) TEST_ERROR;
    if(min_count != 1) TEST_ERROR;
    if(min_valid != 1) TEST_ERROR;

    /* Remove last block at min. size */
    if (H5BT_remove(f, H5P_DATASET_XFER_DEFAULT, bt_addr, (haddr_t)150, (hsize_t)15)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    } /* end if */

    if (H5BT_get_total_size(f, H5P_DATASET_XFER_DEFAULT, bt_addr, &tot_size)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    } /* end if */
    /* Make certain that the size is correct */
    if(tot_size != 70) TEST_ERROR;

    if (H5BT_get_max_info(f, H5P_DATASET_XFER_DEFAULT, bt_addr, &max_size, &max_count, &max_valid)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    } /* end if */
    /* Make certain that the max. info is correct */
    if(max_size != 35) TEST_ERROR;
    if(max_count != 2) TEST_ERROR;
    if(max_valid != 1) TEST_ERROR;

    if (H5BT_get_min_info(f, H5P_DATASET_XFER_DEFAULT, bt_addr, &min_size, &min_count, &min_valid)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    } /* end if */
    /* Make certain that the min. info is correct */
    if(min_size != 35) TEST_ERROR;
    if(min_count != 2) TEST_ERROR;
    if(min_valid != 0) TEST_ERROR;

    PASSED();

    TESTING("remove entire block, at top of block size range");

    /* Remove first block at max. size */
    if (H5BT_remove(f, H5P_DATASET_XFER_DEFAULT, bt_addr, (haddr_t)200, (hsize_t)35)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    } /* end if */

    if (H5BT_get_total_size(f, H5P_DATASET_XFER_DEFAULT, bt_addr, &tot_size)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    } /* end if */
    /* Make certain that the size is correct */
    if(tot_size != 35) TEST_ERROR;

    if (H5BT_get_max_info(f, H5P_DATASET_XFER_DEFAULT, bt_addr, &max_size, &max_count, &max_valid)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    } /* end if */
    /* Make certain that the max. info is correct */
    if(max_size != 35) TEST_ERROR;
    if(max_count != 1) TEST_ERROR;
    if(max_valid != 1) TEST_ERROR;

    if (H5BT_get_min_info(f, H5P_DATASET_XFER_DEFAULT, bt_addr, &min_size, &min_count, &min_valid)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    } /* end if */
    /* Make certain that the min. info is correct */
    if(min_size != 35) TEST_ERROR;
    if(min_count != 1) TEST_ERROR;
    if(min_valid != 0) TEST_ERROR;

    /* Remove last block at max. size */
    if (H5BT_remove(f, H5P_DATASET_XFER_DEFAULT, bt_addr, (haddr_t)250, (hsize_t)35)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    } /* end if */

    if (H5BT_get_total_size(f, H5P_DATASET_XFER_DEFAULT, bt_addr, &tot_size)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    } /* end if */
    /* Make certain that the size is correct */
    if(tot_size != 0) TEST_ERROR;

    if (H5BT_get_max_info(f, H5P_DATASET_XFER_DEFAULT, bt_addr, &max_size, &max_count, &max_valid)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    } /* end if */
    /* Make certain that the max. info is correct */
    if(max_size != 0) TEST_ERROR;
    if(max_count != 0) TEST_ERROR;
    if(max_valid != 0) TEST_ERROR;

    if (H5BT_get_min_info(f, H5P_DATASET_XFER_DEFAULT, bt_addr, &min_size, &min_count, &min_valid)<0) {
        H5_FAILED();
        H5Eprint_stack(H5E_DEFAULT, stdout);
        goto error;
    } /* end if */
    /* Make certain that the min. info is correct */
    if(min_size != HSIZET_MAX) TEST_ERROR;
    if(min_count != 0) TEST_ERROR;
    if(min_valid != 0) TEST_ERROR;

    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR;

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_remove_whole() */


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Test the block tracker code
 *
 * Return:	Success:	
 *
 *		Failure:	
 *
 * Programmer:	Quincey Koziol
 *              Wednesday, March  9, 2005
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

    /* Test block tracker creation */
    nerrors += test_create(fapl);

    /* Test block tracker insertion */
    nerrors += test_insert_one(fapl);
    nerrors += test_insert_few(fapl);
    nerrors += test_insert_many(fapl);
    nerrors += test_insert_overlap(fapl);
    nerrors += test_insert_merge(fapl);

    /* Test block tracker removal */
    nerrors += test_remove_whole(fapl);

    if (nerrors) goto error;
    puts("All block tracker tests passed.");
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

