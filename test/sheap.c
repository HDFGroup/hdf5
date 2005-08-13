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
 *              Wednesday, March 23, 2005
 */
#include "h5test.h"

/*
 * This file needs to access private datatypes from the H5SH package.
 * This file also needs to access the segmented heap testing code.
 */
#define H5SH_PACKAGE
#define H5SH_TESTING
#include "H5SHpkg.h"

/* Other private headers that this test requires */
#include "H5Iprivate.h"

const char *FILENAME[] = {
    "sheap",
    NULL
};

#define SHEAP_TYPE H5SH_META
#define SHEAP_MIN_SIZE  4096
#define SHEAP_MAX_EXPAND_SIZE  65536


/*-------------------------------------------------------------------------
 * Function:	test_create
 *
 * Purpose:	Basic tests for the segmented heap code
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Wednesday, March 23, 2005
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
    haddr_t     sh_addr;                /* Address of block tracker created */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;

    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	TEST_ERROR;
    } /* end if */

    /*
     * Test segmented heap creation
     */
    TESTING("segmented heap creation");
    if (H5SH_create(f, H5P_DATASET_XFER_DEFAULT, &sh_addr/*out*/, SHEAP_TYPE, (hsize_t)SHEAP_MIN_SIZE, (hsize_t)SHEAP_MAX_EXPAND_SIZE)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

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
 * Function:	test_allocate_simple
 *
 * Purpose:	Basic tests for the segmented heap code
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Thursday, March 24, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_allocate_simple(hid_t fapl)
{
    hid_t	file=-1;
    char	filename[1024];
    H5F_t	*f=NULL;
    haddr_t     sh_addr;                /* Address of block tracker created */
    haddr_t     obj_addr;               /* Address for object */
    hsize_t     obj_len;                /* Length of object */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) TEST_ERROR;

    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file))) {
	H5Eprint_stack(H5E_DEFAULT, stdout);
	TEST_ERROR;
    } /* end if */

    /* Create segmented heap */
    if (H5SH_create(f, H5P_DATASET_XFER_DEFAULT, &sh_addr/*out*/, SHEAP_TYPE, (hsize_t)SHEAP_MIN_SIZE, (hsize_t)SHEAP_MAX_EXPAND_SIZE)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */

    /*
     * Test segmented heap allocation
     */
    TESTING("segmented heap allocate first block");

    /* Allocate space for a simple object */
    obj_len = 10;
    if (H5SH_alloc(f, H5P_DATASET_XFER_DEFAULT, sh_addr, obj_len, &obj_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */
    if(obj_addr != 1178) TEST_ERROR;

    PASSED();

    TESTING("segmented heap allocate into free space");

    /* Allocate space for a simple object */
    obj_len = 3800;
    if (H5SH_alloc(f, H5P_DATASET_XFER_DEFAULT, sh_addr, obj_len, &obj_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */
    if(obj_addr != 1188) TEST_ERROR;

    PASSED();

    TESTING("segmented heap allocate another heap block");

    /* Allocate space for a simple object */
    obj_len = 512;
    if (H5SH_alloc(f, H5P_DATASET_XFER_DEFAULT, sh_addr, obj_len, &obj_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */
    if(obj_addr != 6298) TEST_ERROR;

    PASSED();

    TESTING("segmented heap extend heap block");

    /* Allocate space for a simple object */
    obj_len = 4096;
    if (H5SH_alloc(f, H5P_DATASET_XFER_DEFAULT, sh_addr, obj_len, &obj_addr/*out*/)<0) {
	H5_FAILED();
	H5Eprint_stack(H5E_DEFAULT, stdout);
	goto error;
    } /* end if */
    if(obj_addr != 6810) TEST_ERROR;

    PASSED();

    if (H5Fclose(file)<0) TEST_ERROR;

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_allocate_simple() */


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Test the segmented heap code
 *
 * Return:	Success:
 *
 *		Failure:
 *
 * Programmer:	Quincey Koziol
 *              Wednesday, March 23, 2005
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

    /* Test segmented heap creation */
    nerrors += test_create(fapl);

    /* Test segmented heap allocation */
    nerrors += test_allocate_simple(fapl);

    if (nerrors) goto error;
    puts("All segmented heap tests passed.");
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

