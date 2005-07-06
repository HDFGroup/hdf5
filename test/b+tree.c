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
 *              Tuesday, April 19, 2005
 */
#include "h5test.h"

/*
 * This file needs to access private datatypes from the H5BP package.
 * This file also needs to access the B+ tree testing code.
 */
#define H5BP_PACKAGE
#define H5BP_TESTING
#include "H5BPpkg.h"

/* Other private headers that this test requires */
#include "H5Iprivate.h"

const char *FILENAME[] = {
    "b+tree",
    NULL
};

#define NODE_SIZE       512


/*-------------------------------------------------------------------------
 * Function:	test_create
 *
 * Purpose:	Basic tests for the B+ tree code
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, April 19, 2005
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
    haddr_t     bpt_addr;               /* Address of B+ tree created */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0)
        STACK_ERROR
	
    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file)))
	STACK_ERROR

    /*
     * Test B+ tree creation
     */
    TESTING("B+ tree creation");
    if (H5BP_create(f, H5P_DATASET_XFER_DEFAULT, H5BP_TEST, NODE_SIZE, 100, 40, &bpt_addr/*out*/)<0)
	FAIL_STACK_ERROR

    PASSED();

    if (H5Fclose(file)<0)
        STACK_ERROR

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_create() */


/*-------------------------------------------------------------------------
 * Function:	test_insert_basic
 *
 * Purpose:	Basic tests for the B+ tree code
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, April 19, 2005
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
    char        *record;                /* Record to insert into tree */
    haddr_t     bpt_addr;               /* Address of B+ tree created */
    herr_t      ret;                    /* Generic error return value */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create the file to work on */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0)
        STACK_ERROR
	
    /* Get a pointer to the internal file object */
    if (NULL==(f=H5I_object(file)))
        STACK_ERROR

    /* Create B+ tree */
    if (H5BP_create(f, H5P_DATASET_XFER_DEFAULT, H5BP_TEST, NODE_SIZE, 100, 40, &bpt_addr/*out*/)<0)
	FAIL_STACK_ERROR

    /*
     * Test B+ tree creation
     */
    TESTING("B+ tree single record insert");
    record="Foo!";
    if (H5BP_insert(f, H5P_DATASET_XFER_DEFAULT, H5BP_TEST, bpt_addr, &record)<0)
	FAIL_STACK_ERROR

    PASSED();

    if (H5Fclose(file)<0)
        STACK_ERROR

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_insert_basic() */


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Test the B+ tree code
 *
 * Return:	Success:	
 *
 *		Failure:	
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, April 19, 2005
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

    /* Test B+ tree creation */
    nerrors += test_create(fapl);

    /* Test B+ tree record insertion */
    /* Iteration, find & index routines tested in these routines as well */
#ifdef QAK
    nerrors += test_insert_basic(fapl);
#endif /* QAK */

    if (nerrors) goto error;
    puts("All B+ tree tests passed.");
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

