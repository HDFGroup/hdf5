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
 *              Friday, February 24, 2006
 */
#include "h5test.h"

/*
 * This file needs to access private datatypes from the H5HF package.
 * This file also needs to access the fractal heap testing code.
 */
#define H5HF_PACKAGE
#define H5HF_TESTING
#include "H5HFpkg.h"

/* Other private headers that this test requires */
#include "H5Iprivate.h"

const char *FILENAME[] = {
    "fheap",
    NULL
};


/*-------------------------------------------------------------------------
 * Function:	test_create
 *
 * Purpose:	Create fractal heap
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Friday, February 24, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_create(hid_t fapl)
{
    hid_t	file = -1;              /* File ID */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_type_t heap_type, test_heap_type; /* Type of address mapping for fractal heap */
    haddr_t     fh_addr;                /* Address of fractal heap created */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /*
     * Test fractal heap creation (w/absolute address mapping)
     */
    TESTING("Fractal heap creation (w/absolute address mapping)");
    heap_type = H5HF_ABSOLUTE;
    if(H5HF_create(f, H5P_DATASET_XFER_DEFAULT, heap_type, &fh_addr/*out*/) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    PASSED()

    /* Query the type of address mapping */
    TESTING("Query absolute address mapping setting");
    test_heap_type = H5HF_MAPPED;
    if(H5HF_get_addrmap_test(f, H5P_DATASET_XFER_DEFAULT, fh_addr, &test_heap_type) < 0)
        FAIL_STACK_ERROR
    if(test_heap_type != heap_type)
        FAIL_STACK_ERROR
    PASSED()

    /*
     * Test fractal heap creation (w/mapped address mapping)
     */
    TESTING("Fractal heap creation (w/mapped address mapping)");
    heap_type = H5HF_MAPPED;
    if(H5HF_create(f, H5P_DATASET_XFER_DEFAULT, heap_type, &fh_addr/*out*/) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    PASSED()

    /* Query the type of address mapping */
    TESTING("Query mapped address mapping setting");
    test_heap_type = H5HF_ABSOLUTE;
    if(H5HF_get_addrmap_test(f, H5P_DATASET_XFER_DEFAULT, fh_addr, &test_heap_type) < 0)
        FAIL_STACK_ERROR
    if(test_heap_type != heap_type)
        FAIL_STACK_ERROR
    PASSED()

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_create() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_insert_first
 *
 * Purpose:	Test inserting first object into absolute heap
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Friday, February 24, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_insert_first(hid_t fapl)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    haddr_t     fh_addr;                /* Address of fractal heap created */
    unsigned char obj[10];              /* Buffer for object to insert */
    haddr_t     heap_id;                /* Heap ID for object inserted */
    unsigned    u;                      /* Local index variable */
    herr_t      ret;                    /* Generic error return value */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(H5HF_create(f, dxpl, H5HF_ABSOLUTE, &fh_addr/*out*/) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
#ifdef QAK
HDfprintf(stdout, "Fractal heap header address = %a\n", fh_addr);
#endif /* QAK */

    /* Initialize object buffer */
    for(u = 0; u < sizeof(obj); u++)
        obj[u] = u;

    /*
     * Test inserting first (small) object into absolute heap
     */
    TESTING("Inserting first (small) object into absolute heap");
    heap_id = HADDR_UNDEF;
    if(H5HF_insert(f, dxpl, fh_addr, sizeof(obj), obj, &heap_id) < 0)
        FAIL_STACK_ERROR
#ifdef QAK
HDfprintf(stdout, "heap_id = %a\n", heap_id);
#endif /* QAK */
    PASSED()

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_abs_insert_first() */


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Test the fractal heap code
 *
 * Return:	Success:
 *
 *		Failure:
 *
 * Programmer:	Quincey Koziol
 *              Friday, February 24, 2006
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

    /* Test fractal heap creation */
    nerrors += test_create(fapl);

    /* Test fractal heap object insertion */
    nerrors += test_abs_insert_first(fapl);

    if(nerrors)
        goto error;
    puts("All fractal heap tests passed.");
#ifndef QAK
    h5_cleanup(FILENAME, fapl);
#else /* QAK */
HDfprintf(stderr, "Uncomment cleanup!\n");
#endif /* QAK */
    return 0;

error:
    puts("*** TESTS FAILED ***");
    H5E_BEGIN_TRY {
	H5Pclose(fapl);
    } H5E_END_TRY;
    return 1;
}

