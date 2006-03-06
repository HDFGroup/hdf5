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

/* "Standard" creation table parameters */
#define STD_ADDRMAP     H5HF_ABSOLUTE           /* Heap address mapping */
#define STD_STAND_SIZE  (64 * 1024)             /* Standalone obj. min. size */
#define STD_FIXED_LEN_SIZE 0                    /* Fixed length obj. size */
#define STD_REF_COUNT_SIZE 0                    /* Size of ref. count for obj. */
#define STD_MAN_WIDTH   32                      /* Managed obj. table width */
#define STD_MAN_START_BLOCK_SIZE 1024           /* Managed obj. starting block size */
#define STD_MAN_MAX_DIRECT_SIZE (1024 * 1024)   /* Managed obj. max. direct block size */
#define STD_MAN_MAX_INDEX 64                    /* Managed obj. # of bits for total heap size */
#define STD_MAN_START_ROOT_ROWS 1               /* Managed obj. starting # of root indirect block rows */

const char *FILENAME[] = {
    "fheap",
    NULL
};


/*-------------------------------------------------------------------------
 * Function:	init_std_cparam
 *
 * Purpose:	Initialize heap creation parameter structure with standard
 *              settings
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, February 27, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
init_std_cparam(H5HF_create_t *cparam)
{
    /* Wipe out background */
    HDmemset(cparam, 0, sizeof(H5HF_create_t));

    /* General parameters */
    cparam->addrmap = STD_ADDRMAP;
    cparam->standalone_size = STD_STAND_SIZE;
    cparam->fixed_len_size = STD_FIXED_LEN_SIZE;
    cparam->ref_count_size = STD_REF_COUNT_SIZE;

    /* Managed object doubling-table parameters */
    cparam->managed.width = STD_MAN_WIDTH;
    cparam->managed.start_block_size = STD_MAN_START_BLOCK_SIZE;
    cparam->managed.max_direct_size = STD_MAN_MAX_DIRECT_SIZE;
    cparam->managed.max_index = STD_MAN_MAX_INDEX;
    cparam->managed.start_root_rows = STD_MAN_START_ROOT_ROWS;

    return(0);
} /* init_std_cparam() */


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
    H5HF_create_t cparam, test_cparam;  /* Creation parameters for heap */
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
    init_std_cparam(&cparam);
    if(H5HF_create(f, H5P_DATASET_XFER_DEFAULT, &cparam, &fh_addr/*out*/) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    PASSED()

    /* Query the type of address mapping */
    TESTING("Query absolute address mapping setting");
    HDmemset(&test_cparam, 0, sizeof(H5HF_create_t));
    if(H5HF_get_cparam_test(f, H5P_DATASET_XFER_DEFAULT, fh_addr, &test_cparam) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(&cparam, &test_cparam, sizeof(H5HF_create_t)))
        FAIL_STACK_ERROR
    PASSED()

    /*
     * Test fractal heap creation (w/mapped address mapping)
     */
    TESTING("Fractal heap creation (w/mapped address mapping)");
    init_std_cparam(&cparam);
    cparam.addrmap = H5HF_MAPPED;
    if(H5HF_create(f, H5P_DATASET_XFER_DEFAULT, &cparam, &fh_addr/*out*/) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    PASSED()

    /* Query the type of address mapping */
    TESTING("Query mapped address mapping setting");
    HDmemset(&test_cparam, 0, sizeof(H5HF_create_t));
    if(H5HF_get_cparam_test(f, H5P_DATASET_XFER_DEFAULT, fh_addr, &test_cparam) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(&cparam, &test_cparam, sizeof(H5HF_create_t)))
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
    H5HF_create_t cparam;               /* Creation parameters for heap */
    haddr_t     fh_addr;                /* Address of fractal heap created */
    unsigned char obj[10];              /* Buffer for object to insert */
    hsize_t     heap_id;                /* Heap ID for object inserted */
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
    init_std_cparam(&cparam);
    if(H5HF_create(f, dxpl, &cparam, &fh_addr/*out*/) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
#ifndef QAK
HDfprintf(stdout, "Fractal heap header address = %a\n", fh_addr);
#endif /* QAK */

    /* Initialize object buffer */
    for(u = 0; u < sizeof(obj); u++)
        obj[u] = u;

    /*
     * Test inserting first (small) object into absolute heap
     */
    TESTING("Inserting first (small) object into absolute heap");
    heap_id = 0;
    if(H5HF_insert(f, dxpl, fh_addr, sizeof(obj), obj, &heap_id) < 0)
        FAIL_STACK_ERROR
#ifndef QAK
HDfprintf(stdout, "heap_id = %Hu\n", heap_id);
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
#ifdef QAK
    nerrors += test_create(fapl);

    /* Test fractal heap object insertion */
    nerrors += test_abs_insert_first(fapl);
#endif /* QAK */

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

