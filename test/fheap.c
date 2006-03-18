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

static int init_std_cparam(H5HF_create_t *cparam);
static int check_stats(H5F_t *f, hid_t dxpl, haddr_t fh_addr, hsize_t total_size,
    hsize_t man_size, hsize_t std_size, hsize_t man_free_space, hsize_t nobjs);


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
 * Function:	check_stats
 *
 * Purpose:	Verify stats for a heap
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, March  6, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
check_stats(H5F_t *f, hid_t dxpl, haddr_t fh_addr, hsize_t total_size,
    hsize_t man_size, hsize_t std_size, hsize_t man_free_space, hsize_t nobjs)
{
    H5HF_stat_t heap_stats;             /* Statistics about the heap */

    /* Get statistics for heap and verify they are correct */
    HDmemset(&heap_stats, 0, sizeof(H5HF_stat_t));
    if(H5HF_stat_info(f, dxpl, fh_addr, &heap_stats) < 0)
        FAIL_STACK_ERROR
    if(heap_stats.total_size != total_size) {
        HDfprintf(stdout, "heap_stats.total_size = %Hu, total_size = %Hu\n", heap_stats.total_size, total_size);
        FAIL_STACK_ERROR
    } /* end if */
    if(heap_stats.man_size != man_size) {
        HDfprintf(stdout, "heap_stats.man_size = %Hu, man_size = %Hu\n", heap_stats.man_size, man_size);
        FAIL_STACK_ERROR
    } /* end if */
    if(heap_stats.std_size != std_size) {
        HDfprintf(stdout, "heap_stats.std_size = %Hu, std_size = %Hu\n", heap_stats.std_size, std_size);
        FAIL_STACK_ERROR
    } /* end if */
    if(heap_stats.man_free_space != man_free_space) {
        HDfprintf(stdout, "heap_stats.man_free_space = %Hu, man_free_space = %Hu\n", heap_stats.man_free_space, man_free_space);
        FAIL_STACK_ERROR
    } /* end if */
    if(heap_stats.nobjs != nobjs) {
        HDfprintf(stdout, "heap_stats.nobjs = %Hu, nobjs = %Hu\n", heap_stats.nobjs, nobjs);
        FAIL_STACK_ERROR
    } /* end if */

    /* All tests passed */
    return(0);

error:
    return(1);
} /* check_stats() */


/*-------------------------------------------------------------------------
 * Function:	fill_heap
 *
 * Purpose:	Insert (small) objects to fill up the free space in a heap
 *              (Generally used to create & fill up a new direct block)
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, March  7, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
fill_heap(H5F_t *f, hid_t dxpl, haddr_t fh_addr, hsize_t heap_size,
    unsigned start_nobjs, unsigned *nobjs_ptr)
{
    H5HF_stat_t heap_stats;             /* Statistics about the heap */
    hsize_t     heap_id;                /* Heap ID for object inserted */
    unsigned char obj[10];              /* Buffer for first object to insert */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    unsigned    u;                      /* Local index variable */

    /* Initialize object buffer */
    for(u = 0; u < sizeof(obj); u++)
        obj[u] = u;

    /* Insert first object */
    heap_id = 0;
    if(H5HF_insert(f, dxpl, fh_addr, sizeof(obj), obj, &heap_id) < 0)
        FAIL_STACK_ERROR

    /* Increment object count */
    nobjs++;

    if(check_stats(f, H5P_DATASET_XFER_DEFAULT, fh_addr, heap_size, heap_size, (hsize_t)0, (hsize_t)(996 - (nobjs * (sizeof(obj) + 3))), (hsize_t)(start_nobjs + nobjs)))
        FAIL_STACK_ERROR

    /* Get statistics for heap */
    HDmemset(&heap_stats, 0, sizeof(H5HF_stat_t));
    if(H5HF_stat_info(f, dxpl, fh_addr, &heap_stats) < 0)
        FAIL_STACK_ERROR

    /* Loop over inserting objects into the root direct block, until there's no more space */
    /* (The "+ 2" in the equation below allows for the length of the object) */
    while(heap_stats.man_free_space > (sizeof(obj) + 2)) {
        /* Initialize object buffer */
        for(u = 0; u < sizeof(obj); u++)
            obj[u] = u + nobjs;

        heap_id = 0;
        if(H5HF_insert(f, dxpl, fh_addr, sizeof(obj), obj, &heap_id) < 0)
            FAIL_STACK_ERROR

        /* Increment object count */
        nobjs++;

        if(check_stats(f, H5P_DATASET_XFER_DEFAULT, fh_addr, heap_size, heap_size, (hsize_t)0, (hsize_t)(996 - (nobjs * (sizeof(obj) + 3))), (hsize_t)(start_nobjs + nobjs)))
            FAIL_STACK_ERROR

        /* Get statistics for heap */
        HDmemset(&heap_stats, 0, sizeof(H5HF_stat_t));
        if(H5HF_stat_info(f, dxpl, fh_addr, &heap_stats) < 0)
            FAIL_STACK_ERROR
    } /* end while */

    /* Initialize object buffer */
    for(u = 0; u < sizeof(obj); u++)
        obj[u] = u + nobjs;

    /* Insert last object into the heap, using the remaining free space */
    heap_id = 0;
    if(H5HF_insert(f, dxpl, fh_addr, (size_t)(heap_stats.man_free_space - 3), obj, &heap_id) < 0)
        FAIL_STACK_ERROR

    /* Increment object count */
    nobjs++;

    /* Verify that the heap is full */
    if(check_stats(f, H5P_DATASET_XFER_DEFAULT, fh_addr, heap_size, heap_size, (hsize_t)0, (hsize_t)0, (hsize_t)(start_nobjs + nobjs)))
        FAIL_STACK_ERROR

    /* Set the number of objects, if requested */
    if(nobjs_ptr)
        *nobjs_ptr = nobjs;

    /* Operations succeeded */
    return(0);

error:
    return(1);
} /* fill_heap() */


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
    if(check_stats(f, H5P_DATASET_XFER_DEFAULT, fh_addr, (hsize_t)0, (hsize_t)0, (hsize_t)0, (hsize_t)0, (hsize_t)0))
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
    if(check_stats(f, H5P_DATASET_XFER_DEFAULT, fh_addr, (hsize_t)0, (hsize_t)0, (hsize_t)0, (hsize_t)0, (hsize_t)0))
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
    unsigned char obj2[10];             /* Buffer for object to read */
    hsize_t     heap_id;                /* Heap ID for object inserted */
    unsigned    u;                      /* Local index variable */

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
    if(check_stats(f, H5P_DATASET_XFER_DEFAULT, fh_addr, (hsize_t)0, (hsize_t)0, (hsize_t)0, (hsize_t)0, (hsize_t)0))
        FAIL_STACK_ERROR

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
    if(check_stats(f, H5P_DATASET_XFER_DEFAULT, fh_addr, (hsize_t)1024, (hsize_t)1024, (hsize_t)0, (hsize_t)983, (hsize_t)1))
        FAIL_STACK_ERROR

    /* Check reading back in the object */
    if(H5HF_read(f, dxpl, fh_addr, &heap_id, obj2) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(obj, obj2, sizeof(obj)))
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
} /* test_abs_insert_first() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_insert_second
 *
 * Purpose:	Test inserting two objects into absolute heap
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, March  6, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_insert_second(hid_t fapl)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_create_t cparam;               /* Creation parameters for heap */
    haddr_t     fh_addr;                /* Address of fractal heap created */
    unsigned char obj[10];              /* Buffer for first object to insert */
    unsigned char obj2[20];             /* Buffer for second object to insert */
    hsize_t     heap_id;                /* Heap ID for object inserted */
    unsigned    u;                      /* Local index variable */

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

    /* Initialize object buffers */
    for(u = 0; u < sizeof(obj); u++)
        obj[u] = u;
    for(u = 0; u < sizeof(obj2); u++)
        obj2[u] = u + 10;

    /*
     * Test inserting first (small) object into absolute heap
     */
    TESTING("Inserting two (small) objects into absolute heap");
    heap_id = 0;
    if(H5HF_insert(f, dxpl, fh_addr, sizeof(obj), obj, &heap_id) < 0)
        FAIL_STACK_ERROR
    if(check_stats(f, H5P_DATASET_XFER_DEFAULT, fh_addr, (hsize_t)1024, (hsize_t)1024, (hsize_t)0, (hsize_t)983, (hsize_t)1))
        FAIL_STACK_ERROR

    heap_id = 0;
    if(H5HF_insert(f, dxpl, fh_addr, sizeof(obj2), obj2, &heap_id) < 0)
        FAIL_STACK_ERROR
    if(check_stats(f, H5P_DATASET_XFER_DEFAULT, fh_addr, (hsize_t)1024, (hsize_t)1024, (hsize_t)0, (hsize_t)960, (hsize_t)2))
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
} /* test_abs_insert_second() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_insert_root_mult
 *
 * Purpose:	Test inserting mult. objects into absolute heap, up to the
 *              limit of a root direct block
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, March  6, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_insert_root_mult(hid_t fapl)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_create_t cparam;               /* Creation parameters for heap */
    haddr_t     fh_addr;                /* Address of fractal heap created */

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

    /*
     * Test inserting mult. (small) object into absolute heap
     */
    TESTING("inserting objects to fill absolute heap's root direct block");

    /* Fill the heap up */
    if(fill_heap(f, dxpl, fh_addr, (hsize_t)1024, 0, NULL))
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
} /* test_abs_insert_root_mult() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_insert_force_indirect
 *
 * Purpose:	Test inserting mult. objects into absolute heap, filling the
 *              root direct block and forcing the root block to be converted
 *              into an indirect block
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, March  6, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_insert_force_indirect(hid_t fapl)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_create_t cparam;               /* Creation parameters for heap */
    haddr_t     fh_addr;                /* Address of fractal heap created */
    unsigned char obj[10];              /* Buffer for first object to insert */
    hsize_t     heap_id;                /* Heap ID for object inserted */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    unsigned    u;                      /* Local index variable */

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

    /*
     * Test forcing creation of indirect root block & second direct block
     */
    TESTING("inserting enough objects to create root indirect block");

    /* Fill the heap up */
    if(fill_heap(f, dxpl, fh_addr, (hsize_t)1024, 0, &nobjs))
        FAIL_STACK_ERROR

    /* Insert one more object, to force root indirect block creation */

    /* Initialize object buffer */
    for(u = 0; u < sizeof(obj); u++)
        obj[u] = u + nobjs;

    /* Insert another object, forcing the creation of an indirect block for the root block */
    heap_id = 0;
    if(H5HF_insert(f, dxpl, fh_addr, sizeof(obj), obj, &heap_id) < 0)
        FAIL_STACK_ERROR

    /* Increment object count */
    nobjs++;

    if(check_stats(f, H5P_DATASET_XFER_DEFAULT, fh_addr, (hsize_t)2048, (hsize_t)2048, (hsize_t)0, (hsize_t)983, (hsize_t)nobjs))
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
} /* test_abs_insert_force_indirect() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_insert_fill_second
 *
 * Purpose:	Test inserting mult. objects into absolute heap, filling the
 *              root direct block, forcing the root block to be converted
 *              into an indirect block and filling the secnod indirect block
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, March  7, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_insert_fill_second(hid_t fapl)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_create_t cparam;               /* Creation parameters for heap */
    haddr_t     fh_addr;                /* Address of fractal heap created */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    unsigned    tot_nobjs = 0;          /* Total number of objects inserted */

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

    /*
     * Test inserting mult. (small) objects to fill second direct block
     */
    TESTING("inserting enough objects to fill second direct block");

    /* Fill the first direct block heap up */
    if(fill_heap(f, dxpl, fh_addr, (hsize_t)1024, tot_nobjs, &nobjs))
        FAIL_STACK_ERROR
    tot_nobjs += nobjs;

    /* Fill the second direct block heap up (also creates initial root indirect block) */
    if(fill_heap(f, dxpl, fh_addr, (hsize_t)2048, tot_nobjs, &nobjs))
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
} /* test_abs_insert_fill_second() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_insert_third_direct
 *
 * Purpose:	Test inserting mult. objects into absolute heap, filling the
 *              root direct block, forcing the root block to be converted
 *              into an indirect block, filling the secnod indirect block and
 *              creating a third direct block
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, March  7, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_insert_third_direct(hid_t fapl)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_create_t cparam;               /* Creation parameters for heap */
    haddr_t     fh_addr;                /* Address of fractal heap created */
    unsigned char obj[10];              /* Buffer for first object to insert */
    hsize_t     heap_id;                /* Heap ID for object inserted */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    unsigned    tot_nobjs = 0;          /* Total number of objects inserted */
    unsigned    u;                      /* Local index variable */

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

    /*
     * Test inserting mult. (small) objects to create third direct block
     */
    TESTING("inserting enough objects to create third direct block");

    /* Fill the first direct block heap up */
    if(fill_heap(f, dxpl, fh_addr, (hsize_t)1024, tot_nobjs, &nobjs))
        FAIL_STACK_ERROR
    tot_nobjs += nobjs;

    /* Fill the second direct block heap up (also creates initial root indirect block) */
    if(fill_heap(f, dxpl, fh_addr, (hsize_t)2048, tot_nobjs, &nobjs))
        FAIL_STACK_ERROR
    tot_nobjs += nobjs;

    /* Insert one more object, to force creation of third direct block */

    /* Initialize object buffer */
    for(u = 0; u < sizeof(obj); u++)
        obj[u] = u + nobjs;

    /* Insert another object, forcing the creation of the third direct block */
    heap_id = 0;
    if(H5HF_insert(f, dxpl, fh_addr, sizeof(obj), obj, &heap_id) < 0)
        FAIL_STACK_ERROR

    /* Increment object count */
    tot_nobjs++;

    if(check_stats(f, H5P_DATASET_XFER_DEFAULT, fh_addr, (hsize_t)3072, (hsize_t)3072, (hsize_t)0, (hsize_t)983, (hsize_t)tot_nobjs))
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
} /* test_abs_insert_third_direct() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_fill_first_row
 *
 * Purpose:	Test inserting mult. objects into absolute heap, creating
 *              enough direct blocks to fill first row of root indirect
 *              block.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, March 13, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_fill_first_row(hid_t fapl)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_create_t cparam;               /* Creation parameters for heap */
    haddr_t     fh_addr;                /* Address of fractal heap created */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    unsigned    tot_nobjs = 0;          /* Total number of objects inserted */
    unsigned    u;                      /* Local index variable */

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

    /*
     * Test inserting mult. (small) objects to fill first row in root indirect block
     */
    TESTING("inserting enough objects to fill first row of root indirect block");

    /* Loop over filling direct blocks, until first root indirect row is full */
    for(u = 0; u < STD_MAN_WIDTH; u++) {
        /* Fill a direct heap block up */
        if(fill_heap(f, dxpl, fh_addr, (hsize_t)(u + 1) * STD_MAN_START_BLOCK_SIZE, tot_nobjs, &nobjs))
            FAIL_STACK_ERROR
        tot_nobjs += nobjs;
    } /* end for */

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
} /* test_abs_fill_first_row() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_start_second_row
 *
 * Purpose:	Test inserting mult. objects into absolute heap, creating
 *              enough direct blocks to fill first row of root indirect
 *              block, then add another object to start second row.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, March 14, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_start_second_row(hid_t fapl)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_create_t cparam;               /* Creation parameters for heap */
    haddr_t     fh_addr;                /* Address of fractal heap created */
    unsigned char obj[10];              /* Buffer for first object to insert */
    hsize_t     heap_id;                /* Heap ID for object inserted */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    unsigned    tot_nobjs = 0;          /* Total number of objects inserted */
    unsigned    u;                      /* Local index variable */

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
#ifdef QAK
HDfprintf(stderr, "Fractal heap header address = %a\n", fh_addr);
#endif /* QAK */

    /*
     * Test inserting mult. (small) objects to start second row in root indirect block
     */
    TESTING("inserting enough objects to start second row of root indirect block");

    /* Loop over filling direct blocks, until first root indirect row is full */
    for(u = 0; u < STD_MAN_WIDTH; u++) {
        /* Fill a direct heap block up */
        if(fill_heap(f, dxpl, fh_addr, (hsize_t)(u + 1) * STD_MAN_START_BLOCK_SIZE, tot_nobjs, &nobjs))
            FAIL_STACK_ERROR
        tot_nobjs += nobjs;
    } /* end for */

    /* Insert one more object, to force root indirect block creation */

    /* Initialize object buffer */
    for(u = 0; u < sizeof(obj); u++)
        obj[u] = u + tot_nobjs;

    /* Insert another object, forcing the creation of an indirect block for the root block */
    heap_id = 0;
    if(H5HF_insert(f, dxpl, fh_addr, sizeof(obj), obj, &heap_id) < 0)
        FAIL_STACK_ERROR

    /* Increment object count */
    tot_nobjs++;

    if(check_stats(f, H5P_DATASET_XFER_DEFAULT, fh_addr, (hsize_t)((STD_MAN_WIDTH + 1) * STD_MAN_START_BLOCK_SIZE), (hsize_t)((STD_MAN_WIDTH + 1) * STD_MAN_START_BLOCK_SIZE), (hsize_t)0, (hsize_t)983, (hsize_t)tot_nobjs))
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
} /* test_abs_start_second_row() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_fill_second_row
 *
 * Purpose:	Test inserting mult. objects into absolute heap, creating
 *              enough direct blocks to fill first row of root indirect
 *              block, then fill the second row also.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, March 14, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_fill_second_row(hid_t fapl)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_create_t cparam;               /* Creation parameters for heap */
    haddr_t     fh_addr;                /* Address of fractal heap created */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    unsigned    tot_nobjs = 0;          /* Total number of objects inserted */
    unsigned    u;                      /* Local index variable */

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
HDfprintf(stderr, "Fractal heap header address = %a\n", fh_addr);
#endif /* QAK */

    /*
     * Test inserting mult. (small) objects to start second row in root indirect block
     */
    TESTING("inserting enough objects to fill second row of root indirect block");

    /* Loop over filling direct blocks, until first root indirect row is full */
    for(u = 0; u < STD_MAN_WIDTH; u++) {
        /* Fill a direct heap block up */
        if(fill_heap(f, dxpl, fh_addr, (hsize_t)(u + 1) * STD_MAN_START_BLOCK_SIZE, tot_nobjs, &nobjs))
            FAIL_STACK_ERROR
        tot_nobjs += nobjs;
    } /* end for */

    /* Loop over filling direct blocks, until second root indirect row is full */
    for(u = 0; u < STD_MAN_WIDTH; u++) {
        /* Fill a direct heap block up */
        if(fill_heap(f, dxpl, fh_addr, (hsize_t)((STD_MAN_WIDTH * STD_MAN_START_BLOCK_SIZE) + (u + 1) * STD_MAN_START_BLOCK_SIZE), tot_nobjs, &nobjs))
            FAIL_STACK_ERROR
        tot_nobjs += nobjs;
    } /* end for */

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
} /* test_abs_fill_second_row() */


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
#ifndef QAK
    nerrors += test_create(fapl);

    /* Test fractal heap object insertion */
    nerrors += test_abs_insert_first(fapl);
    nerrors += test_abs_insert_second(fapl);
    nerrors += test_abs_insert_root_mult(fapl);
    nerrors += test_abs_insert_force_indirect(fapl);
    nerrors += test_abs_insert_fill_second(fapl);
    nerrors += test_abs_insert_third_direct(fapl);
    nerrors += test_abs_fill_first_row(fapl);
    nerrors += test_abs_fill_first_row(fapl);
    nerrors += test_abs_start_second_row(fapl);
#else /* QAK */
HDfprintf(stderr, "Uncomment tests!\n");
#endif /* QAK */
#ifdef QAK
    nerrors += test_abs_fill_second_row(fapl);
#else /* QAK */
HDfprintf(stderr, "Uncomment tests!\n");
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

