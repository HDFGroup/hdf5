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
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Vprivate.h"		/* Vectors and arrays 			*/

/* "Small" heap creation parameters */
#define SMALL_ADDRMAP     H5HF_ABSOLUTE           /* Heap address mapping */
#define SMALL_STAND_SIZE  (8 * 1024)              /* Standalone obj. min. size */
#define SMALL_MAN_WIDTH   4                       /* Managed obj. table width */
#define SMALL_MAN_START_BLOCK_SIZE 512            /* Managed obj. starting block size */
#define SMALL_MAN_MAX_DIRECT_SIZE (64 * 1024)     /* Managed obj. max. direct block size */
#define SMALL_MAN_MAX_INDEX 32                    /* Managed obj. # of bits for total heap size */
#define SMALL_MAN_START_ROOT_ROWS 1               /* Managed obj. starting # of root indirect block rows */

/* "Standard" heap creation parameters */
#define STD_ADDRMAP     H5HF_ABSOLUTE           /* Heap address mapping */
#define STD_STAND_SIZE  (64 * 1024)             /* Standalone obj. min. size */
#define STD_MAN_WIDTH   32                      /* Managed obj. table width */
#define STD_MAN_START_BLOCK_SIZE 1024           /* Managed obj. starting block size */
#define STD_MAN_MAX_DIRECT_SIZE (1024 * 1024)   /* Managed obj. max. direct block size */
#define STD_MAN_MAX_INDEX 64                    /* Managed obj. # of bits for total heap size */
#define STD_MAN_START_ROOT_ROWS 1               /* Managed obj. starting # of root indirect block rows */

/* Heap metadata */
#define DBLOCK_OVERHEAD         32              /* # of bytes in direct block overhead */
#define OBJ_PREFIX_LEN           1              /* # of bytes in object prefix overhead */
#define HEAP_ID_LEN             12              /* # of bytes to use for heap ID */

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

    /* Managed object doubling-table parameters */
    cparam->managed.width = STD_MAN_WIDTH;
    cparam->managed.start_block_size = STD_MAN_START_BLOCK_SIZE;
    cparam->managed.max_direct_size = STD_MAN_MAX_DIRECT_SIZE;
    cparam->managed.max_index = STD_MAN_MAX_INDEX;
    cparam->managed.start_root_rows = STD_MAN_START_ROOT_ROWS;

    return(0);
} /* init_std_cparam() */


/*-------------------------------------------------------------------------
 * Function:	init_small_cparam
 *
 * Purpose:	Initialize heap creation parameter structure with small
 *              settings
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, March 21, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
init_small_cparam(H5HF_create_t *cparam)
{
    /* Wipe out background */
    HDmemset(cparam, 0, sizeof(H5HF_create_t));

    /* General parameters */
    cparam->addrmap = SMALL_ADDRMAP;
    cparam->standalone_size = SMALL_STAND_SIZE;

    /* Managed object doubling-table parameters */
    cparam->managed.width = SMALL_MAN_WIDTH;
    cparam->managed.start_block_size = SMALL_MAN_START_BLOCK_SIZE;
    cparam->managed.max_direct_size = SMALL_MAN_MAX_DIRECT_SIZE;
    cparam->managed.max_index = SMALL_MAN_MAX_INDEX;
    cparam->managed.start_root_rows = SMALL_MAN_START_ROOT_ROWS;

    return(0);
} /* init_small_cparam() */


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
fill_heap(H5F_t *f, hid_t dxpl, haddr_t fh_addr, const H5HF_create_t *cparam,
    hsize_t heap_size, size_t block_size,
    hsize_t extra_free,
    unsigned start_nobjs, unsigned *nobjs_ptr)
{
    H5HF_stat_t heap_stats;             /* Statistics about the heap */
    unsigned char heap_id[HEAP_ID_LEN]; /* Heap ID for object inserted */
    hsize_t     free_space;             /* Size of free space in heap */
    unsigned char obj[10];              /* Buffer for object to insert */
    unsigned char robj[10];             /* Buffer for reading object */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    size_t      alloc_ids = 0;          /* # of heap IDs allocated in array */
    unsigned char *ids = NULL;          /* Array of heap IDs */
    size_t      data_size;              /* Size of data portion of heap block */
    size_t      free_overhead;          /* Size of free space overhead for each object */
    unsigned    free_frag_size;         /* Size of free space fragment */
    size_t      last_obj_len;           /* Size of last object inserted into heap */
    unsigned    u, v;                   /* Local index variable */

    /* Initialize variables */
    if(block_size <= (64 * 1024)) {
        data_size = block_size - (OBJ_PREFIX_LEN + DBLOCK_OVERHEAD + cparam->managed.max_index / 8);    /* '28' is the size of the direct block's overhead */
        free_overhead = 4;
    } /* end if */
    else {
        data_size = block_size - (OBJ_PREFIX_LEN + DBLOCK_OVERHEAD + 1 + cparam->managed.max_index / 8);    /* '29' is the size of the direct block's overhead */
        free_overhead = 6;
    } /* end else */

    /* Initialize object buffer */
    for(u = 0; u < sizeof(obj); u++)
        obj[u] = u;

    /* Insert first object */
    HDmemset(heap_id, 0, sizeof(heap_id));
    if(H5HF_insert(f, dxpl, fh_addr, sizeof(obj), obj, heap_id) < 0)
        FAIL_STACK_ERROR

    /* Increment object count */
    nobjs++;

    /* Check for needing to increase size of heap ID array */
    if(nobjs > alloc_ids) {
        alloc_ids = MAX(1024, (alloc_ids * 2));
        if(NULL == (ids = H5MM_realloc(ids, HEAP_ID_LEN * alloc_ids)))
            FAIL_STACK_ERROR
    } /* end if */
    HDmemcpy(&ids[(nobjs - 1) * HEAP_ID_LEN], heap_id, HEAP_ID_LEN);

    free_space = extra_free + (data_size - (nobjs * (sizeof(obj) + OBJ_PREFIX_LEN)));
#ifdef QAK
HDfprintf(stderr, "extra_free = %Hu\n", extra_free);
HDfprintf(stderr, "free_space = %Hu\n", free_space);
#endif /* QAK */
    if(check_stats(f, dxpl, fh_addr, heap_size, heap_size, (hsize_t)0, free_space, (hsize_t)(start_nobjs + nobjs)))
        FAIL_STACK_ERROR

    /* Get statistics for heap */
    if(H5HF_stat_info(f, dxpl, fh_addr, &heap_stats) < 0)
        FAIL_STACK_ERROR

    /* Loop over inserting objects into the root direct block, until there's no more space */
    free_frag_size = 0;
    while((heap_stats.man_free_space - extra_free) > (sizeof(obj) + OBJ_PREFIX_LEN)) {
        /* Initialize object buffer */
        for(u = 0; u < sizeof(obj); u++)
            obj[u] = u + nobjs;

        HDmemset(heap_id, 0, sizeof(heap_id));
        if(H5HF_insert(f, dxpl, fh_addr, sizeof(obj), obj, heap_id) < 0)
            FAIL_STACK_ERROR

        /* Increment object count */
        nobjs++;

        /* Check for needing to increase size of heap ID array */
        if(nobjs > alloc_ids) {
            alloc_ids = MAX(1024, (alloc_ids * 2));
            if(NULL == (ids = H5MM_realloc(ids, HEAP_ID_LEN * alloc_ids)))
                FAIL_STACK_ERROR
        } /* end if */
        HDmemcpy(&ids[(nobjs - 1) * HEAP_ID_LEN], heap_id, HEAP_ID_LEN);

        /* Check stats for heap */
        if(((heap_stats.man_free_space - extra_free) - sizeof(obj)) <= free_overhead)
            free_frag_size = (heap_stats.man_free_space - extra_free) - (sizeof(obj) + OBJ_PREFIX_LEN);
#ifdef QAK
HDfprintf(stderr, "free_frag_size = %u\n", free_frag_size);
#endif /* QAK */
        free_space = extra_free + (data_size - ((nobjs * (sizeof(obj) + OBJ_PREFIX_LEN)) + free_frag_size));
#ifdef QAK
HDfprintf(stderr, "free_space = %Hu\n", free_space);
#endif /* QAK */
        if(check_stats(f, dxpl, fh_addr, heap_size, heap_size, (hsize_t)0, free_space, (hsize_t)(start_nobjs + nobjs)))
            FAIL_STACK_ERROR

        /* Get statistics for heap */
        if(H5HF_stat_info(f, dxpl, fh_addr, &heap_stats) < 0)
            FAIL_STACK_ERROR
    } /* end while */

    /* Check for adding smaller last object to heap block */
    if((heap_stats.man_free_space - extra_free) > 0) {
        last_obj_len = (size_t)((heap_stats.man_free_space - extra_free) - OBJ_PREFIX_LEN);

        /* Initialize object buffer */
        for(u = 0; u < sizeof(obj); u++)
            obj[u] = u + nobjs;

        /* Insert last object into the heap, using the remaining free space */
        HDmemset(heap_id, 0, sizeof(heap_id));
        if(H5HF_insert(f, dxpl, fh_addr, last_obj_len, obj, heap_id) < 0)
            FAIL_STACK_ERROR

        /* Increment object count */
        nobjs++;

        /* Check for needing to increase size of heap ID array */
        if(nobjs > alloc_ids) {
            alloc_ids = MAX(1024, (alloc_ids * 2));
            if(NULL == (ids = H5MM_realloc(ids, HEAP_ID_LEN * alloc_ids)))
                FAIL_STACK_ERROR
        } /* end if */
        HDmemcpy(&ids[(nobjs - 1) * HEAP_ID_LEN], heap_id, HEAP_ID_LEN);

        /* Verify that the heap is full */
        if(check_stats(f, dxpl, fh_addr, heap_size, heap_size, (hsize_t)0, extra_free, (hsize_t)(start_nobjs + nobjs)))
            FAIL_STACK_ERROR
    } /* end if */
    else
        last_obj_len = sizeof(obj);     /* Normal sized last object */

    /* Verify reading the objects written out */
    for(v = 0; v < nobjs; v++) {
        /* Initialize object buffer */
        for(u = 0; u < sizeof(obj); u++)
            obj[u] = u + v;

        /* Read in object */
        if(H5HF_read(f, dxpl, fh_addr, &ids[v * HEAP_ID_LEN], robj) < 0)
            FAIL_STACK_ERROR
        if(HDmemcmp(obj, robj, (v == (nobjs - 1) ? last_obj_len : sizeof(obj))))
            FAIL_STACK_ERROR
    } /* end for */

    /* Set the number of objects, if requested */
    if(nobjs_ptr)
        *nobjs_ptr = nobjs;

    /* Operations succeeded */
    H5MM_xfree(ids);
    return(0);

error:
    H5MM_xfree(ids);
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
test_create(hid_t fapl, H5HF_create_t *cparam)
{
    hid_t	file = -1;              /* File ID */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_create_t test_cparam;          /* Creation parameters for heap */
    haddr_t     fh_addr;                /* Address of fractal heap created */
    size_t      id_len;                 /* Size of fractal heap IDs */

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
    TESTING("fractal heap creation (w/absolute address mapping)");
    if(H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam, &fh_addr/*out*/, &id_len/*out*/) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(check_stats(f, H5P_DATASET_XFER_DEFAULT, fh_addr, (hsize_t)0, (hsize_t)0, (hsize_t)0, (hsize_t)0, (hsize_t)0))
        FAIL_STACK_ERROR
    PASSED()

    /* Query the type of address mapping */
    TESTING("query heap creation parameters");
    HDmemset(&test_cparam, 0, sizeof(H5HF_create_t));
    if(H5HF_get_cparam_test(f, H5P_DATASET_XFER_DEFAULT, fh_addr, &test_cparam) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(cparam, &test_cparam, sizeof(H5HF_create_t)))
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
test_abs_insert_first(hid_t fapl, H5HF_create_t *cparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    haddr_t     fh_addr;                /* Address of fractal heap created */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned char obj[10];              /* Buffer for object to insert */
    unsigned char robj[10];             /* Buffer for object to read */
    unsigned char heap_id[HEAP_ID_LEN]; /* Heap ID for object inserted */
    hsize_t     free_space;             /* Size of free space in heap */
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
    if(H5HF_create(f, dxpl, cparam, &fh_addr/*out*/, &id_len/*out*/) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(check_stats(f, H5P_DATASET_XFER_DEFAULT, fh_addr, (hsize_t)0, (hsize_t)0, (hsize_t)0, (hsize_t)0, (hsize_t)0))
        FAIL_STACK_ERROR

    /* Initialize object buffer */
    for(u = 0; u < sizeof(obj); u++)
        obj[u] = u;

    /*
     * Test inserting first (small) object into absolute heap
     */
    TESTING("inserting first (small) object into absolute heap");
    HDmemset(heap_id, 0, sizeof(heap_id));
    if(H5HF_insert(f, dxpl, fh_addr, sizeof(obj), obj, heap_id) < 0)
        FAIL_STACK_ERROR
    free_space = cparam->managed.start_block_size - ((sizeof(obj) + OBJ_PREFIX_LEN) + DBLOCK_OVERHEAD + (cparam->managed.max_index / 8));
    if(check_stats(f, H5P_DATASET_XFER_DEFAULT, fh_addr, (hsize_t)cparam->managed.start_block_size, (hsize_t)cparam->managed.start_block_size, (hsize_t)0, free_space, (hsize_t)1))
        FAIL_STACK_ERROR

    /* Check reading back in the object */
    if(H5HF_read(f, dxpl, fh_addr, heap_id, robj) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(obj, robj, sizeof(obj)))
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
test_abs_insert_second(hid_t fapl, H5HF_create_t *cparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    haddr_t     fh_addr;                /* Address of fractal heap created */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned char obj[10];              /* Buffer for first object to insert */
    unsigned char robj[10];             /* Buffer for reading first object */
    unsigned char obj2[20];             /* Buffer for second object to insert */
    unsigned char robj2[20];            /* Buffer for reading second object */
    unsigned char heap_id[HEAP_ID_LEN]; /* Heap ID for object inserted */
    hsize_t     free_space;             /* Size of free space in heap */
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
    if(H5HF_create(f, dxpl, cparam, &fh_addr/*out*/, &id_len/*out*/) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR

    /* Initialize object buffers */
    for(u = 0; u < sizeof(obj); u++)
        obj[u] = u;
    for(u = 0; u < sizeof(obj2); u++)
        obj2[u] = u + 10;

    /*
     * Test inserting first (small) object into absolute heap
     */
    TESTING("inserting two (small) objects into absolute heap");
    HDmemset(heap_id, 0, sizeof(heap_id));
    if(H5HF_insert(f, dxpl, fh_addr, sizeof(obj), obj, heap_id) < 0)
        FAIL_STACK_ERROR
    free_space = cparam->managed.start_block_size - ((sizeof(obj) + OBJ_PREFIX_LEN) + DBLOCK_OVERHEAD + (cparam->managed.max_index / 8));
    if(check_stats(f, H5P_DATASET_XFER_DEFAULT, fh_addr, (hsize_t)cparam->managed.start_block_size, (hsize_t)cparam->managed.start_block_size, (hsize_t)0, free_space, (hsize_t)1))
        FAIL_STACK_ERROR

    /* Check reading back in the first object */
    if(H5HF_read(f, dxpl, fh_addr, heap_id, robj) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(obj, robj, sizeof(obj)))
        FAIL_STACK_ERROR

    /* Insert second object */
    HDmemset(heap_id, 0, sizeof(heap_id));
    if(H5HF_insert(f, dxpl, fh_addr, sizeof(obj2), obj2, heap_id) < 0)
        FAIL_STACK_ERROR
    free_space = cparam->managed.start_block_size - ((sizeof(obj) + OBJ_PREFIX_LEN) + (sizeof(obj2) + OBJ_PREFIX_LEN) + DBLOCK_OVERHEAD + (cparam->managed.max_index / 8));
    if(check_stats(f, H5P_DATASET_XFER_DEFAULT, fh_addr, (hsize_t)cparam->managed.start_block_size, (hsize_t)cparam->managed.start_block_size, (hsize_t)0, free_space, (hsize_t)2))
        FAIL_STACK_ERROR

    /* Check reading back in the second object */
    if(H5HF_read(f, dxpl, fh_addr, heap_id, robj2) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(obj2, robj2, sizeof(obj2)))
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
test_abs_insert_root_mult(hid_t fapl, H5HF_create_t *cparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    haddr_t     fh_addr;                /* Address of fractal heap created */
    size_t      id_len;                 /* Size of fractal heap IDs */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(H5HF_create(f, dxpl, cparam, &fh_addr/*out*/, &id_len/*out*/) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR

    /*
     * Test inserting mult. (small) object into absolute heap
     */
    TESTING("inserting objects to fill absolute heap's root direct block");

    /* Fill the heap up */
    if(fill_heap(f, dxpl, fh_addr, cparam, (hsize_t)cparam->managed.start_block_size, cparam->managed.start_block_size, (hsize_t)0, 0, NULL))
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
test_abs_insert_force_indirect(hid_t fapl, H5HF_create_t *cparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    haddr_t     fh_addr;                /* Address of fractal heap created */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned char obj[10];              /* Buffer for first object to insert */
    unsigned char heap_id[HEAP_ID_LEN]; /* Heap ID for object inserted */
    hsize_t     free_space;             /* Size of free space in heap */
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
    if(H5HF_create(f, dxpl, cparam, &fh_addr/*out*/, &id_len/*out*/) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR

    /*
     * Test forcing creation of indirect root block & second direct block
     */
    TESTING("inserting objects to create root indirect block");

    /* Fill the heap up */
    if(fill_heap(f, dxpl, fh_addr, cparam, (hsize_t)cparam->managed.start_block_size, cparam->managed.start_block_size, (hsize_t)0, 0, &nobjs))
        FAIL_STACK_ERROR

    /* Insert one more object, to force root indirect block creation */

    /* Initialize object buffer */
    for(u = 0; u < sizeof(obj); u++)
        obj[u] = u + nobjs;

    /* Insert another object, forcing the creation of an indirect block for the root block */
    HDmemset(heap_id, 0, sizeof(heap_id));
    if(H5HF_insert(f, dxpl, fh_addr, sizeof(obj), obj, heap_id) < 0)
        FAIL_STACK_ERROR

    /* Increment object count */
    nobjs++;

    free_space = cparam->managed.start_block_size - ((sizeof(obj) + OBJ_PREFIX_LEN) + DBLOCK_OVERHEAD + (cparam->managed.max_index / 8));
    if(check_stats(f, H5P_DATASET_XFER_DEFAULT, fh_addr, (hsize_t)(2 * cparam->managed.start_block_size), (hsize_t)(2 * cparam->managed.start_block_size), (hsize_t)0, free_space, (hsize_t)nobjs))
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
test_abs_insert_fill_second(hid_t fapl, H5HF_create_t *cparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    haddr_t     fh_addr;                /* Address of fractal heap created */
    size_t      id_len;                 /* Size of fractal heap IDs */
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
    if(H5HF_create(f, dxpl, cparam, &fh_addr/*out*/, &id_len/*out*/) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR

    /*
     * Test inserting mult. (small) objects to fill second direct block
     */
    TESTING("inserting objects to fill second direct block");

    /* Fill the first direct block heap up */
    if(fill_heap(f, dxpl, fh_addr, cparam, (hsize_t)cparam->managed.start_block_size, cparam->managed.start_block_size, (hsize_t)0, tot_nobjs, &nobjs))
        FAIL_STACK_ERROR
    tot_nobjs += nobjs;

    /* Fill the second direct block heap up (also creates initial root indirect block) */
    if(fill_heap(f, dxpl, fh_addr, cparam, (hsize_t)(2 * cparam->managed.start_block_size), cparam->managed.start_block_size, (hsize_t)0, tot_nobjs, &nobjs))
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
test_abs_insert_third_direct(hid_t fapl, H5HF_create_t *cparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    haddr_t     fh_addr;                /* Address of fractal heap created */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned char obj[10];              /* Buffer for first object to insert */
    unsigned char robj[10];             /* Buffer for reading object */
    unsigned char heap_id[HEAP_ID_LEN]; /* Heap ID for object inserted */
    hsize_t     free_space;             /* Size of free space in heap */
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
    if(H5HF_create(f, dxpl, cparam, &fh_addr/*out*/, &id_len/*out*/) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR

    /*
     * Test inserting mult. (small) objects to create third direct block
     */
    TESTING("inserting objects to create third direct block");

    /* Fill the first direct block up */
    if(fill_heap(f, dxpl, fh_addr, cparam, (hsize_t)cparam->managed.start_block_size, cparam->managed.start_block_size, (hsize_t)0, tot_nobjs, &nobjs))
        FAIL_STACK_ERROR
    tot_nobjs += nobjs;

    /* Fill the second direct block heap up (also creates initial root indirect block) */
    if(fill_heap(f, dxpl, fh_addr, cparam, (hsize_t)(2 * cparam->managed.start_block_size), cparam->managed.start_block_size, (hsize_t)0, tot_nobjs, &nobjs))
        FAIL_STACK_ERROR
    tot_nobjs += nobjs;

    /* Insert one more object, to force creation of third direct block */

    /* Initialize object buffer */
    for(u = 0; u < sizeof(obj); u++)
        obj[u] = u + nobjs;

    /* Insert another object, forcing the creation of the third direct block */
    HDmemset(heap_id, 0, sizeof(heap_id));
    if(H5HF_insert(f, dxpl, fh_addr, sizeof(obj), obj, heap_id) < 0)
        FAIL_STACK_ERROR

    /* Increment object count */
    tot_nobjs++;

    free_space = cparam->managed.start_block_size - ((sizeof(obj) + OBJ_PREFIX_LEN) + DBLOCK_OVERHEAD + (cparam->managed.max_index / 8));
    if(check_stats(f, H5P_DATASET_XFER_DEFAULT, fh_addr, (hsize_t)(3 * cparam->managed.start_block_size), (hsize_t)(3 * cparam->managed.start_block_size), (hsize_t)0, free_space, (hsize_t)tot_nobjs))
        FAIL_STACK_ERROR

    /* Read in object */
    if(H5HF_read(f, dxpl, fh_addr, heap_id, robj) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(obj, robj, sizeof(obj)))
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
test_abs_fill_first_row(hid_t fapl, H5HF_create_t *cparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    haddr_t     fh_addr;                /* Address of fractal heap created */
    size_t      id_len;                 /* Size of fractal heap IDs */
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
    if(H5HF_create(f, dxpl, cparam, &fh_addr/*out*/, &id_len/*out*/) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR

    /*
     * Test inserting mult. (small) objects to fill first row in root indirect block
     */
    TESTING("inserting objects to fill first row of root indirect block");

    /* Loop over filling direct blocks, until first root indirect row is full */
    for(u = 0; u < cparam->managed.width; u++) {
        /* Fill a direct heap block up */
        if(fill_heap(f, dxpl, fh_addr, cparam, (hsize_t)(u + 1) * cparam->managed.start_block_size, cparam->managed.start_block_size, (hsize_t)0, tot_nobjs, &nobjs))
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
test_abs_start_second_row(hid_t fapl, H5HF_create_t *cparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    haddr_t     fh_addr;                /* Address of fractal heap created */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned char obj[10];              /* Buffer for object to insert */
    unsigned char robj[10];             /* Buffer for reading object */
    unsigned char heap_id[HEAP_ID_LEN]; /* Heap ID for object inserted */
    hsize_t     free_space;             /* Size of free space in heap */
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
    if(H5HF_create(f, dxpl, cparam, &fh_addr/*out*/, &id_len/*out*/) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR

    /*
     * Test inserting mult. (small) objects to start second row in root indirect block
     */
    TESTING("inserting objects to start second row of root indirect block");

    /* Loop over filling direct blocks, until first root indirect row is full */
    for(u = 0; u < cparam->managed.width; u++) {
        /* Fill a direct heap block up */
        if(fill_heap(f, dxpl, fh_addr, cparam, (hsize_t)(u + 1) * cparam->managed.start_block_size, cparam->managed.start_block_size, (hsize_t)0, tot_nobjs, &nobjs))
            FAIL_STACK_ERROR
        tot_nobjs += nobjs;
    } /* end for */

    /* Insert one more object, to force expanding root indirect block to two rows */

    /* Initialize object buffer */
    for(u = 0; u < sizeof(obj); u++)
        obj[u] = u + tot_nobjs;

    /* Insert another object, forcing the creation of an indirect block for the root block */
    HDmemset(heap_id, 0, sizeof(heap_id));
    if(H5HF_insert(f, dxpl, fh_addr, sizeof(obj), obj, heap_id) < 0)
        FAIL_STACK_ERROR

    /* Increment object count */
    tot_nobjs++;

    free_space = cparam->managed.start_block_size - ((sizeof(obj) + OBJ_PREFIX_LEN) + DBLOCK_OVERHEAD + (cparam->managed.max_index / 8));
    if(check_stats(f, H5P_DATASET_XFER_DEFAULT, fh_addr, (hsize_t)((cparam->managed.width + 1) * cparam->managed.start_block_size), (hsize_t)((cparam->managed.width + 1) * cparam->managed.start_block_size), (hsize_t)0, free_space, (hsize_t)tot_nobjs))
        FAIL_STACK_ERROR

    /* Read in object */
    if(H5HF_read(f, dxpl, fh_addr, heap_id, robj) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(obj, robj, sizeof(obj)))
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
test_abs_fill_second_row(hid_t fapl, H5HF_create_t *cparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    haddr_t     fh_addr;                /* Address of fractal heap created */
    size_t      id_len;                 /* Size of fractal heap IDs */
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
    if(H5HF_create(f, dxpl, cparam, &fh_addr/*out*/, &id_len/*out*/) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR

    /*
     * Test inserting mult. (small) objects to start second row in root indirect block
     */
    TESTING("inserting objects to fill second row of root indirect block");

    /* Loop over filling direct blocks, until first root indirect row is full */
    for(u = 0; u < cparam->managed.width; u++) {
        /* Fill a direct heap block up */
        if(fill_heap(f, dxpl, fh_addr, cparam, (hsize_t)(u + 1) * cparam->managed.start_block_size, cparam->managed.start_block_size, (hsize_t)0, tot_nobjs, &nobjs))
            FAIL_STACK_ERROR
        tot_nobjs += nobjs;
    } /* end for */

    /* Loop over filling direct blocks, until second root indirect row is full */
    for(u = 0; u < cparam->managed.width; u++) {
        /* Fill a direct heap block up */
        if(fill_heap(f, dxpl, fh_addr, cparam, (hsize_t)((cparam->managed.width * cparam->managed.start_block_size) + (u + 1) * cparam->managed.start_block_size), cparam->managed.start_block_size, (hsize_t)0, tot_nobjs, &nobjs))
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
 * Function:	test_abs_start_third_row
 *
 * Purpose:	Test inserting mult. objects into absolute heap, creating
 *              enough direct blocks to fill first row of root indirect
 *              block, fill the second row also, then add another object to
 *              start the third row.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, March 20, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_start_third_row(hid_t fapl, H5HF_create_t *cparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    haddr_t     fh_addr;                /* Address of fractal heap created */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned char obj[10];              /* Buffer for object to insert */
    unsigned char robj[10];             /* Buffer for reading object */
    unsigned char heap_id[HEAP_ID_LEN]; /* Heap ID for object inserted */
    hsize_t     free_space;             /* Size of free space in heap */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    unsigned    tot_nobjs = 0;          /* Total number of objects inserted */
    hsize_t     heap_size;              /* Total size of heap */
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
    if(H5HF_create(f, dxpl, cparam, &fh_addr/*out*/, &id_len/*out*/) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR

    /*
     * Test inserting mult. (small) objects to start third row in root indirect block
     */
    TESTING("inserting objects to start third row of root indirect block");

    /* Loop over filling direct blocks, until first root indirect row is full */
    for(u = 0; u < cparam->managed.width; u++) {
        /* Fill a direct heap block up */
        if(fill_heap(f, dxpl, fh_addr, cparam, (hsize_t)(u + 1) * cparam->managed.start_block_size, cparam->managed.start_block_size, (hsize_t)0, tot_nobjs, &nobjs))
            FAIL_STACK_ERROR
        tot_nobjs += nobjs;
    } /* end for */

    /* Loop over filling direct blocks, until second root indirect row is full */
    for(u = 0; u < cparam->managed.width; u++) {
        /* Fill a direct heap block up */
        if(fill_heap(f, dxpl, fh_addr, cparam, (hsize_t)((cparam->managed.width * cparam->managed.start_block_size) + (u + 1) * cparam->managed.start_block_size), cparam->managed.start_block_size, (hsize_t)0, tot_nobjs, &nobjs))
            FAIL_STACK_ERROR
        tot_nobjs += nobjs;
    } /* end for */

    /* Insert one more object, to force expanding root indirect block to three rows */

    /* Initialize object buffer */
    for(u = 0; u < sizeof(obj); u++)
        obj[u] = u + tot_nobjs;

    /* Insert object */
    HDmemset(heap_id, 0, sizeof(heap_id));
    if(H5HF_insert(f, dxpl, fh_addr, sizeof(obj), obj, heap_id) < 0)
        FAIL_STACK_ERROR

    /* Increment object count */
    tot_nobjs++;

    heap_size = (2 * cparam->managed.width) * cparam->managed.start_block_size +
            (2 * cparam->managed.start_block_size);
    free_space = (2 * cparam->managed.start_block_size) - ((sizeof(obj) + OBJ_PREFIX_LEN) + DBLOCK_OVERHEAD + (cparam->managed.max_index / 8));
    if(check_stats(f, H5P_DATASET_XFER_DEFAULT, fh_addr, heap_size, heap_size, (hsize_t)0, free_space, (hsize_t)tot_nobjs))
        FAIL_STACK_ERROR

    /* Read in object */
    if(H5HF_read(f, dxpl, fh_addr, heap_id, robj) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(obj, robj, sizeof(obj)))
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
} /* test_abs_start_third_row() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_fill_fourth_row
 *
 * Purpose:	Test inserting mult. objects into absolute heap, creating
 *              enough direct blocks to fill first four rows of root indirect
 *              block.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, March 20, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_fill_fourth_row(hid_t fapl, H5HF_create_t *cparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    haddr_t     fh_addr;                /* Address of fractal heap created */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    unsigned    tot_nobjs = 0;          /* Total number of objects inserted */
    size_t      block_size;             /* Size of block added */
    hsize_t     heap_size;              /* Total size of heap */
    unsigned    u, v;                   /* Local index variables */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(H5HF_create(f, dxpl, cparam, &fh_addr/*out*/, &id_len/*out*/) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR

    /*
     * Test inserting mult. (small) objects to fill four rows in root indirect block
     */
    TESTING("inserting objects to fill four rows of root indirect block");

    /* Loop over rows */
    block_size = cparam->managed.start_block_size;
    heap_size = 0;
    for(v = 0; v < 4; v++) {
        /* Loop over filling direct blocks for a row */
        for(u = 0; u < cparam->managed.width; u++) {
            /* Fill a direct heap block up */
            heap_size += block_size;
            if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                FAIL_STACK_ERROR
            tot_nobjs += nobjs;
        } /* end for */

        /* Adjust block size for row */
        if(v > 0)
            block_size *= 2;
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
} /* test_abs_fill_fourth_row() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_fill_all_root_direct
 *
 * Purpose:	Test inserting mult. objects into absolute heap, creating
 *              enough direct blocks to fill all direct rows of root indirect
 *              block.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, March 20, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_fill_all_root_direct(hid_t fapl, H5HF_create_t *cparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    haddr_t     fh_addr;                /* Address of fractal heap created */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    unsigned    tot_nobjs = 0;          /* Total number of objects inserted */
    size_t      block_size;             /* Size of block added */
    hsize_t     heap_size;              /* Total size of heap */
    unsigned    nrows;                  /* Number of rows inserted */
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
    if(H5HF_create(f, dxpl, cparam, &fh_addr/*out*/, &id_len/*out*/) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR

    /*
     * Test inserting mult. (small) objects to fill all direct  rows in root indirect block
     */
    TESTING("inserting objects to fill all direct rows of root indirect block");

    /* Loop over rows */
    block_size = cparam->managed.start_block_size;
    heap_size = 0;
    nrows = 0;
    while(block_size <= cparam->managed.max_direct_size) {
        /* Loop over filling direct blocks for a row */
        for(u = 0; u < cparam->managed.width; u++) {
            /* Fill a direct heap block up */
            heap_size += block_size;
            if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                FAIL_STACK_ERROR
            tot_nobjs += nobjs;
        } /* end for */

        /* Adjust block size for row */
        if(nrows > 0)
            block_size *= 2;

        /* Increment row count */
        nrows++;
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
} /* test_abs_fill_all_root_direct() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_first_recursive_indirec5
 *
 * Purpose:	Test inserting mult. objects into absolute heap, creating
 *              enough direct blocks to fill all direct rows of root indirect
 *              block and create first recursive indirect block.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, March 20, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_first_recursive_indirect(hid_t fapl, H5HF_create_t *cparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    haddr_t     fh_addr;                /* Address of fractal heap created */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned char obj[10];              /* Buffer for object to insert */
    unsigned char robj[10];             /* Buffer for reading object */
    unsigned char heap_id[HEAP_ID_LEN]; /* Heap ID for object inserted */
    hsize_t     free_space;             /* Size of free space in heap */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    unsigned    tot_nobjs = 0;          /* Total number of objects inserted */
    size_t      block_size;             /* Size of block added */
    hsize_t     heap_size;              /* Total size of heap */
    unsigned    nrows;                  /* Number of rows inserted */
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
    if(H5HF_create(f, dxpl, cparam, &fh_addr/*out*/, &id_len/*out*/) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR

    /*
     * Test inserting mult. (small) objects to force creation of first recursive indirect block
     */
    TESTING("inserting objects to create first recursive indirect block");

    /* Loop over rows */
    block_size = cparam->managed.start_block_size;
    heap_size = 0;
    nrows = 0;
    while(block_size <= cparam->managed.max_direct_size) {
        /* Loop over filling direct blocks for a row */
        for(u = 0; u < cparam->managed.width; u++) {
            /* Fill a direct heap block up */
            heap_size += block_size;
            if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                FAIL_STACK_ERROR
            tot_nobjs += nobjs;
        } /* end for */

        /* Adjust block size for row */
        if(nrows > 0)
            block_size *= 2;

        /* Increment row count */
        nrows++;
    } /* end for */

    /* Insert one more object, to force creation of first recursive indirect block */

    /* Initialize object buffer */
    for(u = 0; u < sizeof(obj); u++)
        obj[u] = u + tot_nobjs;

    /* Insert object */
    HDmemset(heap_id, 0, sizeof(heap_id));
    if(H5HF_insert(f, dxpl, fh_addr, sizeof(obj), obj, heap_id) < 0)
        FAIL_STACK_ERROR

    /* Increment object count */
    tot_nobjs++;

    heap_size += cparam->managed.start_block_size;
    free_space = cparam->managed.start_block_size - ((sizeof(obj) + OBJ_PREFIX_LEN) + DBLOCK_OVERHEAD + (cparam->managed.max_index / 8));
    if(check_stats(f, H5P_DATASET_XFER_DEFAULT, fh_addr, heap_size, heap_size, (hsize_t)0, free_space, (hsize_t)tot_nobjs))
        FAIL_STACK_ERROR

    /* Read in object */
    if(H5HF_read(f, dxpl, fh_addr, heap_id, robj) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(obj, robj, sizeof(obj)))
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
} /* test_abs_first_recursive_indirect() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_second_direct_recursive_indirec5
 *
 * Purpose:	Test inserting mult. objects into absolute heap, creating
 *              enough direct blocks to fill all direct rows of root indirect
 *              block, create first recursive indirect block and start second
 *              direct block in that indirect block.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, March 21, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_second_direct_recursive_indirect(hid_t fapl, H5HF_create_t *cparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    haddr_t     fh_addr;                /* Address of fractal heap created */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned char obj[10];              /* Buffer for object to insert */
    unsigned char robj[10];             /* Buffer for reading object */
    unsigned char heap_id[HEAP_ID_LEN]; /* Heap ID for object inserted */
    hsize_t     free_space;             /* Size of free space in heap */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    unsigned    tot_nobjs = 0;          /* Total number of objects inserted */
    size_t      block_size;             /* Size of block added */
    hsize_t     heap_size;              /* Total size of heap */
    unsigned    nrows;                  /* Number of rows inserted */
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
    if(H5HF_create(f, dxpl, cparam, &fh_addr/*out*/, &id_len/*out*/) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR

    /*
     * Test inserting mult. (small) objects to force creation of second direct
     *  block in first recursive indirect block
     */
    TESTING("inserting objects to create second direct block in first recursive indirect block");

    /* Loop over rows */
    block_size = cparam->managed.start_block_size;
    heap_size = 0;
    nrows = 0;
    while(block_size <= cparam->managed.max_direct_size) {
        /* Loop over filling direct blocks for a row */
        for(u = 0; u < cparam->managed.width; u++) {
            /* Fill a direct heap block up */
            heap_size += block_size;
            if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                FAIL_STACK_ERROR
            tot_nobjs += nobjs;
        } /* end for */

        /* Adjust block size for row */
        if(nrows > 0)
            block_size *= 2;

        /* Increment row count */
        nrows++;
    } /* end for */

    /* Fill the first direct block in the recursive indirect block up */
    heap_size += cparam->managed.start_block_size;
    if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, cparam->managed.start_block_size, (hsize_t)0, tot_nobjs, &nobjs))
        FAIL_STACK_ERROR
    tot_nobjs += nobjs;

    /* Insert one more object, to force creation of second direct block in
     * first recursive indirect block
     */

    /* Initialize object buffer */
    for(u = 0; u < sizeof(obj); u++)
        obj[u] = u + tot_nobjs;

    /* Insert object */
    HDmemset(heap_id, 0, sizeof(heap_id));
    if(H5HF_insert(f, dxpl, fh_addr, sizeof(obj), obj, heap_id) < 0)
        FAIL_STACK_ERROR

    /* Increment object count */
    tot_nobjs++;

    heap_size += cparam->managed.start_block_size;
    free_space = cparam->managed.start_block_size - ((sizeof(obj) + OBJ_PREFIX_LEN) + DBLOCK_OVERHEAD + (cparam->managed.max_index / 8));
    if(check_stats(f, H5P_DATASET_XFER_DEFAULT, fh_addr, heap_size, heap_size, (hsize_t)0, free_space, (hsize_t)tot_nobjs))
        FAIL_STACK_ERROR

    /* Read in object */
    if(H5HF_read(f, dxpl, fh_addr, heap_id, robj) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(obj, robj, sizeof(obj)))
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
} /* test_abs_second_direct_recursive_indirect() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_fill_first_recursive_indirect
 *
 * Purpose:	Test inserting mult. objects into absolute heap, creating
 *              enough direct blocks to fill all direct rows of root indirect
 *              block, create first recursive indirect block and filling all
 *              direct blocks in that indirect block.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, March 21, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_fill_first_recursive_indirect(hid_t fapl, H5HF_create_t *cparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    haddr_t     fh_addr;                /* Address of fractal heap created */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    unsigned    tot_nobjs = 0;          /* Total number of objects inserted */
    size_t      block_size;             /* Size of block added */
    size_t      max_block_size;         /* Max. size of block to add */
    hsize_t     heap_size;              /* Total size of heap */
    unsigned    nrows;                  /* Number of rows inserted */
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
    if(H5HF_create(f, dxpl, cparam, &fh_addr/*out*/, &id_len/*out*/) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in first recursive indirect block
     */
    TESTING("inserting objects to fill all direct blocks in first recursive indirect block");

    /* Loop over direct block rows in root indirect block */
    block_size = cparam->managed.start_block_size;
    heap_size = 0;
    nrows = 0;
    while(block_size <= cparam->managed.max_direct_size) {
        /* Loop over filling direct blocks for a row */
        for(u = 0; u < cparam->managed.width; u++) {
            /* Fill a direct heap block up */
            heap_size += block_size;
            if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                FAIL_STACK_ERROR
            tot_nobjs += nobjs;
        } /* end for */

        /* Adjust block size for row */
        if(nrows > 0)
            block_size *= 2;

        /* Increment row count */
        nrows++;
    } /* end for */

    /* Loop over direct block rows in first recursive indirect block */
    block_size = cparam->managed.start_block_size;
    nrows = 0;
    max_block_size = block_size * (1 << ((H5V_log2_of2((2 * cparam->managed.max_direct_size) / cparam->managed.width) -
            (H5V_log2_of2(cparam->managed.start_block_size) +
                H5V_log2_of2(cparam->managed.width))) + 1));
    while(block_size <= max_block_size) {
        /* Loop over filling direct blocks for a row */
        for(u = 0; u < cparam->managed.width; u++) {
            /* Fill a direct heap block up */
            heap_size += block_size;
            if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                FAIL_STACK_ERROR
            tot_nobjs += nobjs;
        } /* end for */

        /* Adjust block size for row */
        if(nrows > 0)
            block_size *= 2;

        /* Increment row count */
        nrows++;
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
} /* test_abs_fill_first_recursive_indirect() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_second_recursive_indirect
 *
 * Purpose:	Test inserting mult. objects into absolute heap, creating
 *              enough direct blocks to fill all direct rows of root indirect
 *              block, create first recursive indirect block, filling all
 *              direct blocks in that indirect block and adding another
 *              object to force creation of second recursive indirect block.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, March 21, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_second_recursive_indirect(hid_t fapl, H5HF_create_t *cparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    haddr_t     fh_addr;                /* Address of fractal heap created */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned char obj[10];              /* Buffer for object to insert */
    unsigned char robj[10];             /* Buffer for reading object */
    unsigned char heap_id[HEAP_ID_LEN]; /* Heap ID for object inserted */
    hsize_t     free_space;             /* Size of free space in heap */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    unsigned    tot_nobjs = 0;          /* Total number of objects inserted */
    size_t      block_size;             /* Size of block added */
    size_t      max_block_size;         /* Max. size of block to add */
    hsize_t     heap_size;              /* Total size of heap */
    unsigned    nrows;                  /* Number of rows inserted */
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
    if(H5HF_create(f, dxpl, cparam, &fh_addr/*out*/, &id_len/*out*/) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in first recursive indirect block
     */
    TESTING("inserting objects to start second recursive indirect block");

    /* Loop over direct block rows in root indirect block */
    block_size = cparam->managed.start_block_size;
    heap_size = 0;
    nrows = 0;
    while(block_size <= cparam->managed.max_direct_size) {
        /* Loop over filling direct blocks for a row */
        for(u = 0; u < cparam->managed.width; u++) {
            /* Fill a direct heap block up */
            heap_size += block_size;
            if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                FAIL_STACK_ERROR
            tot_nobjs += nobjs;
        } /* end for */

        /* Adjust block size for row */
        if(nrows > 0)
            block_size *= 2;

        /* Increment row count */
        nrows++;
    } /* end for */

    /* Loop over direct block rows in first recursive indirect block */
    block_size = cparam->managed.start_block_size;
    nrows = 0;
    max_block_size = block_size * (1 << ((H5V_log2_of2((2 * cparam->managed.max_direct_size) / cparam->managed.width) -
            (H5V_log2_of2(cparam->managed.start_block_size) +
                H5V_log2_of2(cparam->managed.width))) + 1));
    while(block_size <= max_block_size) {
        /* Loop over filling direct blocks for a row */
        for(u = 0; u < cparam->managed.width; u++) {
            /* Fill a direct heap block up */
            heap_size += block_size;
            if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                FAIL_STACK_ERROR
            tot_nobjs += nobjs;
        } /* end for */

        /* Adjust block size for row */
        if(nrows > 0)
            block_size *= 2;

        /* Increment row count */
        nrows++;
    } /* end for */

    /* Insert one more object, to force creation of second 
     * recursive indirect block
     */

    /* Initialize object buffer */
    for(u = 0; u < sizeof(obj); u++)
        obj[u] = u + tot_nobjs;

    /* Insert object */
    HDmemset(heap_id, 0, sizeof(heap_id));
    if(H5HF_insert(f, dxpl, fh_addr, sizeof(obj), obj, heap_id) < 0)
        FAIL_STACK_ERROR

    /* Increment object count */
    tot_nobjs++;

    heap_size += cparam->managed.start_block_size;
    free_space = cparam->managed.start_block_size - ((sizeof(obj) + OBJ_PREFIX_LEN) + DBLOCK_OVERHEAD + (cparam->managed.max_index / 8));
    if(check_stats(f, H5P_DATASET_XFER_DEFAULT, fh_addr, heap_size, heap_size, (hsize_t)0, free_space, (hsize_t)tot_nobjs))
        FAIL_STACK_ERROR

    /* Read in object */
    if(H5HF_read(f, dxpl, fh_addr, heap_id, robj) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(obj, robj, sizeof(obj)))
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
} /* test_abs_second_recursive_indirect() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_fill_second_recursive_indirect
 *
 * Purpose:	Test inserting mult. objects into absolute heap, creating
 *              enough direct blocks to fill all direct rows of root indirect
 *              block, create first recursive indirect block, filling all
 *              direct blocks in that indirect block and then create second
 *              recursive indirect block and fill all direct blocks in that
 *              indirect block.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, March 21, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_fill_second_recursive_indirect(hid_t fapl, H5HF_create_t *cparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    haddr_t     fh_addr;                /* Address of fractal heap created */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    unsigned    tot_nobjs = 0;          /* Total number of objects inserted */
    size_t      block_size;             /* Size of block added */
    size_t      max_block_size;         /* Max. size of block to add */
    hsize_t     heap_size;              /* Total size of heap */
    unsigned    nrows;                  /* Number of rows inserted */
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
    if(H5HF_create(f, dxpl, cparam, &fh_addr/*out*/, &id_len/*out*/) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in first recursive indirect block
     */
    TESTING("inserting objects to fill all direct blocks in second recursive indirect block");

    /* Loop over direct block rows in root indirect block */
    block_size = cparam->managed.start_block_size;
    heap_size = 0;
    nrows = 0;
    while(block_size <= cparam->managed.max_direct_size) {
        /* Loop over filling direct blocks for a row */
        for(u = 0; u < cparam->managed.width; u++) {
            /* Fill a direct heap block up */
            heap_size += block_size;
            if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                FAIL_STACK_ERROR
            tot_nobjs += nobjs;
        } /* end for */

        /* Adjust block size for row */
        if(nrows > 0)
            block_size *= 2;

        /* Increment row count */
        nrows++;
    } /* end for */

    /* Loop over direct block rows in first recursive indirect block */
    block_size = cparam->managed.start_block_size;
    nrows = 0;
    max_block_size = block_size * (1 << ((H5V_log2_of2((2 * cparam->managed.max_direct_size) / cparam->managed.width) -
            (H5V_log2_of2(cparam->managed.start_block_size) +
                H5V_log2_of2(cparam->managed.width))) + 1));
    while(block_size <= max_block_size) {
        /* Loop over filling direct blocks for a row */
        for(u = 0; u < cparam->managed.width; u++) {
            /* Fill a direct heap block up */
            heap_size += block_size;
            if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                FAIL_STACK_ERROR
            tot_nobjs += nobjs;
        } /* end for */

        /* Adjust block size for row */
        if(nrows > 0)
            block_size *= 2;

        /* Increment row count */
        nrows++;
    } /* end for */

    /* Loop over direct block rows in second recursive indirect block */
    block_size = cparam->managed.start_block_size;
    nrows = 0;
    max_block_size = block_size * (1 << ((H5V_log2_of2((2 * cparam->managed.max_direct_size) / cparam->managed.width) -
            (H5V_log2_of2(cparam->managed.start_block_size) +
                H5V_log2_of2(cparam->managed.width))) + 1));
    while(block_size <= max_block_size) {
        /* Loop over filling direct blocks for a row */
        for(u = 0; u < cparam->managed.width; u++) {
            /* Fill a direct heap block up */
            heap_size += block_size;
            if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                FAIL_STACK_ERROR
            tot_nobjs += nobjs;
        } /* end for */

        /* Adjust block size for row */
        if(nrows > 0)
            block_size *= 2;

        /* Increment row count */
        nrows++;
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
} /* test_abs_fill_second_recursive_indirect() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_fill_recursive_indirect_row
 *
 * Purpose:	Test inserting mult. objects into absolute heap, creating
 *              enough direct blocks to fill all direct rows of root indirect
 *              block, create first recursive indirect block, filling all
 *              direct blocks in that indirect block and then create second
 *              recursive indirect block and fill all direct blocks in that
 *              indirect block.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, March 21, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_fill_recursive_indirect_row(hid_t fapl, H5HF_create_t *cparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    haddr_t     fh_addr;                /* Address of fractal heap created */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    unsigned    tot_nobjs = 0;          /* Total number of objects inserted */
    size_t      block_size;             /* Size of block added */
    size_t      max_block_size;         /* Max. size of block to add */
    hsize_t     heap_size;              /* Total size of heap */
    unsigned    nrows;                  /* Number of rows inserted */
    unsigned    u, v;                   /* Local index variables */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(H5HF_create(f, dxpl, cparam, &fh_addr/*out*/, &id_len/*out*/) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in first recursive indirect block
     */
    TESTING("inserting objects to fill all direct blocks in first row of recursive indirect block");

    /* Loop over direct block rows in root indirect block */
    block_size = cparam->managed.start_block_size;
    heap_size = 0;
    nrows = 0;
    while(block_size <= cparam->managed.max_direct_size) {
        /* Loop over filling direct blocks for a row */
        for(u = 0; u < cparam->managed.width; u++) {
            /* Fill a direct heap block up */
            heap_size += block_size;
            if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                FAIL_STACK_ERROR
            tot_nobjs += nobjs;
        } /* end for */

        /* Adjust block size for row */
        if(nrows > 0)
            block_size *= 2;

        /* Increment row count */
        nrows++;
    } /* end for */

    /* Loop over row of indirect blocks */
    for(v = 0; v < cparam->managed.width; v++) {
        /* Loop over direct block rows in first recursive indirect block */
        block_size = cparam->managed.start_block_size;
        nrows = 0;
        max_block_size = block_size * (1 << ((H5V_log2_of2((2 * cparam->managed.max_direct_size) / cparam->managed.width) -
                (H5V_log2_of2(cparam->managed.start_block_size) +
                    H5V_log2_of2(cparam->managed.width))) + 1));
        while(block_size <= max_block_size) {
            /* Loop over filling direct blocks for a row */
            for(u = 0; u < cparam->managed.width; u++) {
                /* Fill a direct heap block up */
                heap_size += block_size;
                if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                    FAIL_STACK_ERROR
                tot_nobjs += nobjs;
            } /* end for */

            /* Adjust block size for row */
            if(nrows > 0)
                block_size *= 2;

            /* Increment row count */
            nrows++;
        } /* end for */
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
} /* test_abs_fill_recursive_indirect_row() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_start_2nd_recursive_indirect
 *
 * Purpose:	Test inserting mult. objects into absolute heap, creating
 *              enough direct blocks to fill all direct rows of root indirect
 *              block, fill all direct blocks in the first row of indirect
 *              blocks and start on first block in second row of indirect blocks
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, March 27, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_start_2nd_recursive_indirect(hid_t fapl, H5HF_create_t *cparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    haddr_t     fh_addr;                /* Address of fractal heap created */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned char obj[10];              /* Buffer for object to insert */
    unsigned char robj[10];             /* Buffer for reading object */
    unsigned char heap_id[HEAP_ID_LEN]; /* Heap ID for object inserted */
    hsize_t     free_space;             /* Size of free space in heap */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    unsigned    tot_nobjs = 0;          /* Total number of objects inserted */
    size_t      block_size;             /* Size of block added */
    size_t      max_block_size;         /* Max. size of block to add */
    hsize_t     heap_size;              /* Total size of heap */
    unsigned    nrows;                  /* Number of rows inserted */
    unsigned    u, v;                   /* Local index variables */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(H5HF_create(f, dxpl, cparam, &fh_addr/*out*/, &id_len/*out*/) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in first recursive indirect block
     */
    TESTING("inserting objects to start second row of recursive indirect blocks");

    /* Loop over direct block rows in root indirect block */
    block_size = cparam->managed.start_block_size;
    heap_size = 0;
    nrows = 0;
    while(block_size <= cparam->managed.max_direct_size) {
        /* Loop over filling direct blocks for a row */
        for(u = 0; u < cparam->managed.width; u++) {
            /* Fill a direct heap block up */
            heap_size += block_size;
            if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                FAIL_STACK_ERROR
            tot_nobjs += nobjs;
        } /* end for */

        /* Adjust block size for row */
        if(nrows > 0)
            block_size *= 2;

        /* Increment row count */
        nrows++;
    } /* end for */

    /* Loop over row of indirect blocks */
    for(v = 0; v < cparam->managed.width; v++) {
        /* Loop over direct block rows in first recursive indirect block */
        block_size = cparam->managed.start_block_size;
        nrows = 0;
        max_block_size = block_size * (1 << ((H5V_log2_of2((2 * cparam->managed.max_direct_size) / cparam->managed.width) -
                (H5V_log2_of2(cparam->managed.start_block_size) +
                    H5V_log2_of2(cparam->managed.width))) + 1));
        while(block_size <= max_block_size) {
            /* Loop over filling direct blocks for a row */
            for(u = 0; u < cparam->managed.width; u++) {
                /* Fill a direct heap block up */
                heap_size += block_size;
                if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                    FAIL_STACK_ERROR
                tot_nobjs += nobjs;
            } /* end for */

            /* Adjust block size for row */
            if(nrows > 0)
                block_size *= 2;

            /* Increment row count */
            nrows++;
        } /* end for */
    } /* end for */

    /* Insert one more object, to force creation of second 
     * recursive indirect block
     */

    /* Initialize object buffer */
    for(u = 0; u < sizeof(obj); u++)
        obj[u] = u + tot_nobjs;

    /* Insert object */
    HDmemset(heap_id, 0, sizeof(heap_id));
    if(H5HF_insert(f, dxpl, fh_addr, sizeof(obj), obj, heap_id) < 0)
        FAIL_STACK_ERROR

    /* Increment object count */
    tot_nobjs++;

    heap_size += cparam->managed.start_block_size;
    free_space = cparam->managed.start_block_size - ((sizeof(obj) + OBJ_PREFIX_LEN) + DBLOCK_OVERHEAD + (cparam->managed.max_index / 8));
    if(check_stats(f, H5P_DATASET_XFER_DEFAULT, fh_addr, heap_size, heap_size, (hsize_t)0, free_space, (hsize_t)tot_nobjs))
        FAIL_STACK_ERROR

    /* Read in object */
    if(H5HF_read(f, dxpl, fh_addr, heap_id, robj) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(obj, robj, sizeof(obj)))
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
} /* test_abs_start_2nd_recursive_indirect() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_recursive_indirect_two_deep
 *
 * Purpose:	Test inserting mult. objects into absolute heap, creating
 *              enough direct blocks to fill all direct rows of root indirect
 *              block, fill all direct blocks in the row of indirect
 *              blocks that are 2 levels deep
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, March 27, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_recursive_indirect_two_deep(hid_t fapl, H5HF_create_t *cparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    haddr_t     fh_addr;                /* Address of fractal heap created */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    unsigned    tot_nobjs = 0;          /* Total number of objects inserted */
    size_t      block_size;             /* Size of block added */
    size_t      max_block_size;         /* Max. size of block to add */
    hsize_t     heap_size;              /* Total size of heap */
    unsigned    nrows;                  /* Number of rows inserted */
    unsigned    u, v, w;                /* Local index variables */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(H5HF_create(f, dxpl, cparam, &fh_addr/*out*/, &id_len/*out*/) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in first recursive indirect block
     */
    TESTING("inserting objects to fill recursive indirect blocks two levels deep");

    /* Loop over direct block rows in root indirect block */
    block_size = cparam->managed.start_block_size;
    heap_size = 0;
    nrows = 0;
    while(block_size <= cparam->managed.max_direct_size) {
        /* Loop over filling direct blocks for a row */
        for(u = 0; u < cparam->managed.width; u++) {
            /* Fill a direct heap block up */
            heap_size += block_size;
            if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                FAIL_STACK_ERROR
            tot_nobjs += nobjs;
        } /* end for */

        /* Adjust block size for row */
        if(nrows > 0)
            block_size *= 2;

        /* Increment row count */
        nrows++;
    } /* end for */

    /* Loop over rows of 2nd level deep indirect blocks */
    for(w = 0; w < (H5V_log2_of2(cparam->managed.width) + 1); w++) {
        for(v = 0; v < cparam->managed.width; v++) {
            /* Loop over direct block rows in first recursive indirect block */
            block_size = cparam->managed.start_block_size;
            nrows = 0;
            max_block_size = block_size * (1 << ((H5V_log2_of2(cparam->managed.max_direct_size / cparam->managed.width) -
                    (H5V_log2_of2(cparam->managed.start_block_size) +
                        H5V_log2_of2(cparam->managed.width))) + (w + 1) + 1));
            while(block_size <= max_block_size) {
                /* Loop over filling direct blocks for a row */
                for(u = 0; u < cparam->managed.width; u++) {
                    /* Fill a direct heap block up */
                    heap_size += block_size;
                    if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                        FAIL_STACK_ERROR
                    tot_nobjs += nobjs;
                } /* end for */

                /* Adjust block size for row */
                if(nrows > 0)
                    block_size *= 2;

                /* Increment row count */
                nrows++;
            } /* end for */
        } /* end for */
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
} /* test_abs_recursive_indirect_two_deep() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_start_3rd_recursive_indirect
 *
 * Purpose:	Test inserting mult. objects into absolute heap, creating
 *              enough direct blocks to fill all direct rows of root indirect
 *              block, fill all direct blocks in the row of indirect
 *              blocks that are 2 levels deep and start first direct block
 *              in 3rd level of indirect blocks
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, March 27, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_start_3rd_recursive_indirect(hid_t fapl, H5HF_create_t *cparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    haddr_t     fh_addr;                /* Address of fractal heap created */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned char obj[10];              /* Buffer for object to insert */
    unsigned char robj[10];             /* Buffer for reading object */
    unsigned char heap_id[HEAP_ID_LEN]; /* Heap ID for object inserted */
    hsize_t     free_space;             /* Size of free space in heap */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    unsigned    tot_nobjs = 0;          /* Total number of objects inserted */
    size_t      block_size;             /* Size of block added */
    size_t      max_block_size;         /* Max. size of block to add */
    hsize_t     heap_size;              /* Total size of heap */
    unsigned    nrows;                  /* Number of rows inserted */
    unsigned    u, v, w;                /* Local index variables */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(H5HF_create(f, dxpl, cparam, &fh_addr/*out*/, &id_len/*out*/) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in first recursive indirect block
     */
    TESTING("inserting objects to start recursive indirect blocks three levels deep");

    /* Loop over direct block rows in root indirect block */
    block_size = cparam->managed.start_block_size;
    heap_size = 0;
    nrows = 0;
    while(block_size <= cparam->managed.max_direct_size) {
        /* Loop over filling direct blocks for a row */
        for(u = 0; u < cparam->managed.width; u++) {
            /* Fill a direct heap block up */
            heap_size += block_size;
            if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                FAIL_STACK_ERROR
            tot_nobjs += nobjs;
        } /* end for */

        /* Adjust block size for row */
        if(nrows > 0)
            block_size *= 2;

        /* Increment row count */
        nrows++;
    } /* end for */

    /* Loop over rows of 2nd level deep indirect blocks */
    for(w = 0; w < (H5V_log2_of2(cparam->managed.width) + 1); w++) {
        for(v = 0; v < cparam->managed.width; v++) {
            /* Loop over direct block rows in first recursive indirect block */
            block_size = cparam->managed.start_block_size;
            nrows = 0;
            max_block_size = block_size * (1 << ((H5V_log2_of2(cparam->managed.max_direct_size / cparam->managed.width) -
                    (H5V_log2_of2(cparam->managed.start_block_size) +
                        H5V_log2_of2(cparam->managed.width))) + (w + 1) + 1));
            while(block_size <= max_block_size) {
                /* Loop over filling direct blocks for a row */
                for(u = 0; u < cparam->managed.width; u++) {
                    /* Fill a direct heap block up */
                    heap_size += block_size;
                    if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                        FAIL_STACK_ERROR
                    tot_nobjs += nobjs;
                } /* end for */

                /* Adjust block size for row */
                if(nrows > 0)
                    block_size *= 2;

                /* Increment row count */
                nrows++;
            } /* end for */
        } /* end for */
    } /* end for */

    /* Insert one more object, to force creation of third level deep
     * recursive indirect block
     */

    /* Initialize object buffer */
    for(u = 0; u < sizeof(obj); u++)
        obj[u] = u + tot_nobjs;

    /* Insert object */
    HDmemset(heap_id, 0, sizeof(heap_id));
    if(H5HF_insert(f, dxpl, fh_addr, sizeof(obj), obj, heap_id) < 0)
        FAIL_STACK_ERROR

    /* Increment object count */
    tot_nobjs++;

    heap_size += cparam->managed.start_block_size;
    free_space = cparam->managed.start_block_size - ((sizeof(obj) + OBJ_PREFIX_LEN) + DBLOCK_OVERHEAD + (cparam->managed.max_index / 8));
    if(check_stats(f, H5P_DATASET_XFER_DEFAULT, fh_addr, heap_size, heap_size, (hsize_t)0, free_space, (hsize_t)tot_nobjs))
        FAIL_STACK_ERROR

    /* Read in object */
    if(H5HF_read(f, dxpl, fh_addr, heap_id, robj) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(obj, robj, sizeof(obj)))
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
} /* test_abs_start_3rd_recursive_indirect() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_fill_first_3rd_recursive_indirect
 *
 * Purpose:	Test inserting mult. objects into absolute heap, creating
 *              enough direct blocks to fill all direct rows of root indirect
 *              block, fill all direct blocks in the row of indirect
 *              blocks that are 2 levels deep and fill first indirect block
 *              in 3rd level of indirect blocks
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, March 27, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_fill_first_3rd_recursive_indirect(hid_t fapl, H5HF_create_t *cparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    haddr_t     fh_addr;                /* Address of fractal heap created */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    unsigned    tot_nobjs = 0;          /* Total number of objects inserted */
    size_t      block_size;             /* Size of block added */
    size_t      max_block_size;         /* Max. size of block to add */
    hsize_t     heap_size;              /* Total size of heap */
    unsigned    nrows;                  /* Number of rows inserted */
    unsigned    u, v, w;                /* Local index variables */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(H5HF_create(f, dxpl, cparam, &fh_addr/*out*/, &id_len/*out*/) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in first recursive indirect block
     */
    TESTING("inserting objects to fill first indirect block of recursive indirect blocks three levels deep");

    /* Loop over direct block rows in root indirect block */
    block_size = cparam->managed.start_block_size;
    heap_size = 0;
    nrows = 0;
    while(block_size <= cparam->managed.max_direct_size) {
        /* Loop over filling direct blocks for a row */
        for(u = 0; u < cparam->managed.width; u++) {
            /* Fill a direct heap block up */
            heap_size += block_size;
            if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                FAIL_STACK_ERROR
            tot_nobjs += nobjs;
        } /* end for */

        /* Adjust block size for row */
        if(nrows > 0)
            block_size *= 2;

        /* Increment row count */
        nrows++;
    } /* end for */

    /* Loop over rows of 2nd level deep indirect blocks */
    for(w = 0; w < (H5V_log2_of2(cparam->managed.width) + 1); w++) {
        for(v = 0; v < cparam->managed.width; v++) {
            /* Loop over direct block rows in first recursive indirect block */
            block_size = cparam->managed.start_block_size;
            nrows = 0;
            max_block_size = block_size * (1 << ((H5V_log2_of2(cparam->managed.max_direct_size / cparam->managed.width) -
                    (H5V_log2_of2(cparam->managed.start_block_size) +
                        H5V_log2_of2(cparam->managed.width))) + (w + 1) + 1));
            while(block_size <= max_block_size) {
                /* Loop over filling direct blocks for a row */
                for(u = 0; u < cparam->managed.width; u++) {
                    /* Fill a direct heap block up */
                    heap_size += block_size;
                    if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                        FAIL_STACK_ERROR
                    tot_nobjs += nobjs;
                } /* end for */

                /* Adjust block size for row */
                if(nrows > 0)
                    block_size *= 2;

                /* Increment row count */
                nrows++;
            } /* end for */
        } /* end for */
    } /* end for */

    /* Loop over direct block rows in third level indirect block */
    block_size = cparam->managed.start_block_size;
    nrows = 0;
    while(block_size <= cparam->managed.max_direct_size) {
        /* Loop over filling direct blocks for a row */
        for(u = 0; u < cparam->managed.width; u++) {
            /* Fill a direct heap block up */
            heap_size += block_size;
            if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                FAIL_STACK_ERROR
            tot_nobjs += nobjs;
        } /* end for */

        /* Adjust block size for row */
        if(nrows > 0)
            block_size *= 2;

        /* Increment row count */
        nrows++;
    } /* end for */

    /* Loop over row of indirect blocks */
    for(v = 0; v < cparam->managed.width; v++) {
        /* Loop over direct block rows in first recursive indirect block */
        block_size = cparam->managed.start_block_size;
        nrows = 0;
        max_block_size = block_size * (1 << ((H5V_log2_of2((2 * cparam->managed.max_direct_size) / cparam->managed.width) -
                (H5V_log2_of2(cparam->managed.start_block_size) +
                    H5V_log2_of2(cparam->managed.width))) + 1));
        while(block_size <= max_block_size) {
            /* Loop over filling direct blocks for a row */
            for(u = 0; u < cparam->managed.width; u++) {
                /* Fill a direct heap block up */
                heap_size += block_size;
                if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                    FAIL_STACK_ERROR
                tot_nobjs += nobjs;
            } /* end for */

            /* Adjust block size for row */
            if(nrows > 0)
                block_size *= 2;

            /* Increment row count */
            nrows++;
        } /* end for */
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
} /* test_abs_fill_first_3rd_recursive_indirect() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_fill_3rd_recursive_indirect_row
 *
 * Purpose:	Test inserting mult. objects into absolute heap, creating
 *              enough direct blocks to fill all direct rows of root indirect
 *              block, fill all direct blocks in the row of indirect
 *              blocks that are 2 levels deep and fill all indirect blocks
 *              first row of 3rd level of indirect blocks
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, March 27, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_fill_3rd_recursive_indirect_row(hid_t fapl, H5HF_create_t *cparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    haddr_t     fh_addr;                /* Address of fractal heap created */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    unsigned    tot_nobjs = 0;          /* Total number of objects inserted */
    size_t      block_size;             /* Size of block added */
    size_t      max_block_size;         /* Max. size of block to add */
    hsize_t     heap_size;              /* Total size of heap */
    unsigned    nrows;                  /* Number of rows inserted */
    unsigned    u, v, w;                /* Local index variables */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(H5HF_create(f, dxpl, cparam, &fh_addr/*out*/, &id_len/*out*/) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in first recursive indirect block
     */
    TESTING("inserting objects to fill row of indirect blocks in recursive indirect blocks three levels deep");

    /* Loop over direct block rows in root indirect block */
    block_size = cparam->managed.start_block_size;
    heap_size = 0;
    nrows = 0;
    while(block_size <= cparam->managed.max_direct_size) {
        /* Loop over filling direct blocks for a row */
        for(u = 0; u < cparam->managed.width; u++) {
            /* Fill a direct heap block up */
            heap_size += block_size;
            if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                FAIL_STACK_ERROR
            tot_nobjs += nobjs;
        } /* end for */

        /* Adjust block size for row */
        if(nrows > 0)
            block_size *= 2;

        /* Increment row count */
        nrows++;
    } /* end for */

    /* Loop over rows of 2nd level deep indirect blocks */
    for(w = 0; w < (H5V_log2_of2(cparam->managed.width) + 1); w++) {
        for(v = 0; v < cparam->managed.width; v++) {
            /* Loop over direct block rows in first recursive indirect block */
            block_size = cparam->managed.start_block_size;
            nrows = 0;
            max_block_size = block_size * (1 << ((H5V_log2_of2(cparam->managed.max_direct_size / cparam->managed.width) -
                    (H5V_log2_of2(cparam->managed.start_block_size) +
                        H5V_log2_of2(cparam->managed.width))) + (w + 1) + 1));
            while(block_size <= max_block_size) {
                /* Loop over filling direct blocks for a row */
                for(u = 0; u < cparam->managed.width; u++) {
                    /* Fill a direct heap block up */
                    heap_size += block_size;
                    if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                        FAIL_STACK_ERROR
                    tot_nobjs += nobjs;
                } /* end for */

                /* Adjust block size for row */
                if(nrows > 0)
                    block_size *= 2;

                /* Increment row count */
                nrows++;
            } /* end for */
        } /* end for */
    } /* end for */

    /* Loop over row of 3rd level deep indirect blocks */
    for(w = 0; w < cparam->managed.width; w++) {
        /* Loop over direct block rows in third level indirect block */
        block_size = cparam->managed.start_block_size;
        nrows = 0;
        while(block_size <= cparam->managed.max_direct_size) {
            /* Loop over filling direct blocks for a row */
            for(u = 0; u < cparam->managed.width; u++) {
                /* Fill a direct heap block up */
                heap_size += block_size;
                if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                    FAIL_STACK_ERROR
                tot_nobjs += nobjs;
            } /* end for */

            /* Adjust block size for row */
            if(nrows > 0)
                block_size *= 2;

            /* Increment row count */
            nrows++;
        } /* end for */

        /* Loop over row of indirect blocks */
        for(v = 0; v < cparam->managed.width; v++) {
            /* Loop over direct block rows in first recursive indirect block */
            block_size = cparam->managed.start_block_size;
            nrows = 0;
            max_block_size = block_size * (1 << ((H5V_log2_of2((2 * cparam->managed.max_direct_size) / cparam->managed.width) -
                    (H5V_log2_of2(cparam->managed.start_block_size) +
                        H5V_log2_of2(cparam->managed.width))) + 1));
            while(block_size <= max_block_size) {
                /* Loop over filling direct blocks for a row */
                for(u = 0; u < cparam->managed.width; u++) {
                    /* Fill a direct heap block up */
                    heap_size += block_size;
                    if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                        FAIL_STACK_ERROR
                    tot_nobjs += nobjs;
                } /* end for */

                /* Adjust block size for row */
                if(nrows > 0)
                    block_size *= 2;

                /* Increment row count */
                nrows++;
            } /* end for */
        } /* end for */
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
} /* test_abs_fill_3rd_recursive_indirect_row() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_fill_all_3rd_recursive_indirect
 *
 * Purpose:	Test inserting mult. objects into absolute heap, creating
 *              enough direct blocks to fill all direct rows of root indirect
 *              block, fill all direct blocks in the row of indirect
 *              blocks that are 2 levels deep and fill all indirect blocks
 *              that are three levels deep
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, March 27, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_fill_all_3rd_recursive_indirect(hid_t fapl, H5HF_create_t *cparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    haddr_t     fh_addr;                /* Address of fractal heap created */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    unsigned    tot_nobjs = 0;          /* Total number of objects inserted */
    size_t      block_size;             /* Size of block added */
    size_t      max_block_size;         /* Max. size of block to add */
    hsize_t     heap_size;              /* Total size of heap */
    unsigned    nrows;                  /* Number of rows inserted */
    unsigned    u, v, w, x, y;          /* Local index variables */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(H5HF_create(f, dxpl, cparam, &fh_addr/*out*/, &id_len/*out*/) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in first recursive indirect block
     */
    TESTING("inserting objects to fill row of indirect blocks in recursive indirect blocks three levels deep");

    /* Loop over direct block rows in root indirect block */
    block_size = cparam->managed.start_block_size;
    heap_size = 0;
    nrows = 0;
    while(block_size <= cparam->managed.max_direct_size) {
        /* Loop over filling direct blocks for a row */
        for(u = 0; u < cparam->managed.width; u++) {
            /* Fill a direct heap block up */
            heap_size += block_size;
            if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                FAIL_STACK_ERROR
            tot_nobjs += nobjs;
        } /* end for */

        /* Adjust block size for row */
        if(nrows > 0)
            block_size *= 2;

        /* Increment row count */
        nrows++;
    } /* end for */

    /* Loop over rows of 2nd level deep indirect blocks */
    for(w = 0; w < (H5V_log2_of2(cparam->managed.width) + 1); w++) {
        for(v = 0; v < cparam->managed.width; v++) {
            /* Loop over direct block rows in first recursive indirect block */
            block_size = cparam->managed.start_block_size;
            nrows = 0;
            max_block_size = block_size * (1 << ((H5V_log2_of2(cparam->managed.max_direct_size / cparam->managed.width) -
                    (H5V_log2_of2(cparam->managed.start_block_size) +
                        H5V_log2_of2(cparam->managed.width))) + (w + 1) + 1));
            while(block_size <= max_block_size) {
                /* Loop over filling direct blocks for a row */
                for(u = 0; u < cparam->managed.width; u++) {
                    /* Fill a direct heap block up */
                    heap_size += block_size;
                    if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                        FAIL_STACK_ERROR
                    tot_nobjs += nobjs;
                } /* end for */

                /* Adjust block size for row */
                if(nrows > 0)
                    block_size *= 2;

                /* Increment row count */
                nrows++;
            } /* end for */
        } /* end for */
    } /* end for */

    /* Loop over rows of 3rd level deep indirect blocks */
    for(y = 0; y < (H5V_log2_of2(cparam->managed.width) + 1); y++) {

        /* Loop over row of 3rd level deep indirect blocks */
        for(x = 0; x < cparam->managed.width; x++) {

            /* Loop over direct block rows in third level indirect block */
            block_size = cparam->managed.start_block_size;
            nrows = 0;
            while(block_size <= cparam->managed.max_direct_size) {
                /* Loop over filling direct blocks for a row */
                for(u = 0; u < cparam->managed.width; u++) {
                    /* Fill a direct heap block up */
                    heap_size += block_size;
                    if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                        FAIL_STACK_ERROR
                    tot_nobjs += nobjs;
                } /* end for */

                /* Adjust block size for row */
                if(nrows > 0)
                    block_size *= 2;

                /* Increment row count */
                nrows++;
            } /* end for */

            /* Loop over rows of 2nd level deep indirect blocks */
            for(w = 0; w < (y + 1); w++) {

                /* Loop over row of indirect blocks */
                for(v = 0; v < cparam->managed.width; v++) {
                    /* Loop over direct block rows in first recursive indirect block */
                    block_size = cparam->managed.start_block_size;
                    nrows = 0;
                    max_block_size = block_size * (1 << ((H5V_log2_of2(cparam->managed.max_direct_size / cparam->managed.width) -
                            (H5V_log2_of2(cparam->managed.start_block_size) +
                                H5V_log2_of2(cparam->managed.width))) + (w + 1) + 1));
                    while(block_size <= max_block_size) {
                        /* Loop over filling direct blocks for a row */
                        for(u = 0; u < cparam->managed.width; u++) {
                            /* Fill a direct heap block up */
                            heap_size += block_size;
                            if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                                FAIL_STACK_ERROR
                            tot_nobjs += nobjs;
                        } /* end for */

                        /* Adjust block size for row */
                        if(nrows > 0)
                            block_size *= 2;

                        /* Increment row count */
                        nrows++;
                    } /* end for */
                } /* end for */
            } /* end for */
        } /* end for */
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
} /* test_abs_fill_all_3rd_recursive_indirect() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_start_4th_recursive_indirect
 *
 * Purpose:	Test inserting mult. objects into absolute heap, creating
 *              enough direct blocks to fill all direct rows of root indirect
 *              block, fill all direct blocks in the row of indirect
 *              blocks that are 2 levels deep, fill all indirect blocks
 *              that are three levels deep and start first direct block that
 *              is four levels deep
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, March 27, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_start_4th_recursive_indirect(hid_t fapl, H5HF_create_t *cparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    haddr_t     fh_addr;                /* Address of fractal heap created */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned char obj[10];              /* Buffer for object to insert */
    unsigned char robj[10];             /* Buffer for reading object */
    unsigned char heap_id[HEAP_ID_LEN]; /* Heap ID for object inserted */
    hsize_t     free_space;             /* Size of free space in heap */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    unsigned    tot_nobjs = 0;          /* Total number of objects inserted */
    size_t      block_size;             /* Size of block added */
    size_t      max_block_size;         /* Max. size of block to add */
    hsize_t     heap_size;              /* Total size of heap */
    unsigned    nrows;                  /* Number of rows inserted */
    unsigned    u, v, w, x, y;          /* Local index variables */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(H5HF_create(f, dxpl, cparam, &fh_addr/*out*/, &id_len/*out*/) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in first recursive indirect block
     */
    TESTING("inserting objects to start first direct block in recursive indirect blocks four levels deep");

    /* Loop over direct block rows in root indirect block */
    block_size = cparam->managed.start_block_size;
    heap_size = 0;
    nrows = 0;
    while(block_size <= cparam->managed.max_direct_size) {
        /* Loop over filling direct blocks for a row */
        for(u = 0; u < cparam->managed.width; u++) {
            /* Fill a direct heap block up */
            heap_size += block_size;
            if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                FAIL_STACK_ERROR
            tot_nobjs += nobjs;
        } /* end for */

        /* Adjust block size for row */
        if(nrows > 0)
            block_size *= 2;

        /* Increment row count */
        nrows++;
    } /* end for */

    /* Loop over rows of 2nd level deep indirect blocks */
    for(w = 0; w < (H5V_log2_of2(cparam->managed.width) + 1); w++) {
        for(v = 0; v < cparam->managed.width; v++) {
            /* Loop over direct block rows in first recursive indirect block */
            block_size = cparam->managed.start_block_size;
            nrows = 0;
            max_block_size = block_size * (1 << ((H5V_log2_of2(cparam->managed.max_direct_size / cparam->managed.width) -
                    (H5V_log2_of2(cparam->managed.start_block_size) +
                        H5V_log2_of2(cparam->managed.width))) + (w + 1) + 1));
            while(block_size <= max_block_size) {
                /* Loop over filling direct blocks for a row */
                for(u = 0; u < cparam->managed.width; u++) {
                    /* Fill a direct heap block up */
                    heap_size += block_size;
                    if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                        FAIL_STACK_ERROR
                    tot_nobjs += nobjs;
                } /* end for */

                /* Adjust block size for row */
                if(nrows > 0)
                    block_size *= 2;

                /* Increment row count */
                nrows++;
            } /* end for */
        } /* end for */
    } /* end for */

    /* Loop over rows of 3rd level deep indirect blocks */
    for(y = 0; y < (H5V_log2_of2(cparam->managed.width) + 1); y++) {

        /* Loop over row of 3rd level deep indirect blocks */
        for(x = 0; x < cparam->managed.width; x++) {

            /* Loop over direct block rows in third level indirect block */
            block_size = cparam->managed.start_block_size;
            nrows = 0;
            while(block_size <= cparam->managed.max_direct_size) {
                /* Loop over filling direct blocks for a row */
                for(u = 0; u < cparam->managed.width; u++) {
                    /* Fill a direct heap block up */
                    heap_size += block_size;
                    if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                        FAIL_STACK_ERROR
                    tot_nobjs += nobjs;
                } /* end for */

                /* Adjust block size for row */
                if(nrows > 0)
                    block_size *= 2;

                /* Increment row count */
                nrows++;
            } /* end for */

            /* Loop over rows of 2nd level deep indirect blocks */
            for(w = 0; w < (y + 1); w++) {

                /* Loop over row of indirect blocks */
                for(v = 0; v < cparam->managed.width; v++) {
                    /* Loop over direct block rows in first recursive indirect block */
                    block_size = cparam->managed.start_block_size;
                    nrows = 0;
                    max_block_size = block_size * (1 << ((H5V_log2_of2(cparam->managed.max_direct_size / cparam->managed.width) -
                            (H5V_log2_of2(cparam->managed.start_block_size) +
                                H5V_log2_of2(cparam->managed.width))) + (w + 1) + 1));
                    while(block_size <= max_block_size) {
                        /* Loop over filling direct blocks for a row */
                        for(u = 0; u < cparam->managed.width; u++) {
                            /* Fill a direct heap block up */
                            heap_size += block_size;
                            if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                                FAIL_STACK_ERROR
                            tot_nobjs += nobjs;
                        } /* end for */

                        /* Adjust block size for row */
                        if(nrows > 0)
                            block_size *= 2;

                        /* Increment row count */
                        nrows++;
                    } /* end for */
                } /* end for */
            } /* end for */
        } /* end for */
    } /* end for */

    /* Insert one more object, to force creation of four level deep
     * recursive indirect block
     */

    /* Initialize object buffer */
    for(u = 0; u < sizeof(obj); u++)
        obj[u] = u + tot_nobjs;

    /* Insert object */
    HDmemset(heap_id, 0, sizeof(heap_id));
    if(H5HF_insert(f, dxpl, fh_addr, sizeof(obj), obj, heap_id) < 0)
        FAIL_STACK_ERROR

    /* Increment object count */
    tot_nobjs++;

    heap_size += cparam->managed.start_block_size;
    free_space = cparam->managed.start_block_size - ((sizeof(obj) + OBJ_PREFIX_LEN) + DBLOCK_OVERHEAD + (cparam->managed.max_index / 8));
    if(check_stats(f, H5P_DATASET_XFER_DEFAULT, fh_addr, heap_size, heap_size, (hsize_t)0, free_space, (hsize_t)tot_nobjs))
        FAIL_STACK_ERROR

    /* Read in object */
    if(H5HF_read(f, dxpl, fh_addr, heap_id, robj) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(obj, robj, sizeof(obj)))
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
} /* test_abs_start_4th_recursive_indirect() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_fill_first_4th_recursive_indirect
 *
 * Purpose:	Test inserting mult. objects into absolute heap, creating
 *              enough direct blocks to fill all direct rows of root indirect
 *              block, fill all direct blocks in the row of indirect
 *              blocks that are 2 levels deep, fill all indirect blocks
 *              that are three levels deep and fill the first (3rd level)
 *              indirect block that is four levels deep
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, March 27, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_fill_first_4th_recursive_indirect(hid_t fapl, H5HF_create_t *cparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    haddr_t     fh_addr;                /* Address of fractal heap created */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    unsigned    tot_nobjs = 0;          /* Total number of objects inserted */
    size_t      block_size;             /* Size of block added */
    size_t      max_block_size;         /* Max. size of block to add */
    hsize_t     heap_size;              /* Total size of heap */
    unsigned    nrows;                  /* Number of rows inserted */
    unsigned    u, v, w, x, y;          /* Local index variables */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(H5HF_create(f, dxpl, cparam, &fh_addr/*out*/, &id_len/*out*/) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in first recursive indirect block
     */
    TESTING("inserting objects to fill first (3rd level) indirect block in recursive indirect block four levels deep");

    /* Loop over direct block rows in root indirect block */
    block_size = cparam->managed.start_block_size;
    heap_size = 0;
    nrows = 0;
    while(block_size <= cparam->managed.max_direct_size) {
        /* Loop over filling direct blocks for a row */
        for(u = 0; u < cparam->managed.width; u++) {
            /* Fill a direct heap block up */
            heap_size += block_size;
            if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                FAIL_STACK_ERROR
            tot_nobjs += nobjs;
        } /* end for */

        /* Adjust block size for row */
        if(nrows > 0)
            block_size *= 2;

        /* Increment row count */
        nrows++;
    } /* end for */

    /* Loop over rows of 2nd level deep indirect blocks */
    for(w = 0; w < (H5V_log2_of2(cparam->managed.width) + 1); w++) {
        for(v = 0; v < cparam->managed.width; v++) {
            /* Loop over direct block rows in first recursive indirect block */
            block_size = cparam->managed.start_block_size;
            nrows = 0;
            max_block_size = block_size * (1 << ((H5V_log2_of2(cparam->managed.max_direct_size / cparam->managed.width) -
                    (H5V_log2_of2(cparam->managed.start_block_size) +
                        H5V_log2_of2(cparam->managed.width))) + (w + 1) + 1));
            while(block_size <= max_block_size) {
                /* Loop over filling direct blocks for a row */
                for(u = 0; u < cparam->managed.width; u++) {
                    /* Fill a direct heap block up */
                    heap_size += block_size;
                    if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                        FAIL_STACK_ERROR
                    tot_nobjs += nobjs;
                } /* end for */

                /* Adjust block size for row */
                if(nrows > 0)
                    block_size *= 2;

                /* Increment row count */
                nrows++;
            } /* end for */
        } /* end for */
    } /* end for */

    /* Loop over rows of 3rd level deep indirect blocks */
    for(y = 0; y < (H5V_log2_of2(cparam->managed.width) + 1); y++) {

        /* Loop over row of 3rd level deep indirect blocks */
        for(x = 0; x < cparam->managed.width; x++) {

            /* Loop over direct block rows in third level indirect block */
            block_size = cparam->managed.start_block_size;
            nrows = 0;
            while(block_size <= cparam->managed.max_direct_size) {
                /* Loop over filling direct blocks for a row */
                for(u = 0; u < cparam->managed.width; u++) {
                    /* Fill a direct heap block up */
                    heap_size += block_size;
                    if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                        FAIL_STACK_ERROR
                    tot_nobjs += nobjs;
                } /* end for */

                /* Adjust block size for row */
                if(nrows > 0)
                    block_size *= 2;

                /* Increment row count */
                nrows++;
            } /* end for */

            /* Loop over rows of 2nd level deep indirect blocks */
            for(w = 0; w < (y + 1); w++) {

                /* Loop over row of indirect blocks */
                for(v = 0; v < cparam->managed.width; v++) {
                    /* Loop over direct block rows in first recursive indirect block */
                    block_size = cparam->managed.start_block_size;
                    nrows = 0;
                    max_block_size = block_size * (1 << ((H5V_log2_of2(cparam->managed.max_direct_size / cparam->managed.width) -
                            (H5V_log2_of2(cparam->managed.start_block_size) +
                                H5V_log2_of2(cparam->managed.width))) + (w + 1) + 1));
                    while(block_size <= max_block_size) {
                        /* Loop over filling direct blocks for a row */
                        for(u = 0; u < cparam->managed.width; u++) {
                            /* Fill a direct heap block up */
                            heap_size += block_size;
                            if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                                FAIL_STACK_ERROR
                            tot_nobjs += nobjs;
                        } /* end for */

                        /* Adjust block size for row */
                        if(nrows > 0)
                            block_size *= 2;

                        /* Increment row count */
                        nrows++;
                    } /* end for */
                } /* end for */
            } /* end for */
        } /* end for */
    } /* end for */

    /* Loop over direct block rows in fourth level indirect block */
    block_size = cparam->managed.start_block_size;
    nrows = 0;
    while(block_size <= cparam->managed.max_direct_size) {
        /* Loop over filling direct blocks for a row */
        for(u = 0; u < cparam->managed.width; u++) {
            /* Fill a direct heap block up */
            heap_size += block_size;
            if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                FAIL_STACK_ERROR
            tot_nobjs += nobjs;
        } /* end for */

        /* Adjust block size for row */
        if(nrows > 0)
            block_size *= 2;

        /* Increment row count */
        nrows++;
    } /* end for */

    /* Loop over rows of 2nd level deep indirect blocks */
    for(w = 0; w < (H5V_log2_of2(cparam->managed.width) + 1); w++) {

        /* Loop over row of indirect blocks */
        for(v = 0; v < cparam->managed.width; v++) {
            /* Loop over direct block rows in first recursive indirect block */
            block_size = cparam->managed.start_block_size;
            nrows = 0;
            max_block_size = block_size * (1 << ((H5V_log2_of2(cparam->managed.max_direct_size / cparam->managed.width) -
                    (H5V_log2_of2(cparam->managed.start_block_size) +
                        H5V_log2_of2(cparam->managed.width))) + (w + 1) + 1));
            while(block_size <= max_block_size) {
                /* Loop over filling direct blocks for a row */
                for(u = 0; u < cparam->managed.width; u++) {
                    /* Fill a direct heap block up */
                    heap_size += block_size;
                    if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                        FAIL_STACK_ERROR
                    tot_nobjs += nobjs;
                } /* end for */

                /* Adjust block size for row */
                if(nrows > 0)
                    block_size *= 2;

                /* Increment row count */
                nrows++;
            } /* end for */
        } /* end for */
    } /* end for */

    /* Loop over row of 3rd level deep indirect blocks */
    for(x = 0; x < cparam->managed.width; x++) {

        /* Loop over direct block rows in third level indirect block */
        block_size = cparam->managed.start_block_size;
        nrows = 0;
        while(block_size <= cparam->managed.max_direct_size) {
            /* Loop over filling direct blocks for a row */
            for(u = 0; u < cparam->managed.width; u++) {
                /* Fill a direct heap block up */
                heap_size += block_size;
                if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                    FAIL_STACK_ERROR
                tot_nobjs += nobjs;
            } /* end for */

            /* Adjust block size for row */
            if(nrows > 0)
                block_size *= 2;

            /* Increment row count */
            nrows++;
        } /* end for */

        /* Loop over rows of 2nd level deep indirect blocks */
        for(w = 0; w < 1; w++) {

            /* Loop over row of indirect blocks */
            for(v = 0; v < cparam->managed.width; v++) {
                /* Loop over direct block rows in first recursive indirect block */
                block_size = cparam->managed.start_block_size;
                nrows = 0;
                max_block_size = block_size * (1 << ((H5V_log2_of2(cparam->managed.max_direct_size / cparam->managed.width) -
                        (H5V_log2_of2(cparam->managed.start_block_size) +
                            H5V_log2_of2(cparam->managed.width))) + (w + 1) + 1));
                while(block_size <= max_block_size) {
                    /* Loop over filling direct blocks for a row */
                    for(u = 0; u < cparam->managed.width; u++) {
                        /* Fill a direct heap block up */
                        heap_size += block_size;
                        if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                            FAIL_STACK_ERROR
                        tot_nobjs += nobjs;
                    } /* end for */

                    /* Adjust block size for row */
                    if(nrows > 0)
                        block_size *= 2;

                    /* Increment row count */
                    nrows++;
                } /* end for */
            } /* end for */
        } /* end for */
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
} /* test_abs_fill_first_4th_recursive_indirect() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_fill_4th_recursive_indirect_row
 *
 * Purpose:	Test inserting mult. objects into absolute heap, creating
 *              enough direct blocks to fill all direct rows of root indirect
 *              block, fill all direct blocks in the row of indirect
 *              blocks that are 2 levels deep, fill all indirect blocks
 *              that are three levels deep and fill the first row of
 *              indirect block that is four levels deep
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, March 27, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_fill_4th_recursive_indirect_row(hid_t fapl, H5HF_create_t *cparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    haddr_t     fh_addr;                /* Address of fractal heap created */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    unsigned    tot_nobjs = 0;          /* Total number of objects inserted */
    size_t      block_size;             /* Size of block added */
    size_t      max_block_size;         /* Max. size of block to add */
    hsize_t     heap_size;              /* Total size of heap */
    unsigned    nrows;                  /* Number of rows inserted */
    unsigned    u, v, w, x, y, z;       /* Local index variables */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(H5HF_create(f, dxpl, cparam, &fh_addr/*out*/, &id_len/*out*/) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in first recursive indirect block
     */
    TESTING("inserting objects to fill first row of recursive indirect blocks four levels deep");

    /* Loop over direct block rows in root indirect block */
    block_size = cparam->managed.start_block_size;
    heap_size = 0;
    nrows = 0;
    while(block_size <= cparam->managed.max_direct_size) {
        /* Loop over filling direct blocks for a row */
        for(u = 0; u < cparam->managed.width; u++) {
            /* Fill a direct heap block up */
            heap_size += block_size;
            if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                FAIL_STACK_ERROR
            tot_nobjs += nobjs;
        } /* end for */

        /* Adjust block size for row */
        if(nrows > 0)
            block_size *= 2;

        /* Increment row count */
        nrows++;
    } /* end for */

    /* Loop over rows of 2nd level deep indirect blocks */
    for(w = 0; w < (H5V_log2_of2(cparam->managed.width) + 1); w++) {
        for(v = 0; v < cparam->managed.width; v++) {
            /* Loop over direct block rows in first recursive indirect block */
            block_size = cparam->managed.start_block_size;
            nrows = 0;
            max_block_size = block_size * (1 << ((H5V_log2_of2(cparam->managed.max_direct_size / cparam->managed.width) -
                    (H5V_log2_of2(cparam->managed.start_block_size) +
                        H5V_log2_of2(cparam->managed.width))) + (w + 1) + 1));
            while(block_size <= max_block_size) {
                /* Loop over filling direct blocks for a row */
                for(u = 0; u < cparam->managed.width; u++) {
                    /* Fill a direct heap block up */
                    heap_size += block_size;
                    if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                        FAIL_STACK_ERROR
                    tot_nobjs += nobjs;
                } /* end for */

                /* Adjust block size for row */
                if(nrows > 0)
                    block_size *= 2;

                /* Increment row count */
                nrows++;
            } /* end for */
        } /* end for */
    } /* end for */

    /* Loop over rows of 3rd level deep indirect blocks */
    for(y = 0; y < (H5V_log2_of2(cparam->managed.width) + 1); y++) {

        /* Loop over row of 3rd level deep indirect blocks */
        for(x = 0; x < cparam->managed.width; x++) {

            /* Loop over direct block rows in third level indirect block */
            block_size = cparam->managed.start_block_size;
            nrows = 0;
            while(block_size <= cparam->managed.max_direct_size) {
                /* Loop over filling direct blocks for a row */
                for(u = 0; u < cparam->managed.width; u++) {
                    /* Fill a direct heap block up */
                    heap_size += block_size;
                    if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                        FAIL_STACK_ERROR
                    tot_nobjs += nobjs;
                } /* end for */

                /* Adjust block size for row */
                if(nrows > 0)
                    block_size *= 2;

                /* Increment row count */
                nrows++;
            } /* end for */

            /* Loop over rows of 2nd level deep indirect blocks */
            for(w = 0; w < (y + 1); w++) {

                /* Loop over row of indirect blocks */
                for(v = 0; v < cparam->managed.width; v++) {
                    /* Loop over direct block rows in first recursive indirect block */
                    block_size = cparam->managed.start_block_size;
                    nrows = 0;
                    max_block_size = block_size * (1 << ((H5V_log2_of2(cparam->managed.max_direct_size / cparam->managed.width) -
                            (H5V_log2_of2(cparam->managed.start_block_size) +
                                H5V_log2_of2(cparam->managed.width))) + (w + 1) + 1));
                    while(block_size <= max_block_size) {
                        /* Loop over filling direct blocks for a row */
                        for(u = 0; u < cparam->managed.width; u++) {
                            /* Fill a direct heap block up */
                            heap_size += block_size;
                            if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                                FAIL_STACK_ERROR
                            tot_nobjs += nobjs;
                        } /* end for */

                        /* Adjust block size for row */
                        if(nrows > 0)
                            block_size *= 2;

                        /* Increment row count */
                        nrows++;
                    } /* end for */
                } /* end for */
            } /* end for */
        } /* end for */
    } /* end for */

    /* Loop over row of 4th level indirect blocks */
    for(z = 0; z < cparam->managed.width; z++) {

        /* Loop over direct block rows in fourth level indirect block */
        block_size = cparam->managed.start_block_size;
        nrows = 0;
        while(block_size <= cparam->managed.max_direct_size) {
            /* Loop over filling direct blocks for a row */
            for(u = 0; u < cparam->managed.width; u++) {
                /* Fill a direct heap block up */
                heap_size += block_size;
                if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                    FAIL_STACK_ERROR
                tot_nobjs += nobjs;
            } /* end for */

            /* Adjust block size for row */
            if(nrows > 0)
                block_size *= 2;

            /* Increment row count */
            nrows++;
        } /* end for */

        /* Loop over rows of 2nd level deep indirect blocks */
        for(w = 0; w < (H5V_log2_of2(cparam->managed.width) + 1); w++) {

            /* Loop over row of indirect blocks */
            for(v = 0; v < cparam->managed.width; v++) {
                /* Loop over direct block rows in first recursive indirect block */
                block_size = cparam->managed.start_block_size;
                nrows = 0;
                max_block_size = block_size * (1 << ((H5V_log2_of2(cparam->managed.max_direct_size / cparam->managed.width) -
                        (H5V_log2_of2(cparam->managed.start_block_size) +
                            H5V_log2_of2(cparam->managed.width))) + (w + 1) + 1));
                while(block_size <= max_block_size) {
                    /* Loop over filling direct blocks for a row */
                    for(u = 0; u < cparam->managed.width; u++) {
                        /* Fill a direct heap block up */
                        heap_size += block_size;
                        if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                            FAIL_STACK_ERROR
                        tot_nobjs += nobjs;
                    } /* end for */

                    /* Adjust block size for row */
                    if(nrows > 0)
                        block_size *= 2;

                    /* Increment row count */
                    nrows++;
                } /* end for */
            } /* end for */
        } /* end for */

        /* Loop over rows of 3rd level deep indirect blocks */
        for(y = 0; y < 1; y++) {

            /* Loop over row of 3rd level deep indirect blocks */
            for(x = 0; x < cparam->managed.width; x++) {

                /* Loop over direct block rows in third level indirect block */
                block_size = cparam->managed.start_block_size;
                nrows = 0;
                while(block_size <= cparam->managed.max_direct_size) {
                    /* Loop over filling direct blocks for a row */
                    for(u = 0; u < cparam->managed.width; u++) {
                        /* Fill a direct heap block up */
                        heap_size += block_size;
                        if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                            FAIL_STACK_ERROR
                        tot_nobjs += nobjs;
                    } /* end for */

                    /* Adjust block size for row */
                    if(nrows > 0)
                        block_size *= 2;

                    /* Increment row count */
                    nrows++;
                } /* end for */

                /* Loop over rows of 2nd level deep indirect blocks */
                for(w = 0; w < (y + 1); w++) {

                    /* Loop over row of indirect blocks */
                    for(v = 0; v < cparam->managed.width; v++) {
                        /* Loop over direct block rows in first recursive indirect block */
                        block_size = cparam->managed.start_block_size;
                        nrows = 0;
                        max_block_size = block_size * (1 << ((H5V_log2_of2(cparam->managed.max_direct_size / cparam->managed.width) -
                                (H5V_log2_of2(cparam->managed.start_block_size) +
                                    H5V_log2_of2(cparam->managed.width))) + (w + 1) + 1));
                        while(block_size <= max_block_size) {
                            /* Loop over filling direct blocks for a row */
                            for(u = 0; u < cparam->managed.width; u++) {
                                /* Fill a direct heap block up */
                                heap_size += block_size;
                                if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                                    FAIL_STACK_ERROR
                                tot_nobjs += nobjs;
                            } /* end for */

                            /* Adjust block size for row */
                            if(nrows > 0)
                                block_size *= 2;

                            /* Increment row count */
                            nrows++;
                        } /* end for */
                    } /* end for */
                } /* end for */
            } /* end for */
        } /* end for */
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
} /* test_abs_fill_4th_recursive_indirect_row() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_fill_all_4th_recursive_indirect
 *
 * Purpose:	Test inserting mult. objects into absolute heap, creating
 *              enough direct blocks to fill all direct rows of root indirect
 *              block, fill all direct blocks in the row of indirect
 *              blocks that are 2 levels deep, fill all indirect blocks
 *              that are three levels deep and fill all rows of
 *              indirect blocks that are four levels deep
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, March 27, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_fill_all_4th_recursive_indirect(hid_t fapl, H5HF_create_t *cparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    haddr_t     fh_addr;                /* Address of fractal heap created */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    unsigned    tot_nobjs = 0;          /* Total number of objects inserted */
    size_t      block_size;             /* Size of block added */
    size_t      max_block_size;         /* Max. size of block to add */
    hsize_t     heap_size;              /* Total size of heap */
    unsigned    nrows;                  /* Number of rows inserted */
    unsigned    u, v, w, x, y, z, uu;   /* Local index variables */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(H5HF_create(f, dxpl, cparam, &fh_addr/*out*/, &id_len/*out*/) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in recursive indirect blocks four levels deep
     */
    TESTING("inserting objects to fill all rows of recursive indirect blocks four levels deep");

    /* Loop over direct block rows in root indirect block */
    block_size = cparam->managed.start_block_size;
    heap_size = 0;
    nrows = 0;
    while(block_size <= cparam->managed.max_direct_size) {
        /* Loop over filling direct blocks for a row */
        for(u = 0; u < cparam->managed.width; u++) {
            /* Fill a direct heap block up */
            heap_size += block_size;
            if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                FAIL_STACK_ERROR
            tot_nobjs += nobjs;
        } /* end for */

        /* Adjust block size for row */
        if(nrows > 0)
            block_size *= 2;

        /* Increment row count */
        nrows++;
    } /* end for */

    /* Loop over rows of 2nd level deep indirect blocks */
    for(w = 0; w < (H5V_log2_of2(cparam->managed.width) + 1); w++) {
        for(v = 0; v < cparam->managed.width; v++) {
            /* Loop over direct block rows in first recursive indirect block */
            block_size = cparam->managed.start_block_size;
            nrows = 0;
            max_block_size = block_size * (1 << ((H5V_log2_of2(cparam->managed.max_direct_size / cparam->managed.width) -
                    (H5V_log2_of2(cparam->managed.start_block_size) +
                        H5V_log2_of2(cparam->managed.width))) + (w + 1) + 1));
            while(block_size <= max_block_size) {
                /* Loop over filling direct blocks for a row */
                for(u = 0; u < cparam->managed.width; u++) {
                    /* Fill a direct heap block up */
                    heap_size += block_size;
                    if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                        FAIL_STACK_ERROR
                    tot_nobjs += nobjs;
                } /* end for */

                /* Adjust block size for row */
                if(nrows > 0)
                    block_size *= 2;

                /* Increment row count */
                nrows++;
            } /* end for */
        } /* end for */
    } /* end for */

    /* Loop over rows of 3rd level deep indirect blocks */
    for(y = 0; y < (H5V_log2_of2(cparam->managed.width) + 1); y++) {

        /* Loop over row of 3rd level deep indirect blocks */
        for(x = 0; x < cparam->managed.width; x++) {

            /* Loop over direct block rows in third level indirect block */
            block_size = cparam->managed.start_block_size;
            nrows = 0;
            while(block_size <= cparam->managed.max_direct_size) {
                /* Loop over filling direct blocks for a row */
                for(u = 0; u < cparam->managed.width; u++) {
                    /* Fill a direct heap block up */
                    heap_size += block_size;
                    if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                        FAIL_STACK_ERROR
                    tot_nobjs += nobjs;
                } /* end for */

                /* Adjust block size for row */
                if(nrows > 0)
                    block_size *= 2;

                /* Increment row count */
                nrows++;
            } /* end for */

            /* Loop over rows of 2nd level deep indirect blocks */
            for(w = 0; w < (y + 1); w++) {

                /* Loop over row of indirect blocks */
                for(v = 0; v < cparam->managed.width; v++) {
                    /* Loop over direct block rows in first recursive indirect block */
                    block_size = cparam->managed.start_block_size;
                    nrows = 0;
                    max_block_size = block_size * (1 << ((H5V_log2_of2(cparam->managed.max_direct_size / cparam->managed.width) -
                            (H5V_log2_of2(cparam->managed.start_block_size) +
                                H5V_log2_of2(cparam->managed.width))) + (w + 1) + 1));
                    while(block_size <= max_block_size) {
                        /* Loop over filling direct blocks for a row */
                        for(u = 0; u < cparam->managed.width; u++) {
                            /* Fill a direct heap block up */
                            heap_size += block_size;
                            if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                                FAIL_STACK_ERROR
                            tot_nobjs += nobjs;
                        } /* end for */

                        /* Adjust block size for row */
                        if(nrows > 0)
                            block_size *= 2;

                        /* Increment row count */
                        nrows++;
                    } /* end for */
                } /* end for */
            } /* end for */
        } /* end for */
    } /* end for */

    /* Loop over rows of 2nd level deep indirect blocks */
    for(uu = 0; uu < (H5V_log2_of2(cparam->managed.width) + 1); uu++) {

        /* Loop over row of 4th level indirect blocks */
        for(z = 0; z < cparam->managed.width; z++) {

            /* Loop over direct block rows in fourth level indirect block */
            block_size = cparam->managed.start_block_size;
            nrows = 0;
            while(block_size <= cparam->managed.max_direct_size) {
                /* Loop over filling direct blocks for a row */
                for(u = 0; u < cparam->managed.width; u++) {
                    /* Fill a direct heap block up */
                    heap_size += block_size;
                    if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                        FAIL_STACK_ERROR
                    tot_nobjs += nobjs;
                } /* end for */

                /* Adjust block size for row */
                if(nrows > 0)
                    block_size *= 2;

                /* Increment row count */
                nrows++;
            } /* end for */

            /* Loop over rows of 2nd level deep indirect blocks */
            for(w = 0; w < (H5V_log2_of2(cparam->managed.width) + 1); w++) {

                /* Loop over row of indirect blocks */
                for(v = 0; v < cparam->managed.width; v++) {
                    /* Loop over direct block rows in first recursive indirect block */
                    block_size = cparam->managed.start_block_size;
                    nrows = 0;
                    max_block_size = block_size * (1 << ((H5V_log2_of2(cparam->managed.max_direct_size / cparam->managed.width) -
                            (H5V_log2_of2(cparam->managed.start_block_size) +
                                H5V_log2_of2(cparam->managed.width))) + (w + 1) + 1));
                    while(block_size <= max_block_size) {
                        /* Loop over filling direct blocks for a row */
                        for(u = 0; u < cparam->managed.width; u++) {
                            /* Fill a direct heap block up */
                            heap_size += block_size;
                            if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                                FAIL_STACK_ERROR
                            tot_nobjs += nobjs;
                        } /* end for */

                        /* Adjust block size for row */
                        if(nrows > 0)
                            block_size *= 2;

                        /* Increment row count */
                        nrows++;
                    } /* end for */
                } /* end for */
            } /* end for */

            /* Loop over rows of 3rd level deep indirect blocks */
            for(y = 0; y < (uu + 1); y++) {

                /* Loop over row of 3rd level deep indirect blocks */
                for(x = 0; x < cparam->managed.width; x++) {

                    /* Loop over direct block rows in third level indirect block */
                    block_size = cparam->managed.start_block_size;
                    nrows = 0;
                    while(block_size <= cparam->managed.max_direct_size) {
                        /* Loop over filling direct blocks for a row */
                        for(u = 0; u < cparam->managed.width; u++) {
                            /* Fill a direct heap block up */
                            heap_size += block_size;
                            if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                                FAIL_STACK_ERROR
                            tot_nobjs += nobjs;
                        } /* end for */

                        /* Adjust block size for row */
                        if(nrows > 0)
                            block_size *= 2;

                        /* Increment row count */
                        nrows++;
                    } /* end for */

                    /* Loop over rows of 2nd level deep indirect blocks */
                    for(w = 0; w < (y + 1); w++) {

                        /* Loop over row of indirect blocks */
                        for(v = 0; v < cparam->managed.width; v++) {
                            /* Loop over direct block rows in first recursive indirect block */
                            block_size = cparam->managed.start_block_size;
                            nrows = 0;
                            max_block_size = block_size * (1 << ((H5V_log2_of2(cparam->managed.max_direct_size / cparam->managed.width) -
                                    (H5V_log2_of2(cparam->managed.start_block_size) +
                                        H5V_log2_of2(cparam->managed.width))) + (w + 1) + 1));
                            while(block_size <= max_block_size) {
                                /* Loop over filling direct blocks for a row */
                                for(u = 0; u < cparam->managed.width; u++) {
                                    /* Fill a direct heap block up */
                                    heap_size += block_size;
                                    if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                                        FAIL_STACK_ERROR
                                    tot_nobjs += nobjs;
                                } /* end for */

                                /* Adjust block size for row */
                                if(nrows > 0)
                                    block_size *= 2;

                                /* Increment row count */
                                nrows++;
                            } /* end for */
                        } /* end for */
                    } /* end for */
                } /* end for */
            } /* end for */
        } /* end for */
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
} /* test_abs_fill_all_4th_recursive_indirect() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_start_5th_recursive_indirect
 *
 * Purpose:	Test inserting mult. objects into absolute heap, creating
 *              enough direct blocks to fill all direct rows of root indirect
 *              block, fill all direct blocks in the row of indirect
 *              blocks that are 2 levels deep, fill all indirect blocks
 *              that are three levels deep, fill all rows of indirect blocks
 *              that are four levels deep and start first direct block in 
 *              indirect blocks five levels deep
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, March 27, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_start_5th_recursive_indirect(hid_t fapl, H5HF_create_t *cparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    haddr_t     fh_addr;                /* Address of fractal heap created */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned char obj[10];              /* Buffer for object to insert */
    unsigned char robj[10];             /* Buffer for reading object */
    unsigned char heap_id[HEAP_ID_LEN]; /* Heap ID for object inserted */
    hsize_t     free_space;             /* Size of free space in heap */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    unsigned    tot_nobjs = 0;          /* Total number of objects inserted */
    size_t      block_size;             /* Size of block added */
    size_t      max_block_size;         /* Max. size of block to add */
    hsize_t     heap_size;              /* Total size of heap */
    unsigned    nrows;                  /* Number of rows inserted */
    unsigned    u, v, w, x, y, z, uu;   /* Local index variables */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(H5HF_create(f, dxpl, cparam, &fh_addr/*out*/, &id_len/*out*/) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
#ifdef QAK
HDfprintf(stderr, "Fractal heap header address = %a\n", fh_addr);
#endif /* QAK */

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in recursive indirect blocks four levels deep
     */
    TESTING("inserting objects to create first direct block in recursive indirect blocks five levels deep");

    /* Loop over direct block rows in root indirect block */
    block_size = cparam->managed.start_block_size;
    heap_size = 0;
    nrows = 0;
    while(block_size <= cparam->managed.max_direct_size) {
#ifdef QAK
HDfprintf(stderr, "block_size = %Zu\n", block_size);
#endif /* QAK */
        /* Loop over filling direct blocks for a row */
#ifdef QAK
HDfprintf(stderr, "block number: ");
#endif /* QAK */
        for(u = 0; u < cparam->managed.width; u++) {
#ifdef QAK
HDfprintf(stderr, "%u ", u);
#endif /* QAK */
            /* Fill a direct heap block up */
            heap_size += block_size;
            if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                FAIL_STACK_ERROR
            tot_nobjs += nobjs;
        } /* end for */
#ifdef QAK
HDfprintf(stderr, "\n");
#endif /* QAK */

        /* Adjust block size for row */
        if(nrows > 0)
            block_size *= 2;

        /* Increment row count */
        nrows++;
    } /* end for */

    /* Loop over rows of 2nd level deep indirect blocks */
    for(w = 0; w < (H5V_log2_of2(cparam->managed.width) + 1); w++) {
#ifdef QAK
HDfprintf(stderr, "indirect row # = %u\n", w);
#endif /* QAK */
        for(v = 0; v < cparam->managed.width; v++) {
#ifdef QAK
HDfprintf(stderr, "indirect block # = %u\n", v);
#endif /* QAK */
            /* Loop over direct block rows in first recursive indirect block */
            block_size = cparam->managed.start_block_size;
            nrows = 0;
            max_block_size = block_size * (1 << ((H5V_log2_of2(cparam->managed.max_direct_size / cparam->managed.width) -
                    (H5V_log2_of2(cparam->managed.start_block_size) +
                        H5V_log2_of2(cparam->managed.width))) + (w + 1) + 1));
#ifdef QAK
HDfprintf(stderr, "max_block_size = %Zu\n", max_block_size);
#endif /* QAK */
            while(block_size <= max_block_size) {
#ifdef QAK
HDfprintf(stderr, "block_size = %Zu\n", block_size);
#endif /* QAK */
                /* Loop over filling direct blocks for a row */
#ifdef QAK
HDfprintf(stderr, "block number: ");
#endif /* QAK */
                for(u = 0; u < cparam->managed.width; u++) {
#ifdef QAK
HDfprintf(stderr, "%u ", u);
#endif /* QAK */
                    /* Fill a direct heap block up */
                    heap_size += block_size;
                    if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                        FAIL_STACK_ERROR
                    tot_nobjs += nobjs;
                } /* end for */
#ifdef QAK
HDfprintf(stderr, "\n");
#endif /* QAK */

                /* Adjust block size for row */
                if(nrows > 0)
                    block_size *= 2;

                /* Increment row count */
                nrows++;
            } /* end for */
        } /* end for */
    } /* end for */

    /* Loop over rows of 3rd level deep indirect blocks */
    for(y = 0; y < (H5V_log2_of2(cparam->managed.width) + 1); y++) {
#ifdef QAK
HDfprintf(stderr, "3rd indirect row # = %u\n", y);
#endif /* QAK */

        /* Loop over row of 3rd level deep indirect blocks */
        for(x = 0; x < cparam->managed.width; x++) {
#ifdef QAK
HDfprintf(stderr, "3rd indirect block # = %u\n", x);
#endif /* QAK */

            /* Loop over direct block rows in third level indirect block */
            block_size = cparam->managed.start_block_size;
            nrows = 0;
            while(block_size <= cparam->managed.max_direct_size) {
                /* Loop over filling direct blocks for a row */
                for(u = 0; u < cparam->managed.width; u++) {
                    /* Fill a direct heap block up */
                    heap_size += block_size;
                    if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                        FAIL_STACK_ERROR
                    tot_nobjs += nobjs;
                } /* end for */

                /* Adjust block size for row */
                if(nrows > 0)
                    block_size *= 2;

                /* Increment row count */
                nrows++;
            } /* end for */

            /* Loop over rows of 2nd level deep indirect blocks */
            for(w = 0; w < (y + 1); w++) {
#ifdef QAK
HDfprintf(stderr, "indirect row # = %u\n", w);
#endif /* QAK */

                /* Loop over row of indirect blocks */
                for(v = 0; v < cparam->managed.width; v++) {
#ifdef QAK
HDfprintf(stderr, "indirect block # = %u\n", v);
#endif /* QAK */
                    /* Loop over direct block rows in first recursive indirect block */
                    block_size = cparam->managed.start_block_size;
                    nrows = 0;
                    max_block_size = block_size * (1 << ((H5V_log2_of2(cparam->managed.max_direct_size / cparam->managed.width) -
                            (H5V_log2_of2(cparam->managed.start_block_size) +
                                H5V_log2_of2(cparam->managed.width))) + (w + 1) + 1));
#ifdef QAK
HDfprintf(stderr, "max_block_size = %Zu\n", max_block_size);
#endif /* QAK */
                    while(block_size <= max_block_size) {
#ifdef QAK
HDfprintf(stderr, "block_size = %Zu\n", block_size);
#endif /* QAK */
                        /* Loop over filling direct blocks for a row */
#ifdef QAK
HDfprintf(stderr, "block number: ");
#endif /* QAK */
                        for(u = 0; u < cparam->managed.width; u++) {
#ifdef QAK
HDfprintf(stderr, "%u ", u);
#endif /* QAK */
                            /* Fill a direct heap block up */
                            heap_size += block_size;
                            if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                                FAIL_STACK_ERROR
                            tot_nobjs += nobjs;
                        } /* end for */
#ifdef QAK
HDfprintf(stderr, "\n");
#endif /* QAK */

                        /* Adjust block size for row */
                        if(nrows > 0)
                            block_size *= 2;

                        /* Increment row count */
                        nrows++;
                    } /* end for */
                } /* end for */
            } /* end for */
        } /* end for */
    } /* end for */

    /* Loop over rows of 2nd level deep indirect blocks */
    for(uu = 0; uu < (H5V_log2_of2(cparam->managed.width) + 1); uu++) {
#ifdef QAK
HDfprintf(stderr, "4th indirect row # = %u\n", uu);
#endif /* QAK */

        /* Loop over row of 4th level indirect blocks */
        for(z = 0; z < cparam->managed.width; z++) {
#ifdef QAK
HDfprintf(stderr, "4th indirect block # = %u\n", z);
#endif /* QAK */

            /* Loop over direct block rows in fourth level indirect block */
            block_size = cparam->managed.start_block_size;
            nrows = 0;
            while(block_size <= cparam->managed.max_direct_size) {
                /* Loop over filling direct blocks for a row */
                for(u = 0; u < cparam->managed.width; u++) {
                    /* Fill a direct heap block up */
                    heap_size += block_size;
                    if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                        FAIL_STACK_ERROR
                    tot_nobjs += nobjs;
                } /* end for */

                /* Adjust block size for row */
                if(nrows > 0)
                    block_size *= 2;

                /* Increment row count */
                nrows++;
            } /* end for */

            /* Loop over rows of 2nd level deep indirect blocks */
            for(w = 0; w < (H5V_log2_of2(cparam->managed.width) + 1); w++) {
#ifdef QAK
HDfprintf(stderr, "indirect row # = %u\n", w);
#endif /* QAK */

                /* Loop over row of indirect blocks */
                for(v = 0; v < cparam->managed.width; v++) {
#ifdef QAK
HDfprintf(stderr, "indirect block # = %u\n", v);
#endif /* QAK */
                    /* Loop over direct block rows in first recursive indirect block */
                    block_size = cparam->managed.start_block_size;
                    nrows = 0;
                    max_block_size = block_size * (1 << ((H5V_log2_of2(cparam->managed.max_direct_size / cparam->managed.width) -
                            (H5V_log2_of2(cparam->managed.start_block_size) +
                                H5V_log2_of2(cparam->managed.width))) + (w + 1) + 1));
#ifdef QAK
HDfprintf(stderr, "max_block_size = %Zu\n", max_block_size);
#endif /* QAK */
                    while(block_size <= max_block_size) {
#ifdef QAK
HDfprintf(stderr, "block_size = %Zu\n", block_size);
#endif /* QAK */
                        /* Loop over filling direct blocks for a row */
#ifdef QAK
HDfprintf(stderr, "block number: ");
#endif /* QAK */
                        for(u = 0; u < cparam->managed.width; u++) {
#ifdef QAK
HDfprintf(stderr, "%u ", u);
#endif /* QAK */
                            /* Fill a direct heap block up */
                            heap_size += block_size;
                            if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                                FAIL_STACK_ERROR
                            tot_nobjs += nobjs;
                        } /* end for */
#ifdef QAK
HDfprintf(stderr, "\n");
#endif /* QAK */

                        /* Adjust block size for row */
                        if(nrows > 0)
                            block_size *= 2;

                        /* Increment row count */
                        nrows++;
                    } /* end for */
                } /* end for */
            } /* end for */

            /* Loop over rows of 3rd level deep indirect blocks */
            for(y = 0; y < (uu + 1); y++) {
#ifdef QAK
HDfprintf(stderr, "3rd indirect row # = %u\n", y);
#endif /* QAK */

                /* Loop over row of 3rd level deep indirect blocks */
                for(x = 0; x < cparam->managed.width; x++) {
#ifdef QAK
HDfprintf(stderr, "3rd indirect block # = %u\n", x);
#endif /* QAK */

                    /* Loop over direct block rows in third level indirect block */
                    block_size = cparam->managed.start_block_size;
                    nrows = 0;
                    while(block_size <= cparam->managed.max_direct_size) {
                        /* Loop over filling direct blocks for a row */
                        for(u = 0; u < cparam->managed.width; u++) {
                            /* Fill a direct heap block up */
                            heap_size += block_size;
                            if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                                FAIL_STACK_ERROR
                            tot_nobjs += nobjs;
                        } /* end for */

                        /* Adjust block size for row */
                        if(nrows > 0)
                            block_size *= 2;

                        /* Increment row count */
                        nrows++;
                    } /* end for */

                    /* Loop over rows of 2nd level deep indirect blocks */
                    for(w = 0; w < (y + 1); w++) {
#ifdef QAK
HDfprintf(stderr, "indirect row # = %u\n", w);
#endif /* QAK */

                        /* Loop over row of indirect blocks */
                        for(v = 0; v < cparam->managed.width; v++) {
#ifdef QAK
HDfprintf(stderr, "indirect block # = %u\n", v);
#endif /* QAK */
                            /* Loop over direct block rows in first recursive indirect block */
                            block_size = cparam->managed.start_block_size;
                            nrows = 0;
                            max_block_size = block_size * (1 << ((H5V_log2_of2(cparam->managed.max_direct_size / cparam->managed.width) -
                                    (H5V_log2_of2(cparam->managed.start_block_size) +
                                        H5V_log2_of2(cparam->managed.width))) + (w + 1) + 1));
#ifdef QAK
HDfprintf(stderr, "max_block_size = %Zu\n", max_block_size);
#endif /* QAK */
                            while(block_size <= max_block_size) {
#ifdef QAK
HDfprintf(stderr, "block_size = %Zu\n", block_size);
#endif /* QAK */
                                /* Loop over filling direct blocks for a row */
#ifdef QAK
HDfprintf(stderr, "block number: ");
#endif /* QAK */
                                for(u = 0; u < cparam->managed.width; u++) {
#ifdef QAK
HDfprintf(stderr, "%u ", u);
#endif /* QAK */
                                    /* Fill a direct heap block up */
                                    heap_size += block_size;
                                    if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, (hsize_t)0, tot_nobjs, &nobjs))
                                        FAIL_STACK_ERROR
                                    tot_nobjs += nobjs;
                                } /* end for */
#ifdef QAK
HDfprintf(stderr, "\n");
#endif /* QAK */

                                /* Adjust block size for row */
                                if(nrows > 0)
                                    block_size *= 2;

                                /* Increment row count */
                                nrows++;
                            } /* end for */
                        } /* end for */
                    } /* end for */
                } /* end for */
            } /* end for */
        } /* end for */
    } /* end for */

    /* Insert one more object, to force creation of five level deep
     * recursive indirect block
     */

    /* Initialize object buffer */
    for(u = 0; u < sizeof(obj); u++)
        obj[u] = u + tot_nobjs;

    /* Insert object */
    HDmemset(heap_id, 0, sizeof(heap_id));
    if(H5HF_insert(f, dxpl, fh_addr, sizeof(obj), obj, heap_id) < 0)
        FAIL_STACK_ERROR

    /* Increment object count */
    tot_nobjs++;

    heap_size += cparam->managed.start_block_size;
    free_space = cparam->managed.start_block_size - ((sizeof(obj) + OBJ_PREFIX_LEN) + OBJ_PREFIX_LEN + DBLOCK_OVERHEAD + (cparam->managed.max_index / 8));
    if(check_stats(f, H5P_DATASET_XFER_DEFAULT, fh_addr, heap_size, heap_size, (hsize_t)0, free_space, (hsize_t)tot_nobjs))
        FAIL_STACK_ERROR

    /* Read in object */
    if(H5HF_read(f, dxpl, fh_addr, heap_id, robj) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(obj, robj, sizeof(obj)))
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
} /* test_abs_start_5th_recursive_indirect() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_skip_start_block
 *
 * Purpose:	Test inserting object into absolute heap which is too large
 *              for starting block size, which forces root indirect block
 *              creation
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, March 27, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_skip_start_block(hid_t fapl, H5HF_create_t *cparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    haddr_t     fh_addr;                /* Address of fractal heap created */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned char *obj = NULL;          /* Buffer for object to insert */
    unsigned char *robj = NULL;         /* Buffer for object to read */
    unsigned char heap_id[HEAP_ID_LEN]; /* Heap ID for object inserted */
    hsize_t     free_space;             /* Size of free space in heap */
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
    if(H5HF_create(f, dxpl, cparam, &fh_addr/*out*/, &id_len/*out*/) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(check_stats(f, H5P_DATASET_XFER_DEFAULT, fh_addr, (hsize_t)0, (hsize_t)0, (hsize_t)0, (hsize_t)0, (hsize_t)0))
        FAIL_STACK_ERROR

    /* Initialize object buffer */
    obj = H5MM_malloc(cparam->managed.start_block_size + 1);
    for(u = 0; u < (cparam->managed.start_block_size + 1); u++)
        obj[u] = u;

    /*
     * Test inserting object into absolute heap which doesn't fit into starting
     *  block size
     */
    TESTING("inserting object that is too large for starting block");
    HDmemset(heap_id, 0, sizeof(heap_id));
    if(H5HF_insert(f, dxpl, fh_addr, (cparam->managed.start_block_size + 1), obj, heap_id) < 0)
        FAIL_STACK_ERROR
    free_space = (2 * cparam->managed.width * (cparam->managed.start_block_size - (OBJ_PREFIX_LEN + DBLOCK_OVERHEAD + (cparam->managed.max_index / 8)))) + (2 * cparam->managed.start_block_size) - (((cparam->managed.start_block_size + 1) + OBJ_PREFIX_LEN) + OBJ_PREFIX_LEN + DBLOCK_OVERHEAD + (cparam->managed.max_index / 8));
    if(check_stats(f, H5P_DATASET_XFER_DEFAULT, fh_addr, (hsize_t)(2 * cparam->managed.start_block_size), (hsize_t)(2 * cparam->managed.start_block_size), (hsize_t)0, free_space, (hsize_t)1))
        FAIL_STACK_ERROR

    /* Check reading back in the object */
    robj = H5MM_malloc(cparam->managed.start_block_size + 1);
    if(H5HF_read(f, dxpl, fh_addr, heap_id, robj) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(obj, robj, (cparam->managed.start_block_size + 1)))
        FAIL_STACK_ERROR

    PASSED()

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    H5MM_xfree(obj);
    H5MM_xfree(robj);
    return(0);

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
        H5MM_xfree(obj);
        H5MM_xfree(robj);
    } H5E_END_TRY;
    return(1);
} /* test_abs_skip_start_block() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_skip_start_block_add_back
 *
 * Purpose:	Test inserting object into absolute heap which is too large
 *              for starting block size, which forces root indirect block
 *              creation, then add object which fits in skipped direct block
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, March 28, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_skip_start_block_add_back(hid_t fapl, H5HF_create_t *cparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    haddr_t     fh_addr;                /* Address of fractal heap created */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned char *obj = NULL;          /* Buffer for object to insert */
    unsigned char *robj = NULL;         /* Buffer for object to read */
    unsigned char obj2[20];             /* Buffer for second object to insert */
    unsigned char robj2[20];            /* Buffer for reading second object */
    unsigned char heap_id[HEAP_ID_LEN]; /* Heap ID for object inserted */
    hsize_t     free_space;             /* Size of free space in heap */
    size_t      obj_size;               /* Size of object to add */
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
    if(H5HF_create(f, dxpl, cparam, &fh_addr/*out*/, &id_len/*out*/) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(check_stats(f, H5P_DATASET_XFER_DEFAULT, fh_addr, (hsize_t)0, (hsize_t)0, (hsize_t)0, (hsize_t)0, (hsize_t)0))
        FAIL_STACK_ERROR

    /* Initialize object buffer */
    obj_size = cparam->managed.start_block_size + 1;
    obj = H5MM_malloc(obj_size);
    for(u = 0; u < obj_size; u++)
        obj[u] = u;

    /*
     * Test inserting object into absolute heap which doesn't fit into starting
     *  block size
     */
    TESTING("skipping starting block, then adding object back to first block");
    HDmemset(heap_id, 0, sizeof(heap_id));
    if(H5HF_insert(f, dxpl, fh_addr, obj_size, obj, heap_id) < 0)
        FAIL_STACK_ERROR
    free_space = (2 * cparam->managed.width * (cparam->managed.start_block_size - (OBJ_PREFIX_LEN + DBLOCK_OVERHEAD + (cparam->managed.max_index / 8)))) + (2 * cparam->managed.start_block_size) - ((obj_size + OBJ_PREFIX_LEN) + OBJ_PREFIX_LEN + DBLOCK_OVERHEAD + (cparam->managed.max_index / 8));
    if(check_stats(f, H5P_DATASET_XFER_DEFAULT, fh_addr, (hsize_t)(2 * cparam->managed.start_block_size), (hsize_t)(2 * cparam->managed.start_block_size), (hsize_t)0, free_space, (hsize_t)1))
        FAIL_STACK_ERROR

    /* Check reading back in the object */
    robj = H5MM_malloc(obj_size);
    if(H5HF_read(f, dxpl, fh_addr, heap_id, robj) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(obj, robj, obj_size))
        FAIL_STACK_ERROR

    /* Insert an object to fill up the heap block just created */

    /* Initialize object buffer */
    H5MM_xfree(obj);
    obj_size = (2 * cparam->managed.start_block_size) - (OBJ_PREFIX_LEN + (cparam->managed.start_block_size + 1 + OBJ_PREFIX_LEN) + DBLOCK_OVERHEAD + (cparam->managed.max_index / 8));
    obj = H5MM_malloc(obj_size);
    for(u = 0; u < obj_size; u++)
        obj[u] = u;

    HDmemset(heap_id, 0, sizeof(heap_id));
    if(H5HF_insert(f, dxpl, fh_addr, obj_size, obj, heap_id) < 0)
        FAIL_STACK_ERROR
    free_space -= obj_size;     /* no object prefix, there's no more space in the node */
    if(check_stats(f, H5P_DATASET_XFER_DEFAULT, fh_addr, (hsize_t)(2 * cparam->managed.start_block_size), (hsize_t)(2 * cparam->managed.start_block_size), (hsize_t)0, free_space, (hsize_t)2))
        FAIL_STACK_ERROR

    /* Check reading back in the object */
    H5MM_xfree(robj);
    robj = H5MM_malloc(obj_size);
    if(H5HF_read(f, dxpl, fh_addr, heap_id, robj) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(obj, robj, obj_size))
        FAIL_STACK_ERROR

    /* Insert second "real" object, which should go in earlier direct block */
    HDmemset(heap_id, 0, sizeof(heap_id));
    if(H5HF_insert(f, dxpl, fh_addr, sizeof(obj2), obj2, heap_id) < 0)
        FAIL_STACK_ERROR
    free_space -= (OBJ_PREFIX_LEN + sizeof(obj2));
    if(check_stats(f, H5P_DATASET_XFER_DEFAULT, fh_addr, (hsize_t)(3 * cparam->managed.start_block_size), (hsize_t)(3 * cparam->managed.start_block_size), (hsize_t)0, free_space, (hsize_t)3))
        FAIL_STACK_ERROR

    /* Check reading back in the second object */
    if(H5HF_read(f, dxpl, fh_addr, heap_id, robj2) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(obj2, robj2, sizeof(obj2)))
        FAIL_STACK_ERROR

    PASSED()

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    H5MM_xfree(obj);
    H5MM_xfree(robj);
    return(0);

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
        H5MM_xfree(obj);
        H5MM_xfree(robj);
    } H5E_END_TRY;
    return(1);
} /* test_abs_skip_start_block_add_back() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_skip_start_block_add_skipped
 *
 * Purpose:	Test inserting object into absolute heap which is too large
 *              for starting block size, which forces root indirect block
 *              creation, then add objects to fill skipped direct blocks
 *              and add another object to start on next "normal" block
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, March 28, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_skip_start_block_add_skipped(hid_t fapl, H5HF_create_t *cparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    haddr_t     fh_addr;                /* Address of fractal heap created */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned char *obj = NULL;          /* Buffer for object to insert */
    unsigned char *robj = NULL;         /* Buffer for object to read */
    unsigned char obj2[20];             /* Buffer for second object to insert */
    unsigned char robj2[20];            /* Buffer for reading second object */
    unsigned char heap_id[HEAP_ID_LEN]; /* Heap ID for object inserted */
    hsize_t     free_space;             /* Size of free space in heap */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    unsigned    tot_nobjs = 0;          /* Total number of objects inserted */
    size_t      obj_size;               /* Size of object to add */
    size_t      block_size;             /* Size of block added */
    hsize_t     heap_size;              /* Total size of heap */
    unsigned    u, v;                   /* Local index variable */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(H5HF_create(f, dxpl, cparam, &fh_addr/*out*/, &id_len/*out*/) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(check_stats(f, H5P_DATASET_XFER_DEFAULT, fh_addr, (hsize_t)0, (hsize_t)0, (hsize_t)0, (hsize_t)0, (hsize_t)0))
        FAIL_STACK_ERROR

    /* Initialize object buffer */
    obj_size = cparam->managed.start_block_size + 1;
    obj = H5MM_malloc(obj_size);
    for(u = 0; u < obj_size; u++)
        obj[u] = u;

    /*
     * Test inserting object into absolute heap which doesn't fit into starting
     *  block size
     */
    TESTING("skipping starting block, then adding objects to backfill and extend");
    HDmemset(heap_id, 0, sizeof(heap_id));
    if(H5HF_insert(f, dxpl, fh_addr, obj_size, obj, heap_id) < 0)
        FAIL_STACK_ERROR
    free_space = (2 * cparam->managed.width * (cparam->managed.start_block_size - (OBJ_PREFIX_LEN + DBLOCK_OVERHEAD + (cparam->managed.max_index / 8)))) + (2 * cparam->managed.start_block_size) - ((obj_size + OBJ_PREFIX_LEN) + OBJ_PREFIX_LEN + DBLOCK_OVERHEAD + (cparam->managed.max_index / 8));
    if(check_stats(f, H5P_DATASET_XFER_DEFAULT, fh_addr, (hsize_t)(2 * cparam->managed.start_block_size), (hsize_t)(2 * cparam->managed.start_block_size), (hsize_t)0, free_space, (hsize_t)1))
        FAIL_STACK_ERROR

    /* Check reading back in the object */
    robj = H5MM_malloc(obj_size);
    if(H5HF_read(f, dxpl, fh_addr, heap_id, robj) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(obj, robj, obj_size))
        FAIL_STACK_ERROR

    /* Insert an object to fill up the heap block just created */

    /* Initialize object buffer */
    H5MM_xfree(obj);
    obj_size = (2 * cparam->managed.start_block_size) - (OBJ_PREFIX_LEN + (cparam->managed.start_block_size + 1 + OBJ_PREFIX_LEN) + DBLOCK_OVERHEAD + (cparam->managed.max_index / 8));
    obj = H5MM_malloc(obj_size);
    for(u = 0; u < obj_size; u++)
        obj[u] = u;

    HDmemset(heap_id, 0, sizeof(heap_id));
    if(H5HF_insert(f, dxpl, fh_addr, obj_size, obj, heap_id) < 0)
        FAIL_STACK_ERROR
    free_space -= obj_size;     /* no object prefix, there's no more space in the node */
#ifdef QAK
HDfprintf(stderr, "free_space = %Hu\n", free_space);
#endif /* QAK */
    if(check_stats(f, H5P_DATASET_XFER_DEFAULT, fh_addr, (hsize_t)(2 * cparam->managed.start_block_size), (hsize_t)(2 * cparam->managed.start_block_size), (hsize_t)0, free_space, (hsize_t)2))
        FAIL_STACK_ERROR

    /* Check reading back in the object */
    H5MM_xfree(robj);
    robj = H5MM_malloc(obj_size);
    if(H5HF_read(f, dxpl, fh_addr, heap_id, robj) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(obj, robj, obj_size))
        FAIL_STACK_ERROR

    /* Add row of blocks to "backfill" direct blocks that were skipped */
    heap_size = (2 * cparam->managed.start_block_size) + cparam->managed.start_block_size;
    block_size = cparam->managed.start_block_size;
    tot_nobjs = nobjs = 2;
    free_space -= cparam->managed.start_block_size - (OBJ_PREFIX_LEN + DBLOCK_OVERHEAD + (cparam->managed.max_index / 8));
#ifdef QAK
HDfprintf(stderr, "free_space = %Hu\n", free_space);
#endif /* QAK */
    for(v = 0; v < 2; v++) {
        for(u = 0; u < cparam->managed.width; u++) {
            /* Fill a direct heap block up */
#ifdef QAK
HDfprintf(stderr, "heap_size = %Hu\n", heap_size);
#endif /* QAK */
            if(fill_heap(f, dxpl, fh_addr, cparam, heap_size, block_size, free_space, tot_nobjs, &nobjs))
                FAIL_STACK_ERROR
            free_space -= cparam->managed.start_block_size - (OBJ_PREFIX_LEN + DBLOCK_OVERHEAD + (cparam->managed.max_index / 8));
            heap_size += block_size;
            tot_nobjs += nobjs;
        } /* end for */
    } /* end for */

    /* Insert another object, which should go extend direct blocks, instead of backfill */
    HDmemset(heap_id, 0, sizeof(heap_id));
    if(H5HF_insert(f, dxpl, fh_addr, sizeof(obj2), obj2, heap_id) < 0)
        FAIL_STACK_ERROR
    heap_size += cparam->managed.start_block_size;
    free_space = (2 * cparam->managed.start_block_size) - (OBJ_PREFIX_LEN + DBLOCK_OVERHEAD + (cparam->managed.max_index / 8));
    free_space -= (OBJ_PREFIX_LEN + sizeof(obj2));
#ifdef QAK
HDfprintf(stderr, "free_space = %Hu\n", free_space);
HDfprintf(stderr, "heap_size = %Hu\n", heap_size);
#endif /* QAK */
    tot_nobjs++;
    if(check_stats(f, H5P_DATASET_XFER_DEFAULT, fh_addr, heap_size, heap_size, (hsize_t)0, free_space, (hsize_t)tot_nobjs))
        FAIL_STACK_ERROR

    /* Check reading back in the second object */
    if(H5HF_read(f, dxpl, fh_addr, heap_id, robj2) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(obj2, robj2, sizeof(obj2)))
        FAIL_STACK_ERROR

    PASSED()

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    H5MM_xfree(obj);
    H5MM_xfree(robj);
    return(0);

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
        H5MM_xfree(obj);
        H5MM_xfree(robj);
    } H5E_END_TRY;
    return(1);
} /* test_abs_skip_start_block_add_skipped() */


/*-------------------------------------------------------------------------
 * Function:	run_tests
 *
 * Purpose:	Test the fractal heap code, with different file access property
 *              lists and heap creation parameters
 *
 * Return:	Success:
 *
 *		Failure:
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, March 21, 2006
 *
 *-------------------------------------------------------------------------
 */
static unsigned
run_tests(hid_t fapl, H5HF_create_t *cparam)
{
    unsigned	nerrors = 0;            /* Cumulative error count */

    /* Test fractal heap creation */
    nerrors += test_create(fapl, cparam);

    /* Test fractal heap object insertion */
#ifdef QAK
    nerrors += test_abs_insert_first(fapl, cparam);
    nerrors += test_abs_insert_second(fapl, cparam);
    nerrors += test_abs_insert_root_mult(fapl, cparam);
    nerrors += test_abs_insert_force_indirect(fapl, cparam);
    nerrors += test_abs_insert_fill_second(fapl, cparam);
    nerrors += test_abs_insert_third_direct(fapl, cparam);
    nerrors += test_abs_fill_first_row(fapl, cparam);
    nerrors += test_abs_start_second_row(fapl, cparam);
    nerrors += test_abs_fill_second_row(fapl, cparam);
    nerrors += test_abs_start_third_row(fapl, cparam);
    nerrors += test_abs_fill_fourth_row(fapl, cparam);
    nerrors += test_abs_fill_all_root_direct(fapl, cparam);
    nerrors += test_abs_first_recursive_indirect(fapl, cparam);
    nerrors += test_abs_second_direct_recursive_indirect(fapl, cparam);
    nerrors += test_abs_fill_first_recursive_indirect(fapl, cparam);
    nerrors += test_abs_second_recursive_indirect(fapl, cparam);
    nerrors += test_abs_fill_second_recursive_indirect(fapl, cparam);
    nerrors += test_abs_fill_recursive_indirect_row(fapl, cparam);
    nerrors += test_abs_start_2nd_recursive_indirect(fapl, cparam);
    nerrors += test_abs_recursive_indirect_two_deep(fapl, cparam);
    nerrors += test_abs_start_3rd_recursive_indirect(fapl, cparam);
    nerrors += test_abs_fill_first_3rd_recursive_indirect(fapl, cparam);
    nerrors += test_abs_fill_3rd_recursive_indirect_row(fapl, cparam);
    nerrors += test_abs_fill_all_3rd_recursive_indirect(fapl, cparam);
    nerrors += test_abs_start_4th_recursive_indirect(fapl, cparam);
    nerrors += test_abs_fill_first_4th_recursive_indirect(fapl, cparam);
    nerrors += test_abs_fill_4th_recursive_indirect_row(fapl, cparam);
    nerrors += test_abs_fill_all_4th_recursive_indirect(fapl, cparam);
#endif /* QAK */
    /* If this test fails, uncomment the tests above, which build up to this
     * level of complexity gradually. -QAK
     */
#ifndef QAK
    nerrors += test_abs_start_5th_recursive_indirect(fapl, cparam);
#else /* QAK */
HDfprintf(stderr, "Uncomment tests!\n");
#endif /* QAK */

#ifndef QAK
    nerrors += test_abs_skip_start_block(fapl, cparam);
    nerrors += test_abs_skip_start_block_add_back(fapl, cparam);
    nerrors += test_abs_skip_start_block_add_skipped(fapl, cparam);
#else /* QAK */
HDfprintf(stderr, "Uncomment tests!\n");
#endif /* QAK */

    return nerrors;
} /* end run_tests() */


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
    H5HF_create_t cparam;               /* Creation parameters for heap */
    hid_t	fapl = -1;              /* File access property list for data files */
    unsigned	nerrors = 0;            /* Cumulative error count */

    /* Reset library */
    h5_reset();
    fapl = h5_fileaccess();

#ifndef QAK
    /* Test fractal heap with small parameters */
    init_small_cparam(&cparam);
    puts("Testing with small heap creation parameters:");
    nerrors += run_tests(fapl, &cparam);

#else /* QAK */
HDfprintf(stderr, "Uncomment tests!\n");
#endif /* QAK */
#ifdef QAK
    /* Test fractal heap with standard parameters */
    init_std_cparam(&cparam);
    puts("Testing with standard heap creation parameters:");
    nerrors += run_tests(fapl, &cparam);
#else /* QAK */
HDfprintf(stderr, "Uncomment tests!\n");
#endif /* QAK */

    if(nerrors)
        goto error;
    puts("All fractal heap tests passed.");
#ifdef QAK
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
} /* end main() */

