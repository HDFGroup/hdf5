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
#include "H5HFpkg.h"		/* Fractal heaps			*/

/* Other private headers that this test requires */
#include "H5Iprivate.h"
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Vprivate.h"		/* Vectors and arrays 			*/

/* Object size macros */
#define SMALL_OBJ_SIZE1         10
#define SMALL_OBJ_SIZE2         20

/* "Small" heap creation parameters */
#define SMALL_ADDRMAP     H5HF_ABSOLUTE           /* Heap address mapping */
#define SMALL_STAND_SIZE  (48 * 1024)             /* Standalone obj. min. size */
#define SMALL_MAN_WIDTH   4                       /* Managed obj. table width */
#define SMALL_MAN_START_BLOCK_SIZE 512            /* Managed obj. starting block size */
#define SMALL_MAN_MAX_DIRECT_SIZE (64 * 1024)     /* Managed obj. max. direct block size */
#define SMALL_MAN_MAX_INDEX 32                    /* Managed obj. # of bits for total heap size */
#define SMALL_MAN_START_ROOT_ROWS 1               /* Managed obj. starting # of root indirect block rows */

/* Define this macro to enable all insertion tests */
/* #define ALL_INSERT_TESTS */

/* Heap metadata macros */
#define DBLOCK_OVERHEAD(fh) H5HF_get_dblock_overhead(fh) /* # of bytes in direct block overhead */
#define HEAP_ID_LEN             12              /* # of bytes to use for heap ID */
#define DBLOCK_SIZE(fh, r) H5HF_get_dblock_size_test(fh, r)
#define DBLOCK_FREE(fh, r) H5HF_get_dblock_free_test(fh, r)

const char *FILENAME[] = {
    "fheap",
    NULL
};

/* Testing parameters */
typedef struct fheap_test_param_t {
    hbool_t reopen_heap;        /* Whether to re-open the heap during the test */
} fheap_test_param_t;

/* Types of tests to perform */
typedef enum {
    FHEAP_TEST_NORMAL,          /* "Normal" test, with no testing parameters set */
    FHEAP_TEST_REOPEN,          /* Set the reopen_heap flag */
    FHEAP_TEST_NTESTS           /* The number of test types, must be last */
} fheap_test_type_t;


/* Local routines */
static int init_small_cparam(H5HF_create_t *cparam);
static int check_stats(H5HF_t *fh, hsize_t total_size,
    hsize_t man_size, hsize_t std_size, hsize_t man_free_space, hsize_t nobjs);



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
check_stats(H5HF_t *fh, hsize_t total_size,
    hsize_t man_size, hsize_t std_size, hsize_t man_free_space, hsize_t nobjs)
{
    H5HF_stat_t heap_stats;             /* Statistics about the heap */

    /* Get statistics for heap and verify they are correct */
    if(H5HF_stat_info(fh, &heap_stats) < 0)
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
 * Function:	add_obj
 *
 * Purpose:	Add an object to heap
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, April 10, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
add_obj(H5HF_t *fh, hid_t dxpl,
    hsize_t heap_size, hsize_t *free_space, unsigned *nobjs_ptr,
    unsigned obj_init, size_t obj_size)
{
    unsigned char heap_id[HEAP_ID_LEN]; /* Heap ID for object inserted */
    unsigned char *obj = NULL;          /* Buffer for object to insert */
    unsigned char *robj = NULL;         /* Buffer for reading object */
    size_t u;                           /* Local counting variable */

    /* Sanity check */
    HDassert(nobjs_ptr);
    HDassert(free_space);

    /* Initialize object buffer */
    obj = H5MM_malloc(obj_size);
    HDassert(obj);
    for(u = 0; u < obj_size; u++)
        obj[u] = u + obj_init;

    /* Insert object */
    HDmemset(heap_id, 0, sizeof(heap_id));
    if(H5HF_insert(fh, dxpl, obj_size, obj, heap_id) < 0)
        FAIL_STACK_ERROR

    /* Increment the number of objects */
    (*nobjs_ptr)++;

    /* Check free space left in heap */
    *free_space -= obj_size;
    if(check_stats(fh, heap_size, heap_size, (hsize_t)0, *free_space, (hsize_t)*nobjs_ptr))
        FAIL_STACK_ERROR

    /* Read in object */
    robj = H5MM_malloc(obj_size);
    if(H5HF_read(fh, dxpl, heap_id, robj) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(obj, robj, obj_size))
        FAIL_STACK_ERROR

    /* Operations succeeded */
    H5MM_xfree(obj);
    H5MM_xfree(robj);
    return(0);

error:
    H5MM_xfree(obj);
    H5MM_xfree(robj);
    return(1);
} /* add_obj() */


/*-------------------------------------------------------------------------
 * Function:	fill_heap
 *
 * Purpose:	Insert (small) objects to fill up the free space in a heap block
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
fill_heap(H5HF_t *fh, hid_t dxpl, const H5HF_create_t *cparam,
    hsize_t heap_size, size_t block_size,
    hsize_t extra_free, unsigned *nobjs_ptr)
{
    H5HF_stat_t heap_stats;             /* Statistics about the heap */
    hsize_t     free_space;             /* Size of free space in heap */
    unsigned char obj[SMALL_OBJ_SIZE1]; /* Buffer for object to insert */
    unsigned char robj[SMALL_OBJ_SIZE1]; /* Buffer for reading object */
    size_t      num_ids = 0;            /* # of heap IDs in array */
    size_t      alloc_ids = 0;          /* # of heap IDs allocated in array */
    unsigned char *ids = NULL;          /* Array of heap IDs */
    size_t      data_size;              /* Size of data portion of heap block */
    size_t      last_obj_len;           /* Size of last object inserted into heap */
    unsigned    u, v;                   /* Local index variable */

    /* Sanity check */
    HDassert(nobjs_ptr);

    /* Initialize variables */
    data_size = block_size - DBLOCK_OVERHEAD(fh);

    /* Initialize object buffer */
    for(u = 0; u < sizeof(obj); u++)
        obj[u] = u;

    /* Increment object count */
    num_ids++;

    /* Check for needing to increase size of heap ID array */
    if(num_ids > alloc_ids) {
        alloc_ids = MAX(1024, (alloc_ids * 2));
        if(NULL == (ids = H5MM_realloc(ids, HEAP_ID_LEN * alloc_ids)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert first object */
    if(H5HF_insert(fh, dxpl, sizeof(obj), obj, &ids[(num_ids - 1) * HEAP_ID_LEN]) < 0)
        FAIL_STACK_ERROR

    free_space = extra_free + (data_size - (num_ids * sizeof(obj)));
#ifdef QAK
HDfprintf(stderr, "extra_free = %Hu\n", extra_free);
HDfprintf(stderr, "free_space = %Hu\n", free_space);
#endif /* QAK */
    if(check_stats(fh, heap_size, heap_size, (hsize_t)0, free_space, (hsize_t)(*nobjs_ptr + num_ids)))
        FAIL_STACK_ERROR

    /* Get statistics for heap */
    if(H5HF_stat_info(fh, &heap_stats) < 0)
        FAIL_STACK_ERROR

    /* Loop over inserting objects into the root direct block, until there's no more space */
    while((heap_stats.man_free_space - extra_free) > sizeof(obj)) {
        /* Initialize object buffer */
        for(u = 0; u < sizeof(obj); u++)
            obj[u] = u + num_ids;

        /* Increment object count */
        num_ids++;

        /* Check for needing to increase size of heap ID array */
        if(num_ids > alloc_ids) {
            alloc_ids = MAX(1024, (alloc_ids * 2));
            if(NULL == (ids = H5MM_realloc(ids, HEAP_ID_LEN * alloc_ids)))
                FAIL_STACK_ERROR
        } /* end if */

        if(H5HF_insert(fh, dxpl, sizeof(obj), obj, &ids[(num_ids - 1) * HEAP_ID_LEN]) < 0)
            FAIL_STACK_ERROR

        /* Check stats for heap */
        free_space = extra_free + (data_size - (num_ids * sizeof(obj)));
#ifdef QAK
HDfprintf(stderr, "free_space = %Hu\n", free_space);
#endif /* QAK */
        if(check_stats(fh, heap_size, heap_size, (hsize_t)0, free_space, (hsize_t)(num_ids + *nobjs_ptr)))
            FAIL_STACK_ERROR

        /* Get statistics for heap */
        if(H5HF_stat_info(fh, &heap_stats) < 0)
            FAIL_STACK_ERROR
    } /* end while */

    /* Check for adding smaller last object to heap block */
    if((heap_stats.man_free_space - extra_free) > 0) {
        last_obj_len = (size_t)(heap_stats.man_free_space - extra_free);

        /* Initialize object buffer */
        for(u = 0; u < sizeof(obj); u++)
            obj[u] = u + num_ids;

        /* Increment object count */
        num_ids++;

        /* Check for needing to increase size of heap ID array */
        if(num_ids > alloc_ids) {
            alloc_ids = MAX(1024, (alloc_ids * 2));
            if(NULL == (ids = H5MM_realloc(ids, HEAP_ID_LEN * alloc_ids)))
                FAIL_STACK_ERROR
        } /* end if */

        /* Insert last object into the heap, using the remaining free space */
        if(H5HF_insert(fh, dxpl, last_obj_len, obj, &ids[(num_ids - 1) * HEAP_ID_LEN]) < 0)
            FAIL_STACK_ERROR

        /* Verify that the heap is full */
        if(check_stats(fh, heap_size, heap_size, (hsize_t)0, extra_free, (hsize_t)(num_ids + *nobjs_ptr)))
            FAIL_STACK_ERROR
    } /* end if */
    else
        last_obj_len = sizeof(obj);     /* Normal sized last object */

    /* Verify reading the objects written out */
    for(v = 0; v < num_ids; v++) {
        /* Initialize object buffer */
        for(u = 0; u < sizeof(obj); u++)
            obj[u] = u + v;

        /* Read in object */
        if(H5HF_read(fh, dxpl, &ids[v * HEAP_ID_LEN], robj) < 0)
            FAIL_STACK_ERROR
        if(HDmemcmp(obj, robj, (v == (num_ids - 1) ? last_obj_len : sizeof(obj))))
            FAIL_STACK_ERROR
    } /* end for */

    /* Set the number of objects inserted */
    *nobjs_ptr += num_ids;

    /* Operations succeeded */
    H5MM_xfree(ids);
    return(0);

error:
    H5MM_xfree(ids);
    return(1);
} /* fill_heap() */


/*-------------------------------------------------------------------------
 * Function:	fill_root_row
 *
 * Purpose:	Fill up a row of direct blocks in the root indirect block
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, April 10, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
fill_root_row(H5HF_t *fh, hid_t dxpl, const H5HF_create_t *cparam,
    hsize_t *base_heap_size, hsize_t *base_free_space, unsigned row,
    unsigned *nobjs_ptr)
{
    hsize_t     first_free_space;       /* Size of free space in heap after the first block */
    hsize_t     all_free_space;         /* Size of free space in heap after all blocks */
    hsize_t     free_space;             /* Size of free space in heap */
    hsize_t     first_heap_size;        /* Total size of the heap after the first block */
    hsize_t     all_heap_size;          /* Total size of the heap after all blocks */
    hsize_t     heap_size;              /* Total size of the heap */
    size_t      block_size;             /* Block size for row */
    unsigned    expand_rows;            /* # of rows to expand heap by */
    unsigned    u;                      /* Local index variable */

    /* Sanity check */
    HDassert(nobjs_ptr);
    HDassert(base_heap_size);
    HDassert(base_free_space);

    /* Get the correct size for blocks in this row */
    if(row == 0)
        block_size = cparam->managed.start_block_size;
    else
        block_size = cparam->managed.start_block_size * (1 << (row - 1));

    /* Compute the number of rows to expand heap by */
    if(row < 2)
        expand_rows = 1;
    else if(POWER_OF_TWO(row))
        expand_rows = row;
    else 
        expand_rows = 0;

    /* Compute first block & all blocks heap size & free space */
    if(*base_heap_size == 0) {
        first_heap_size = block_size;
        first_free_space = DBLOCK_FREE(fh, row);
        all_heap_size = cparam->managed.width * block_size;
        all_free_space = (cparam->managed.width - 1) * DBLOCK_FREE(fh, row);
    } /* end if */
    else if(expand_rows ==0) {
        all_heap_size = *base_heap_size;
        all_free_space = *base_free_space;
        first_heap_size = all_heap_size;
        first_free_space = all_free_space;
        all_free_space -= DBLOCK_FREE(fh, row);      /* Account for shift from first free space */
    } /* end if */
    else {
        hsize_t block_mult;             /* Base block size multiplier */

        all_heap_size = *base_heap_size;
        all_free_space = 0;
        block_mult = 1;
        for(u = 0; u < expand_rows; u++) {
            all_heap_size += cparam->managed.width * block_size * block_mult;
            all_free_space += cparam->managed.width * DBLOCK_FREE(fh, row + u);
            block_mult *= 2;
        } /* end for */
        first_heap_size = all_heap_size;
        first_free_space = all_free_space;
        all_free_space -= DBLOCK_FREE(fh, row);      /* Account for shift from first free space */
    } /* end else */

    /* Loop over filling direct blocks, until first root indirect row is full */
    heap_size = first_heap_size;
    free_space = first_free_space;
    for(u = 0; u < cparam->managed.width; u++) {
        /* Set heap's size & free space correctly */
        if(u == 1) {
            heap_size = all_heap_size;
            free_space = all_free_space;
        } /* end if */
        free_space -= DBLOCK_FREE(fh, row);

        /* Fill a direct heap block up */
        if(fill_heap(fh, dxpl, cparam, heap_size, block_size, free_space, nobjs_ptr))
            FAIL_STACK_ERROR
    } /* end for */

    /* Update heap size & free space */
    *base_heap_size = heap_size;
    *base_free_space = free_space;

    /* Operations succeeded */
    return(0);

error:
    return(1);
} /* fill_root_row() */


/*-------------------------------------------------------------------------
 * Function:	fill_row
 *
 * Purpose:	Fill up a row of direct blocks in an non-root indirect block
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, April 10, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
fill_row(H5HF_t *fh, hid_t dxpl, const H5HF_create_t *cparam,
    hsize_t *heap_size, hsize_t *free_space, unsigned row,
    unsigned *nobjs_ptr)
{
    size_t      block_size;             /* Block size for row */
    unsigned    u;                      /* Local index variable */

    /* Sanity check */
    HDassert(nobjs_ptr);
    HDassert(heap_size);
    HDassert(free_space);

    /* Get the correct size for blocks in this row */
    if(row == 0)
        block_size = cparam->managed.start_block_size;
    else
        block_size = cparam->managed.start_block_size * (1 << (row - 1));

    /* Loop over filling direct blocks, until indirect row is full */
    for(u = 0; u < cparam->managed.width; u++) {
        /* Fill a direct heap block up */
        *free_space -= DBLOCK_FREE(fh, row);
        if(fill_heap(fh, dxpl, cparam, *heap_size, block_size, *free_space, nobjs_ptr))
            FAIL_STACK_ERROR
    } /* end for */

    /* Operations succeeded */
    return(0);

error:
    return(1);
} /* fill_row() */


/*-------------------------------------------------------------------------
 * Function:	fill_root_direct
 *
 * Purpose:	Insert (small) objects to fill up the free space in all direct
 *              heap blocks in the root indirect block
 *              (Generally used to create & fill up direct blocks in a new
 *              indirect block)
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, April  3, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
fill_root_direct(H5HF_t *fh, hid_t dxpl, const H5HF_create_t *cparam,
    hsize_t *heap_size, hsize_t *free_space, unsigned *nobjs)
{
    size_t      block_size;             /* Size of block added */
    unsigned    nrows;                  /* Number of rows inserted */

    /* Loop over rows */
    block_size = cparam->managed.start_block_size;
    nrows = 0;
    while(block_size <= cparam->managed.max_direct_size) {
        if(fill_root_row(fh, dxpl, cparam, heap_size, free_space, nrows, nobjs))
            FAIL_STACK_ERROR

        /* Adjust block size for row */
        if(nrows > 0)
            block_size *= 2;

        /* Increment row count */
        nrows++;
    } /* end for */

    /* Operations succeeded */
    return(0);

error:
    return(1);
} /* fill_root_direct() */


/*-------------------------------------------------------------------------
 * Function:	fill_2nd_indirect
 *
 * Purpose:	Insert (small) objects to fill up the free space in all direct
 *              heap blocks in a second-level indirect block (which only has
 *              direct blocks)
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, April 10, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
fill_2nd_indirect(H5HF_t *fh, hid_t dxpl, const H5HF_create_t *cparam,
    hsize_t *heap_size, hsize_t *free_space, unsigned *nobjs,
    unsigned pos)
{
    size_t      iblock_size;            /* Indirect block's size */
    size_t      max_dblock_size;        /* Max. size of direct block to add */
    size_t      dblock_size;            /* Size of direct block added */
    unsigned    nrows;                  /* Number of rows inserted */

    /* Loop over rows */
    nrows = 0;
    dblock_size = cparam->managed.start_block_size;
    iblock_size = cparam->managed.max_direct_size * (1 << pos);
    max_dblock_size = dblock_size * (1 << ((H5V_log2_of2(iblock_size / cparam->managed.width) -
            (H5V_log2_of2(cparam->managed.start_block_size) +
                H5V_log2_of2(cparam->managed.width))) + 1));
    while(dblock_size <= max_dblock_size) {
        if(fill_row(fh, dxpl, cparam, heap_size, free_space, nrows, nobjs))
            FAIL_STACK_ERROR

        /* Adjust block size for row */
        if(nrows > 0)
            dblock_size *= 2;

        /* Increment row count */
        nrows++;
    } /* end for */

    /* Operations succeeded */
    return(0);

error:
    return(1);
} /* fill_2nd_direct() */


/*-------------------------------------------------------------------------
 * Function:	fill_all_direct
 *
 * Purpose:	Insert (small) objects to fill up the free space in all direct
 *              heap blocks up to the maximum direct block size
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, April 10, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
fill_all_direct(H5HF_t *fh, hid_t dxpl, const H5HF_create_t *cparam,
    hsize_t *heap_size, hsize_t *free_space, unsigned *nobjs)
{
    size_t      dblock_size;            /* Size of direct block added */
    unsigned    nrows;                  /* Number of rows inserted */

    /* Loop over rows */
    nrows = 0;
    dblock_size = cparam->managed.start_block_size;
    while(dblock_size <= cparam->managed.max_direct_size) {
        if(fill_row(fh, dxpl, cparam, heap_size, free_space, nrows, nobjs))
            FAIL_STACK_ERROR

        /* Adjust block size for row */
        if(nrows > 0)
            dblock_size *= 2;

        /* Increment row count */
        nrows++;
    } /* end for */

    /* Operations succeeded */
    return(0);

error:
    return(1);
} /* fill_all_direct() */


/*-------------------------------------------------------------------------
 * Function:	fill_2nd_indirect_row
 *
 * Purpose:	Insert (small) objects to fill up the free space in all direct
 *              heap blocks in a row of second-level indirect block (which only
 *              have direct blocks)
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, April 10, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
fill_2nd_indirect_row(H5HF_t *fh, hid_t dxpl, const H5HF_create_t *cparam,
    hsize_t *heap_size, hsize_t *free_space, unsigned *nobjs,
    unsigned pos)
{
    unsigned    u;                      /* Local index variable */

    /* Loop over row of indirect blocks */
    for(u = 0; u < cparam->managed.width; u++)
        if(fill_2nd_indirect(fh, dxpl, cparam, heap_size, free_space, nobjs, pos))
            FAIL_STACK_ERROR

    /* Operations succeeded */
    return(0);

error:
    return(1);
} /* fill_2nd_direct_row() */


/*-------------------------------------------------------------------------
 * Function:	fill_all_2nd_indirect_rows
 *
 * Purpose:	Insert (small) objects to fill up the free space in all direct
 *              heap blocks in all rows of second-level indirect blocks (which only
 *              have direct blocks)
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, April 10, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
fill_all_2nd_indirect_rows(H5HF_t *fh, hid_t dxpl, const H5HF_create_t *cparam,
    hsize_t *heap_size, hsize_t *free_space, unsigned *nobjs)
{
    unsigned    u;                      /* Local index variable */

    /* Loop over rows of 2nd level deep indirect blocks */
    for(u = 0; u < (H5V_log2_of2(cparam->managed.width) + 1); u++)
        if(fill_2nd_indirect_row(fh, dxpl, cparam, heap_size, free_space, nobjs, (u + 1)))
            FAIL_STACK_ERROR

    /* Operations succeeded */
    return(0);

error:
    return(1);
} /* fill_2nd_direct_row() */


/*-------------------------------------------------------------------------
 * Function:	fill_3rd_indirect
 *
 * Purpose:	Insert (small) objects to fill up the free space in all direct
 *              heap blocks in a third-level indirect block (which 
 *              has one more level of indirect blocks)
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, April 18, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
fill_3rd_indirect(H5HF_t *fh, hid_t dxpl, const H5HF_create_t *cparam,
    hsize_t *heap_size, hsize_t *free_space, unsigned *nobjs,
    unsigned pos)
{
    unsigned    u;                      /* Local index variable */

    /* Fill all direct block rows in third level indirect block */
    if(fill_all_direct(fh, dxpl, cparam, heap_size, free_space, nobjs))
        FAIL_STACK_ERROR

    /* Fill rows of recursive indirect blocks in third level indirect block */
    for(u = 0; u < pos; u++)
        if(fill_2nd_indirect_row(fh, dxpl, cparam, heap_size, free_space, nobjs, (u + 1)))
            FAIL_STACK_ERROR

    /* Operations succeeded */
    return(0);

error:
    return(1);
} /* fill_3rd_indirect() */


/*-------------------------------------------------------------------------
 * Function:	fill_3rd_indirect_row
 *
 * Purpose:	Insert (small) objects to fill up the free space in all direct
 *              heap blocks in a row of third-level indirect block (which 
 *              have one more level of indirect blocks)
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, April 10, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
fill_3rd_indirect_row(H5HF_t *fh, hid_t dxpl, const H5HF_create_t *cparam,
    hsize_t *heap_size, hsize_t *free_space, unsigned *nobjs,
    unsigned pos)
{
    unsigned    u;              /* Local index variable */

    /* Loop over row of 3rd level indirect blocks */
    for(u = 0; u < cparam->managed.width; u++)
        /* Fill third level indirect block */
        if(fill_3rd_indirect(fh, dxpl, cparam, heap_size, free_space, nobjs, pos))
            FAIL_STACK_ERROR

    /* Operations succeeded */
    return(0);

error:
    return(1);
} /* fill_3rd_direct_row() */


/*-------------------------------------------------------------------------
 * Function:	fill_all_3rd_indirect_rows
 *
 * Purpose:	Insert (small) objects to fill up the free space in all direct
 *              heap blocks in all rows of third-level indirect blocks (which 
 *              have one more level of indirect blocks)
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, April 10, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
fill_all_3rd_indirect_rows(H5HF_t *fh, hid_t dxpl, const H5HF_create_t *cparam,
    hsize_t *heap_size, hsize_t *free_space, unsigned *nobjs)
{
    unsigned    u;                      /* Local index variable */

    /* Loop over rows of 3rd level deep indirect blocks */
    for(u = 0; u < (H5V_log2_of2(cparam->managed.width) + 1); u++)
        /* Fill row of 3rd level indirect blocks */
        if(fill_3rd_indirect_row(fh, dxpl, cparam, heap_size, free_space, nobjs, u + 1))
            FAIL_STACK_ERROR

    /* Operations succeeded */
    return(0);

error:
    return(1);
} /* fill_all_3rd_direct_rows() */


/*-------------------------------------------------------------------------
 * Function:	fill_4th_indirect_row
 *
 * Purpose:	Insert (small) objects to fill up the free space in all direct
 *              heap blocks in a row of fourth-level indirect blocks (which 
 *              have two more levels of indirect blocks)
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, April 10, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
fill_4th_indirect_row(H5HF_t *fh, hid_t dxpl, const H5HF_create_t *cparam,
    hsize_t *heap_size, hsize_t *free_space, unsigned *nobjs,
    unsigned pos)
{
    unsigned    u, v;                   /* Local index variables */

    /* Loop over row of 4th level indirect blocks */
    for(u = 0; u < cparam->managed.width; u++) {
        /* Fill all direct block rows in fourth level indirect block */
        if(fill_all_direct(fh, dxpl, cparam, heap_size, free_space, nobjs))
            FAIL_STACK_ERROR

        /* Fill all rows of 2nd level deep indirect blocks in fourth level indirect block */
        if(fill_all_2nd_indirect_rows(fh, dxpl, cparam, heap_size, free_space, nobjs))
            FAIL_STACK_ERROR

        /* Fill rows of third level indirect blocks in fourth level indirect block */
        for(v = 0; v < pos; v++)
            if(fill_3rd_indirect_row(fh, dxpl, cparam, heap_size, free_space, nobjs, (v + 1)))
                FAIL_STACK_ERROR
    } /* end for */

    /* Operations succeeded */
    return(0);

error:
    return(1);
} /* fill_4th_direct_row() */


/*-------------------------------------------------------------------------
 * Function:	fill_all_4th_indirect_rows
 *
 * Purpose:	Insert (small) objects to fill up the free space in all direct
 *              heap blocks in all rows of fourth-level indirect blocks (which 
 *              have two more levels of indirect blocks)
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, April 10, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
fill_all_4th_indirect_rows(H5HF_t *fh, hid_t dxpl, const H5HF_create_t *cparam,
    hsize_t *heap_size, hsize_t *free_space, unsigned *nobjs)
{
    unsigned    u;                      /* Local index variable */

    /* Loop over rows of 4th level deep indirect blocks */
    for(u = 0; u < (H5V_log2_of2(cparam->managed.width) + 1); u++) {
        /* Fill row of 4th level indirect blocks */
        if(fill_4th_indirect_row(fh, dxpl, cparam, heap_size, free_space, nobjs, (u + 1)))
            FAIL_STACK_ERROR

        /* Account for root indirect block doubling # of rows again */
        /* (From 16 rows to the max. # of rows: 22) */
        if(u == 0) {
            hsize_t block_size;         /* Current block size */
            hsize_t max_block_size;     /* Maximum size of blocks in heap */
            unsigned row;               /* Row in heap */

            /* Compute current & maximum block size in heap */
            block_size = cparam->managed.start_block_size * ((hsize_t)1 << 15);
            max_block_size = (((hsize_t)1 << cparam->managed.max_index) / 2) /
                    cparam->managed.width;

            /* Increase heap size & free space */
            row = 16;
            while(block_size <= max_block_size) {
                *heap_size += cparam->managed.width * block_size;
                *free_space += cparam->managed.width * DBLOCK_FREE(fh, row);

                /* Advance to next row */
                block_size *= 2;
                row++;
            } /* end while */
        } /* end if */
    } /* end for */

    /* Operations succeeded */
    return(0);

error:
    return(1);
} /* fill_all_4th_direct_rows() */


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
test_create(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t UNUSED *tparam)
{
    hid_t	file = -1;              /* File ID */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_create_t test_cparam;          /* Creation parameters for heap */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
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
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(check_stats(fh, (hsize_t)0, (hsize_t)0, (hsize_t)0, (hsize_t)0, (hsize_t)0))
        FAIL_STACK_ERROR
    PASSED()

    /* Query the type of address mapping */
    TESTING("query heap creation parameters");
    HDmemset(&test_cparam, 0, sizeof(H5HF_create_t));
    if(H5HF_get_cparam_test(fh, &test_cparam) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(cparam, &test_cparam, sizeof(H5HF_create_t)))
        FAIL_STACK_ERROR
    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, H5P_DATASET_XFER_DEFAULT) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, H5P_DATASET_XFER_DEFAULT);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_create() */


/*-------------------------------------------------------------------------
 * Function:	test_reopen
 *
 * Purpose:	Create & reopen a fractal heap
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, April 18, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_reopen(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t UNUSED *tparam)
{
    hid_t	file = -1;              /* File ID */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_create_t test_cparam;          /* Creation parameters for heap */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
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

    TESTING("reopening existing fractal heap (w/absolute address mapping)");

    /* Create heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    if(check_stats(fh, (hsize_t)0, (hsize_t)0, (hsize_t)0, (hsize_t)0, (hsize_t)0))
        FAIL_STACK_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, H5P_DATASET_XFER_DEFAULT) < 0)
        TEST_ERROR

    /* Re-open the heap */
    if(NULL == (fh = H5HF_open(f, H5P_DATASET_XFER_DEFAULT, fh_addr)))
        FAIL_STACK_ERROR

    /* Query the type of address mapping */
    HDmemset(&test_cparam, 0, sizeof(H5HF_create_t));
    if(H5HF_get_cparam_test(fh, &test_cparam) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(cparam, &test_cparam, sizeof(H5HF_create_t)))
        FAIL_STACK_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, H5P_DATASET_XFER_DEFAULT) < 0)
        TEST_ERROR
    PASSED()

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, H5P_DATASET_XFER_DEFAULT);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_reopen() */

#ifdef ALL_INSERT_TESTS

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
test_abs_insert_first(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    hsize_t     free_space;             /* Size of free space in heap */
    hsize_t     heap_size;              /* Total size of the heap */
    unsigned    nobjs = 0;              /* Total number of objects inserted */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, dxpl, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    if(check_stats(fh, (hsize_t)0, (hsize_t)0, (hsize_t)0, (hsize_t)0, (hsize_t)0))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close (empty) heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /*
     * Test inserting first (small) object into absolute heap
     */
    TESTING("inserting first (small) object into absolute heap");
    free_space = DBLOCK_FREE(fh, 0);
    heap_size = cparam->managed.start_block_size;
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 10, SMALL_OBJ_SIZE1))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Check for correctly sized heap */
    if(check_stats(fh, heap_size, heap_size, (hsize_t)0, free_space, (hsize_t)1))
        FAIL_STACK_ERROR

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
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
test_abs_insert_second(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    hsize_t     free_space;             /* Size of free space in heap */
    unsigned    nobjs = 0;              /* Total number of objects inserted */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, dxpl, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR

    /*
     * Test inserting first (small) object into absolute heap
     */
    TESTING("inserting two (small) objects into absolute heap");
    free_space = DBLOCK_FREE(fh, 0);
    if(add_obj(fh, dxpl, (hsize_t)cparam->managed.start_block_size, &free_space, &nobjs, 10, SMALL_OBJ_SIZE1))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert second object */
    if(add_obj(fh, dxpl, (hsize_t)cparam->managed.start_block_size, &free_space, &nobjs, 20, SMALL_OBJ_SIZE2))
        FAIL_STACK_ERROR

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
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
test_abs_insert_root_mult(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    nobjs = 0;              /* Number of objects inserted */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, dxpl, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR

    /*
     * Test inserting mult. (small) object into absolute heap
     */
    TESTING("inserting objects to fill absolute heap's root direct block");

    /* Fill the heap up */
    if(fill_heap(fh, dxpl, cparam, (hsize_t)cparam->managed.start_block_size, cparam->managed.start_block_size, (hsize_t)0, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
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
test_abs_insert_force_indirect(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    hsize_t     free_space;             /* Size of free space in heap */
    hsize_t     heap_size;              /* Total size of the heap */
    unsigned    nobjs = 0;              /* Number of objects inserted */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR

    /*
     * Test forcing creation of indirect root block & second direct block
     */
    TESTING("inserting objects to create root indirect block");

    /* Fill the heap up */
    if(fill_heap(fh, dxpl, cparam, (hsize_t)cparam->managed.start_block_size, cparam->managed.start_block_size, (hsize_t)0, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert one more object, to force root indirect block creation */
    free_space = (cparam->managed.width - 1) * DBLOCK_FREE(fh, 0);
    heap_size = cparam->managed.width * cparam->managed.start_block_size;
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 10, SMALL_OBJ_SIZE1))
        FAIL_STACK_ERROR

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
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
test_abs_insert_fill_second(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    hsize_t     free_space;             /* Size of free space in heap */
    hsize_t     heap_size;              /* Total size of the heap */
    unsigned    nobjs = 0;              /* Number of objects inserted */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR

    /*
     * Test inserting mult. (small) objects to fill second direct block
     */
    TESTING("inserting objects to fill second direct block");

    /* Fill the first direct block heap up */
    if(fill_heap(fh, dxpl, cparam, (hsize_t)cparam->managed.start_block_size, cparam->managed.start_block_size, (hsize_t)0, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill the second direct block heap up (also creates initial root indirect block) */
    free_space = (cparam->managed.width - 2) * DBLOCK_FREE(fh, 0);
    heap_size = cparam->managed.width * cparam->managed.start_block_size;
    if(fill_heap(fh, dxpl, cparam, heap_size, cparam->managed.start_block_size, free_space, &nobjs))
        FAIL_STACK_ERROR

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
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
test_abs_insert_third_direct(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    hsize_t     free_space;             /* Size of free space in heap */
    hsize_t     heap_size;              /* Total size of the heap */
    unsigned    nobjs = 0;              /* Number of objects inserted */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR

    /*
     * Test inserting mult. (small) objects to create third direct block
     */
    TESTING("inserting objects to create third direct block");

    /* Fill the first direct block up */
    if(fill_heap(fh, dxpl, cparam, (hsize_t)cparam->managed.start_block_size, cparam->managed.start_block_size, (hsize_t)0, &nobjs))
        FAIL_STACK_ERROR

    /* Fill the second direct block heap up (also creates initial root indirect block) */
    free_space = (cparam->managed.width - 2) * DBLOCK_FREE(fh, 0);
    heap_size = cparam->managed.width * cparam->managed.start_block_size;
    if(fill_heap(fh, dxpl, cparam, heap_size, cparam->managed.start_block_size, free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert one more object, to force creation of third direct block */
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 10, SMALL_OBJ_SIZE1))
        FAIL_STACK_ERROR

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
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
test_abs_fill_first_row(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    hsize_t     free_space;             /* Size of free space in heap */
    hsize_t     heap_size;              /* Total size of heap */
    unsigned    nobjs = 0;              /* Total number of objects inserted */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR

    /*
     * Test inserting mult. (small) objects to fill first row in root indirect block
     */
    TESTING("inserting objects to fill first row of root indirect block");

    /* Fill first row of [root] indirect block */
    heap_size = 0;
    free_space = 0;
    if(fill_root_row(fh, dxpl, cparam, &heap_size, &free_space, 0, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
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
test_abs_start_second_row(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    hsize_t     free_space;             /* Size of free space in heap */
    hsize_t     heap_size;              /* Total size of the heap */
    unsigned    nobjs = 0;              /* Number of objects inserted */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR

    /*
     * Test inserting mult. (small) objects to start second row in root indirect block
     */
    TESTING("inserting objects to start second row of root indirect block");

    /* Fill first root indirect row */
    heap_size = 0;
    free_space = 0;
    if(fill_root_row(fh, dxpl, cparam, &heap_size, &free_space, 0, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert one more object, to force expanding root indirect block to two rows */
    heap_size += cparam->managed.width * cparam->managed.start_block_size;
    free_space += cparam->managed.width * DBLOCK_FREE(fh, 1);
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 10, SMALL_OBJ_SIZE1))
        FAIL_STACK_ERROR

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
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
test_abs_fill_second_row(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    hsize_t     free_space;             /* Size of free space in heap */
    hsize_t     heap_size;              /* Total size of the heap */
    unsigned    nobjs = 0;              /* Number of objects inserted */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR

    /*
     * Test inserting mult. (small) objects to start second row in root indirect block
     */
    TESTING("inserting objects to fill second row of root indirect block");

    /* Fill first root indirect row */
    heap_size = 0;
    free_space = 0;
    if(fill_root_row(fh, dxpl, cparam, &heap_size, &free_space, 0, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill second root indirect row */
    if(fill_root_row(fh, dxpl, cparam, &heap_size, &free_space, 1, &nobjs))
        FAIL_STACK_ERROR

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
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
test_abs_start_third_row(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    hsize_t     free_space;             /* Size of free space in heap */
    hsize_t     heap_size;              /* Total size of heap */
    unsigned    nobjs = 0;              /* Number of objects inserted */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR

    /*
     * Test inserting mult. (small) objects to start third row in root indirect block
     */
    TESTING("inserting objects to start third row of root indirect block");

    /* Fill first root indirect row */
    heap_size = 0;
    free_space = 0;
    if(fill_root_row(fh, dxpl, cparam, &heap_size, &free_space, 0, &nobjs))
        FAIL_STACK_ERROR

    /* Fill second root indirect row */
    if(fill_root_row(fh, dxpl, cparam, &heap_size, &free_space, 1, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert one more object, to force expanding root indirect block to four rows */
    /* (Goes to four rows because it's doubling) */
    heap_size += cparam->managed.width * cparam->managed.start_block_size * 2;
    heap_size += cparam->managed.width * cparam->managed.start_block_size * 4;
    free_space += cparam->managed.width * DBLOCK_FREE(fh, 2);
    free_space += cparam->managed.width * DBLOCK_FREE(fh, 3);
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 10, SMALL_OBJ_SIZE1))
        FAIL_STACK_ERROR

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
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
test_abs_fill_fourth_row(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    hsize_t     free_space;             /* Size of free space in heap */
    hsize_t     heap_size;              /* Total size of heap */
    unsigned    u;                      /* Local index variables */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR

    /*
     * Test inserting mult. (small) objects to fill four rows in root indirect block
     */
    TESTING("inserting objects to fill four rows of root indirect block");

    /* Loop over rows */
    heap_size = 0;
    free_space = 0;
    for(u = 0; u < 4; u++)
        if(fill_root_row(fh, dxpl, cparam, &heap_size, &free_space, u, &nobjs))
            FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
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
test_abs_fill_all_root_direct(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    hsize_t     free_space;             /* Size of free space in heap */
    hsize_t     heap_size;              /* Total size of heap */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR

    /*
     * Test inserting mult. (small) objects to fill all direct  rows in root indirect block
     */
    TESTING("inserting objects to fill all direct rows of root indirect block");

    /* Fill all direct blocks in root indirect block */
    heap_size = 0;
    free_space = 0;
    if(fill_root_direct(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_abs_fill_all_root_direct() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_first_recursive_indirect
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
test_abs_first_recursive_indirect(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    hsize_t     free_space;             /* Size of free space in heap */
    hsize_t     heap_size;              /* Total size of heap */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR

    /*
     * Test inserting mult. (small) objects to force creation of first recursive indirect block
     */
    TESTING("inserting objects to create first recursive indirect block");

    /* Fill direct blocks up */
    heap_size = 0;
    free_space = 0;
    if(fill_root_direct(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert one more object, to force creation of first recursive indirect block */
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 10, SMALL_OBJ_SIZE1))
        FAIL_STACK_ERROR

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_abs_first_recursive_indirect() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_second_direct_recursive_indirect
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
test_abs_second_direct_recursive_indirect(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    hsize_t     free_space;             /* Size of free space in heap */
    hsize_t     heap_size;              /* Total size of heap */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR

    /*
     * Test inserting mult. (small) objects to force creation of second direct
     *  block in first recursive indirect block
     */
    TESTING("inserting objects to create second direct block in first recursive indirect block");

    /* Fill direct blocks up */
    heap_size = 0;
    free_space = 0;
    if(fill_root_direct(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Fill the first direct block in the recursive indirect block up */
    free_space -= DBLOCK_FREE(fh, 0);
    if(fill_heap(fh, dxpl, cparam, heap_size, cparam->managed.start_block_size, free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert one more object, to force creation of second direct block in
     * first recursive indirect block
     */
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 10, SMALL_OBJ_SIZE1))
        FAIL_STACK_ERROR

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
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
test_abs_fill_first_recursive_indirect(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    hsize_t     free_space;             /* Size of free space in heap */
    hsize_t     heap_size;              /* Total size of heap */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in first recursive indirect block
     */
    TESTING("inserting objects to fill all direct blocks in first recursive indirect block");

    /* Fill direct blocks up in root indirect block */
    heap_size = 0;
    free_space = 0;
    if(fill_root_direct(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill first recursive indirect block */
    if(fill_2nd_indirect(fh, dxpl, cparam, &heap_size, &free_space, &nobjs, 1))
        FAIL_STACK_ERROR

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
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
test_abs_second_recursive_indirect(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    hsize_t     free_space;             /* Size of free space in heap */
    hsize_t     heap_size;              /* Total size of heap */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in first recursive indirect block
     */
    TESTING("inserting objects to start second recursive indirect block");

    /* Fill direct blocks up in root indirect block */
    heap_size = 0;
    free_space = 0;
    if(fill_root_direct(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Fill first recursive indirect block */
    if(fill_2nd_indirect(fh, dxpl, cparam, &heap_size, &free_space, &nobjs, 1))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert one more object, to force creation of second 
     * recursive indirect block
     */
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 10, SMALL_OBJ_SIZE1))
        FAIL_STACK_ERROR

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
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
test_abs_fill_second_recursive_indirect(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    hsize_t     free_space;             /* Size of free space in heap */
    hsize_t     heap_size;              /* Total size of heap */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in first recursive indirect block
     */
    TESTING("inserting objects to fill all direct blocks in second recursive indirect block");

    /* Fill direct blocks up in root indirect block */
    heap_size = 0;
    free_space = 0;
    if(fill_root_direct(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Fill first recursive indirect block */
    if(fill_2nd_indirect(fh, dxpl, cparam, &heap_size, &free_space, &nobjs, 1))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill 2nd recursive indirect block */
    if(fill_2nd_indirect(fh, dxpl, cparam, &heap_size, &free_space, &nobjs, 1))
        FAIL_STACK_ERROR

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
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
test_abs_fill_recursive_indirect_row(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    hsize_t     free_space;             /* Size of free space in heap */
    hsize_t     heap_size;              /* Total size of heap */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in first recursive indirect block
     */
    TESTING("inserting objects to fill all direct blocks in first row of recursive indirect block");

    /* Fill direct blocks in root indirect block up */
    heap_size = 0;
    free_space = 0;
    if(fill_root_direct(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill row of recursive indirect blocks */
    if(fill_2nd_indirect_row(fh, dxpl, cparam, &heap_size, &free_space, &nobjs, 1))
        FAIL_STACK_ERROR

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
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
test_abs_start_2nd_recursive_indirect(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    hsize_t     free_space;             /* Size of free space in heap */
    hsize_t     heap_size;              /* Total size of heap */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in first recursive indirect block
     */
    TESTING("inserting objects to start second row of recursive indirect blocks");

    /* Fill direct blocks in root indirect block up */
    heap_size = 0;
    free_space = 0;
    if(fill_root_direct(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Fill row of recursive indirect blocks */
    if(fill_2nd_indirect_row(fh, dxpl, cparam, &heap_size, &free_space, &nobjs, 1))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert one more object, to force creation of second 
     * recursive indirect block
     */
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 10, SMALL_OBJ_SIZE1))
        FAIL_STACK_ERROR

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
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
test_abs_recursive_indirect_two_deep(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    hsize_t     free_space;             /* Size of free space in heap */
    hsize_t     heap_size;              /* Total size of heap */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in first recursive indirect block
     */
    TESTING("inserting objects to fill recursive indirect blocks two levels deep");

    /* Fill direct blocks up in root indirect block */
    heap_size = 0;
    free_space = 0;
    if(fill_root_direct(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill all rows of 2nd level indirect blocks */
    if(fill_all_2nd_indirect_rows(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
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
test_abs_start_3rd_recursive_indirect(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    hsize_t     free_space;             /* Size of free space in heap */
    hsize_t     heap_size;              /* Total size of heap */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in first recursive indirect block
     */
    TESTING("inserting objects to start recursive indirect blocks three levels deep");

    /* Fill direct blocks up in root indirect block */
    heap_size = 0;
    free_space = 0;
    if(fill_root_direct(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Fill all rows of 2nd level indirect blocks */
    if(fill_all_2nd_indirect_rows(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert one more object, to force creation of third level deep
     * recursive indirect block
     */
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 10, SMALL_OBJ_SIZE1))
        FAIL_STACK_ERROR

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
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
test_abs_fill_first_3rd_recursive_indirect(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    hsize_t     free_space;             /* Size of free space in heap */
    hsize_t     heap_size;              /* Total size of heap */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in first recursive indirect block
     */
    TESTING("inserting objects to fill first indirect block of recursive indirect blocks three levels deep");

    /* Fill direct blocks up in root indirect block */
    heap_size = 0;
    free_space = 0;
    if(fill_root_direct(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Fill all rows of 2nd level indirect blocks */
    if(fill_all_2nd_indirect_rows(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Fill all direct block rows in third level indirect block */
    if(fill_all_direct(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill row of recursive indirect blocks in third level indirect block */
    if(fill_2nd_indirect_row(fh, dxpl, cparam, &heap_size, &free_space, &nobjs, 1))
        FAIL_STACK_ERROR

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
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
test_abs_fill_3rd_recursive_indirect_row(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    hsize_t     free_space;             /* Size of free space in heap */
    hsize_t     heap_size;              /* Total size of heap */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in first recursive indirect block
     */
    TESTING("inserting objects to fill row of indirect blocks in recursive indirect blocks three levels deep");

    /* Fill direct blocks up in root indirect block */
    heap_size = 0;
    free_space = 0;
    if(fill_root_direct(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Fill all rows of 2nd level indirect blocks */
    if(fill_all_2nd_indirect_rows(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill 1st row of 3rd level indirect blocks */
    if(fill_3rd_indirect_row(fh, dxpl, cparam, &heap_size, &free_space, &nobjs, 1))
        FAIL_STACK_ERROR

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
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
test_abs_fill_all_3rd_recursive_indirect(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    hsize_t     free_space;             /* Size of free space in heap */
    hsize_t     heap_size;              /* Total size of heap */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in first recursive indirect block
     */
    TESTING("inserting objects to fill row of indirect blocks in recursive indirect blocks three levels deep");

    /* Fill direct blocks up in root indirect block */
    heap_size = 0;
    free_space = 0;
    if(fill_root_direct(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Fill all rows of 2nd level indirect blocks */
    if(fill_all_2nd_indirect_rows(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill all rows of 3rd level indirect blocks */
    if(fill_all_3rd_indirect_rows(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
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
test_abs_start_4th_recursive_indirect(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    hsize_t     free_space;             /* Size of free space in heap */
    hsize_t     heap_size;              /* Total size of heap */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in first recursive indirect block
     */
    TESTING("inserting objects to start first direct block in recursive indirect blocks four levels deep");

    /* Fill direct blocks up in root indirect block */
    heap_size = 0;
    free_space = 0;
    if(fill_root_direct(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Fill all rows of 2nd level indirect blocks */
    if(fill_all_2nd_indirect_rows(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Fill all rows of 3rd level indirect blocks */
    if(fill_all_3rd_indirect_rows(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert one more object, to force creation of four level deep
     * recursive indirect block
     */
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 10, SMALL_OBJ_SIZE1))
        FAIL_STACK_ERROR

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
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
test_abs_fill_first_4th_recursive_indirect(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    hsize_t     free_space;             /* Size of free space in heap */
    hsize_t     heap_size;              /* Total size of heap */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in first recursive indirect block
     */
    TESTING("inserting objects to fill first (3rd level) indirect block in recursive indirect block four levels deep");

    /* Fill direct blocks up in root indirect block */
    heap_size = 0;
    free_space = 0;
    if(fill_root_direct(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Fill all rows of 2nd level indirect blocks */
    if(fill_all_2nd_indirect_rows(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Fill all rows of 3rd level indirect blocks */
    if(fill_all_3rd_indirect_rows(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Fill direct block rows in fourth level indirect block */
    if(fill_all_direct(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Fill all rows of 2nd level deep indirect blocks in fourth level indirect block */
    if(fill_all_2nd_indirect_rows(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill first row of 3rd level deep indirect blocks in fourth level indirect block */
    if(fill_3rd_indirect_row(fh, dxpl, cparam, &heap_size, &free_space, &nobjs, 1))
        FAIL_STACK_ERROR

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
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
test_abs_fill_4th_recursive_indirect_row(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    hsize_t     free_space;             /* Size of free space in heap */
    hsize_t     heap_size;              /* Total size of heap */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in first recursive indirect block
     */
    TESTING("inserting objects to fill first row of recursive indirect blocks four levels deep");

    /* Fill direct blocks up in root indirect block */
    heap_size = 0;
    free_space = 0;
    if(fill_root_direct(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Fill all rows of 2nd level indirect blocks */
    if(fill_all_2nd_indirect_rows(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Fill all rows of 3rd level indirect blocks */
    if(fill_all_3rd_indirect_rows(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill 1st row of 4th level indirect blocks */
    if(fill_4th_indirect_row(fh, dxpl, cparam, &heap_size, &free_space, &nobjs, 1))
        FAIL_STACK_ERROR

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
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
test_abs_fill_all_4th_recursive_indirect(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    hsize_t     free_space;             /* Size of free space in heap */
    hsize_t     heap_size;              /* Total size of heap */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in recursive indirect blocks four levels deep
     */
    TESTING("inserting objects to fill all rows of recursive indirect blocks four levels deep");

    /* Fill direct blocks up in root indirect block */
    heap_size = 0;
    free_space = 0;
    if(fill_root_direct(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Fill all rows of 2nd level indirect blocks */
    if(fill_all_2nd_indirect_rows(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Fill all rows of 3rd level indirect blocks */
    if(fill_all_3rd_indirect_rows(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill all rows of 4th level indirect blocks */
    if(fill_all_4th_indirect_rows(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_abs_fill_all_4th_recursive_indirect() */
#endif /* ALL_INSERT_TESTS */

#ifndef QAK

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
test_abs_start_5th_recursive_indirect(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    hsize_t     free_space;             /* Size of free space in heap */
    hsize_t     heap_size;              /* Total size of heap */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in recursive indirect blocks four levels deep and add one more
     *  block, to make a five level deep structure
     */
    TESTING("inserting objects to create first direct block in recursive indirect blocks five levels deep");

    /* Fill direct blocks up in root indirect block */
    heap_size = 0;
    free_space = 0;
    if(fill_root_direct(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill all rows of 2nd level indirect blocks */
    if(fill_all_2nd_indirect_rows(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill all rows of 3rd level indirect blocks */
    if(fill_all_3rd_indirect_rows(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill all rows of 4th level indirect blocks */
    if(fill_all_4th_indirect_rows(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert one more object, to force creation of five level deep
     * recursive indirect block
     */
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 10, SMALL_OBJ_SIZE1))
        FAIL_STACK_ERROR

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_abs_start_5th_recursive_indirect() */
#endif /* QAK */

#ifndef QAK

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
test_abs_skip_start_block(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    size_t      obj_size;               /* Size of object */
    hsize_t     free_space;             /* Size of free space in heap */
    hsize_t     heap_size;              /* Total size of heap */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    if(check_stats(fh, (hsize_t)0, (hsize_t)0, (hsize_t)0, (hsize_t)0, (hsize_t)0))
        FAIL_STACK_ERROR

    /*
     * Test inserting object into absolute heap which doesn't fit into starting
     *  block size
     */
    TESTING("inserting object that is too large for starting block");

    obj_size = cparam->managed.start_block_size + 1;
    free_space = 2 * cparam->managed.width * DBLOCK_FREE(fh, 0);
    free_space += cparam->managed.width * DBLOCK_FREE(fh, 2);
    heap_size = 2 * cparam->managed.width * cparam->managed.start_block_size;
    heap_size += cparam->managed.width * (cparam->managed.start_block_size * 2);
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 10, obj_size))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
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
test_abs_skip_start_block_add_back(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    size_t      obj_size;               /* Size of object */
    hsize_t     free_space;             /* Size of free space in heap */
    hsize_t     heap_size;              /* Total size of heap */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    if(check_stats(fh, (hsize_t)0, (hsize_t)0, (hsize_t)0, (hsize_t)0, (hsize_t)0))
        FAIL_STACK_ERROR

    /*
     * Test inserting object into absolute heap which doesn't fit into starting
     *  block size
     */
    TESTING("skipping starting block, then adding object back to first block");

    /* Insert object too large for starting block size */
    obj_size = cparam->managed.start_block_size + 1;
    free_space = 2 * cparam->managed.width * DBLOCK_FREE(fh, 0);
    free_space += cparam->managed.width * DBLOCK_FREE(fh, 2);
    heap_size = 2 * cparam->managed.width * cparam->managed.start_block_size;
    heap_size += cparam->managed.width * (cparam->managed.start_block_size * 2);
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 10, obj_size))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert an object to fill up the heap block just created */
    obj_size = DBLOCK_FREE(fh, 2) - obj_size;
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 10, obj_size))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert second "real" object, which should go in earlier direct block */
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 20, SMALL_OBJ_SIZE2))
        FAIL_STACK_ERROR

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
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
test_abs_skip_start_block_add_skipped(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    size_t      obj_size;               /* Size of object */
    hsize_t     free_space;             /* Size of free space in heap */
    hsize_t     heap_size;              /* Total size of heap */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    if(check_stats(fh, (hsize_t)0, (hsize_t)0, (hsize_t)0, (hsize_t)0, (hsize_t)0))
        FAIL_STACK_ERROR

    /*
     * Test inserting object into absolute heap which doesn't fit into starting
     *  block size
     */
    TESTING("skipping starting block, then adding objects to backfill and extend");

    /* Insert object too large for starting block size */
    obj_size = cparam->managed.start_block_size + 1;
    free_space = 2 * cparam->managed.width * DBLOCK_FREE(fh, 0);
    free_space += cparam->managed.width * DBLOCK_FREE(fh, 2);
    heap_size = 2 * cparam->managed.width * cparam->managed.start_block_size;
    heap_size += cparam->managed.width * (cparam->managed.start_block_size * 2);
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 10, obj_size))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert an object to fill up the heap block just created */
    obj_size = DBLOCK_FREE(fh, 2) - obj_size;
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 10, obj_size))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Add rows of blocks to "backfill" direct blocks that were skipped */
    if(fill_row(fh, dxpl, cparam, &heap_size, &free_space, 0, &nobjs))
        FAIL_STACK_ERROR
    if(fill_row(fh, dxpl, cparam, &heap_size, &free_space, 1, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert another object, which should go extend direct blocks, instead of backfill */
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 20, SMALL_OBJ_SIZE2))
        FAIL_STACK_ERROR

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_abs_skip_start_block_add_skipped() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_skip_2nd_block
 *
 * Purpose:	Test inserting object into absolute heap which is small
 *              enough for starting block size, then add object too large
 *              for any blocks in first row of direct blocks, to force
 *              early creation of indirect block (and range of skipped blocks)
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Saturday, April  1, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_skip_2nd_block(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    size_t      obj_size;               /* Size of object */
    hsize_t     free_space;             /* Size of free space in heap */
    hsize_t     heap_size;              /* Total size of heap */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR

    /*
     * Test inserting first (small) object into absolute heap
     */
    TESTING("insert object to initial block, then add object too large for starting direct blocks");

    /* Insert small object, to create root direct block */
    obj_size = SMALL_OBJ_SIZE1;
    free_space = DBLOCK_FREE(fh, 0);
    heap_size = cparam->managed.start_block_size;
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 10, obj_size))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert large object, to force creation of indirect block and
     * range of skipped blocks that are too small to hold the second object
     */
    obj_size = cparam->managed.start_block_size + 1;
    free_space += (cparam->managed.width - 1 )* DBLOCK_FREE(fh, 0);
    free_space += cparam->managed.width * DBLOCK_FREE(fh, 1);
    free_space += cparam->managed.width * DBLOCK_FREE(fh, 2);
    heap_size += (cparam->managed.width - 1) * cparam->managed.start_block_size;
    heap_size += cparam->managed.width * cparam->managed.start_block_size;
    heap_size += cparam->managed.width * cparam->managed.start_block_size * 2;
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 20, obj_size))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_abs_skip_2nd_block() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_skip_2nd_block_add_skipped
 *
 * Purpose:	Test inserting object into absolute heap which is small
 *              enough for starting block size, then add object too large
 *              for any blocks in first row of direct blocks, to force
 *              early creation of indirect block (and range of skipped blocks).
 *              Then add more objects to fill up remainder of initial direct
 *              block and all the skipped blocks, and one more object (to
 *              start next "normal" block).
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Saturday, April  1, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_skip_2nd_block_add_skipped(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    size_t      obj_size;               /* Size of object */
    hsize_t     free_space;             /* Size of free space in heap */
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
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR

    /*
     * Test inserting first (small) object into absolute heap
     */
    TESTING("insert object to initial block, then add object too large for starting direct blocks, then backfill and extend");

    /* Insert small object, to create root direct block */
    obj_size = SMALL_OBJ_SIZE1;
    free_space = DBLOCK_FREE(fh, 0);
    heap_size = cparam->managed.start_block_size;
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 10, obj_size))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert large object, to force creation of indirect block and
     * range of skipped blocks that are too small to hold the second object
     */
    obj_size = cparam->managed.start_block_size + 1;
    free_space += (cparam->managed.width - 1 )* DBLOCK_FREE(fh, 0);
    free_space += cparam->managed.width * DBLOCK_FREE(fh, 1);
    free_space += cparam->managed.width * DBLOCK_FREE(fh, 2);
    heap_size += (cparam->managed.width - 1) * cparam->managed.start_block_size;
    heap_size += cparam->managed.width * cparam->managed.start_block_size;
    heap_size += cparam->managed.width * cparam->managed.start_block_size * 2;
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 20, obj_size))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert an object to fill up the (smaller) heap block just created */
    obj_size = DBLOCK_FREE(fh, 0) - SMALL_OBJ_SIZE1;
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 10, obj_size))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill remainder of 2 * start size block */
    obj_size = DBLOCK_FREE(fh, 2) - (cparam->managed.start_block_size + 1);
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 20, obj_size))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert objects to fill remaining rows of the starting block size */
    for(u = 0; u < 2; u++) {
        /* Fill a row of direct heap blocks up */
        for(v = 0; v < (cparam->managed.width - 1); v++) {
            free_space -= DBLOCK_FREE(fh, u);
            if(fill_heap(fh, dxpl, cparam, heap_size, cparam->managed.start_block_size, free_space, &nobjs))
                FAIL_STACK_ERROR
        } /* end for */
    } /* end for */

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert one more object, to create new 2 * start size direct block */
    obj_size = SMALL_OBJ_SIZE1;
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 10, obj_size))
        FAIL_STACK_ERROR

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_abs_skip_2nd_block_add_skipped() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_fill_one_partial_skip_2nd_block_add_skipped
 *
 * Purpose:	Test filling initial direct block, then add object small enough
 *              for initial block size (to create root indirect block), then
 *              add object too large for any blocks in first three rows of
 *              direct blocks, to force extension of indirect block (and range
 *              of skipped blocks).
 *
 *              Then add more objects to fill up remainder of partial direct
 *              block and all the skipped blocks, and one more object (to
 *              start next "normal" block).
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, April  3, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_fill_one_partial_skip_2nd_block_add_skipped(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    size_t      obj_size;               /* Size of object */
    hsize_t     free_space;             /* Size of free space in heap */
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
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR

    /*
     * Test absolute heap
     */
    TESTING("skipping blocks with indirect root, then backfill and extend");

    /* Fill initial direct block */
    heap_size = cparam->managed.start_block_size;
    free_space = 0;
    if(fill_heap(fh, dxpl, cparam, heap_size, cparam->managed.start_block_size, free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert small object, to create root indirect block */
    obj_size = SMALL_OBJ_SIZE1;
    heap_size += (cparam->managed.width - 1) * cparam->managed.start_block_size;
    free_space += (cparam->managed.width - 1) * DBLOCK_FREE(fh, 0);
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 10, obj_size))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert large object, to force creation of indirect block and
     * range of skipped blocks that are too small to hold the large object
     */
    obj_size = (2 * cparam->managed.start_block_size) + 1;
    heap_size += cparam->managed.width * cparam->managed.start_block_size;
    heap_size += cparam->managed.width * cparam->managed.start_block_size * 2;
    heap_size += cparam->managed.width * cparam->managed.start_block_size * 4;
    free_space += cparam->managed.width * DBLOCK_FREE(fh, 1);
    free_space += cparam->managed.width * DBLOCK_FREE(fh, 2);
    free_space += cparam->managed.width * DBLOCK_FREE(fh, 3);
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 20, obj_size))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert an object to fill up the (smaller) heap block just created */
    obj_size = DBLOCK_FREE(fh, 0) - SMALL_OBJ_SIZE1;
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 10, obj_size))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert object to fill remainder of 4 * start size block */
    obj_size = DBLOCK_FREE(fh, 3) - ((2 * cparam->managed.start_block_size) + 1);
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 20, obj_size))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert objects to fill remaining heaps in first row */
    for(u = 0; u < (cparam->managed.width - 2); u++) {
        /* Fill a direct heap block up */
        free_space -= DBLOCK_FREE(fh, 0);
        if(fill_heap(fh, dxpl, cparam, heap_size, cparam->managed.start_block_size, free_space, &nobjs))
            FAIL_STACK_ERROR
    } /* end for */

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert objects to fill remaining heaps in second row */
    if(fill_row(fh, dxpl, cparam, &heap_size, &free_space, 1, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert objects to fill remaining heaps in third row */
    if(fill_row(fh, dxpl, cparam, &heap_size, &free_space, 2, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert one more object, to create new 4 * start size direct block */
    obj_size = SMALL_OBJ_SIZE1;
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 10, obj_size))
        FAIL_STACK_ERROR

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_abs_fill_one_partial_skip_2nd_block_add_skipped() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_fill_row_skip_add_skipped
 *
 * Purpose:	Test filling first row of direct blocks, then
 *              add object too large for any blocks in first three rows of
 *              direct blocks, to force extension of indirect block (and range
 *              of skipped blocks).
 *
 *              Then add more objects to fill up remainder of partial direct
 *              block and all the skipped blocks, and one more object (to
 *              start next "normal" block).
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, May 15, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_fill_row_skip_add_skipped(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    size_t      obj_size;               /* Size of object */
    hsize_t     free_space;             /* Size of free space in heap */
    hsize_t     heap_size;              /* Total size of heap */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR

    /*
     * Test absolute heap
     */
    TESTING("filling first row, then skipping rows, then backfill and extend");

    /* Fill first row of direct blocks */
    heap_size = 0;
    free_space = 0;
    if(fill_root_row(fh, dxpl, cparam, &heap_size, &free_space, 0, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert large object, to force creation of indirect block and
     * range of skipped blocks that are too small to hold the large object
     */
    obj_size = (2 * cparam->managed.start_block_size) + 1;
    heap_size += cparam->managed.width * DBLOCK_SIZE(fh, 1);
    heap_size += cparam->managed.width * DBLOCK_SIZE(fh, 2);
    heap_size += cparam->managed.width * DBLOCK_SIZE(fh, 3);
    free_space += cparam->managed.width * DBLOCK_FREE(fh, 1);
    free_space += cparam->managed.width * DBLOCK_FREE(fh, 2);
    free_space += cparam->managed.width * DBLOCK_FREE(fh, 3);
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 20, obj_size))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert object to fill remainder of 4 * start size block */
    obj_size = DBLOCK_FREE(fh, 3) - obj_size;
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 20, obj_size))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert objects to fill remaining heaps in second row */
    if(fill_row(fh, dxpl, cparam, &heap_size, &free_space, 1, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert objects to fill remaining heaps in third row */
    if(fill_row(fh, dxpl, cparam, &heap_size, &free_space, 2, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert one more object, to create new 4 * start size direct block */
    obj_size = SMALL_OBJ_SIZE1;
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 10, obj_size))
        FAIL_STACK_ERROR

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_abs_fill_row_skip_add_skipped() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_fill_direct_skip_indirect_start_block_add_skipped
 *
 * Purpose:	Test filling all direct blocks in root indirect block, then
 *              add object too large for initial block in first row of direct
 *              blocks in indirect block, to force extension of non-root
 *              indirect block (and range of skipped blocks).
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, April  3, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_fill_direct_skip_indirect_start_block_add_skipped(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    size_t      obj_size;               /* Size of object */
    hsize_t     free_space;             /* Size of free space in heap */
    hsize_t     heap_size;              /* Total size of heap */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR

    /*
     * Test absolute heap
     */
    TESTING("filling direct blocks and skipping blocks in non-root indirect block, then backfill and extend");

    /* Fill direct blocks in root indirect block */
    heap_size = 0;
    free_space = 0;
    if(fill_root_direct(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert large object, to force creation of indirect block and
     * range of skipped blocks that are too small to hold the large object
     */
    obj_size = (2 * cparam->managed.start_block_size) + 1;
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 20, obj_size))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Add rows of blocks to "backfill" direct blocks that were skipped */
    if(fill_row(fh, dxpl, cparam, &heap_size, &free_space, 0, &nobjs))
        FAIL_STACK_ERROR
    if(fill_row(fh, dxpl, cparam, &heap_size, &free_space, 1, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert an object to fill up the (biggest) heap block created */
    obj_size = DBLOCK_FREE(fh, 3) - ((2 * cparam->managed.start_block_size) + 1);
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 20, obj_size))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill direct block heaps with 2 * initial block size in nested indirect block */
    if(fill_row(fh, dxpl, cparam, &heap_size, &free_space, 2, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert one more object, to create new 4 * start size direct block */
    obj_size = SMALL_OBJ_SIZE1;
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 10, obj_size))
        FAIL_STACK_ERROR

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_abs_fill_direct_skip_indirect_start_block_add_skipped() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_fill_direct_skip_2nd_indirect_start_block_add_skipped
 *
 * Purpose:	Test filling all direct blocks in root indirect block, then
 *              add object too large for all direct blocks in first row of 
 *              indirect blocks, to force skipping a row of indirect blocks
 *              (and range of skipped blocks), then backfill all direct blocks
 *              skipped and extend to next "normal" direct block.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, April  3, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_fill_direct_skip_2nd_indirect_start_block_add_skipped(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    first_row_bits;         /* Number of bits used bit addresses in first row */
    unsigned    first_indirect_block_size;      /* Range of addresses covered by each of the first indirect blocks */
    unsigned    num_first_indirect_rows;        /* Number of rows (of direct blocks) in each of the first indirect blocks */
    size_t      obj_block_size;         /* Size of block to hold large object added */
    size_t      block_size;             /* Size of block added */
    unsigned    row;                    /* Current row in indirect block */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    size_t      obj_size;               /* Size of object */
    hsize_t     free_space;             /* Size of free space in heap */
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
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR

    /* Compute # of bits used in first row */
    first_row_bits = H5V_log2_of2(cparam->managed.start_block_size) +
                        H5V_log2_of2(cparam->managed.width);
    first_indirect_block_size = 2 * cparam->managed.max_direct_size;
    num_first_indirect_rows = (H5V_log2_of2(first_indirect_block_size) - first_row_bits) + 1;
    obj_block_size = 1 << ((num_first_indirect_rows + H5V_log2_of2(cparam->managed.start_block_size)) - 1);
#ifdef QAK
HDfprintf(stderr, "first_row_bits = %u\n", first_row_bits);
HDfprintf(stderr, "first_indirect_block_size = %u\n", first_indirect_block_size);
HDfprintf(stderr, "num_first_indirect_rows = %u\n", num_first_indirect_rows);
HDfprintf(stderr, "obj_block_size = %Zu\n", obj_block_size);
#endif /* QAK */

    /*
     * Test absolute heap
     */
    TESTING("filling direct blocks and skipping row of non-root indirect blocks, then backfill and extend");

    /* Fill direct blocks in root indirect block */
    heap_size = 0;
    free_space = 0;
    if(fill_root_direct(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert large object, to force creation of indirect block and
     * range of skipped (indirect) blocks that are too small to hold the large
     * object
     */
    obj_size = (1 << ((num_first_indirect_rows + H5V_log2_of2(cparam->managed.start_block_size)) - 2)) + 1;
#ifdef QAK
HDfprintf(stderr, "obj_size = %Zu\n", obj_size);
#endif /* QAK */
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 20, obj_size))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert object to fill space in (large) block created */
    obj_size = DBLOCK_FREE(fh, num_first_indirect_rows) - obj_size;
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 20, obj_size))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill all rows of direct blocks that are smaller than large object's block size */
    block_size = cparam->managed.start_block_size;
    row = 0;
    while(block_size < obj_block_size) {
        /* Fill rows of direct blocks in skipped indirect blocks */
        for(u = 0; u < cparam->managed.width; u++)
            if(fill_row(fh, dxpl, cparam, &heap_size, &free_space, row, &nobjs))
                FAIL_STACK_ERROR

        /* Fill row of direct blocks in largest (i.e. non-skipped) indirect block */
        if(fill_row(fh, dxpl, cparam, &heap_size, &free_space, row, &nobjs))
            FAIL_STACK_ERROR

        /* Advance position in blocks */
        if(row > 0)
            block_size *= 2;
        row++;
    } /* end while */

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Add one more object, to create another "large" block */
    obj_size = SMALL_OBJ_SIZE1;
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 10, obj_size))
        FAIL_STACK_ERROR

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_abs_fill_direct_skip_2nd_indirect_start_block_add_skipped() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_fill_2nd_direct_less_one_wrap_start_block_add_skipped
 *
 * Purpose:	Test filling all direct blocks in root indirect block and all
 *              direct blocks in 2nd level indirect blocks, except the last
 *              one, then insert object insert object that is too large to
 *              hold in row of 2nd level indirect blocks (forcing the use of
 *              the next row of 2nd level blocks), then backfill all skipped
 *              direct blocks & extend.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, April 18, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_fill_2nd_direct_less_one_wrap_start_block_add_skipped(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    first_row_bits;         /* Number of bits used bit addresses in first row */
    unsigned    first_indirect_block_size;      /* Range of addresses covered by each of the first indirect blocks */
    unsigned    num_first_indirect_rows;        /* Number of rows (of direct blocks) in each of the first indirect blocks */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    size_t      obj_size;               /* Size of object */
    hsize_t     free_space;             /* Size of free space in heap */
    hsize_t     heap_size;              /* Total size of heap */
    unsigned    u;                      /* Local index variables */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR

    /* Compute # of bits used in first row */
    first_row_bits = H5V_log2_of2(cparam->managed.start_block_size) +
                        H5V_log2_of2(cparam->managed.width);
    first_indirect_block_size = 2 * cparam->managed.max_direct_size;
    num_first_indirect_rows = (H5V_log2_of2(first_indirect_block_size) - first_row_bits) + 1;
#ifdef QAK
HDfprintf(stderr, "first_row_bits = %u\n", first_row_bits);
HDfprintf(stderr, "first_indirect_block_size = %u\n", first_indirect_block_size);
HDfprintf(stderr, "num_first_indirect_rows = %u\n", num_first_indirect_rows);
#endif /* QAK */

    /*
     * Test absolute heap
     */
    TESTING("filling direct blocks, filling 2nd level indirect blocks, except last one, and insert object too large for 2nd level indirect blocks, then backfill and extend");

    /* Fill direct blocks in root indirect block */
    heap_size = 0;
    free_space = 0;
    if(fill_root_direct(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill first row (except one) of 2nd level indirect blocks */
    for(u = 0; u < cparam->managed.width - 1; u++) {
        /* Fill all rows of 2nd level indirect blocks in root block */
        if(fill_2nd_indirect(fh, dxpl, cparam, &heap_size, &free_space, &nobjs, 1))
            FAIL_STACK_ERROR
    } /* end for */

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert large object, to force creation of indirect block and
     * range of skipped (indirect) blocks that are too small to hold the large
     * object
     */
    obj_size = (1 << ((num_first_indirect_rows + H5V_log2_of2(cparam->managed.start_block_size)) - 2)) + 1;
#ifdef QAK
HDfprintf(stderr, "obj_size = %Zu\n", obj_size);
#endif /* QAK */
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 20, obj_size))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert object to fill space in (large) block created */
    obj_size = DBLOCK_FREE(fh, num_first_indirect_rows) - obj_size;
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 20, obj_size))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill rows skipped over in 2nd level indirect block's direct blocks
     * (and rows of next 2nd level indirect block's direct blocks)
     */
    for(u = 0; u < num_first_indirect_rows; u++) {
        /* Direct block rows in skipped 2nd level indirect block */
        if(fill_row(fh, dxpl, cparam, &heap_size, &free_space, u, &nobjs))
            FAIL_STACK_ERROR

        /* Direct block row in current 2nd level indirect block */
        if(fill_row(fh, dxpl, cparam, &heap_size, &free_space, u, &nobjs))
            FAIL_STACK_ERROR
    } /* end for */

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Add one more object, to create another "large" block */
    obj_size = SMALL_OBJ_SIZE1;
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 10, obj_size))
        FAIL_STACK_ERROR

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_abs_fill_2nd_direct_less_one_wrap_start_block_add_skipped() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_fill_direct_skip_2nd_indirect_skip_2nd_block_add_skipped
 *
 * Purpose:	Test filling all direct blocks in root indirect block, then
 *              add object too large for all direct blocks in first row of 
 *              indirect blocks, to force skipping a row of indirect blocks
 *              (and range of skipped blocks), then add object that is too
 *              large for initial block size in skipped indirect blocks, then
 *              backfill all direct blocks and extend to next "normal" direct
 *              block (but insert first block of backfilling with object
 *              too large for initial block size in skipped indirect block
 *              row's direct blocks).
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, April 11, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_fill_direct_skip_2nd_indirect_skip_2nd_block_add_skipped(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    first_row_bits;         /* Number of bits used bit addresses in first row */
    unsigned    first_indirect_block_size;      /* Range of addresses covered by each of the first indirect blocks */
    unsigned    num_first_indirect_rows;        /* Number of rows (of direct blocks) in each of the first indirect blocks */
    size_t      obj_block_size;         /* Size of block to hold large object added */
    size_t      block_size;             /* Size of block added */
    unsigned    row;                    /* Current row in indirect block */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    size_t      obj_size;               /* Size of object */
    hsize_t     free_space;             /* Size of free space in heap */
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
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR

    /* Compute # of bits used in first row */
    first_row_bits = H5V_log2_of2(cparam->managed.start_block_size) +
                        H5V_log2_of2(cparam->managed.width);
    first_indirect_block_size = 2 * cparam->managed.max_direct_size;
    num_first_indirect_rows = (H5V_log2_of2(first_indirect_block_size) - first_row_bits) + 1;
    obj_block_size = 1 << ((num_first_indirect_rows + H5V_log2_of2(cparam->managed.start_block_size)) - 1);
#ifdef QAK
HDfprintf(stderr, "first_row_bits = %u\n", first_row_bits);
HDfprintf(stderr, "first_indirect_block_size = %u\n", first_indirect_block_size);
HDfprintf(stderr, "num_first_indirect_rows = %u\n", num_first_indirect_rows);
HDfprintf(stderr, "obj_block_size = %Zu\n", obj_block_size);
#endif /* QAK */

    /*
     * Test absolute heap
     */
    TESTING("filling direct blocks and skipping row of non-root indirect blocks, then skip row of direct blocks, then backfill and extend");

    /* Fill direct blocks in root indirect block */
    heap_size = 0;
    free_space = 0;
    if(fill_root_direct(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert large object, to force creation of indirect block and
     * range of skipped (indirect) blocks that are too small to hold the large
     * object
     */
    obj_size = (1 << ((num_first_indirect_rows + H5V_log2_of2(cparam->managed.start_block_size)) - 2)) + 1;
#ifdef QAK
HDfprintf(stderr, "obj_size = %Zu\n", obj_size);
#endif /* QAK */
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 20, obj_size))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert object to fill space in (large) block created */
    obj_size = DBLOCK_FREE(fh, num_first_indirect_rows) - obj_size;
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 20, obj_size))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert object too large for initial block size in skipped indirect blocks */
    obj_size = (cparam->managed.start_block_size * 4) + 1;
#ifdef QAK
HDfprintf(stderr, "obj_size = %Zu\n", obj_size);
#endif /* QAK */
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 10, obj_size))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert object to fill space in (medium) block just created */
    obj_size = DBLOCK_FREE(fh, 4) - obj_size;
#ifdef QAK
HDfprintf(stderr, "obj_size = %Zu\n", obj_size);
#endif /* QAK */
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 20, obj_size))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Finish off blocks in row of medium block size (just to make row filling easier below) */
    obj_size = DBLOCK_FREE(fh, 4);
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 20, obj_size))
        FAIL_STACK_ERROR
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 20, obj_size))
        FAIL_STACK_ERROR
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 20, obj_size))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill all rows of direct blocks that are smaller than large object's block size */
    block_size = cparam->managed.start_block_size;
    row = 0;
    while(block_size < obj_block_size) {
        /* Fill rows of direct blocks in skipped indirect blocks */
        for(u = 0; u < cparam->managed.width; u++)
            if(fill_row(fh, dxpl, cparam, &heap_size, &free_space, row, &nobjs))
                FAIL_STACK_ERROR

        /* Fill row of direct blocks in largest (i.e. non-skipped) indirect block */
        /* (Skip the row of blocks filled above) */
        if(row != 4)
            if(fill_row(fh, dxpl, cparam, &heap_size, &free_space, row, &nobjs))
                FAIL_STACK_ERROR

        /* Advance position in blocks */
        if(row > 0)
            block_size *= 2;
        row++;
    } /* end while */

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Add one more object, to create another "large" block */
    obj_size = SMALL_OBJ_SIZE1;
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 10, obj_size))
        FAIL_STACK_ERROR

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_abs_fill_direct_skip_2nd_indirect_skip_2nd_block_add_skipped() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_fill_direct_skip_indirect_two_rows_add_skipped
 *
 * Purpose:	Test filling all direct blocks in root indirect block, then
 *              add object too large for initial block in first two rows of
 *              indirect blocks, to force extension of non-root
 *              indirect block (and range of skipped blocks).
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Saturday, April 15, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_fill_direct_skip_indirect_two_rows_add_skipped(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    first_row_bits;         /* Number of bits used bit addresses in first row */
    unsigned    first_indirect_block_size;      /* Range of addresses covered by each of the first indirect blocks */
    unsigned    num_first_indirect_rows;        /* Number of rows (of direct blocks) in each of the first indirect blocks */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    size_t      obj_size;               /* Size of object */
    hsize_t     free_space;             /* Size of free space in heap */
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
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR

    /* Compute # of bits used in first row */
    first_row_bits = H5V_log2_of2(cparam->managed.start_block_size) +
                        H5V_log2_of2(cparam->managed.width);
    first_indirect_block_size = 2 * cparam->managed.max_direct_size;
    num_first_indirect_rows = (H5V_log2_of2(first_indirect_block_size) - first_row_bits) + 1;

    /*
     * Test absolute heap
     */
    TESTING("filling direct blocks and skipping two rows of root indirect block, then backfill and extend");

    /* Fill direct blocks in root indirect block */
    heap_size = 0;
    free_space = 0;
    if(fill_root_direct(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert large object, to force creation of indirect block and
     * range of skipped blocks that are too small to hold the large object
     */
    obj_size = (cparam->managed.max_direct_size / 2) + 1;
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 20, obj_size))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert an object to fill up the (biggest) heap block created */
    obj_size = DBLOCK_FREE(fh, num_first_indirect_rows + 1) - obj_size;
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 20, obj_size))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill rows skipped over in indirect block's direct blocks
     */
    for(u = 0; u < num_first_indirect_rows; u++) {
        /* Direct block rows in first row of skipped 2nd level indirect blocks */
        for(v = 0; v < cparam->managed.width; v++)
            if(fill_row(fh, dxpl, cparam, &heap_size, &free_space, u, &nobjs))
                FAIL_STACK_ERROR

        /* Direct block rows in second row of skipped 2nd level indirect blocks */
        for(v = 0; v < cparam->managed.width; v++)
            if(fill_row(fh, dxpl, cparam, &heap_size, &free_space, u, &nobjs))
                FAIL_STACK_ERROR

        /* Direct block row in used 2nd level indirect block */
        if(fill_row(fh, dxpl, cparam, &heap_size, &free_space, u, &nobjs))
            FAIL_STACK_ERROR
    } /* end for */

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill rows in second row of skipped 2nd level indirect blocks (and used 2nd level block) */

    /* Direct block rows in skipped 2nd level indirect blocks */
    for(v = 0; v < cparam->managed.width; v++)
        if(fill_row(fh, dxpl, cparam, &heap_size, &free_space, num_first_indirect_rows, &nobjs))
            FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Direct block row in used 2nd level indirect block */
    if(fill_row(fh, dxpl, cparam, &heap_size, &free_space, num_first_indirect_rows, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Add one more object, to create another "large" block */
    obj_size = SMALL_OBJ_SIZE1;
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 10, obj_size))
        FAIL_STACK_ERROR

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_abs_fill_direct_skip_indirect_two_rows_add_skipped() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_fill_2nd_direct_skip_start_block_add_skipped
 *
 * Purpose:	Test filling all direct blocks in root indirect block and all
 *              direct blocks in 2nd level indirect blocks, the insert object
 *              that is too large to hold in first row of direct blocks of
 *              3rd level indirect block, then backfill & extend all skipped
 *              3rd level indirect block's direct blocks.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, April 11, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_fill_2nd_direct_skip_start_block_add_skipped(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    size_t      obj_size;               /* Size of object */
    hsize_t     free_space;             /* Size of free space in heap */
    hsize_t     heap_size;              /* Total size of heap */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR

    /*
     * Test absolute heap
     */
    TESTING("filling direct blocks, filling 2nd level indirect blocks, and skip first rows of direct blocks of 3rd level indirect block, then backfill and extend");

    /* Fill direct blocks in root indirect block */
    heap_size = 0;
    free_space = 0;
    if(fill_root_direct(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill all rows of 2nd level indirect blocks */
    if(fill_all_2nd_indirect_rows(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert large object, to force creation of indirect block and
     * range of skipped (indirect) blocks that are too small to hold the large
     * object
     */
    obj_size = (cparam->managed.start_block_size * 2) + 1;
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 10, obj_size))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert object to fill space in (large) block created */
    obj_size = DBLOCK_FREE(fh, 3) - obj_size;
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 20, obj_size))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill rows skipped over in 3rd level indirect block's direct blocks */
    if(fill_row(fh, dxpl, cparam, &heap_size, &free_space, 0, &nobjs))
        FAIL_STACK_ERROR
    if(fill_row(fh, dxpl, cparam, &heap_size, &free_space, 1, &nobjs))
        FAIL_STACK_ERROR
    if(fill_row(fh, dxpl, cparam, &heap_size, &free_space, 2, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Add one more object, to create another "large" block */
    obj_size = SMALL_OBJ_SIZE1;
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 10, obj_size))
        FAIL_STACK_ERROR

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_abs_fill_2nd_direct_skip_start_block_add_skipped() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_fill_2nd_direct_skip_2nd_indirect_start_block_add_skipped
 *
 * Purpose:	Test filling all direct blocks in root indirect block and all
 *              direct blocks in 2nd level indirect blocks, fill all direct
 *              blocks in 3rd level indirect block, then insert object
 *              that is too large to hold in first row of direct blocks of
 *              3rd level indirect block's first 2nd level indirect block, then
 *              backfill & extend all skipped 2nd level indirect block's direct
 *              blocks.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, April 11, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_fill_2nd_direct_skip_2nd_indirect_start_block_add_skipped(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    size_t      obj_size;               /* Size of object */
    hsize_t     free_space;             /* Size of free space in heap */
    hsize_t     heap_size;              /* Total size of heap */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR

    /*
     * Test absolute heap
     */
    TESTING("filling direct blocks, filling 2nd level indirect blocks, filling 3rd level indirect block's direct blocks, and skip first rows of direct blocks of 3rd level indirect block's 2nd level indirect block, then backfill and extend");

    /* Fill direct blocks in root indirect block */
    heap_size = 0;
    free_space = 0;
    if(fill_root_direct(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill all rows of 2nd level indirect blocks */
    if(fill_all_2nd_indirect_rows(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill all direct block rows in third level indirect block */
    if(fill_all_direct(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert large object, to force creation of indirect block and
     * range of skipped (indirect) blocks that are too small to hold the large
     * object
     */
    obj_size = (cparam->managed.start_block_size * 2) + 1;
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 10, obj_size))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert object to fill space in (large) block created */
    obj_size = DBLOCK_FREE(fh, 3) - obj_size;
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 20, obj_size))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill rows skipped over in (3rd level indirect block's) 2nd level
     *  indirect block's direct blocks
     */
    if(fill_row(fh, dxpl, cparam, &heap_size, &free_space, 0, &nobjs))
        FAIL_STACK_ERROR
    if(fill_row(fh, dxpl, cparam, &heap_size, &free_space, 1, &nobjs))
        FAIL_STACK_ERROR
    if(fill_row(fh, dxpl, cparam, &heap_size, &free_space, 2, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Add one more object, to create another "large" block */
    obj_size = SMALL_OBJ_SIZE1;
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 10, obj_size))
        FAIL_STACK_ERROR

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_abs_fill_2nd_direct_skip_2nd_indirect_start_block_add_skipped() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_fill_2nd_direct_fill_direct_skip_3rd_indirect_start_block_add_skipped
 *
 * Purpose:	Test filling all direct blocks in root indirect block and all
 *              direct blocks in 2nd level indirect blocks, fill all direct
 *              blocks in 3rd level indirect block, then insert object
 *              that is too large to hold in first row of 2nd level indirect
 *              blocks of 3rd level indirect block, then backfill & extend all
 *              skipped direct blocks.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, April 11, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_fill_2nd_direct_fill_direct_skip_3rd_indirect_start_block_add_skipped(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    first_row_bits;         /* Number of bits used bit addresses in first row */
    unsigned    first_indirect_block_size;      /* Range of addresses covered by each of the first indirect blocks */
    unsigned    num_first_indirect_rows;        /* Number of rows (of direct blocks) in each of the first indirect blocks */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    size_t      obj_size;               /* Size of object */
    hsize_t     free_space;             /* Size of free space in heap */
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
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR

    /* Compute # of bits used in first row */
    first_row_bits = H5V_log2_of2(cparam->managed.start_block_size) +
                        H5V_log2_of2(cparam->managed.width);
    first_indirect_block_size = 2 * cparam->managed.max_direct_size;
    num_first_indirect_rows = (H5V_log2_of2(first_indirect_block_size) - first_row_bits) + 1;
#ifdef QAK
HDfprintf(stderr, "first_row_bits = %u\n", first_row_bits);
HDfprintf(stderr, "first_indirect_block_size = %u\n", first_indirect_block_size);
HDfprintf(stderr, "num_first_indirect_rows = %u\n", num_first_indirect_rows);
#endif /* QAK */

    /*
     * Test absolute heap
     */
    TESTING("filling direct blocks, filling 2nd level indirect blocks, filling 3rd level indirect block's direct blocks, and skip first row of indirect blocks of 3rd level indirect block, then backfill and extend");

    /* Fill direct blocks in root indirect block */
    heap_size = 0;
    free_space = 0;
    if(fill_root_direct(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill all rows of 2nd level indirect blocks */
    if(fill_all_2nd_indirect_rows(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill all direct block rows in third level indirect block */
    if(fill_all_direct(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert large object, to force creation of indirect block and
     * range of skipped (indirect) blocks that are too small to hold the large
     * object
     */
    obj_size = (1 << ((num_first_indirect_rows + H5V_log2_of2(cparam->managed.start_block_size)) - 2)) + 1;
#ifdef QAK
HDfprintf(stderr, "obj_size = %Zu\n", obj_size);
#endif /* QAK */
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 20, obj_size))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert object to fill space in (large) block created */
    obj_size = DBLOCK_FREE(fh, num_first_indirect_rows) - obj_size;
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 20, obj_size))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill rows skipped over in (first 3rd level indirect block's) 2nd level
     *  indirect block's direct blocks
     *  (and second 3rd level indirect block's direct blocks)
     */
    for(u = 0; u < num_first_indirect_rows; u++) {
        /* Direct block rows in 2nd level indirect blocks */
        for(v = 0; v < cparam->managed.width; v++)
            if(fill_row(fh, dxpl, cparam, &heap_size, &free_space, u, &nobjs))
                FAIL_STACK_ERROR

        /* Direct block row in 3rd level indirect block */
        if(fill_row(fh, dxpl, cparam, &heap_size, &free_space, u, &nobjs))
            FAIL_STACK_ERROR
    } /* end for */

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Add one more object, to create another "large" block */
    obj_size = SMALL_OBJ_SIZE1;
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 10, obj_size))
        FAIL_STACK_ERROR

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_abs_fill_2nd_direct_fill_direct_skip_3rd_indirect_start_block_add_skipped() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_fill_2nd_direct_fill_direct_skip2_3rd_indirect_start_block_add_skipped
 *
 * Purpose:	Test filling all direct blocks in root indirect block and all
 *              direct blocks in 2nd level indirect blocks, fill all direct
 *              blocks in 3rd level indirect block, then insert object
 *              that is too large to hold in first & second rows of 2nd level
 *              indirect blocks (although this 3rd level indirect block only
 *              has one row of 2nd level indirect blocks) of 3rd level indirect
 *             block, then backfill & extend all skipped direct blocks.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, April 11, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_fill_2nd_direct_fill_direct_skip2_3rd_indirect_start_block_add_skipped(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    first_row_bits;         /* Number of bits used bit addresses in first row */
    unsigned    first_indirect_block_size;      /* Range of addresses covered by each of the first indirect blocks */
    unsigned    num_first_indirect_rows;        /* Number of rows (of direct blocks) in each of the first indirect blocks */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    size_t      obj_size;               /* Size of object */
    hsize_t     free_space;             /* Size of free space in heap */
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
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR

    /* Compute # of bits used in first row */
    first_row_bits = H5V_log2_of2(cparam->managed.start_block_size) +
                        H5V_log2_of2(cparam->managed.width);
    first_indirect_block_size = 2 * cparam->managed.max_direct_size;
    num_first_indirect_rows = (H5V_log2_of2(first_indirect_block_size) - first_row_bits) + 1;
#ifdef QAK
HDfprintf(stderr, "first_row_bits = %u\n", first_row_bits);
HDfprintf(stderr, "first_indirect_block_size = %u\n", first_indirect_block_size);
HDfprintf(stderr, "num_first_indirect_rows = %u\n", num_first_indirect_rows);
#endif /* QAK */

    /*
     * Test absolute heap
     */
    TESTING("filling direct blocks, filling 2nd level indirect blocks, filling 3rd level indirect block's direct blocks, and skip first two rows of indirect blocks of 3rd level indirect block, then backfill and extend");

    /* Fill direct blocks in root indirect block */
    heap_size = 0;
    free_space = 0;
    if(fill_root_direct(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill all rows of 2nd level indirect blocks */
    if(fill_all_2nd_indirect_rows(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill all direct block rows in third level indirect block */
    if(fill_all_direct(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert large object, to force creation of indirect block and
     * range of skipped (indirect) blocks that are too small to hold the large
     * object
     */
    obj_size = (1 << ((num_first_indirect_rows + H5V_log2_of2(cparam->managed.start_block_size)) - 1)) + 1;
#ifdef QAK
HDfprintf(stderr, "obj_size = %Zu\n", obj_size);
#endif /* QAK */
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 20, obj_size))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert object to fill space in (large) block created */
    obj_size = DBLOCK_FREE(fh, num_first_indirect_rows + 1) - obj_size;
#ifdef QAK
HDfprintf(stderr, "obj_size = %Zu\n", obj_size);
#endif /* QAK */
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 20, obj_size))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill rows skipped over in (first 3rd level indirect block's) 2nd level
     *  indirect block's direct blocks
     *  (and second 3rd level indirect block's direct blocks)
     */
    for(u = 0; u < num_first_indirect_rows; u++) {
        /* Direct block rows in 2nd level indirect blocks */
        for(v = 0; v < cparam->managed.width; v++)
            if(fill_row(fh, dxpl, cparam, &heap_size, &free_space, u, &nobjs))
                FAIL_STACK_ERROR

        /* Direct block row in 3rd level indirect block */
        if(fill_row(fh, dxpl, cparam, &heap_size, &free_space, u, &nobjs))
            FAIL_STACK_ERROR
    } /* end for */

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Add one more object, to create another "large" block */
    obj_size = SMALL_OBJ_SIZE1;
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 10, obj_size))
        FAIL_STACK_ERROR

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_abs_fill_2nd_direct_fill_direct_skip2_3rd_indirect_start_block_add_skipped() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_fill_3rd_direct_less_one_fill_direct_wrap_start_block_add_skipped
 *
 * Purpose:	Test filling all direct blocks in root indirect block and all
 *              direct blocks in 2nd level indirect blocks, all 3rd level
 *              indirect blocks in first row except the last one, fill direct
 *              blocks in lasts 3rd level indirect block, then insert object
 *              insert object that is too large to hold in last 3rd level
 *              indirect block's row of 2nd level indirect blocks (forcing the
 *              use of the next row of 3rd level blocks), then backfill all
 *              skipped direct blocks & extend.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tues, April 18, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_fill_3rd_direct_less_one_fill_direct_wrap_start_block_add_skipped(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    first_row_bits;         /* Number of bits used bit addresses in first row */
    unsigned    first_indirect_block_size;      /* Range of addresses covered by each of the first indirect blocks */
    unsigned    num_first_indirect_rows;        /* Number of rows (of direct blocks) in each of the first indirect blocks */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    size_t      obj_size;               /* Size of object */
    hsize_t     free_space;             /* Size of free space in heap */
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
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR

    /* Compute # of bits used in first row */
    first_row_bits = H5V_log2_of2(cparam->managed.start_block_size) +
                        H5V_log2_of2(cparam->managed.width);
    first_indirect_block_size = 2 * cparam->managed.max_direct_size;
    num_first_indirect_rows = (H5V_log2_of2(first_indirect_block_size) - first_row_bits) + 1;
#ifdef QAK
HDfprintf(stderr, "first_row_bits = %u\n", first_row_bits);
HDfprintf(stderr, "first_indirect_block_size = %u\n", first_indirect_block_size);
HDfprintf(stderr, "num_first_indirect_rows = %u\n", num_first_indirect_rows);
#endif /* QAK */

    /*
     * Test absolute heap
     */
    TESTING("filling direct blocks, filling 2nd level indirect blocks, filling first row of 3rd level indirect blocks, except last one, fill all direct blocks in last 3rd level indirect block, and insert object too large for it's 2nd level indirect blocks, then backfill and extend");

    /* Fill direct blocks in root indirect block */
    heap_size = 0;
    free_space = 0;
    if(fill_root_direct(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill all rows of 2nd level indirect blocks in 4th level indirect block */
    if(fill_all_2nd_indirect_rows(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill first row (except one) of 3rd level indirect blocks */
    for(u = 0; u < cparam->managed.width - 1; u++)
        /* Fill 3rd level indirect block */
        if(fill_3rd_indirect(fh, dxpl, cparam, &heap_size, &free_space, &nobjs, 1))
            FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill all direct block rows in last third level indirect block */
    if(fill_all_direct(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert large object, to force creation of indirect block and
     * range of skipped (indirect) blocks that are too small to hold the large
     * object
     */
    obj_size = (1 << ((num_first_indirect_rows + H5V_log2_of2(cparam->managed.start_block_size)) - 2)) + 1;
#ifdef QAK
HDfprintf(stderr, "obj_size = %Zu\n", obj_size);
#endif /* QAK */
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 20, obj_size))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert object to fill space in (large) block created */
    obj_size = DBLOCK_FREE(fh, num_first_indirect_rows) - obj_size;
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 20, obj_size))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill rows skipped over in 2nd level indirect block's direct blocks
     * (and rows of next 3rd level indirect block's direct blocks)
     */
    for(u = 0; u < num_first_indirect_rows; u++) {
        /* Direct block rows in 2nd level indirect blocks */
        for(v = 0; v < cparam->managed.width; v++)
            if(fill_row(fh, dxpl, cparam, &heap_size, &free_space, u, &nobjs))
                FAIL_STACK_ERROR

        /* Direct block row in current 2nd level indirect block */
        if(fill_row(fh, dxpl, cparam, &heap_size, &free_space, u, &nobjs))
            FAIL_STACK_ERROR
    } /* end for */

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Add one more object, to create another "large" block */
    obj_size = SMALL_OBJ_SIZE1;
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 10, obj_size))
        FAIL_STACK_ERROR

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_abs_fill_3rd_direct_less_one_fill_direct_wrap_start_block_add_skipped() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_fill_1st_row_3rd_direct_fill_2nd_direct_less_one_wrap_start_block_add_skipped
 *
 * Purpose:	Test filling all direct blocks in root indirect block and all
 *              direct blocks in 2nd level indirect blocks, all 3rd level
 *              indirect blocks in first row, fill direct blocks in 2nd row 3rd
 *              level indirect block, fill all direct blocks in 1st row of
 *              2nd level indirect blocks except the last one, then insert
 *              object that is too large to hold in 3rd level indirect block's
 *              first row of 2nd level indirect blocks (forcing the use of the
 *              next row of 2nd level blocks), then backfill all skipped direct
 *              blocks & extend.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tues, April 18, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_fill_1st_row_3rd_direct_fill_2nd_direct_less_one_wrap_start_block_add_skipped(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    first_row_bits;         /* Number of bits used bit addresses in first row */
    unsigned    first_indirect_block_size;      /* Range of addresses covered by each of the first indirect blocks */
    unsigned    num_first_indirect_rows;        /* Number of rows (of direct blocks) in each of the first indirect blocks */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    size_t      obj_size;               /* Size of object */
    hsize_t     free_space;             /* Size of free space in heap */
    hsize_t     heap_size;              /* Total size of heap */
    unsigned    u;                      /* Local index variables */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR

    /* Compute # of bits used in first row */
    first_row_bits = H5V_log2_of2(cparam->managed.start_block_size) +
                        H5V_log2_of2(cparam->managed.width);
    first_indirect_block_size = 2 * cparam->managed.max_direct_size;
    num_first_indirect_rows = (H5V_log2_of2(first_indirect_block_size) - first_row_bits) + 1;
#ifdef QAK
HDfprintf(stderr, "first_row_bits = %u\n", first_row_bits);
HDfprintf(stderr, "first_indirect_block_size = %u\n", first_indirect_block_size);
HDfprintf(stderr, "num_first_indirect_rows = %u\n", num_first_indirect_rows);
#endif /* QAK */

    /*
     * Test absolute heap
     */
    TESTING("filling direct blocks, filling 2nd level indirect blocks, filling first row of 3rd level indirect blocks, fill all direct blocks in next 3rd level indirect block, fill all 1st row of 2nd level indirect blocks, except last one, and insert object too large for 2nd level indirect block, then backfill and extend");

    /* Fill direct blocks in root indirect block */
    heap_size = 0;
    free_space = 0;
    if(fill_root_direct(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill all rows of 2nd level indirect blocks in 4th level indirect block */
    if(fill_all_2nd_indirect_rows(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill first row of 3rd level indirect blocks */
    if(fill_3rd_indirect_row(fh, dxpl, cparam, &heap_size, &free_space, &nobjs, 1))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill all direct block rows in 2nd row third level indirect block */
    if(fill_all_direct(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill first row (except one) of 2nd level indirect blocks */
    for(u = 0; u < cparam->managed.width - 1; u++) {
        if(fill_2nd_indirect(fh, dxpl, cparam, &heap_size, &free_space, &nobjs, 1))
            FAIL_STACK_ERROR
    } /* end for */

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert large object, to force creation of indirect block and
     * range of skipped (indirect) blocks that are too small to hold the large
     * object
     */
    obj_size = (1 << ((num_first_indirect_rows + H5V_log2_of2(cparam->managed.start_block_size)) - 2)) + 1;
#ifdef QAK
HDfprintf(stderr, "obj_size = %Zu\n", obj_size);
#endif /* QAK */
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 20, obj_size))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert object to fill space in (large) block created */
    obj_size = DBLOCK_FREE(fh, num_first_indirect_rows) - obj_size;
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 20, obj_size))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill rows skipped over in 2nd level indirect block's direct blocks
     * (and rows of next 2nd level indirect block's direct blocks)
     */
    for(u = 0; u < num_first_indirect_rows; u++) {
        /* Direct block rows in skipped 2nd level indirect block */
        if(fill_row(fh, dxpl, cparam, &heap_size, &free_space, u, &nobjs))
            FAIL_STACK_ERROR

        /* Direct block row in current 2nd level indirect block */
        if(fill_row(fh, dxpl, cparam, &heap_size, &free_space, u, &nobjs))
            FAIL_STACK_ERROR
    } /* end for */

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Add one more object, to create another "large" block */
    obj_size = SMALL_OBJ_SIZE1;
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 10, obj_size))
        FAIL_STACK_ERROR

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_abs_fill_1st_row_3rd_direct_fill_2nd_direct_less_one_wrap_start_block_add_skipped() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_fill_3rd_direct_fill_direct_skip_start_block_add_skipped
 *
 * Purpose:	Test filling all direct blocks in root indirect block and all
 *              direct blocks in 2nd level indirect blocks, fill all direct
 *              blocks and indirect blocks in 3rd level indirect block, then
 *              fill all direct blocks in 4th level indirect block, then
 *              insert object that is too large to hold in first row of 2nd
 *              level indirect blocks of 4th level indirect block, then
 *              backfill all skipped direct blocks & extend.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Saturday, April 15, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_fill_3rd_direct_fill_direct_skip_start_block_add_skipped(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    first_row_bits;         /* Number of bits used bit addresses in first row */
    unsigned    first_indirect_block_size;      /* Range of addresses covered by each of the first indirect blocks */
    unsigned    num_first_indirect_rows;        /* Number of rows (of direct blocks) in each of the first indirect blocks */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    size_t      obj_size;               /* Size of object */
    hsize_t     free_space;             /* Size of free space in heap */
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
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR

    /* Compute # of bits used in first row */
    first_row_bits = H5V_log2_of2(cparam->managed.start_block_size) +
                        H5V_log2_of2(cparam->managed.width);
    first_indirect_block_size = 2 * cparam->managed.max_direct_size;
    num_first_indirect_rows = (H5V_log2_of2(first_indirect_block_size) - first_row_bits) + 1;
#ifdef QAK
HDfprintf(stderr, "first_row_bits = %u\n", first_row_bits);
HDfprintf(stderr, "first_indirect_block_size = %u\n", first_indirect_block_size);
HDfprintf(stderr, "num_first_indirect_rows = %u\n", num_first_indirect_rows);
#endif /* QAK */

    /*
     * Test absolute heap
     */
    TESTING("filling direct blocks, filling 2nd level indirect blocks, filling 3rd level indirect blocks, fill 4th level indirect block's direct blocks, and skip first row of 2nd indirect blocks of 4th level indirect block, then backfill and extend");

    /* Fill direct blocks in root indirect block */
    heap_size = 0;
    free_space = 0;
    if(fill_root_direct(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill all rows of 2nd level indirect blocks */
    if(fill_all_2nd_indirect_rows(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill all rows of 3rd level indirect blocks */
    if(fill_all_3rd_indirect_rows(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill all direct block rows in fourth level indirect block */
    if(fill_all_direct(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert large object, to force creation of indirect block and
     * range of skipped (indirect) blocks that are too small to hold the large
     * object
     */
    obj_size = (1 << ((num_first_indirect_rows + H5V_log2_of2(cparam->managed.start_block_size)) - 2)) + 1;
#ifdef QAK
HDfprintf(stderr, "obj_size = %Zu\n", obj_size);
#endif /* QAK */
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 20, obj_size))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert object to fill space in (large) block created */
    obj_size = DBLOCK_FREE(fh, num_first_indirect_rows) - obj_size;
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 20, obj_size))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill rows skipped over in (first 4th level indirect block's) 2nd level
     *  indirect block's direct blocks
     *  (and second row of 2nd level indirect block's direct blocks)
     */
    for(u = 0; u < num_first_indirect_rows; u++) {
        /* Direct block rows in 2nd level indirect blocks */
        for(v = 0; v < cparam->managed.width; v++)
            if(fill_row(fh, dxpl, cparam, &heap_size, &free_space, u, &nobjs))
                FAIL_STACK_ERROR

        /* Direct block row in 2nd level indirect block */
        if(fill_row(fh, dxpl, cparam, &heap_size, &free_space, u, &nobjs))
            FAIL_STACK_ERROR
    } /* end for */

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Add one more object, to create another "large" block */
    obj_size = SMALL_OBJ_SIZE1;
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 10, obj_size))
        FAIL_STACK_ERROR

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_abs_fill_3rd_direct_fill_direct_skip_start_block_add_skipped() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_fill_3rd_direct_fill_2nd_direct_fill_direct_skip_3rd_indirect_start_block_add_skipped
 *
 * Purpose:	Test filling all direct blocks in root indirect block and all
 *              direct blocks in 2nd level indirect blocks, fill all direct
 *              blocks and indirect blocks in 3rd level indirect block, then
 *              fill all direct blocks in 4th level indirect block, then
 *              insert object that is too large to hold in first row of 2nd
 *              level indirect blocks of 4th level indirect block, then
 *              backfill all skipped direct blocks & extend.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, April 17, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_fill_3rd_direct_fill_2nd_direct_fill_direct_skip_3rd_indirect_start_block_add_skipped(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    first_row_bits;         /* Number of bits used bit addresses in first row */
    unsigned    first_indirect_block_size;      /* Range of addresses covered by each of the first indirect blocks */
    unsigned    num_first_indirect_rows;        /* Number of rows (of direct blocks) in each of the first indirect blocks */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    size_t      obj_size;               /* Size of object */
    hsize_t     free_space;             /* Size of free space in heap */
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
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR

    /* Compute # of bits used in first row */
    first_row_bits = H5V_log2_of2(cparam->managed.start_block_size) +
                        H5V_log2_of2(cparam->managed.width);
    first_indirect_block_size = 2 * cparam->managed.max_direct_size;
    num_first_indirect_rows = (H5V_log2_of2(first_indirect_block_size) - first_row_bits) + 1;
#ifdef QAK
HDfprintf(stderr, "first_row_bits = %u\n", first_row_bits);
HDfprintf(stderr, "first_indirect_block_size = %u\n", first_indirect_block_size);
HDfprintf(stderr, "num_first_indirect_rows = %u\n", num_first_indirect_rows);
#endif /* QAK */

    /*
     * Test absolute heap
     */
    TESTING("filling direct blocks, filling 2nd level indirect blocks, filling 3rd level indirect blocks, fill 4th level indirect block's direct, 2nd level indirect blocks and 3rd level direct block, and skip first row of 2nd indirect blocks of 4th level indirect block's 3rd level indirect block, then backfill and extend");

    /* Fill direct blocks in root indirect block */
    heap_size = 0;
    free_space = 0;
    if(fill_root_direct(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill all rows of 2nd level indirect blocks */
    if(fill_all_2nd_indirect_rows(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill all rows of 3rd level indirect blocks */
    if(fill_all_3rd_indirect_rows(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill all direct block rows in fourth level indirect block */
    if(fill_all_direct(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill all rows of 2nd level indirect blocks in fourth level indirect block */
    if(fill_all_2nd_indirect_rows(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill all direct block rows in fourth level indirect block's 3rd level indirect block */
    if(fill_all_direct(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert large object, to force creation of indirect block and
     * range of skipped (indirect) blocks that are too small to hold the large
     * object
     */
    obj_size = (1 << ((num_first_indirect_rows + H5V_log2_of2(cparam->managed.start_block_size)) - 2)) + 1;
#ifdef QAK
HDfprintf(stderr, "obj_size = %Zu\n", obj_size);
#endif /* QAK */
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 20, obj_size))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert object to fill space in (large) block created */
    obj_size = DBLOCK_FREE(fh, num_first_indirect_rows) - obj_size;
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 20, obj_size))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill rows skipped over in (first 4th level indirect block's first 3rd
     *  level block's) 2nd level indirect block's direct blocks
     *  (and rows of 2nd 3rd level indirect block's direct blocks)
     */
    for(u = 0; u < num_first_indirect_rows; u++) {
        /* Direct block rows in 2nd level indirect blocks */
        for(v = 0; v < cparam->managed.width; v++)
            if(fill_row(fh, dxpl, cparam, &heap_size, &free_space, u, &nobjs))
                FAIL_STACK_ERROR

        /* Direct block row in 3rd level indirect block */
        if(fill_row(fh, dxpl, cparam, &heap_size, &free_space, u, &nobjs))
            FAIL_STACK_ERROR
    } /* end for */

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Add one more object, to create another "large" block */
    obj_size = SMALL_OBJ_SIZE1;
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 10, obj_size))
        FAIL_STACK_ERROR

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_abs_fill_3rd_direct_fill_2nd_direct_fill_direct_skip_3rd_indirect_start_block_add_skipped() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_fill_3rd_direct_fill_2nd_direct_fill_direct_skip_3rd_indirect_two_rows_start_block_add_skipped
 *
 * Purpose:	Test filling all direct blocks in root indirect block and all
 *              direct blocks in 2nd level indirect blocks, fill all direct
 *              blocks and indirect blocks in 3rd level indirect block, fill all
 *              direct & indirect blocks in first row of 4th level indirect
 *              blocks, then fill all direct blocks in first row of 3rd level
 *              indirect blocks in 4th level indirect block, fill direct blocks
 *              in first block of 2nd row of 3rd level indirect blocks in 4th
 *              level indirect block, then insert object insert object that is
 *              too large to hold in first row of 2nd level indirect blocks of
 *              3rd level indirect block (in 4th level indirect block), then
 *              backfill all skipped direct blocks & extend.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, April 17, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_fill_3rd_direct_fill_2nd_direct_fill_direct_skip_3rd_indirect_two_rows_start_block_add_skipped(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    first_row_bits;         /* Number of bits used bit addresses in first row */
    unsigned    first_indirect_block_size;      /* Range of addresses covered by each of the first indirect blocks */
    unsigned    num_first_indirect_rows;        /* Number of rows (of direct blocks) in each of the first indirect blocks */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    size_t      obj_size;               /* Size of object */
    hsize_t     free_space;             /* Size of free space in heap */
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
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR

    /* Compute # of bits used in first row */
    first_row_bits = H5V_log2_of2(cparam->managed.start_block_size) +
                        H5V_log2_of2(cparam->managed.width);
    first_indirect_block_size = 2 * cparam->managed.max_direct_size;
    num_first_indirect_rows = (H5V_log2_of2(first_indirect_block_size) - first_row_bits) + 1;
#ifdef QAK
HDfprintf(stderr, "first_row_bits = %u\n", first_row_bits);
HDfprintf(stderr, "first_indirect_block_size = %u\n", first_indirect_block_size);
HDfprintf(stderr, "num_first_indirect_rows = %u\n", num_first_indirect_rows);
#endif /* QAK */

    /*
     * Test absolute heap
     */
    TESTING("filling direct blocks, filling 2nd level indirect blocks, filling 3rd level indirect blocks, fill first row of 4th level indirect blocks, fill 2nd row 4th level indirect block's direct, 2nd level indirect blocks, first row of 3rd level indirect blocks, 3rd level direct block in 2nd row, and skip first row of 2nd indirect blocks of 4th level indirect block's 3rd level indirect block, then backfill and extend");

    /* Fill direct blocks in root indirect block */
    heap_size = 0;
    free_space = 0;
    if(fill_root_direct(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill all rows of 2nd level indirect blocks */
    if(fill_all_2nd_indirect_rows(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill all rows of 3rd level indirect blocks */
    if(fill_all_3rd_indirect_rows(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill first row of 4th level indirect blocks */
    if(fill_4th_indirect_row(fh, dxpl, cparam, &heap_size, &free_space, &nobjs, 1))
        FAIL_STACK_ERROR

    /* Account for root indirect block doubling # of rows again */
    /* (From 16 rows to the max. # of rows: 22) */
    {
        hsize_t block_size;         /* Current block size */
        hsize_t max_block_size;     /* Maximum size of blocks in heap */
        unsigned row;               /* Row in heap */

        /* Compute current & maximum block size in heap */
        block_size = cparam->managed.start_block_size * ((hsize_t)1 << 15);
        max_block_size = (((hsize_t)1 << cparam->managed.max_index) / 2) /
                cparam->managed.width;

        /* Increase heap size & free space */
        row = 16;
        while(block_size <= max_block_size) {
            heap_size += cparam->managed.width * block_size;
            free_space += cparam->managed.width * DBLOCK_FREE(fh, row);

            /* Advance to next row */
            block_size *= 2;
            row++;
        } /* end while */
    } /* end if */

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill all direct block rows in 2nd row 4th level indirect block */
    if(fill_all_direct(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill all rows of 2nd level indirect blocks in 2nd row 4th level indirect block */
    if(fill_all_2nd_indirect_rows(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill first row of 3rd level indirect blocks in 2nd row 4th level indirect block */
    if(fill_3rd_indirect_row(fh, dxpl, cparam, &heap_size, &free_space, &nobjs, 1))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill all direct block rows in 4th level indirect block's 2nd row of 3rd level indirect block */
    if(fill_all_direct(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert large object, to force creation of indirect block and
     * range of skipped (indirect) blocks that are too small to hold the large
     * object
     */
    obj_size = (1 << ((num_first_indirect_rows + H5V_log2_of2(cparam->managed.start_block_size)) - 2)) + 1;
#ifdef QAK
HDfprintf(stderr, "obj_size = %Zu\n", obj_size);
#endif /* QAK */
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 20, obj_size))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert object to fill space in (large) block created */
    obj_size = DBLOCK_FREE(fh, num_first_indirect_rows) - obj_size;
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 20, obj_size))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill rows skipped over in (first block in 2nd row  4th level indirect
     *  block's first 3rd level block's) 2nd level indirect block's direct
     * blocks (and rows of 2nd 3rd level indirect block's direct blocks)
     */
    for(u = 0; u < num_first_indirect_rows; u++) {
        /* Direct block rows in 2nd level indirect blocks */
        for(v = 0; v < cparam->managed.width; v++)
            if(fill_row(fh, dxpl, cparam, &heap_size, &free_space, u, &nobjs))
                FAIL_STACK_ERROR

        /* Direct block row in 3rd level indirect block */
        if(fill_row(fh, dxpl, cparam, &heap_size, &free_space, u, &nobjs))
            FAIL_STACK_ERROR
    } /* end for */

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Add one more object, to create another "large" block */
    obj_size = SMALL_OBJ_SIZE1;
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 10, obj_size))
        FAIL_STACK_ERROR

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_abs_fill_3rd_direct_fill_2nd_direct_fill_direct_skip_3rd_indirect_two_rows_start_block_add_skipped() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_fill_3rd_direct_fill_2nd_direct_fill_direct_skip_3rd_indirect_wrap_start_block_add_skipped
 *
 * Purpose:	Test filling all direct blocks in root indirect block and all
 *              direct blocks in 2nd level indirect blocks, fill all direct
 *              blocks and indirect blocks in 3rd level indirect block, fill all
 *              direct & indirect blocks in 4th level indirect
 *              block, then fill all direct blocks in first row of 3rd
 *              level indirect blocks in 4th level indirect block except
 *              the last (3rd level indirect block) in 4th level indirect block,
 *              fill direct blocks in last 3rd level indirect block, then
 *              insert object insert object that is too large to hold in first
 *              row of 2nd level indirect blocks of 3rd level indirect block
 *              (in 4th level indirect block) (forcing the use of the next
 *              4th level block), then backfill all skipped direct blocks &
 *              extend.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, April 17, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_fill_3rd_direct_fill_2nd_direct_fill_direct_skip_3rd_indirect_wrap_start_block_add_skipped(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    first_row_bits;         /* Number of bits used bit addresses in first row */
    unsigned    first_indirect_block_size;      /* Range of addresses covered by each of the first indirect blocks */
    unsigned    num_first_indirect_rows;        /* Number of rows (of direct blocks) in each of the first indirect blocks */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    size_t      obj_size;               /* Size of object */
    hsize_t     free_space;             /* Size of free space in heap */
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
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR

    /* Compute # of bits used in first row */
    first_row_bits = H5V_log2_of2(cparam->managed.start_block_size) +
                        H5V_log2_of2(cparam->managed.width);
    first_indirect_block_size = 2 * cparam->managed.max_direct_size;
    num_first_indirect_rows = (H5V_log2_of2(first_indirect_block_size) - first_row_bits) + 1;
#ifdef QAK
HDfprintf(stderr, "first_row_bits = %u\n", first_row_bits);
HDfprintf(stderr, "first_indirect_block_size = %u\n", first_indirect_block_size);
HDfprintf(stderr, "num_first_indirect_rows = %u\n", num_first_indirect_rows);
#endif /* QAK */

    /*
     * Test absolute heap
     */
    TESTING("filling direct blocks, filling 2nd level indirect blocks, filling 3rd level indirect blocks, fill first row of 3rd level indirect blocks in 4th level indirect block except last 3rd level block, fill direct blocks in 3rd level block, and skip row of 2nd indirect blocks of 4th level indirect block's 3rd level indirect block, then backfill and extend");

    /* Fill direct blocks in root indirect block */
    heap_size = 0;
    free_space = 0;
    if(fill_root_direct(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill all rows of 2nd level indirect blocks */
    if(fill_all_2nd_indirect_rows(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill all rows of 3rd level indirect blocks */
    if(fill_all_3rd_indirect_rows(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill all direct block rows in 4th level indirect block */
    if(fill_all_direct(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill all rows of 2nd level indirect blocks in 4th level indirect block */
    if(fill_all_2nd_indirect_rows(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill first row (except one) of 3rd level indirect blocks in 4th level indirect block */
    for(u = 0; u < cparam->managed.width - 1; u++) {
        /* Fill all direct block rows in 3rd level indirect block */
        if(fill_all_direct(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
            FAIL_STACK_ERROR

        /* Fill row of 2nd level indirect blocks in 3rd level indirect block */
        if(fill_2nd_indirect_row(fh, dxpl, cparam, &heap_size, &free_space, &nobjs, 1))
            FAIL_STACK_ERROR
    } /* end for */

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill all direct block rows in 4th level indirect block's last 3rd level indirect block */
    if(fill_all_direct(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert large object, to force creation of indirect block and
     * range of skipped (indirect) blocks that are too small to hold the large
     * object
     */
    obj_size = (1 << ((num_first_indirect_rows + H5V_log2_of2(cparam->managed.start_block_size)) - 2)) + 1;
#ifdef QAK
HDfprintf(stderr, "obj_size = %Zu\n", obj_size);
#endif /* QAK */
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 20, obj_size))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert object to fill space in (large) block created */
    obj_size = DBLOCK_FREE(fh, num_first_indirect_rows) - obj_size;
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 20, obj_size))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill rows skipped over in (4th level indirect block's first 3rd level
     * block's) 2nd level indirect block's direct blocks (and rows of next 4th
     * level indirect block's direct blocks)
     */
    for(u = 0; u < num_first_indirect_rows; u++) {
        /* Direct block rows in 2nd level indirect blocks */
        for(v = 0; v < cparam->managed.width; v++)
            if(fill_row(fh, dxpl, cparam, &heap_size, &free_space, u, &nobjs))
                FAIL_STACK_ERROR

        /* Direct block row in 4th level indirect block */
        if(fill_row(fh, dxpl, cparam, &heap_size, &free_space, u, &nobjs))
            FAIL_STACK_ERROR
    } /* end for */

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Add one more object, to create another "large" block */
    obj_size = SMALL_OBJ_SIZE1;
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 10, obj_size))
        FAIL_STACK_ERROR

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_abs_fill_3rd_direct_fill_2nd_direct_fill_direct_skip_3rd_indirect_wrap_start_block_add_skipped() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_fill_4th_direct_less_one_fill_2nd_direct_fill_direct_skip_3rd_indirect_wrap_start_block_add_skipped
 *
 * Purpose:	Test filling all direct blocks in root indirect block and all
 *              direct blocks in 2nd level indirect blocks, fill all direct
 *              blocks and indirect blocks in 3rd level indirect block, fill all
 *              direct & indirect blocks in first row of 4th level indirect
 *              blocks, except last one, then fill all direct blocks in first
 *              row of 3rd level indirect blocks in 4th level indirect block
 *              except the last (3rd level indirect block) in 4th level
 *              indirect block, fill direct blocks in last 3rd level indirect
 *              block, then insert object insert object that is too large to
 *              hold in row of 2nd level indirect blocks in 3rd level indirect
 *              block (in 4th level indirect block) (forcing the use of the
 *              next row of 4th level blocks), then backfill all skipped direct
 *              blocks & extend.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, April 17, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_fill_4th_direct_less_one_fill_2nd_direct_fill_direct_skip_3rd_indirect_wrap_start_block_add_skipped(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    first_row_bits;         /* Number of bits used bit addresses in first row */
    unsigned    first_indirect_block_size;      /* Range of addresses covered by each of the first indirect blocks */
    unsigned    num_first_indirect_rows;        /* Number of rows (of direct blocks) in each of the first indirect blocks */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    size_t      obj_size;               /* Size of object */
    hsize_t     free_space;             /* Size of free space in heap */
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
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR

    /* Compute # of bits used in first row */
    first_row_bits = H5V_log2_of2(cparam->managed.start_block_size) +
                        H5V_log2_of2(cparam->managed.width);
    first_indirect_block_size = 2 * cparam->managed.max_direct_size;
    num_first_indirect_rows = (H5V_log2_of2(first_indirect_block_size) - first_row_bits) + 1;
#ifdef QAK
HDfprintf(stderr, "first_row_bits = %u\n", first_row_bits);
HDfprintf(stderr, "first_indirect_block_size = %u\n", first_indirect_block_size);
HDfprintf(stderr, "num_first_indirect_rows = %u\n", num_first_indirect_rows);
#endif /* QAK */

    /*
     * Test absolute heap
     */
    TESTING("filling direct blocks, filling 2nd level indirect blocks, filling 3rd level indirect blocks, fill first row of 4th level indirect blocks, except last one, fill first row of 3rd level indirect blocks in last 4th level indirect block except last 3rd level block, fill direct blocks in 3rd level block, and skip row of 2nd indirect blocks of 4th level indirect block's 3rd level indirect block, then backfill and extend");

    /* Fill direct blocks in root indirect block */
    heap_size = 0;
    free_space = 0;
    if(fill_root_direct(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill all rows of 2nd level indirect blocks */
    if(fill_all_2nd_indirect_rows(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill all rows of 3rd level indirect blocks */
    if(fill_all_3rd_indirect_rows(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill first row (except one) of 4th level indirect blocks */
    for(u = 0; u < cparam->managed.width - 1; u++) {
        /* Fill all direct block rows in 4th level indirect block */
        if(fill_all_direct(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
            FAIL_STACK_ERROR

        /* Fill all rows of 2nd level indirect blocks in 4th level indirect block */
        if(fill_all_2nd_indirect_rows(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
            FAIL_STACK_ERROR

        /* Fill row of 3rd level indirect blocks in 4th level indirect block */
        if(fill_3rd_indirect_row(fh, dxpl, cparam, &heap_size, &free_space, &nobjs, 1))
            FAIL_STACK_ERROR
    } /* end for */

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill all direct block rows in 4th level indirect block */
    if(fill_all_direct(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill all rows of 2nd level indirect blocks in 4th level indirect block */
    if(fill_all_2nd_indirect_rows(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill row (except one) of 3rd level indirect blocks in 4th level indirect block */
    for(u = 0; u < cparam->managed.width - 1; u++) {
        /* Fill all direct block rows in 3rd level indirect block */
        if(fill_all_direct(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
            FAIL_STACK_ERROR

        /* Fill row of 2nd level indirect blocks in 3rd level indirect block */
        if(fill_2nd_indirect_row(fh, dxpl, cparam, &heap_size, &free_space, &nobjs, 1))
            FAIL_STACK_ERROR
    } /* end for */

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill all direct block rows in 4th level indirect block's last 3rd level indirect block */
    if(fill_all_direct(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Account for root indirect block doubling # of rows again */
    /* (From 16 rows to the max. # of rows: 22) */
    {
        hsize_t block_size;         /* Current block size */
        hsize_t max_block_size;     /* Maximum size of blocks in heap */
        unsigned row;               /* Row in heap */

        /* Compute current & maximum block size in heap */
        block_size = cparam->managed.start_block_size * ((hsize_t)1 << 15);
        max_block_size = (((hsize_t)1 << cparam->managed.max_index) / 2) /
                cparam->managed.width;

        /* Increase heap size & free space */
        row = 16;
        while(block_size <= max_block_size) {
            heap_size += cparam->managed.width * block_size;
            free_space += cparam->managed.width * DBLOCK_FREE(fh, row);

            /* Advance to next row */
            block_size *= 2;
            row++;
        } /* end while */
    } /* end if */

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert large object, to force creation of indirect block and
     * range of skipped (indirect) blocks that are too small to hold the large
     * object
     */
    obj_size = (1 << ((num_first_indirect_rows + H5V_log2_of2(cparam->managed.start_block_size)) - 2)) + 1;
#ifdef QAK
HDfprintf(stderr, "obj_size = %Zu\n", obj_size);
#endif /* QAK */
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 20, obj_size))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert object to fill space in (large) block created */
    obj_size = DBLOCK_FREE(fh, num_first_indirect_rows) - obj_size;
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 20, obj_size))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill rows skipped over in (4th level indirect block's first 3rd level
     * block's) 2nd level indirect block's direct blocks (and rows of next 4th
     * level indirect block's direct blocks)
     */
    for(u = 0; u < num_first_indirect_rows; u++) {
        /* Direct block rows in 2nd level indirect blocks */
        for(v = 0; v < cparam->managed.width; v++)
            if(fill_row(fh, dxpl, cparam, &heap_size, &free_space, u, &nobjs))
                FAIL_STACK_ERROR

        /* Direct block row in 4th level indirect block */
        if(fill_row(fh, dxpl, cparam, &heap_size, &free_space, u, &nobjs))
            FAIL_STACK_ERROR
    } /* end for */

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Add one more object, to create another "large" block */
    obj_size = SMALL_OBJ_SIZE1;
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 10, obj_size))
        FAIL_STACK_ERROR

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_abs_fill_4th_direct_less_one_fill_2nd_direct_fill_direct_skip_3rd_indirect_wrap_start_block_add_skipped() */
#endif /* QAK */

#ifndef QAK

/*-------------------------------------------------------------------------
 * Function:	test_abs_skip_direct_skip_indirect_two_rows_add_skipped
 *
 * Purpose:	Test adding object too large for all but the last row in the
 *              direct blocks in root indirect block, then
 *              add object too large for initial block in first two rows of
 *              indirect blocks, to force extension of non-root
 *              indirect block (and range of skipped blocks).
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Saturday, April 15, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_skip_direct_skip_indirect_two_rows_add_skipped(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    num_direct_rows;        /* Number of rows (of direct blocks) in root indirect block */
    unsigned    row;                    /* Current row */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    size_t      obj_size;               /* Size of object */
    hsize_t     free_space;             /* Size of free space in heap */
    hsize_t     heap_size;              /* Total size of heap */
    unsigned    v;                      /* Local index variables */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR

    /* Compute # direct block rows in root indirect block */
    num_direct_rows = (H5V_log2_of2(cparam->managed.max_direct_size) -
            H5V_log2_of2(cparam->managed.start_block_size)) + 3;

    /*
     * Test absolute heap
     */
    TESTING("skipping direct blocks to last row and skipping two rows of root indirect block, then backfill and extend");

    /* Compute heap size & free space when half direct blocks allocated */
    heap_size = 0;
    free_space = 0;
    row = 0;
    do{
        heap_size += cparam->managed.width * DBLOCK_SIZE(fh, row);
        free_space += cparam->managed.width * DBLOCK_FREE(fh, row);
        row++;
    } while(row < (num_direct_rows / 2));

    /* Insert object to extend root block to middle of root direct blocks
     */
    obj_size = (DBLOCK_SIZE(fh, row - 1) / 2) + 1;
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 10, obj_size))
        FAIL_STACK_ERROR

    /* Compute heap size & free space when all direct blocks allocated */
    do{
        heap_size += cparam->managed.width * DBLOCK_SIZE(fh, row);
        free_space += cparam->managed.width * DBLOCK_FREE(fh, row);
        row++;
    } while(row < num_direct_rows);

    /* Insert large objects into last row of direct blocks in root indirect
     * block, to force extension of root indirect block that covers the first
     * row of indirect blocks in root indirect block
     */
    obj_size = (cparam->managed.max_direct_size / 2) + 1;
    for(v = 0; v < cparam->managed.width; v++)
        if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 20, obj_size))
            FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Compute heap size & free space when root indirect block doubles again */
    do{
        heap_size += cparam->managed.width * DBLOCK_SIZE(fh, row);
        free_space += cparam->managed.width * DBLOCK_FREE(fh, row);
        row++;
    } while(row < (2 * num_direct_rows));

    /* Insert large object, to force creation of indirect blocks with
     * range of skipped blocks that are too small to hold the large object
     */
    obj_size = (cparam->managed.max_direct_size / 2) + 1;
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 20, obj_size))
        FAIL_STACK_ERROR

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_abs_skip_direct_skip_indirect_two_rows_add_skipped() */
#endif /* QAK */

#ifndef QAK

/*-------------------------------------------------------------------------
 * Function:	test_abs_frag_simple
 *
 * Purpose:	Test inserting small object to create root direct block, then
 *              insert objects small enough to fit into first row of direct
 *              blocks, but not to share a block with another object, until
 *              initial-block-size * 2 blocks are reached.  Then, go back and
 *              fill in the space in the blocks skipped.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, April 18, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_frag_simple(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    size_t      obj_size;               /* Size of object */
    hsize_t     free_space;             /* Size of free space in heap */
    hsize_t     heap_size;              /* Total size of heap */
    unsigned    u;                      /* Local index variables */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR

    /*
     * Test absolute heap
     */
    TESTING("fragmenting small blocks, then backfill and extend");

    /* Insert objects small enough to fit into initial blocks, but not to
     * share them with other objects of the same size, until the next larger
     * block size is reached.
     */
    obj_size = cparam->managed.start_block_size / 2;
    heap_size = cparam->managed.start_block_size;
    free_space = DBLOCK_FREE(fh, 0);
    for(u = 0; u < cparam->managed.width; u++) {
        if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 10, obj_size))
            FAIL_STACK_ERROR
        if(u == 0) {
            heap_size = cparam->managed.start_block_size * cparam->managed.width;
            free_space += DBLOCK_FREE(fh, 0) * (cparam->managed.width - 1);
        } /* end if */
    } /* end for */
    heap_size += cparam->managed.start_block_size * cparam->managed.width;
    free_space += DBLOCK_FREE(fh, 1) * cparam->managed.width;
    for(u = 0; u < cparam->managed.width; u++)
        if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 10, obj_size))
            FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Add one more object, to create a 2 * start_block_size block */
    /* (Account for doubling root indirect block in calc. below) */
    heap_size += (cparam->managed.start_block_size * 2) * cparam->managed.width;
    heap_size += (cparam->managed.start_block_size * 4) * cparam->managed.width;
    free_space += DBLOCK_FREE(fh, 2) * cparam->managed.width;
    free_space += DBLOCK_FREE(fh, 3) * cparam->managed.width;
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 10, obj_size))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Go back and fill in direct blocks of initial block size (which have large free space in them) */
    obj_size = DBLOCK_FREE(fh, 0) - obj_size;
    for(u = 0; u < cparam->managed.width; u++)
        if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 20, obj_size))
            FAIL_STACK_ERROR
    for(u = 0; u < cparam->managed.width; u++)
        if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 20, obj_size))
            FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill in 2 * start_block_size block */
    obj_size = DBLOCK_FREE(fh, 2) - (cparam->managed.start_block_size / 2);
    if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 20, obj_size))
        FAIL_STACK_ERROR
    
    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_abs_frag_simple() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_frag_direct
 *
 * Purpose:	Test inserting small object to fit into each direct block
 *              in root block, but not to share a block with another object,
 *              Then, go back and fill in the space in the blocks skipped.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, April 18, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_frag_direct(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    hsize_t     block_mult;             /* Base block size multiplier */
    unsigned    base_row;               /* Base row for adding new rows */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    unsigned    root_direct_rows;       /* Number of rows in root indirect block */
    size_t      obj_size;               /* Size of object */
    hsize_t     free_space;             /* Size of free space in heap */
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
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR

    /* Compute # of direct rows in root indirect block */
    root_direct_rows = (H5V_log2_of2(cparam->managed.max_direct_size) - H5V_log2_of2(cparam->managed.start_block_size)) + 2;

    /*
     * Test absolute heap
     */
    TESTING("fragmenting direct blocks, then backfill and extend");

    /* Insert objects small enough to fit into each direct block, but not to
     * share them with other objects of the same size.
     */
    obj_size = cparam->managed.start_block_size / 2;
    heap_size = cparam->managed.start_block_size;
    free_space = DBLOCK_FREE(fh, 0);
    /* First row */
    for(u = 0; u < cparam->managed.width; u++) {
        if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 10, obj_size))
            FAIL_STACK_ERROR
        if(u == 0) {
            heap_size = cparam->managed.start_block_size * cparam->managed.width;
            free_space += DBLOCK_FREE(fh, 0) * (cparam->managed.width - 1);
        } /* end if */
    } /* end for */
    heap_size += cparam->managed.start_block_size * cparam->managed.width;
    free_space += DBLOCK_FREE(fh, 1) * cparam->managed.width;
    /* Second row */
    for(u = 0; u < cparam->managed.width; u++)
        if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 10, obj_size))
            FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* (Account for doubling root indirect block for rows 3-4 */
    base_row = 2;
    block_mult = 2;
    for(u = 0; u < 2; u++) {
        heap_size += (cparam->managed.start_block_size * block_mult) * cparam->managed.width;
        free_space += DBLOCK_FREE(fh, base_row + u) * cparam->managed.width;
        block_mult *= 2;
    } /* end for */

    /* Rows 3-4 */
    block_mult = 2;
    for(u = 0; u < 2; u++) {
        obj_size = (cparam->managed.start_block_size * block_mult) / 2;
        for(v = 0; v < cparam->managed.width; v++)
            if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 10, obj_size))
                FAIL_STACK_ERROR
        block_mult *= 2;
    } /* end for */

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* (Account for doubling root indirect block for rows 5-8 */
    base_row = 4;
    block_mult = 8;
    for(u = 0; u < 4; u++) {
        heap_size += (cparam->managed.start_block_size * block_mult) * cparam->managed.width;
        free_space += DBLOCK_FREE(fh, base_row + u) * cparam->managed.width;
        block_mult *= 2;
    } /* end for */

    /* Rows 5-8 */
    block_mult = 8;
    for(u = 0; u < 4; u++) {
        obj_size = (cparam->managed.start_block_size * block_mult) / 2;
        for(v = 0; v < cparam->managed.width; v++)
            if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 10, obj_size))
                FAIL_STACK_ERROR
        block_mult *= 2;
    } /* end for */
    
    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* (Account for doubling root indirect block for rows 9-16 */
    base_row = 8;
    block_mult = 128;
    for(u = 0; u < 8; u++) {
        heap_size += (cparam->managed.start_block_size * block_mult) * cparam->managed.width;
        free_space += DBLOCK_FREE(fh, base_row + u) * cparam->managed.width;
        block_mult *= 2;
    } /* end for */

    /* Row 9 (last direct block row) */
    block_mult = 128;
    obj_size = (cparam->managed.start_block_size * block_mult) / 2;
    for(v = 0; v < cparam->managed.width; v++)
        if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 10, obj_size))
            FAIL_STACK_ERROR
    
    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Go back and backfill all root block's direct blocks */
    block_mult = 1;
    for(u = 0; u < root_direct_rows; u++) {
        obj_size = DBLOCK_FREE(fh, u) - ((cparam->managed.start_block_size * block_mult) / 2);
        for(v = 0; v < cparam->managed.width; v++)
            if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 10, obj_size))
                FAIL_STACK_ERROR
        if(u != 0)
            block_mult *= 2;
    } /* end for */

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_abs_frag_direct() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_frag_2nd_direct
 *
 * Purpose:	Test filling all direct blocks in root indirect block, then
 *              inserting small object to fit into each direct block
 *              in 2nd level indirect block, but not to share a block with
 *              another object.
 *              Then, go back and fill in the space in the blocks skipped.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, April 18, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_frag_2nd_direct(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    hsize_t     block_mult;             /* Base block size multiplier */
    unsigned    first_row_bits;         /* Number of bits used bit addresses in first row */
    unsigned    first_indirect_block_size;      /* Range of addresses covered by each of the first indirect blocks */
    unsigned    num_first_indirect_rows;        /* Number of rows (of direct blocks) in each of the first indirect blocks */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    size_t      obj_size;               /* Size of object */
    hsize_t     free_space;             /* Size of free space in heap */
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
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR

    /* Compute # of bits used in first row */
    first_row_bits = H5V_log2_of2(cparam->managed.start_block_size) +
                        H5V_log2_of2(cparam->managed.width);
    first_indirect_block_size = 2 * cparam->managed.max_direct_size;
    num_first_indirect_rows = (H5V_log2_of2(first_indirect_block_size) - first_row_bits) + 1;
#ifdef QAK
HDfprintf(stderr, "first_row_bits = %u\n", first_row_bits);
HDfprintf(stderr, "first_indirect_block_size = %u\n", first_indirect_block_size);
HDfprintf(stderr, "num_first_indirect_rows = %u\n", num_first_indirect_rows);
#endif /* QAK */

    /*
     * Test absolute heap
     */
    TESTING("fill root direct blocks, then fragment 2nd level indirect block's direct blocks, then backfill and extend");

    /* Fill direct blocks in root indirect block */
    heap_size = 0;
    free_space = 0;
    if(fill_root_direct(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert objects small enough to fit into each direct block, but not to
     * share them with other objects of the same size.
     */
    block_mult = 1;
    for(u = 0; u < num_first_indirect_rows; u++) {
        obj_size = (cparam->managed.start_block_size * block_mult) / 2;
        for(v = 0; v < cparam->managed.width; v++)
            if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 10, obj_size))
                FAIL_STACK_ERROR
        if(u != 0)
            block_mult *= 2;
    } /* end for */
    
    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Go back and backfill all 2nd level indirect block's direct blocks */
    block_mult = 1;
    for(u = 0; u < num_first_indirect_rows; u++) {
        obj_size = DBLOCK_FREE(fh, u) - ((cparam->managed.start_block_size * block_mult) / 2);
        for(v = 0; v < cparam->managed.width; v++)
            if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 10, obj_size))
                FAIL_STACK_ERROR
        if(u != 0)
            block_mult *= 2;
    } /* end for */

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_abs_frag_2nd_direct() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_frag_3rd_direct
 *
 * Purpose:	Test filling all direct blocks in root indirect block and
 *              all 2nd level indirect blocks, then
 *              inserting small object to fit into each direct block
 *              in 3rd level indirect block, but not to share a block with
 *              another object.
 *              Then, go back and fill in the space in the blocks skipped.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, April 18, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_frag_3rd_direct(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    hsize_t     block_mult;             /* Base block size multiplier */
    unsigned    root_direct_rows;       /* Number of rows in root indirect block */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    size_t      obj_size;               /* Size of object */
    hsize_t     free_space;             /* Size of free space in heap */
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
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR

    /* Compute # of direct rows in root indirect block */
    root_direct_rows = (H5V_log2_of2(cparam->managed.max_direct_size) - H5V_log2_of2(cparam->managed.start_block_size)) + 2;

    /*
     * Test absolute heap
     */
    TESTING("fill root direct blocks and 2nd level indirect blocks, then fragment 3rd level indirect block's direct blocks, then backfill and extend");

    /* Fill direct blocks in root indirect block */
    heap_size = 0;
    free_space = 0;
    if(fill_root_direct(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Fill all rows of 2nd level indirect blocks in root indirect block */
    if(fill_all_2nd_indirect_rows(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Insert objects small enough to fit into each direct block, but not to
     * share them with other objects of the same size.
     */
    block_mult = 1;
    for(u = 0; u < root_direct_rows; u++) {
        obj_size = (cparam->managed.start_block_size * block_mult) / 2;
        for(v = 0; v < cparam->managed.width; v++)
            if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 10, obj_size))
                FAIL_STACK_ERROR
        if(u != 0)
            block_mult *= 2;
    } /* end for */

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Go back and backfill all 3rd level indirect block's direct blocks */
    block_mult = 1;
    for(u = 0; u < root_direct_rows; u++) {
        obj_size = DBLOCK_FREE(fh, u) - ((cparam->managed.start_block_size * block_mult) / 2);
        for(v = 0; v < cparam->managed.width; v++)
            if(add_obj(fh, dxpl, heap_size, &free_space, &nobjs, 10, obj_size))
                FAIL_STACK_ERROR
        if(u != 0)
            block_mult *= 2;
    } /* end for */

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_abs_frag_3rd_direct() */
#endif /* QAK */

#ifndef QAK

/*-------------------------------------------------------------------------
 * Function:	test_abs_random_managed
 *
 * Purpose:	Test inserting random sized objects (that are smaller than
 *              the standalone size) into a heap, and read them back.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, May  9, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_random_managed(hsize_t size_limit, hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned long seed;                 /* Random # seed */
    size_t      num_ids = 0;            /* # of heap IDs in array */
    size_t      alloc_ids = 0;          /* # of heap IDs allocated in array */
    unsigned char *obj = NULL;          /* Buffer for object to insert */
    unsigned char *robj = NULL;         /* Buffer for reading object */
    hsize_t     total_obj_added;        /* Size of objects added */
    size_t      obj_size;               /* Size of object */
    size_t      obj_loc;                /* Location of object in buffer */
    unsigned char *ids = NULL;          /* Array of heap IDs */
    unsigned    u;                      /* Local index variables */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
#ifdef QAK
HDfprintf(stderr, "Fractal heap header address: %a\n", fh_addr);
#endif /* QAK */

    /*
     * Test absolute heap
     */
    TESTING("inserting random-sized objects (smaller than standalone size)");

    /* Choose random # seed */
    seed = (unsigned long)HDtime(NULL);
#ifdef QAK
HDfprintf(stderr, "Random # seed was: %lu\n", seed);
#endif /* QAK */
    HDsrandom(seed);

    /* Initialize the buffer for objects to insert */
    obj = H5MM_malloc(cparam->standalone_size);
    for(u = 0; u < cparam->standalone_size; u++)
        obj[u] = (unsigned char)u;

    /* Loop over adding objects to the heap, until the size limit is reached */
    total_obj_added = 0;
    while(total_obj_added < size_limit) {
        /* Choose a random size of object (from 1 up to stand alone block size) */
        obj_size = (HDrandom() % (cparam->standalone_size - 1)) + 1;

        /* Increment object count */
        num_ids++;
#ifdef QAK
HDfprintf(stderr, "num_ids = %Zu, total_obj_added = %Hu, obj_size = %Zu\n", num_ids, total_obj_added, obj_size);
#endif /* QAK */

        /* Check for needing to increase size of heap ID array */
        if(num_ids > alloc_ids) {
            alloc_ids = MAX(1024, (alloc_ids * 2));
            if(NULL == (ids = H5MM_realloc(ids, HEAP_ID_LEN * alloc_ids)))
                FAIL_STACK_ERROR
        } /* end if */

        /* Insert object */
        obj_loc = cparam->standalone_size - obj_size;
        if(H5HF_insert(fh, dxpl, obj_size, &obj[obj_loc], &ids[(num_ids - 1) * HEAP_ID_LEN]) < 0)
            FAIL_STACK_ERROR

        /* Check for closing & re-opening the heap */
        if(tparam->reopen_heap) {
            /* Close heap */
            if(H5HF_close(fh, dxpl) < 0)
                TEST_ERROR

            /* Re-open heap */
            if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
                FAIL_STACK_ERROR
        } /* end if */

        /* Increment the amount of objects added */
        total_obj_added += obj_size;
    } /* end while */

    /* Allocate buffer for reading objects */
    robj = H5MM_malloc(cparam->standalone_size);

    /* Verify reading the objects written out */
    for(u = 0; u < num_ids; u++) {
        /* Get object length */
        if(H5HF_get_obj_len(fh, &ids[u * HEAP_ID_LEN], &obj_size) < 0)
            FAIL_STACK_ERROR

        /* Clear read buffer */
        HDmemset(robj, 0, obj_size);

        /* Read in object */
        if(H5HF_read(fh, dxpl, &ids[u * HEAP_ID_LEN], robj) < 0)
            FAIL_STACK_ERROR

        /* Check for correct object */
        obj_loc = cparam->standalone_size - obj_size;
        if(HDmemcmp(&obj[obj_loc], robj, obj_size))
            FAIL_STACK_ERROR
    } /* end for */

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    H5MM_xfree(obj);
    H5MM_xfree(robj);
    H5MM_xfree(ids);
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
        H5MM_xfree(obj);
        H5MM_xfree(robj);
        H5MM_xfree(ids);
    } H5E_END_TRY;
    HDfprintf(stderr, "Random # seed was: %lu\n", seed);
    return(1);
} /* test_abs_random_managed() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_random_pow2_managed
 *
 * Purpose:	Test inserting random sized objects (that are smaller than the
 *              standalone size) with a "power of 2 distribution" into a heap,
 *              and read them back.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, May 15, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_random_pow2_managed(hsize_t size_limit, hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned long seed;                 /* Random # seed */
    size_t      num_ids = 0;            /* # of heap IDs in array */
    size_t      alloc_ids = 0;          /* # of heap IDs allocated in array */
    unsigned char *obj = NULL;          /* Buffer for object to insert */
    unsigned char *robj = NULL;         /* Buffer for reading object */
    hsize_t     total_obj_added;        /* Size of objects added */
    size_t      obj_size;               /* Size of object */
    size_t      obj_loc;                /* Location of object in buffer */
    unsigned char *ids = NULL;          /* Array of heap IDs */
    unsigned    u;                      /* Local index variables */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
#ifdef QAK
HDfprintf(stderr, "Fractal heap header address: %a\n", fh_addr);
#endif /* QAK */

    /*
     * Test absolute heap
     */
    TESTING("inserting random-sized objects with power of 2 distribution (smaller than standalone size)");

    /* Choose random # seed */
    seed = (unsigned long)HDtime(NULL);
#ifdef QAK
HDfprintf(stderr, "Random # seed was: %lu\n", seed);
#endif /* QAK */
    HDsrandom(seed);

    /* Initialize the buffer for objects to insert */
    obj = H5MM_malloc(cparam->standalone_size);
    for(u = 0; u < cparam->standalone_size; u++)
        obj[u] = (unsigned char)u;

    /* Loop over adding objects to the heap, until the size limit is reached */
    total_obj_added = 0;
    while(total_obj_added < size_limit) {
        unsigned size_range = 10;       /* Object size range */

        /* Determine the size of the range for this object */
        /* (50% of the objects inserted will use the initial size range,
         *      25% of the objects will be twice as large, 12.5% will be
         *      four times larger, etc.)
         */
        while(HDrandom() < (RAND_MAX / 2) && size_range < cparam->standalone_size)
            size_range *= 2;
        if(size_range > cparam->standalone_size)
            size_range = cparam->standalone_size;

        /* Choose a random size of object (from 1 up to stand alone block size) */
        obj_size = (HDrandom() % (size_range - 1)) + 1;

        /* Increment object count */
        num_ids++;
#ifdef QAK
if((num_ids % 100000) == 1)
    HDfprintf(stderr, "num_ids = %Zu, total_obj_added = %Hu, obj_size = %Zu\n", num_ids, total_obj_added, obj_size);
#endif /* QAK */

        /* Check for needing to increase size of heap ID array */
        if(num_ids > alloc_ids) {
            alloc_ids = MAX(1024, (alloc_ids * 2));
            if(NULL == (ids = H5MM_realloc(ids, HEAP_ID_LEN * alloc_ids)))
                FAIL_STACK_ERROR
        } /* end if */

        /* Insert object */
        obj_loc = cparam->standalone_size - obj_size;
        if(H5HF_insert(fh, dxpl, obj_size, &obj[obj_loc], &ids[(num_ids - 1) * HEAP_ID_LEN]) < 0)
            FAIL_STACK_ERROR

        /* Check for closing & re-opening the heap */
        if(tparam->reopen_heap) {
            /* Close heap */
            if(H5HF_close(fh, dxpl) < 0)
                TEST_ERROR

            /* Re-open heap */
            if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
                FAIL_STACK_ERROR
        } /* end if */

        /* Increment the amount of objects added */
        total_obj_added += obj_size;
    } /* end while */

    /* Allocate buffer for reading objects */
    robj = H5MM_malloc(cparam->standalone_size);

    /* Verify reading the objects written out */
    for(u = 0; u < num_ids; u++) {
        /* Get object length */
        if(H5HF_get_obj_len(fh, &ids[u * HEAP_ID_LEN], &obj_size) < 0)
            FAIL_STACK_ERROR

        /* Clear read buffer */
        HDmemset(robj, 0, obj_size);

        /* Read in object */
        if(H5HF_read(fh, dxpl, &ids[u * HEAP_ID_LEN], robj) < 0)
            FAIL_STACK_ERROR

        /* Check for correct object */
        obj_loc = cparam->standalone_size - obj_size;
        if(HDmemcmp(&obj[obj_loc], robj, obj_size))
            FAIL_STACK_ERROR
    } /* end for */

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    H5MM_xfree(obj);
    H5MM_xfree(robj);
    H5MM_xfree(ids);
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
        H5MM_xfree(obj);
        H5MM_xfree(robj);
        H5MM_xfree(ids);
    } H5E_END_TRY;
    HDfprintf(stderr, "Random # seed was: %lu\n", seed);
    return(1);
} /* test_abs_random_pow2_managed() */
#endif /* QAK */

#ifndef QAK

/*-------------------------------------------------------------------------
 * Function:	test_abs_remove_bogus
 *
 * Purpose:	Test removing bogus heap IDs
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, May 15, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_remove_bogus(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    unsigned char heap_id[HEAP_ID_LEN]; /* Heap ID for object */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    nobjs = 0;              /* Number of objects inserted */
    hsize_t     free_space;             /* Size of free space in heap */
    hsize_t     heap_size;              /* Total size of heap */
    hsize_t     obj_off;                /* Offset of object in heap */
    unsigned    u;                      /* Local index variable */
    herr_t      ret;                    /* Generic return value */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, dxpl, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    if(check_stats(fh, (hsize_t)0, (hsize_t)0, (hsize_t)0, (hsize_t)0, (hsize_t)0))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close (empty) heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /*
     * Test removing bogus IDs from heap
     */
    TESTING("removing bad heap IDs from absolute heap");

    /* Set heap ID to random (non-null) value */
    for(u = 0; u < HEAP_ID_LEN; u++)
        heap_id[u] = HDrandom() + 1;

    /* Try removing bogus heap ID from empty heap */
    H5E_BEGIN_TRY {
        ret = H5HF_remove(fh, dxpl, heap_id);
    } H5E_END_TRY;
    if(ret >= 0)
        FAIL_STACK_ERROR

    /* Fill root direct blocks */
    heap_size = 0;
    free_space = 0;
    if(fill_root_direct(fh, dxpl, cparam, &heap_size, &free_space, &nobjs))
        FAIL_STACK_ERROR

    /* Get offset of random heap ID */
    if(H5HF_get_id_off_test(fh, heap_id, &obj_off) < 0)
        FAIL_STACK_ERROR

    /* Make certain we can't accidentally use a valid heap ID */
    while(obj_off < heap_size) {
        /* Set heap ID to random (non-null) value */
        for(u = 0; u < HEAP_ID_LEN; u++)
            heap_id[u] = HDrandom() + 1;

        /* Get offset of random heap ID */
        if(H5HF_get_id_off_test(fh, heap_id, &obj_off) < 0)
            FAIL_STACK_ERROR
    } /* end while */

    /* Try removing bogus heap ID from empty heap */
    H5E_BEGIN_TRY {
        ret = H5HF_remove(fh, dxpl, heap_id);
    } H5E_END_TRY;
    if(ret >= 0)
        FAIL_STACK_ERROR

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_abs_remove_bogus() */
#endif /* QAK */


/*-------------------------------------------------------------------------
 * Function:	test_abs_remove_one
 *
 * Purpose:	Test removing single object from heap
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, May 15, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_remove_one(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    unsigned char heap_id[HEAP_ID_LEN]; /* Heap ID for object */
    unsigned char obj[SMALL_OBJ_SIZE1]; /* Buffer for object to insert */
    size_t      id_len;                 /* Size of fractal heap IDs */
    hsize_t     free_space;             /* Size of free space in heap */
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
    if(NULL == (fh = H5HF_create(f, dxpl, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    if(check_stats(fh, (hsize_t)0, (hsize_t)0, (hsize_t)0, (hsize_t)0, (hsize_t)0))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close (empty) heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /*
     * Test removing first (small) object from absolute heap
     */
    TESTING("removing single object from absolute heap");

    /* Initialize the buffer for objects to insert */
    for(u = 0; u < sizeof(obj); u++)
        obj[u] = u;

    /* Insert object into heap */
    if(H5HF_insert(fh, dxpl, sizeof(obj), obj, &heap_id) < 0)
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close (empty) heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Check up on heap... */
    heap_size = DBLOCK_SIZE(fh, 0);
    free_space = DBLOCK_FREE(fh, 0) - sizeof(obj);
    if(check_stats(fh, heap_size, heap_size, (hsize_t)0, free_space, (hsize_t)1))
        FAIL_STACK_ERROR

    /* Remove object from heap */
    if(H5HF_remove(fh, dxpl, heap_id) < 0)
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close (empty) heap */
        if(H5HF_close(fh, dxpl) < 0)
            TEST_ERROR

        /* Re-open heap */
        if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Check up on heap... */
    if(check_stats(fh, (hsize_t)0, (hsize_t)0, (hsize_t)0, (hsize_t)0, (hsize_t)0))
        FAIL_STACK_ERROR

    PASSED()

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_abs_remove_one() */


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
    fheap_test_param_t tparam;          /* Testing parameters */
    H5HF_create_t cparam;               /* Creation parameters for heap */
    hid_t	fapl = -1;              /* File access property list for data files */
    fheap_test_type_t curr_test;        /* Current test being worked on */
    unsigned	nerrors = 0;            /* Cumulative error count */

    /* Reset library */
    h5_reset();
    fapl = h5_fileaccess();

    /* Initialize heap's creation parameters */
    init_small_cparam(&cparam);

    /* Iterate over the testing parameters */
#ifndef QAK
    for(curr_test = FHEAP_TEST_NORMAL; curr_test < FHEAP_TEST_NTESTS; curr_test++) {
#else /* QAK */
HDfprintf(stderr, "Uncomment test loop!\n");
curr_test = FHEAP_TEST_NORMAL;
/* curr_test = FHEAP_TEST_REOPEN; */
#endif /* QAK */
        /* Clear the testing parameters */
        HDmemset(&tparam, 0, sizeof(fheap_test_param_t));

        /* Set appropriate testing parameters for each test */
        switch(curr_test) {
            /* "Normal" testing parameters */
            case FHEAP_TEST_NORMAL:
                puts("Testing with normal parameters");
                break;

            /* "Re-open heap" testing parameters */
            case FHEAP_TEST_REOPEN:
                puts("Testing with reopen heap flag set");
                tparam.reopen_heap = TRUE;
                break;

            /* An unknown test? */
            default:
                goto error;
        } /* end switch */

        /* Test fractal heap creation */
        nerrors += test_create(fapl, &cparam, &tparam);
        nerrors += test_reopen(fapl, &cparam, &tparam);

        /*
         * Test fractal heap object insertion
         */
#ifdef ALL_INSERT_TESTS
        /* Simple insertion */
        nerrors += test_abs_insert_first(fapl, &cparam, &tparam);
        nerrors += test_abs_insert_second(fapl, &cparam, &tparam);
        nerrors += test_abs_insert_root_mult(fapl, &cparam, &tparam);
        nerrors += test_abs_insert_force_indirect(fapl, &cparam, &tparam);
        nerrors += test_abs_insert_fill_second(fapl, &cparam, &tparam);
        nerrors += test_abs_insert_third_direct(fapl, &cparam, &tparam);
        nerrors += test_abs_fill_first_row(fapl, &cparam, &tparam);
        nerrors += test_abs_start_second_row(fapl, &cparam, &tparam);
        nerrors += test_abs_fill_second_row(fapl, &cparam, &tparam);
        nerrors += test_abs_start_third_row(fapl, &cparam, &tparam);
        nerrors += test_abs_fill_fourth_row(fapl, &cparam, &tparam);
        nerrors += test_abs_fill_all_root_direct(fapl, &cparam, &tparam);
        nerrors += test_abs_first_recursive_indirect(fapl, &cparam, &tparam);
        nerrors += test_abs_second_direct_recursive_indirect(fapl, &cparam, &tparam);
        nerrors += test_abs_fill_first_recursive_indirect(fapl, &cparam, &tparam);
        nerrors += test_abs_second_recursive_indirect(fapl, &cparam, &tparam);
        nerrors += test_abs_fill_second_recursive_indirect(fapl, &cparam, &tparam);
        nerrors += test_abs_fill_recursive_indirect_row(fapl, &cparam, &tparam);
        nerrors += test_abs_start_2nd_recursive_indirect(fapl, &cparam, &tparam);
        nerrors += test_abs_recursive_indirect_two_deep(fapl, &cparam, &tparam);
        nerrors += test_abs_start_3rd_recursive_indirect(fapl, &cparam, &tparam);
        nerrors += test_abs_fill_first_3rd_recursive_indirect(fapl, &cparam, &tparam);
        nerrors += test_abs_fill_3rd_recursive_indirect_row(fapl, &cparam, &tparam);
        nerrors += test_abs_fill_all_3rd_recursive_indirect(fapl, &cparam, &tparam);
        nerrors += test_abs_start_4th_recursive_indirect(fapl, &cparam, &tparam);
        nerrors += test_abs_fill_first_4th_recursive_indirect(fapl, &cparam, &tparam);
        nerrors += test_abs_fill_4th_recursive_indirect_row(fapl, &cparam, &tparam);
        nerrors += test_abs_fill_all_4th_recursive_indirect(fapl, &cparam, &tparam);
#endif /* ALL_INSERT_TESTS */
        /* If this test fails, uncomment the tests above, which build up to this
         * level of complexity gradually. -QAK
         */
#ifndef QAK
        nerrors += test_abs_start_5th_recursive_indirect(fapl, &cparam, &tparam);
#else /* QAK */
HDfprintf(stderr, "Uncomment tests!\n");
#endif /* QAK */

#ifndef QAK
        /* Skip blocks insertion */
        nerrors += test_abs_skip_start_block(fapl, &cparam, &tparam);
        nerrors += test_abs_skip_start_block_add_back(fapl, &cparam, &tparam);
        nerrors += test_abs_skip_start_block_add_skipped(fapl, &cparam, &tparam);
        nerrors += test_abs_skip_2nd_block(fapl, &cparam, &tparam);
        nerrors += test_abs_skip_2nd_block_add_skipped(fapl, &cparam, &tparam);
        nerrors += test_abs_fill_one_partial_skip_2nd_block_add_skipped(fapl, &cparam, &tparam);
        nerrors += test_abs_fill_row_skip_add_skipped(fapl, &cparam, &tparam);
        nerrors += test_abs_fill_direct_skip_indirect_start_block_add_skipped(fapl, &cparam, &tparam);
        nerrors += test_abs_fill_direct_skip_2nd_indirect_start_block_add_skipped(fapl, &cparam, &tparam);
        nerrors += test_abs_fill_2nd_direct_less_one_wrap_start_block_add_skipped(fapl, &cparam, &tparam);
        nerrors += test_abs_fill_direct_skip_2nd_indirect_skip_2nd_block_add_skipped(fapl, &cparam, &tparam);
        nerrors += test_abs_fill_direct_skip_indirect_two_rows_add_skipped(fapl, &cparam, &tparam);
        nerrors += test_abs_fill_2nd_direct_skip_start_block_add_skipped(fapl, &cparam, &tparam);
        nerrors += test_abs_fill_2nd_direct_skip_2nd_indirect_start_block_add_skipped(fapl, &cparam, &tparam);
        nerrors += test_abs_fill_2nd_direct_fill_direct_skip_3rd_indirect_start_block_add_skipped(fapl, &cparam, &tparam);
        nerrors += test_abs_fill_2nd_direct_fill_direct_skip2_3rd_indirect_start_block_add_skipped(fapl, &cparam, &tparam);
        nerrors += test_abs_fill_3rd_direct_less_one_fill_direct_wrap_start_block_add_skipped(fapl, &cparam, &tparam);
        nerrors += test_abs_fill_1st_row_3rd_direct_fill_2nd_direct_less_one_wrap_start_block_add_skipped(fapl, &cparam, &tparam);
        nerrors += test_abs_fill_3rd_direct_fill_direct_skip_start_block_add_skipped(fapl, &cparam, &tparam);
        nerrors += test_abs_fill_3rd_direct_fill_2nd_direct_fill_direct_skip_3rd_indirect_start_block_add_skipped(fapl, &cparam, &tparam);
        nerrors += test_abs_fill_3rd_direct_fill_2nd_direct_fill_direct_skip_3rd_indirect_two_rows_start_block_add_skipped(fapl, &cparam, &tparam);
        nerrors += test_abs_fill_3rd_direct_fill_2nd_direct_fill_direct_skip_3rd_indirect_wrap_start_block_add_skipped(fapl, &cparam, &tparam);
        nerrors += test_abs_fill_4th_direct_less_one_fill_2nd_direct_fill_direct_skip_3rd_indirect_wrap_start_block_add_skipped(fapl, &cparam, &tparam);
#else /* QAK */
HDfprintf(stderr, "Uncomment tests!\n");
#endif /* QAK */

        /* Additional skipped block insertion tests */
#ifndef QAK
        nerrors += test_abs_skip_direct_skip_indirect_two_rows_add_skipped(fapl, &cparam, &tparam);
#else /* QAK */
HDfprintf(stderr, "Uncomment tests!\n");
#endif /* QAK */

        /* Fragmented block insertion */
#ifndef QAK
        nerrors += test_abs_frag_simple(fapl, &cparam, &tparam);
        nerrors += test_abs_frag_direct(fapl, &cparam, &tparam);
        nerrors += test_abs_frag_2nd_direct(fapl, &cparam, &tparam);
        nerrors += test_abs_frag_3rd_direct(fapl, &cparam, &tparam);
#else /* QAK */
HDfprintf(stderr, "Uncomment tests!\n");
#endif /* QAK */

        /* Random object insertion */
#ifndef QAK
        nerrors += test_abs_random_managed((hsize_t)(100*1000*1000), fapl, &cparam, &tparam);
        nerrors += test_abs_random_pow2_managed((hsize_t)(100*1000*1000), fapl, &cparam, &tparam);
#else /* QAK */
HDfprintf(stderr, "Uncomment tests!\n");
#endif /* QAK */

        /*
         * Test fractal heap object deletion
         */
        /* Simple removal */
#ifndef QAK
        nerrors += test_abs_remove_bogus(fapl, &cparam, &tparam);
#else /* QAK */
HDfprintf(stderr, "Uncomment tests!\n");
#endif /* QAK */
        nerrors += test_abs_remove_one(fapl, &cparam, &tparam);
#ifndef QAK
    } /* end for */
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
} /* end main() */

