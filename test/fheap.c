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
#define HEAP_ID_LEN             6               /* # of bytes to use for heap ID */
#define HEAP_MAX_ROOT_ROWS(fh) H5HF_get_max_root_rows(fh)       /* Max. # of rows in root indirect block */
#define DTABLE_WIDTH(fh) H5HF_get_dtable_width_test(fh) /* Width of doubling table for heap */
#define DTABLE_MAX_DROWS(fh) H5HF_get_dtable_max_drows_test(fh) /* Max. # of direct block rows in any indirect block */
#define IBLOCK_MAX_DROWS(fh, pos) H5HF_get_iblock_max_drows_test(fh, pos) /* Max. # of direct block rows in a indirect block */
#define DBLOCK_SIZE(fh, r) H5HF_get_dblock_size_test(fh, r)     /* Size of a direct block in a given row */
#define DBLOCK_FREE(fh, r) H5HF_get_dblock_free_test(fh, r)     /* Free space in a direct block of a given row */

const char *FILENAME[] = {
    "fheap",
    NULL
};

/* Types of tests to perform */
typedef enum {
    FHEAP_TEST_NORMAL,          /* "Normal" test, with no testing parameters set */
    FHEAP_TEST_REOPEN,          /* Set the reopen_heap flag */
    FHEAP_TEST_NTESTS           /* The number of test types, must be last */
} fheap_test_type_t;

/* Order to delete objects */
typedef enum {
    HEAP_DEL_FORWARD,           /* Delete objects from 0 -> nobjs */
    HEAP_DEL_REVERSE,           /* Delete objects from nobjs -> 0 */
    HEAP_DEL_NDIRS              /* The number of different deletion orders, must be last */
} fheap_test_del_dir_t;

/* Order to delete objects */
typedef enum {
    HEAP_DEL_DRAIN_ALL,         /* Don't drain half of objects first */
    HEAP_DEL_DRAIN_HALF,        /* Don't drain half of objects first */
    HEAP_DEL_DRAIN_N            /* The number of different ways to drain, must be last */
} fheap_test_del_drain_t;

/* Testing parameters */
typedef struct fheap_test_param_t {
    fheap_test_type_t reopen_heap;      /* Whether to re-open the heap during the test */
    fheap_test_del_dir_t del_dir;       /* Whether to delete objects forward or reverse */
    fheap_test_del_drain_t drain_half;  /* Whether to drain half of the objects & refill, when deleting objects */
} fheap_test_param_t;

/* Heap state information */
typedef struct fheap_heap_state_t {
    hsize_t     heap_size;              /* Total size of heap (managed & standalone objects) */
    size_t      nobjs;                  /* # of objects within heap */
    hsize_t     man_size;               /* Size of managed object heap */
    hsize_t     man_alloc_size;         /* Size of managed object heap allocated */
    hsize_t     man_free_space;         /* Managed object free space within heap */
} fheap_heap_state_t;

/* Heap IDs to retain */
typedef struct fheap_heap_ids_t {
    size_t      num_ids;        /* # of heap IDs in array */
    size_t      alloc_ids;      /* # of heap IDs allocated in array */
    unsigned char *ids;         /* Array of object heap IDs */
    size_t *lens;               /* Array of object lengths */
    size_t *offs;               /* Array of object offsets (in global shared write buffer) */
} fheap_heap_ids_t;

/* Local variables */
unsigned char *shared_wobj_g;   /* Pointer to shared write buffer for objects */
unsigned char *shared_robj_g;   /* Pointer to shared read buffer for objects */
size_t shared_obj_size_g;       /* Size of shared objects */
unsigned char *shared_ids_g = NULL;     /* Array of shared object heap IDs */
size_t *shared_lens_g = NULL;   /* Array of shared object lengths */
size_t *shared_offs_g = NULL;   /* Array of shared object offsets */
size_t shared_alloc_ids_g = 0;  /* # of shared heap IDs allocated in array */

/* Local routines */
static int init_small_cparam(H5HF_create_t *cparam);
static int check_stats(const H5HF_t *fh, const fheap_heap_state_t *state);


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
check_stats(const H5HF_t *fh, const fheap_heap_state_t *state)
{
    H5HF_stat_t heap_stats;             /* Statistics about the heap */

    /* Get statistics for heap and verify they are correct */
    if(H5HF_stat_info(fh, &heap_stats) < 0)
        FAIL_STACK_ERROR
    if(heap_stats.total_size != state->heap_size) {
        HDfprintf(stdout, "heap_stats.total_size = %Hu, state->heap_size = %Hu\n", heap_stats.total_size, state->heap_size);
        FAIL_STACK_ERROR
    } /* end if */
    if(heap_stats.nobjs != state->nobjs) {
        HDfprintf(stdout, "heap_stats.nobjs = %Hu, state->nobjs = %Hu\n", heap_stats.nobjs, state->nobjs);
        FAIL_STACK_ERROR
    } /* end if */
    if(heap_stats.man_size != state->man_size) {
        HDfprintf(stdout, "heap_stats.man_size = %Hu, state->man_size = %Hu\n", heap_stats.man_size, state->man_size);
        FAIL_STACK_ERROR
    } /* end if */
    if(heap_stats.man_alloc_size != state->man_alloc_size) {
        HDfprintf(stdout, "heap_stats.man_alloc_size = %Hu, state->man_alloc_size = %Hu\n", heap_stats.man_alloc_size, state->man_alloc_size);
        FAIL_STACK_ERROR
    } /* end if */
    if(heap_stats.man_free_space != state->man_free_space) {
        HDfprintf(stdout, "heap_stats.man_free_space = %Hu, state->man_free_space = %Hu\n", heap_stats.man_free_space, state->man_free_space);
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
 * Note:        The following fields in the 'state' structure are set to
 *              the values expected _after_ any block created for the object:
 *                      heap_size
 *                      man_size
 *                      man_alloc_size
 *                      man_free_space
 *
 *              The following fields in the 'state' structure are set to
 *              the current state, before any block has been created:
 *                      nobjs
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
add_obj(H5HF_t *fh, hid_t dxpl, unsigned obj_off, size_t obj_size,
    fheap_heap_state_t *state, fheap_heap_ids_t *keep_ids)
{
    unsigned char heap_id[HEAP_ID_LEN]; /* Heap ID for object inserted */
    unsigned char *obj;                 /* Buffer for object to insert */

    /* Sanity check */
    HDassert(fh);
    HDassert(state);

    /* Initialize object buffer */
    obj = &shared_wobj_g[obj_off];

    /* Insert object */
    HDmemset(heap_id, 0, sizeof(heap_id));
    if(H5HF_insert(fh, dxpl, obj_size, obj, heap_id) < 0)
        FAIL_STACK_ERROR

    /* Adjust state of heap */
    state->nobjs++;
    state->man_free_space -= obj_size;

    /* Check free space left in heap */
    if(check_stats(fh, state))
        FAIL_STACK_ERROR

    /* Read in object */
    if(H5HF_read(fh, dxpl, heap_id, shared_robj_g) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(obj, shared_robj_g, obj_size))
        FAIL_STACK_ERROR

    /* If the heap IDs are to be retained, append them to the list */
    if(keep_ids) {
        /* Check for needing to increase size of heap ID array */
        if(keep_ids->num_ids + 1 > keep_ids->alloc_ids) {
            keep_ids->alloc_ids = MAX(1024, (keep_ids->alloc_ids * 2));
            if(NULL == (keep_ids->ids = H5MM_realloc(keep_ids->ids, HEAP_ID_LEN * keep_ids->alloc_ids)))
                FAIL_STACK_ERROR
            if(NULL == (keep_ids->lens = H5MM_realloc(keep_ids->lens, sizeof(size_t) * keep_ids->alloc_ids)))
                FAIL_STACK_ERROR
            if(NULL == (keep_ids->offs = H5MM_realloc(keep_ids->offs, sizeof(size_t) * keep_ids->alloc_ids)))
                FAIL_STACK_ERROR
        } /* end if */

        /* Append the object info onto the array */
        HDmemcpy(&keep_ids->ids[keep_ids->num_ids * HEAP_ID_LEN], obj, HEAP_ID_LEN);
        keep_ids->lens[keep_ids->num_ids] = obj_size;
        keep_ids->offs[keep_ids->num_ids] = obj_off;

        /* Increment the number of IDs kept */
        keep_ids->num_ids++;
    } /* end if */

    /* Operations succeeded */
    return(0);

error:
    return(1);
} /* add_obj() */


/*-------------------------------------------------------------------------
 * Function:	get_del_string
 *
 * Purpose:	Return string describing the kind of deletion to perform
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, June  6, 2006
 *
 *-------------------------------------------------------------------------
 */
static char *
get_del_string(fheap_test_param_t *tparam)
{
    char *str;

    /* Remove half of total objects from heap */
    if(tparam->del_dir == HEAP_DEL_FORWARD)
        if(tparam->drain_half == HEAP_DEL_DRAIN_ALL)
            str = HDstrdup("(all - forward)");
        else
            str = HDstrdup("(half, refill, all - forward)");
    else
        if(tparam->drain_half == HEAP_DEL_DRAIN_ALL)
            str = HDstrdup("(all - reverse)");
        else
            str = HDstrdup("(half, refill, all - reverse)");

    return(str);
} /* get_del_string() */


/*-------------------------------------------------------------------------
 * Function:	del_objs_half_refill
 *
 * Purpose:	Remove half of objects from heap and refill
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, June  6, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
del_objs_half_refill(H5F_t *f, hid_t dxpl, H5HF_t **fh, fheap_test_param_t *tparam,
    fheap_heap_state_t *state, fheap_heap_ids_t *keep_ids)
{
    unsigned char *wobj;        /* Buffer for object to insert */
    haddr_t fh_addr;            /* Address of fractal heap */
    size_t half_nobjs;          /* Half of total # of objects */
    size_t obj_idx;             /* Index of the object to remove */
    size_t u;                   /* Local index variable */

    /* Sanity check */
    HDassert(fh);
    HDassert(*fh);
    HDassert(state);
    HDassert(keep_ids);

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        if(H5HF_get_heap_addr(*fh, &fh_addr) < 0)
            FAIL_STACK_ERROR
        if(!H5F_addr_defined(fh_addr))
            FAIL_STACK_ERROR
    } /* end if */

    /* Remove half of total objects from heap */
    if(tparam->del_dir == HEAP_DEL_FORWARD)
        obj_idx = 0;
    else
        obj_idx = state->nobjs - 1;
    half_nobjs = state->nobjs / 2;
    for(u = 0; u < half_nobjs; u++) {
        /* Remove object from heap */
        if(H5HF_remove(*fh, dxpl, &keep_ids->ids[HEAP_ID_LEN * obj_idx]) < 0)
            FAIL_STACK_ERROR

        /* Check for closing & re-opening the heap */
        if(tparam->reopen_heap) {
            /* Close (empty) heap */
            if(H5HF_close(*fh, dxpl) < 0)
                TEST_ERROR

            /* Re-open heap */
            if(NULL == (*fh = H5HF_open(f, dxpl, fh_addr)))
                FAIL_STACK_ERROR
        } /* end if */

        /* Adjust index of object to delete next */
        if(tparam->del_dir == HEAP_DEL_FORWARD)
            obj_idx++;
        else
            obj_idx--;
    } /* end for */

    /* Re-insert half of total objects back into heap */
    if(tparam->del_dir == HEAP_DEL_FORWARD)
        obj_idx = 0;
    else
        obj_idx = state->nobjs - 1;
    for(u = 0; u < half_nobjs; u++) {
        /* Re-insert object */
        wobj = &shared_wobj_g[keep_ids->offs[obj_idx]];
        if(H5HF_insert(*fh, dxpl, keep_ids->lens[obj_idx], wobj, &keep_ids->ids[HEAP_ID_LEN * obj_idx]) < 0)
            FAIL_STACK_ERROR

        /* Check for closing & re-opening the heap */
        if(tparam->reopen_heap) {
            /* Close (empty) heap */
            if(H5HF_close(*fh, dxpl) < 0)
                TEST_ERROR

            /* Re-open heap */
            if(NULL == (*fh = H5HF_open(f, dxpl, fh_addr)))
                FAIL_STACK_ERROR
        } /* end if */

        /* Adjust index of object to delete next */
        if(tparam->del_dir == HEAP_DEL_FORWARD)
            obj_idx++;
        else
            obj_idx--;
    } /* end for */

    /* Operations succeeded */
    return(0);

error:
    return(1);
} /* del_objs_half_refill() */


/*-------------------------------------------------------------------------
 * Function:	del_objs
 *
 * Purpose:	Remove objects from heap
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, June  6, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
del_objs(H5F_t *f, hid_t dxpl, H5HF_t **fh, fheap_test_param_t *tparam,
    fheap_heap_state_t *state, fheap_heap_ids_t *keep_ids)
{
    haddr_t fh_addr;            /* Address of fractal heap */
    size_t obj_idx;             /* Index of the object to remove */
    size_t u;                   /* Local index variable */

    /* Sanity check */
    HDassert(fh);
    HDassert(*fh);
    HDassert(state);
    HDassert(keep_ids);

    /* Check for first deleting half of objects & then re-inserting them */
    if(tparam->drain_half == HEAP_DEL_DRAIN_HALF)
        if(del_objs_half_refill(f, dxpl, fh, tparam, state, keep_ids))
            FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        if(H5HF_get_heap_addr(*fh, &fh_addr) < 0)
            FAIL_STACK_ERROR
        if(!H5F_addr_defined(fh_addr))
            FAIL_STACK_ERROR
    } /* end if */

    /* Remove all objects from heap */
    if(tparam->del_dir == HEAP_DEL_FORWARD)
        obj_idx = 0;
    else
        obj_idx = state->nobjs - 1;
    for(u = 0; u < state->nobjs; u++) {
        /* Remove object from heap */
        if(H5HF_remove(*fh, dxpl, &keep_ids->ids[HEAP_ID_LEN * obj_idx]) < 0)
            FAIL_STACK_ERROR

        /* Check for closing & re-opening the heap */
        if(tparam->reopen_heap) {
            /* Close (empty) heap */
            if(H5HF_close(*fh, dxpl) < 0)
                TEST_ERROR

            /* Re-open heap */
            if(NULL == (*fh = H5HF_open(f, dxpl, fh_addr)))
                FAIL_STACK_ERROR
        } /* end if */

        /* Adjust index of object to delete next */
        if(tparam->del_dir == HEAP_DEL_FORWARD)
            obj_idx++;
        else
            obj_idx--;
    } /* end for */

    /* Check up on heap... */
    state->heap_size = 0;
    state->man_size = 0;
    state->man_alloc_size = 0;
    state->man_free_space = 0;
    state->nobjs = 0;
    if(check_stats(*fh, state))
        FAIL_STACK_ERROR

    /* Operations succeeded */
    return(0);

error:
    return(1);
} /* del_objs() */


/*-------------------------------------------------------------------------
 * Function:	fill_heap
 *
 * Purpose:	Insert (small) objects to fill up the free space in a heap block
 *
 * Note:        The following fields in the 'state' structure are set to
 *              the values expected _after_ the block has been created:
 *                      heap_size
 *                      man_size
 *                      man_alloc_size
 *                      man_free_space
 *
 *              The following fields in the 'state' structure are set to
 *              the current state, before the block has been created:
 *                      nobjs
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
fill_heap(H5HF_t *fh, hid_t dxpl, unsigned block_row, size_t obj_size,
    fheap_heap_state_t *state, fheap_heap_ids_t *keep_ids)
{
    unsigned char *wobj;                /* Buffer for object to insert */
    unsigned char *curr_id_ptr;         /* Pointer into shared ID array */
    size_t *curr_len_ptr;               /* Pointer into shared length array */
    size_t *curr_off_ptr;               /* Pointer into shared offset array */
    size_t      num_ids = 0;            /* # of heap IDs in array */
    size_t      data_size;              /* Size of data portion of heap block */
    size_t      last_obj_len;           /* Size of last object inserted into heap */
    size_t      obj_off;                /* Offset of object in shared write buffer */
    unsigned    u;                      /* Local index variable */

    /* Sanity check */
    HDassert(fh);
    HDassert(state);
    HDassert(obj_size + 256 < shared_obj_size_g);

    /* Initialize starting information */
    data_size = DBLOCK_FREE(fh, block_row);
    wobj = shared_wobj_g;
    curr_id_ptr = shared_ids_g;
    curr_len_ptr = shared_lens_g;
    curr_off_ptr = shared_offs_g;
    obj_off = 0;

    /* Loop over inserting objects into the root direct block, until there's no more space */
    while(data_size >= obj_size) {
        /* Increment object count */
        num_ids++;

        /* Check for needing to increase size of heap ID array */
        if(num_ids > shared_alloc_ids_g) {
            shared_alloc_ids_g = MAX(1024, (shared_alloc_ids_g * 2));
            if(NULL == (shared_ids_g = H5MM_realloc(shared_ids_g, HEAP_ID_LEN * shared_alloc_ids_g)))
                FAIL_STACK_ERROR
            if(NULL == (shared_lens_g = H5MM_realloc(shared_lens_g, sizeof(size_t) * shared_alloc_ids_g)))
                FAIL_STACK_ERROR
            if(NULL == (shared_offs_g = H5MM_realloc(shared_offs_g, sizeof(size_t) * shared_alloc_ids_g)))
                FAIL_STACK_ERROR
            curr_id_ptr = &shared_ids_g[(num_ids - 1) * HEAP_ID_LEN];
            curr_len_ptr = &shared_lens_g[(num_ids - 1)];
            curr_off_ptr = &shared_offs_g[(num_ids - 1)];
        } /* end if */

        /* Insert object */
        if(H5HF_insert(fh, dxpl, obj_size, wobj, curr_id_ptr) < 0)
            FAIL_STACK_ERROR
        *curr_len_ptr = obj_size;
        *curr_off_ptr = obj_off;

        /* Adjust state of heap */
        state->nobjs++;
        state->man_free_space -= obj_size;

        /* Check stats for heap */
        if(check_stats(fh, state))
            FAIL_STACK_ERROR

        /* Adjust object & ID pointers */
        wobj++;
        obj_off++;
        if(obj_off > 255) {
            wobj = shared_wobj_g;
            obj_off = 0;
        } /* end if */
        curr_id_ptr += HEAP_ID_LEN;
        curr_len_ptr++;
        curr_off_ptr++;

        /* Decrement space left in block */
        data_size -= obj_size;
    } /* end while */

    /* Check for adding smaller last object to heap block */
    if(data_size > 0) {
        /* Set size of last object in block */
        last_obj_len = data_size;

        /* Increment object count */
        num_ids++;

        /* Check for needing to increase size of heap ID array */
        if(num_ids > shared_alloc_ids_g) {
            shared_alloc_ids_g = MAX(1024, (shared_alloc_ids_g * 2));
            if(NULL == (shared_ids_g = H5MM_realloc(shared_ids_g, HEAP_ID_LEN * shared_alloc_ids_g)))
                FAIL_STACK_ERROR
            if(NULL == (shared_lens_g = H5MM_realloc(shared_lens_g, sizeof(size_t) * shared_alloc_ids_g)))
                FAIL_STACK_ERROR
            if(NULL == (shared_offs_g = H5MM_realloc(shared_offs_g, sizeof(size_t) * shared_alloc_ids_g)))
                FAIL_STACK_ERROR
            curr_id_ptr = &shared_ids_g[(num_ids - 1) * HEAP_ID_LEN];
            curr_len_ptr = &shared_lens_g[(num_ids - 1)];
            curr_off_ptr = &shared_offs_g[(num_ids - 1)];
        } /* end if */

        /* Insert last object into the heap, using the remaining free space */
        if(H5HF_insert(fh, dxpl, last_obj_len, wobj, curr_id_ptr) < 0)
            FAIL_STACK_ERROR
        *curr_len_ptr = last_obj_len;
        *curr_off_ptr = obj_off;

        /* Adjust state of heap */
        state->nobjs++;
        state->man_free_space -= last_obj_len;

        /* Verify that the heap is full */
        if(check_stats(fh, state))
            FAIL_STACK_ERROR
    } /* end if */
    else
        last_obj_len = obj_size;     /* Normal sized last object */

    /* Verify reading the objects written out */

    /* Verify all the objects */
    wobj = shared_wobj_g;
    curr_id_ptr = shared_ids_g;
    curr_len_ptr = shared_lens_g;
    curr_off_ptr = shared_offs_g;
    for(u = 0; u < num_ids; u++) {
        /* Read in object */
        if(H5HF_read(fh, dxpl, curr_id_ptr, shared_robj_g) < 0)
            FAIL_STACK_ERROR

        /* Check that object is correct */
        wobj = &shared_wobj_g[*curr_off_ptr];
        if(HDmemcmp(wobj, shared_robj_g, *curr_len_ptr))
            FAIL_STACK_ERROR

        /* Adjust object & ID pointers */
        curr_id_ptr += HEAP_ID_LEN;
        curr_len_ptr++;
        curr_off_ptr++;
    } /* end for */

    /* If the heap IDs are to be retained, append them to the list */
    if(keep_ids) {
        /* Check for needing to increase size of heap ID array */
        if(keep_ids->num_ids + num_ids > keep_ids->alloc_ids) {
            keep_ids->alloc_ids = MAX(1024, (keep_ids->alloc_ids * 2));
            if(NULL == (keep_ids->ids = H5MM_realloc(keep_ids->ids, HEAP_ID_LEN * keep_ids->alloc_ids)))
                FAIL_STACK_ERROR
            if(NULL == (keep_ids->lens = H5MM_realloc(keep_ids->lens, sizeof(size_t) * keep_ids->alloc_ids)))
                FAIL_STACK_ERROR
            if(NULL == (keep_ids->offs = H5MM_realloc(keep_ids->offs, sizeof(size_t) * keep_ids->alloc_ids)))
                FAIL_STACK_ERROR
        } /* end if */

        /* Append the IDs onto the array */
        HDmemcpy(&keep_ids->ids[keep_ids->num_ids * HEAP_ID_LEN], shared_ids_g, (num_ids * HEAP_ID_LEN));
        HDmemcpy(&keep_ids->lens[keep_ids->num_ids], shared_lens_g, (num_ids * sizeof(size_t)));
        HDmemcpy(&keep_ids->offs[keep_ids->num_ids], shared_offs_g, (num_ids * sizeof(size_t)));

        /* Increment the number of IDs kept */
        keep_ids->num_ids += num_ids;
    } /* end if */

    /* Operations succeeded */
    return(0);

error:
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
fill_root_row(H5HF_t *fh, hid_t dxpl, unsigned row, size_t obj_size,
    fheap_heap_state_t *state, fheap_heap_ids_t *keep_ids)
{
    hsize_t     first_free_space;       /* Size of free space in heap after the first block */
    hsize_t     all_free_space;         /* Size of free space in heap after all blocks */
    hsize_t     first_heap_size;        /* Total size of the heap after the first block */
    hsize_t     all_heap_size;          /* Total size of the heap after all blocks */
    size_t      block_size;             /* Block size for row */
    size_t      block_free;             /* Free space in empty block of this row */
    unsigned    width;                  /* Width of heap's doubling table */
    unsigned    expand_rows;            /* # of rows to expand heap by */
    unsigned    u;                      /* Local index variable */

    /* Sanity check */
    HDassert(fh);
    HDassert(state);

    /* Get some information for the heap */
    block_size = DBLOCK_SIZE(fh, row);
    block_free = DBLOCK_FREE(fh, row);
    width = DTABLE_WIDTH(fh);

    /* Compute the number of rows to expand heap by */
    if(row < 2)
        expand_rows = 1;
    else if(POWER_OF_TWO(row))
        expand_rows = row;
    else
        expand_rows = 0;

    /* Compute first block & all blocks heap size & free space */
    if(state->heap_size == 0) {
        first_heap_size = block_size;
        first_free_space = block_free;
        all_heap_size = width * block_size;
        all_free_space = (width - 1) * block_free;
    } /* end if */
    else if(expand_rows == 0) {
        all_heap_size = state->heap_size;
        all_free_space = state->man_free_space;
        first_heap_size = all_heap_size;
        first_free_space = all_free_space;
        all_free_space -= block_free;      /* Account for shift from first free space */
    } /* end if */
    else {
        all_heap_size = state->heap_size;
        all_free_space = 0;
        for(u = 0; u < expand_rows; u++) {
            all_heap_size += width * DBLOCK_SIZE(fh, row + u);
            all_free_space += width * DBLOCK_FREE(fh, row + u);
        } /* end for */
        first_heap_size = all_heap_size;
        first_free_space = all_free_space;
        all_free_space -= block_free;      /* Account for shift from first free space */
    } /* end else */

    /* Loop over filling direct blocks, until root indirect row is full */
    state->heap_size = first_heap_size;
    state->man_size = first_heap_size;
    state->man_free_space = first_free_space;
    for(u = 0; u < width; u++) {
        /* Set heap's size & free space correctly */
        if(u == 1) {
            state->heap_size = all_heap_size;
            state->man_size = all_heap_size;
            state->man_free_space = all_free_space;
        } /* end if */

        /* Account for new block added */
        state->man_alloc_size += block_size;

        /* Fill a direct heap block up */
        if(fill_heap(fh, dxpl, row, obj_size, state, keep_ids))
            FAIL_STACK_ERROR
    } /* end for */

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
fill_row(H5HF_t *fh, hid_t dxpl, unsigned row, size_t obj_size,
    fheap_heap_state_t *state, fheap_heap_ids_t *keep_ids)
{
    size_t      block_size;             /* Size of direct block in this row */
    unsigned    width;                  /* Width of heap's doubling table */
    unsigned    u;                      /* Local index variable */

    /* Sanity check */
    HDassert(fh);
    HDassert(state);

    /* Get some information for the heap */
    block_size = DBLOCK_SIZE(fh, row);
    width = DTABLE_WIDTH(fh);

    /* Loop over filling direct blocks, until indirect row is full */
    for(u = 0; u < width; u++) {
        /* Adjust stats for new block */
        state->man_alloc_size += block_size;

        /* Fill a direct heap block up */
        if(fill_heap(fh, dxpl, row, obj_size, state, keep_ids))
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
fill_root_direct(H5HF_t *fh, hid_t dxpl, size_t obj_size,
    fheap_heap_state_t *state, fheap_heap_ids_t *keep_ids)
{
    unsigned    max_dblock_rows;        /* Max. # of direct block rows in indirect block */
    unsigned    row;                    /* Row being created */

    /* Get heap info */
    max_dblock_rows = DTABLE_MAX_DROWS(fh);
    HDassert(max_dblock_rows);

    /* Loop over rows */
    for(row = 0; row < max_dblock_rows; row++)
        if(fill_root_row(fh, dxpl, row, obj_size, state, keep_ids))
            FAIL_STACK_ERROR

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
fill_2nd_indirect(H5HF_t *fh, hid_t dxpl, unsigned pos, size_t obj_size,
    fheap_heap_state_t *state, fheap_heap_ids_t *keep_ids)
{
    unsigned    max_dblock_rows;        /* Max. # of direct block rows in indirect block */
    unsigned    row;                    /* Current row to create */

    /* Get some information for the heap */
    max_dblock_rows = IBLOCK_MAX_DROWS(fh, pos);
    HDassert(max_dblock_rows);

    /* Loop over rows */
    for(row = 0; row < max_dblock_rows; row++)
        if(fill_row(fh, dxpl, row, obj_size, state, keep_ids))
            FAIL_STACK_ERROR

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
fill_all_direct(H5HF_t *fh, hid_t dxpl, size_t obj_size,
    fheap_heap_state_t *state, fheap_heap_ids_t *keep_ids)
{
    unsigned    max_dblock_rows;        /* Max. # of direct block rows in indirect block */
    unsigned    row;                    /* Row being created */

    /* Get heap info */
    max_dblock_rows = DTABLE_MAX_DROWS(fh);
    HDassert(max_dblock_rows);

    /* Loop over rows */
    for(row = 0; row < max_dblock_rows; row++)
        if(fill_row(fh, dxpl, row, obj_size, state, keep_ids))
            FAIL_STACK_ERROR

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
fill_2nd_indirect_row(H5HF_t *fh, hid_t dxpl, unsigned pos, size_t obj_size,
    fheap_heap_state_t *state, fheap_heap_ids_t *keep_ids)
{
    unsigned    width;                  /* Width of heap's doubling table */
    unsigned    u;                      /* Local index variable */

    /* Get some information for the heap */
    width = DTABLE_WIDTH(fh);

    /* Loop over row of indirect blocks */
    for(u = 0; u < width; u++)
        if(fill_2nd_indirect(fh, dxpl, pos, obj_size, state, keep_ids))
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
fill_all_2nd_indirect_rows(H5HF_t *fh, hid_t dxpl, size_t obj_size,
    fheap_heap_state_t *state, fheap_heap_ids_t *keep_ids)
{
    unsigned    width;                  /* Width of heap's doubling table */
    unsigned    u;                      /* Local index variable */

    /* Get some information for the heap */
    width = DTABLE_WIDTH(fh);

    /* Loop over rows of 2nd level deep indirect blocks */
    for(u = 0; u < (H5V_log2_of2(width) + 1); u++)
        if(fill_2nd_indirect_row(fh, dxpl, (u + 1), obj_size, state, keep_ids))
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
fill_3rd_indirect(H5HF_t *fh, hid_t dxpl, unsigned pos, size_t obj_size,
    fheap_heap_state_t *state, fheap_heap_ids_t *keep_ids)
{
    unsigned    u;                      /* Local index variable */

    /* Fill all direct block rows in third level indirect block */
    if(fill_all_direct(fh, dxpl, obj_size, state, keep_ids))
        FAIL_STACK_ERROR

    /* Fill rows of recursive indirect blocks in third level indirect block */
    for(u = 0; u < pos; u++)
        if(fill_2nd_indirect_row(fh, dxpl, (u + 1), obj_size, state, keep_ids))
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
fill_3rd_indirect_row(H5HF_t *fh, hid_t dxpl, unsigned pos, size_t obj_size,
    fheap_heap_state_t *state, fheap_heap_ids_t *keep_ids)
{
    unsigned    width;                  /* Width of heap's doubling table */
    unsigned    u;              /* Local index variable */

    /* Get some information for the heap */
    width = DTABLE_WIDTH(fh);

    /* Loop over row of 3rd level indirect blocks */
    for(u = 0; u < width; u++)
        /* Fill third level indirect block */
        if(fill_3rd_indirect(fh, dxpl, pos, obj_size, state, keep_ids))
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
fill_all_3rd_indirect_rows(H5HF_t *fh, hid_t dxpl, size_t obj_size,
    fheap_heap_state_t *state, fheap_heap_ids_t *keep_ids)
{
    unsigned    width;                  /* Width of heap's doubling table */
    unsigned    u;                      /* Local index variable */

    /* Get some information for the heap */
    width = DTABLE_WIDTH(fh);

    /* Loop over rows of 3rd level deep indirect blocks */
    for(u = 0; u < (H5V_log2_of2(width) + 1); u++)
        /* Fill row of 3rd level indirect blocks */
        if(fill_3rd_indirect_row(fh, dxpl, (u + 1), obj_size, state, keep_ids))
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
fill_4th_indirect_row(H5HF_t *fh, hid_t dxpl, unsigned pos, size_t obj_size,
    fheap_heap_state_t *state, fheap_heap_ids_t *keep_ids)
{
    unsigned    width;                  /* Width of heap's doubling table */
    unsigned    u, v;                   /* Local index variables */

    /* Get some information for the heap */
    width = DTABLE_WIDTH(fh);

    /* Loop over row of 4th level indirect blocks */
    for(u = 0; u < width; u++) {
        /* Fill all direct block rows in fourth level indirect block */
        if(fill_all_direct(fh, dxpl, obj_size, state, keep_ids))
            FAIL_STACK_ERROR

        /* Fill all rows of 2nd level deep indirect blocks in fourth level indirect block */
        if(fill_all_2nd_indirect_rows(fh, dxpl, obj_size, state, keep_ids))
            FAIL_STACK_ERROR

        /* Fill rows of third level indirect blocks in fourth level indirect block */
        for(v = 0; v < pos; v++)
            if(fill_3rd_indirect_row(fh, dxpl, (v + 1), obj_size, state, keep_ids))
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
fill_all_4th_indirect_rows(H5HF_t *fh, hid_t dxpl, size_t obj_size,
    fheap_heap_state_t *state, fheap_heap_ids_t *keep_ids)
{
    unsigned    width;                  /* Width of heap's doubling table */
    unsigned    u;                      /* Local index variable */

    /* Get some information for the heap */
    width = DTABLE_WIDTH(fh);

    /* Loop over rows of 4th level deep indirect blocks */
    for(u = 0; u < (H5V_log2_of2(width) + 1); u++) {
        /* Fill row of 4th level indirect blocks */
        if(fill_4th_indirect_row(fh, dxpl, (u + 1), obj_size, state, keep_ids))
            FAIL_STACK_ERROR

        /* Account for root indirect block doubling # of rows again */
        /* (From 16 rows to the max. # of rows: 22) */
        /* (Note: this is tied to the particular doubling table/heap creation parameters) */
        if(u == 0) {
            unsigned max_root_rows;     /* Maximum # of rows in root indirect block */
            unsigned row;               /* Row in heap */

            /* Get some information for the heap */
            max_root_rows = HEAP_MAX_ROOT_ROWS(fh);

            /* Increase heap size & free space */
            for(row = 16; row < max_root_rows; row++) {
                state->heap_size += width * DBLOCK_SIZE(fh, row);
                state->man_size += width * DBLOCK_SIZE(fh, row);
                state->man_free_space += width * DBLOCK_FREE(fh, row);
            } /* end for */
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
    fheap_heap_state_t state;           /* State of fractal heap */

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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));
    if(check_stats(fh, &state))
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
    fheap_heap_state_t state;           /* State of fractal heap */

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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));
    if(check_stats(fh, &state))
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
    fheap_heap_state_t state;           /* State of fractal heap */

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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));
    if(check_stats(fh, &state))
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
    state.heap_size = DBLOCK_SIZE(fh, 0);
    state.man_size = DBLOCK_SIZE(fh, 0);
    state.man_alloc_size = DBLOCK_SIZE(fh, 0);
    state.man_free_space = DBLOCK_FREE(fh, 0);
    if(add_obj(fh, dxpl, 10, SMALL_OBJ_SIZE1, &state, NULL))
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
    if(check_stats(fh, &state))
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
    fheap_heap_state_t state;           /* State of fractal heap */

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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test inserting first (small) object into absolute heap
     */
    TESTING("inserting two (small) objects into absolute heap");
    state.heap_size = DBLOCK_SIZE(fh, 0);
    state.man_size = DBLOCK_SIZE(fh, 0);
    state.man_alloc_size = DBLOCK_SIZE(fh, 0);
    state.man_free_space = DBLOCK_FREE(fh, 0);
    if(add_obj(fh, dxpl, 10, SMALL_OBJ_SIZE1, &state, NULL))
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
    if(add_obj(fh, dxpl, 20, SMALL_OBJ_SIZE2, &state, NULL))
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
    fheap_heap_state_t state;           /* State of fractal heap */

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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test inserting mult. (small) object into absolute heap
     */
    TESTING("inserting objects to fill absolute heap's root direct block");

    /* Fill the heap up */
    state.heap_size = DBLOCK_SIZE(fh, 0);
    state.man_size = DBLOCK_SIZE(fh, 0);
    state.man_alloc_size = DBLOCK_SIZE(fh, 0);
    state.man_free_space = DBLOCK_FREE(fh, 0);
    if(fill_heap(fh, dxpl, 0, SMALL_OBJ_SIZE1, &state, NULL))
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
    fheap_heap_state_t state;           /* State of fractal heap */

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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test forcing creation of indirect root block & second direct block
     */
    TESTING("inserting objects to create root indirect block");

    /* Fill the heap up */
    state.heap_size = DBLOCK_SIZE(fh, 0);
    state.man_size = DBLOCK_SIZE(fh, 0);
    state.man_alloc_size = DBLOCK_SIZE(fh, 0);
    state.man_free_space = DBLOCK_FREE(fh, 0);
    if(fill_heap(fh, dxpl, 0, SMALL_OBJ_SIZE1, &state, NULL))
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
    state.heap_size = cparam->managed.width * DBLOCK_SIZE(fh, 0);
    state.man_size = cparam->managed.width * DBLOCK_SIZE(fh, 0);
    state.man_alloc_size += DBLOCK_SIZE(fh, 0);
    state.man_free_space = (cparam->managed.width - 1) * DBLOCK_FREE(fh, 0);
    if(add_obj(fh, dxpl, 10, SMALL_OBJ_SIZE1, &state, NULL))
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
    fheap_heap_state_t state;           /* State of fractal heap */

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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test inserting mult. (small) objects to fill second direct block
     */
    TESTING("inserting objects to fill second direct block");

    /* Fill the first direct block heap up */
    state.heap_size = DBLOCK_SIZE(fh, 0);
    state.man_size = DBLOCK_SIZE(fh, 0);
    state.man_alloc_size = DBLOCK_SIZE(fh, 0);
    state.man_free_space = DBLOCK_FREE(fh, 0);
    if(fill_heap(fh, dxpl, 0, SMALL_OBJ_SIZE1, &state, NULL))
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
    state.heap_size = cparam->managed.width * DBLOCK_SIZE(fh, 0);
    state.man_size = cparam->managed.width * DBLOCK_SIZE(fh, 0);
    state.man_alloc_size += DBLOCK_SIZE(fh, 0);
    state.man_free_space = (cparam->managed.width - 1) * DBLOCK_FREE(fh, 0);
    if(fill_heap(fh, dxpl, 0, SMALL_OBJ_SIZE1, &state, NULL))
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
    fheap_heap_state_t state;           /* State of fractal heap */

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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test inserting mult. (small) objects to create third direct block
     */
    TESTING("inserting objects to create third direct block");

    /* Fill the first direct block up */
    state.heap_size = DBLOCK_SIZE(fh, 0);
    state.man_size = DBLOCK_SIZE(fh, 0);
    state.man_alloc_size = DBLOCK_SIZE(fh, 0);
    state.man_free_space = DBLOCK_FREE(fh, 0);
    if(fill_heap(fh, dxpl, 0, SMALL_OBJ_SIZE1, &state, NULL))
        FAIL_STACK_ERROR

    /* Fill the second direct block heap up (also creates initial root indirect block) */
    state.heap_size = cparam->managed.width * DBLOCK_SIZE(fh, 0);
    state.man_size = cparam->managed.width * DBLOCK_SIZE(fh, 0);
    state.man_alloc_size += DBLOCK_SIZE(fh, 0);
    state.man_free_space = (cparam->managed.width - 1) * DBLOCK_FREE(fh, 0);
    if(fill_heap(fh, dxpl, 0, SMALL_OBJ_SIZE1, &state, NULL))
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
    state.man_alloc_size += DBLOCK_SIZE(fh, 0);
    if(add_obj(fh, dxpl, 10, SMALL_OBJ_SIZE1, &state, NULL))
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
    fheap_heap_state_t state;           /* State of fractal heap */

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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test inserting mult. (small) objects to fill first row in root indirect block
     */
    TESTING("inserting objects to fill first row of root indirect block");

    /* Fill first row of [root] indirect block */
    if(fill_root_row(fh, dxpl, 0, SMALL_OBJ_SIZE1, &state, NULL))
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
    fheap_heap_state_t state;           /* State of fractal heap */

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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test inserting mult. (small) objects to start second row in root indirect block
     */
    TESTING("inserting objects to start second row of root indirect block");

    /* Fill first root indirect row */
    if(fill_root_row(fh, dxpl, 0, SMALL_OBJ_SIZE1, &state, NULL))
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
    state.heap_size += cparam->managed.width * DBLOCK_SIZE(fh, 1);
    state.man_size += cparam->managed.width * DBLOCK_SIZE(fh, 1);
    state.man_alloc_size += DBLOCK_SIZE(fh, 1);
    state.man_free_space = cparam->managed.width * DBLOCK_FREE(fh, 1);
    if(add_obj(fh, dxpl, 10, SMALL_OBJ_SIZE1, &state, NULL))
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
    fheap_heap_state_t state;           /* State of fractal heap */

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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test inserting mult. (small) objects to start second row in root indirect block
     */
    TESTING("inserting objects to fill second row of root indirect block");

    /* Fill first root indirect row */
    if(fill_root_row(fh, dxpl, 0, SMALL_OBJ_SIZE1, &state, NULL))
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
    if(fill_root_row(fh, dxpl, 1, SMALL_OBJ_SIZE1, &state, NULL))
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
    fheap_heap_state_t state;           /* State of fractal heap */

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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test inserting mult. (small) objects to start third row in root indirect block
     */
    TESTING("inserting objects to start third row of root indirect block");

    /* Fill first root indirect row */
    if(fill_root_row(fh, dxpl, 0, SMALL_OBJ_SIZE1, &state, NULL))
        FAIL_STACK_ERROR

    /* Fill second root indirect row */
    if(fill_root_row(fh, dxpl, 1, SMALL_OBJ_SIZE1, &state, NULL))
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
    state.heap_size += cparam->managed.width * DBLOCK_SIZE(fh, 2);
    state.heap_size += cparam->managed.width * DBLOCK_SIZE(fh, 3);
    state.man_size += cparam->managed.width * DBLOCK_SIZE(fh, 2);
    state.man_size += cparam->managed.width * DBLOCK_SIZE(fh, 3);
    state.man_alloc_size += DBLOCK_SIZE(fh, 2);
    state.man_free_space += cparam->managed.width * DBLOCK_FREE(fh, 2);
    state.man_free_space += cparam->managed.width * DBLOCK_FREE(fh, 3);
    if(add_obj(fh, dxpl, 10, SMALL_OBJ_SIZE1, &state, NULL))
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
    fheap_heap_state_t state;           /* State of fractal heap */
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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test inserting mult. (small) objects to fill four rows in root indirect block
     */
    TESTING("inserting objects to fill four rows of root indirect block");

    /* Loop over rows */
    for(u = 0; u < 4; u++)
        if(fill_root_row(fh, dxpl, u, SMALL_OBJ_SIZE1, &state, NULL))
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
    fheap_heap_state_t state;           /* State of fractal heap */

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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test inserting mult. (small) objects to fill all direct  rows in root indirect block
     */
    TESTING("inserting objects to fill all direct rows of root indirect block");

    /* Fill all direct blocks in root indirect block */
    if(fill_root_direct(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    fheap_heap_state_t state;           /* State of fractal heap */

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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test inserting mult. (small) objects to force creation of first recursive indirect block
     */
    TESTING("inserting objects to create first recursive indirect block");

    /* Fill direct blocks up */
    if(fill_root_direct(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    state.man_alloc_size += DBLOCK_SIZE(fh, 0);
    if(add_obj(fh, dxpl, 10, SMALL_OBJ_SIZE1, &state, NULL))
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
    fheap_heap_state_t state;           /* State of fractal heap */

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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test inserting mult. (small) objects to force creation of second direct
     *  block in first recursive indirect block
     */
    TESTING("inserting objects to create second direct block in first recursive indirect block");

    /* Fill direct blocks up */
    if(fill_root_direct(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
        FAIL_STACK_ERROR

    /* Fill the first direct block in the recursive indirect block up */
    state.man_alloc_size += DBLOCK_SIZE(fh, 0);
    if(fill_heap(fh, dxpl, 0, SMALL_OBJ_SIZE1, &state, NULL))
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
    state.man_alloc_size += DBLOCK_SIZE(fh, 0);
    if(add_obj(fh, dxpl, 10, SMALL_OBJ_SIZE1, &state, NULL))
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
    fheap_heap_state_t state;           /* State of fractal heap */

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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in first recursive indirect block
     */
    TESTING("inserting objects to fill all direct blocks in first recursive indirect block");

    /* Fill direct blocks up in root indirect block */
    if(fill_root_direct(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    if(fill_2nd_indirect(fh, dxpl, 1, SMALL_OBJ_SIZE1, &state, NULL))
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
    fheap_heap_state_t state;           /* State of fractal heap */

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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in first recursive indirect block
     */
    TESTING("inserting objects to start second recursive indirect block");

    /* Fill direct blocks up in root indirect block */
    if(fill_root_direct(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
        FAIL_STACK_ERROR

    /* Fill first recursive indirect block */
    if(fill_2nd_indirect(fh, dxpl, 1, SMALL_OBJ_SIZE1, &state, NULL))
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
    state.man_alloc_size += DBLOCK_SIZE(fh, 0);
    if(add_obj(fh, dxpl, 10, SMALL_OBJ_SIZE1, &state, NULL))
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
    fheap_heap_state_t state;           /* State of fractal heap */

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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in first recursive indirect block
     */
    TESTING("inserting objects to fill all direct blocks in second recursive indirect block");

    /* Fill direct blocks up in root indirect block */
    if(fill_root_direct(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
        FAIL_STACK_ERROR

    /* Fill first recursive indirect block */
    if(fill_2nd_indirect(fh, dxpl, 1, SMALL_OBJ_SIZE1, &state, NULL))
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
    if(fill_2nd_indirect(fh, dxpl, 1, SMALL_OBJ_SIZE1, &state, NULL))
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
    fheap_heap_state_t state;           /* State of fractal heap */

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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in first recursive indirect block
     */
    TESTING("inserting objects to fill all direct blocks in first row of recursive indirect block");

    /* Fill direct blocks in root indirect block up */
    if(fill_root_direct(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    if(fill_2nd_indirect_row(fh, dxpl, 1, SMALL_OBJ_SIZE1, &state, NULL))
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
    fheap_heap_state_t state;           /* State of fractal heap */

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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in first recursive indirect block
     */
    TESTING("inserting objects to start second row of recursive indirect blocks");

    /* Fill direct blocks in root indirect block up */
    if(fill_root_direct(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
        FAIL_STACK_ERROR

    /* Fill row of recursive indirect blocks */
    if(fill_2nd_indirect_row(fh, dxpl, 1, SMALL_OBJ_SIZE1, &state, NULL))
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
    state.man_alloc_size += DBLOCK_SIZE(fh, 0);
    if(add_obj(fh, dxpl, 10, SMALL_OBJ_SIZE1, &state, NULL))
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
    fheap_heap_state_t state;           /* State of fractal heap */

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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in first recursive indirect block
     */
    TESTING("inserting objects to fill recursive indirect blocks two levels deep");

    /* Fill direct blocks up in root indirect block */
    if(fill_root_direct(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    if(fill_all_2nd_indirect_rows(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    fheap_heap_state_t state;           /* State of fractal heap */

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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in first recursive indirect block
     */
    TESTING("inserting objects to start recursive indirect blocks three levels deep");

    /* Fill direct blocks up in root indirect block */
    if(fill_root_direct(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
        FAIL_STACK_ERROR

    /* Fill all rows of 2nd level indirect blocks */
    if(fill_all_2nd_indirect_rows(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    state.man_alloc_size += DBLOCK_SIZE(fh, 0);
    if(add_obj(fh, dxpl, 10, SMALL_OBJ_SIZE1, &state, NULL))
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
    fheap_heap_state_t state;           /* State of fractal heap */

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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in first recursive indirect block
     */
    TESTING("inserting objects to fill first indirect block of recursive indirect blocks three levels deep");

    /* Fill direct blocks up in root indirect block */
    if(fill_root_direct(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
        FAIL_STACK_ERROR

    /* Fill all rows of 2nd level indirect blocks */
    if(fill_all_2nd_indirect_rows(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
        FAIL_STACK_ERROR

    /* Fill all direct block rows in third level indirect block */
    if(fill_all_direct(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    if(fill_2nd_indirect_row(fh, dxpl, 1, SMALL_OBJ_SIZE1, &state, NULL))
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
    fheap_heap_state_t state;           /* State of fractal heap */

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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in first recursive indirect block
     */
    TESTING("inserting objects to fill row of indirect blocks in recursive indirect blocks three levels deep");

    /* Fill direct blocks up in root indirect block */
    if(fill_root_direct(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
        FAIL_STACK_ERROR

    /* Fill all rows of 2nd level indirect blocks */
    if(fill_all_2nd_indirect_rows(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    if(fill_3rd_indirect_row(fh, dxpl, 1, SMALL_OBJ_SIZE1, &state, NULL))
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
    fheap_heap_state_t state;           /* State of fractal heap */

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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in first recursive indirect block
     */
    TESTING("inserting objects to fill row of indirect blocks in recursive indirect blocks three levels deep");

    /* Fill direct blocks up in root indirect block */
    if(fill_root_direct(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
        FAIL_STACK_ERROR

    /* Fill all rows of 2nd level indirect blocks */
    if(fill_all_2nd_indirect_rows(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    if(fill_all_3rd_indirect_rows(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    fheap_heap_state_t state;           /* State of fractal heap */

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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in first recursive indirect block
     */
    TESTING("inserting objects to start first direct block in recursive indirect blocks four levels deep");

    /* Fill direct blocks up in root indirect block */
    if(fill_root_direct(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
        FAIL_STACK_ERROR

    /* Fill all rows of 2nd level indirect blocks */
    if(fill_all_2nd_indirect_rows(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
        FAIL_STACK_ERROR

    /* Fill all rows of 3rd level indirect blocks */
    if(fill_all_3rd_indirect_rows(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    state.man_alloc_size += DBLOCK_SIZE(fh, 0);
    if(add_obj(fh, dxpl, 10, SMALL_OBJ_SIZE1, &state, NULL))
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
    fheap_heap_state_t state;           /* State of fractal heap */

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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in first recursive indirect block
     */
    TESTING("inserting objects to fill first (3rd level) indirect block in recursive indirect block four levels deep");

    /* Fill direct blocks up in root indirect block */
    if(fill_root_direct(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
        FAIL_STACK_ERROR

    /* Fill all rows of 2nd level indirect blocks */
    if(fill_all_2nd_indirect_rows(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
        FAIL_STACK_ERROR

    /* Fill all rows of 3rd level indirect blocks */
    if(fill_all_3rd_indirect_rows(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
        FAIL_STACK_ERROR

    /* Fill direct block rows in fourth level indirect block */
    if(fill_all_direct(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
        FAIL_STACK_ERROR

    /* Fill all rows of 2nd level deep indirect blocks in fourth level indirect block */
    if(fill_all_2nd_indirect_rows(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    if(fill_3rd_indirect_row(fh, dxpl, 1, SMALL_OBJ_SIZE1, &state, NULL))
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
    fheap_heap_state_t state;           /* State of fractal heap */

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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in first recursive indirect block
     */
    TESTING("inserting objects to fill first row of recursive indirect blocks four levels deep");

    /* Fill direct blocks up in root indirect block */
    if(fill_root_direct(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
        FAIL_STACK_ERROR

    /* Fill all rows of 2nd level indirect blocks */
    if(fill_all_2nd_indirect_rows(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
        FAIL_STACK_ERROR

    /* Fill all rows of 3rd level indirect blocks */
    if(fill_all_3rd_indirect_rows(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    if(fill_4th_indirect_row(fh, dxpl, 1, SMALL_OBJ_SIZE1, &state, NULL))
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
    fheap_heap_state_t state;           /* State of fractal heap */

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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in recursive indirect blocks four levels deep
     */
    TESTING("inserting objects to fill all rows of recursive indirect blocks four levels deep");

    /* Fill direct blocks up in root indirect block */
    if(fill_root_direct(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
        FAIL_STACK_ERROR

    /* Fill all rows of 2nd level indirect blocks */
    if(fill_all_2nd_indirect_rows(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
        FAIL_STACK_ERROR

    /* Fill all rows of 3rd level indirect blocks */
    if(fill_all_3rd_indirect_rows(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    if(fill_all_4th_indirect_rows(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    fheap_heap_state_t state;           /* State of fractal heap */

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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in recursive indirect blocks four levels deep and add one more
     *  block, to make a five level deep structure
     */
    TESTING("inserting objects to create first direct block in recursive indirect blocks five levels deep");

    /* Fill direct blocks up in root indirect block */
    if(fill_root_direct(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    if(fill_all_2nd_indirect_rows(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    if(fill_all_3rd_indirect_rows(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    if(fill_all_4th_indirect_rows(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    state.man_alloc_size += DBLOCK_SIZE(fh, 0);
    if(add_obj(fh, dxpl, 10, SMALL_OBJ_SIZE1, &state, NULL))
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
    size_t      obj_size;               /* Size of object */
    fheap_heap_state_t state;           /* State of fractal heap */

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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));
    if(check_stats(fh, &state))
        FAIL_STACK_ERROR

    /*
     * Test inserting object into absolute heap which doesn't fit into starting
     *  block size
     */
    TESTING("inserting object that is too large for starting block");

    obj_size = DBLOCK_SIZE(fh, 0) + 1;
    state.heap_size = cparam->managed.width * DBLOCK_SIZE(fh, 0);
    state.heap_size += cparam->managed.width * DBLOCK_SIZE(fh, 1);
    state.heap_size += cparam->managed.width * DBLOCK_SIZE(fh, 2);
    state.man_size = cparam->managed.width * DBLOCK_SIZE(fh, 0);
    state.man_size += cparam->managed.width * DBLOCK_SIZE(fh, 1);
    state.man_size += cparam->managed.width * DBLOCK_SIZE(fh, 2);
    state.man_alloc_size = DBLOCK_SIZE(fh, 2);
    state.man_free_space = cparam->managed.width * DBLOCK_FREE(fh, 0);
    state.man_free_space += cparam->managed.width * DBLOCK_FREE(fh, 1);
    state.man_free_space += cparam->managed.width * DBLOCK_FREE(fh, 2);
    if(add_obj(fh, dxpl, 10, obj_size, &state, NULL))
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
    size_t      obj_size;               /* Size of object */
    fheap_heap_state_t state;           /* State of fractal heap */

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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));
    if(check_stats(fh, &state))
        FAIL_STACK_ERROR

    /*
     * Test inserting object into absolute heap which doesn't fit into starting
     *  block size
     */
    TESTING("skipping starting block, then adding object back to first block");

    /* Insert object too large for starting block size */
    obj_size = DBLOCK_SIZE(fh, 0) + 1;
    state.heap_size = cparam->managed.width * DBLOCK_SIZE(fh, 0);
    state.heap_size += cparam->managed.width * DBLOCK_SIZE(fh, 1);
    state.heap_size += cparam->managed.width * DBLOCK_SIZE(fh, 2);
    state.man_size = cparam->managed.width * DBLOCK_SIZE(fh, 0);
    state.man_size += cparam->managed.width * DBLOCK_SIZE(fh, 1);
    state.man_size += cparam->managed.width * DBLOCK_SIZE(fh, 2);
    state.man_alloc_size = DBLOCK_SIZE(fh, 2);
    state.man_free_space = cparam->managed.width * DBLOCK_FREE(fh, 0);
    state.man_free_space += cparam->managed.width * DBLOCK_FREE(fh, 1);
    state.man_free_space += cparam->managed.width * DBLOCK_FREE(fh, 2);
    if(add_obj(fh, dxpl, 10, obj_size, &state, NULL))
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
    if(add_obj(fh, dxpl, 10, obj_size, &state, NULL))
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
    state.man_alloc_size += DBLOCK_SIZE(fh, 0);
    if(add_obj(fh, dxpl, 20, SMALL_OBJ_SIZE2, &state, NULL))
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
    size_t      obj_size;               /* Size of object */
    fheap_heap_state_t state;           /* State of fractal heap */

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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));
    if(check_stats(fh, &state))
        FAIL_STACK_ERROR

    /*
     * Test inserting object into absolute heap which doesn't fit into starting
     *  block size
     */
    TESTING("skipping starting block, then adding objects to backfill and extend");

    /* Insert object too large for starting block size */
    obj_size = DBLOCK_SIZE(fh, 0) + 1;
    state.heap_size = cparam->managed.width * DBLOCK_SIZE(fh, 0);
    state.heap_size += cparam->managed.width * DBLOCK_SIZE(fh, 1);
    state.heap_size += cparam->managed.width * DBLOCK_SIZE(fh, 2);
    state.man_size = cparam->managed.width * DBLOCK_SIZE(fh, 0);
    state.man_size += cparam->managed.width * DBLOCK_SIZE(fh, 1);
    state.man_size += cparam->managed.width * DBLOCK_SIZE(fh, 2);
    state.man_alloc_size = DBLOCK_SIZE(fh, 2);
    state.man_free_space = cparam->managed.width * DBLOCK_FREE(fh, 0);
    state.man_free_space += cparam->managed.width * DBLOCK_FREE(fh, 1);
    state.man_free_space += cparam->managed.width * DBLOCK_FREE(fh, 2);
    if(add_obj(fh, dxpl, 10, obj_size, &state, NULL))
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
    if(add_obj(fh, dxpl, 10, obj_size, &state, NULL))
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
    if(fill_row(fh, dxpl, 0, SMALL_OBJ_SIZE1, &state, NULL))
        FAIL_STACK_ERROR
    if(fill_row(fh, dxpl, 1, SMALL_OBJ_SIZE1, &state, NULL))
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

    /* Insert another object, which should extend direct blocks, instead of backfill */
    state.man_alloc_size += DBLOCK_SIZE(fh, 2);
    if(add_obj(fh, dxpl, 20, SMALL_OBJ_SIZE2, &state, NULL))
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
    size_t      obj_size;               /* Size of object */
    fheap_heap_state_t state;           /* State of fractal heap */

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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test inserting first (small) object into absolute heap
     */
    TESTING("insert object to initial block, then add object too large for starting direct blocks");

    /* Insert small object, to create root direct block */
    state.heap_size = DBLOCK_SIZE(fh, 0);
    state.man_size = DBLOCK_SIZE(fh, 0);
    state.man_alloc_size = DBLOCK_SIZE(fh, 0);
    state.man_free_space = DBLOCK_FREE(fh, 0);
    if(add_obj(fh, dxpl, 10, SMALL_OBJ_SIZE1, &state, NULL))
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
    obj_size = DBLOCK_SIZE(fh, 0) + 1;
    state.heap_size += (cparam->managed.width - 1) * DBLOCK_SIZE(fh, 0);
    state.heap_size += cparam->managed.width * DBLOCK_SIZE(fh, 1);
    state.heap_size += cparam->managed.width * DBLOCK_SIZE(fh, 2);
    state.man_size += (cparam->managed.width - 1) * DBLOCK_SIZE(fh, 0);
    state.man_size += cparam->managed.width * DBLOCK_SIZE(fh, 1);
    state.man_size += cparam->managed.width * DBLOCK_SIZE(fh, 2);
    state.man_alloc_size += DBLOCK_SIZE(fh, 2);
    state.man_free_space += (cparam->managed.width - 1 )* DBLOCK_FREE(fh, 0);
    state.man_free_space += cparam->managed.width * DBLOCK_FREE(fh, 1);
    state.man_free_space += cparam->managed.width * DBLOCK_FREE(fh, 2);
    if(add_obj(fh, dxpl, 20, obj_size, &state, NULL))
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
    size_t      obj_size;               /* Size of object */
    fheap_heap_state_t state;           /* State of fractal heap */
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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test inserting first (small) object into absolute heap
     */
    TESTING("insert object to initial block, then add object too large for starting direct blocks, then backfill and extend");

    /* Insert small object, to create root direct block */
    state.heap_size = DBLOCK_SIZE(fh, 0);
    state.man_size = DBLOCK_SIZE(fh, 0);
    state.man_alloc_size = DBLOCK_SIZE(fh, 0);
    state.man_free_space = DBLOCK_FREE(fh, 0);
    if(add_obj(fh, dxpl, 10, SMALL_OBJ_SIZE1, &state, NULL))
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
    obj_size = DBLOCK_SIZE(fh, 0) + 1;
    state.heap_size += (cparam->managed.width - 1) * DBLOCK_SIZE(fh, 0);
    state.heap_size += cparam->managed.width * DBLOCK_SIZE(fh, 1);
    state.heap_size += cparam->managed.width * DBLOCK_SIZE(fh, 2);
    state.man_size += (cparam->managed.width - 1) * DBLOCK_SIZE(fh, 0);
    state.man_size += cparam->managed.width * DBLOCK_SIZE(fh, 1);
    state.man_size += cparam->managed.width * DBLOCK_SIZE(fh, 2);
    state.man_alloc_size += DBLOCK_SIZE(fh, 2);
    state.man_free_space += (cparam->managed.width - 1 )* DBLOCK_FREE(fh, 0);
    state.man_free_space += cparam->managed.width * DBLOCK_FREE(fh, 1);
    state.man_free_space += cparam->managed.width * DBLOCK_FREE(fh, 2);
    if(add_obj(fh, dxpl, 20, obj_size, &state, NULL))
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
    if(add_obj(fh, dxpl, 10, obj_size, &state, NULL))
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
    obj_size = DBLOCK_FREE(fh, 2) - (DBLOCK_SIZE(fh, 0) + 1);
    if(add_obj(fh, dxpl, 20, obj_size, &state, NULL))
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

    /* Fill remainder of first row of direct heap blocks up */
    for(v = 0; v < (cparam->managed.width - 1); v++) {
        state.man_alloc_size += DBLOCK_SIZE(fh, 0);
        if(fill_heap(fh, dxpl, 0, SMALL_OBJ_SIZE1, &state, NULL))
            FAIL_STACK_ERROR
    } /* end for */

    /* Fill second row of direct blocks */
    if(fill_row(fh, dxpl, 1, SMALL_OBJ_SIZE1, &state, NULL))
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

    /* Insert one more object, to create new 2 * start size direct block */
    state.man_alloc_size += DBLOCK_SIZE(fh, 2);
    if(add_obj(fh, dxpl, 10, SMALL_OBJ_SIZE1, &state, NULL))
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
    size_t      obj_size;               /* Size of object */
    fheap_heap_state_t state;           /* State of fractal heap */
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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test absolute heap
     */
    TESTING("skipping blocks with indirect root, then backfill and extend");

    /* Fill initial direct block */
    state.heap_size = DBLOCK_SIZE(fh, 0);
    state.man_size = DBLOCK_SIZE(fh, 0);
    state.man_alloc_size = DBLOCK_SIZE(fh, 0);
    state.man_free_space = DBLOCK_FREE(fh, 0);
    if(fill_heap(fh, dxpl, 0, SMALL_OBJ_SIZE1, &state, NULL))
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
    state.heap_size += (cparam->managed.width - 1) * DBLOCK_SIZE(fh, 0);
    state.man_size += (cparam->managed.width - 1) * DBLOCK_SIZE(fh, 0);
    state.man_alloc_size += DBLOCK_SIZE(fh, 0);
    state.man_free_space += (cparam->managed.width - 1) * DBLOCK_FREE(fh, 0);
    if(add_obj(fh, dxpl, 10, SMALL_OBJ_SIZE1, &state, NULL))
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
    obj_size = DBLOCK_SIZE(fh, 2) + 1;
    state.heap_size += cparam->managed.width * DBLOCK_SIZE(fh, 1);
    state.heap_size += cparam->managed.width * DBLOCK_SIZE(fh, 2);
    state.heap_size += cparam->managed.width * DBLOCK_SIZE(fh, 3);
    state.man_size += cparam->managed.width * DBLOCK_SIZE(fh, 1);
    state.man_size += cparam->managed.width * DBLOCK_SIZE(fh, 2);
    state.man_size += cparam->managed.width * DBLOCK_SIZE(fh, 3);
    state.man_alloc_size += DBLOCK_SIZE(fh, 3);
    state.man_free_space += cparam->managed.width * DBLOCK_FREE(fh, 1);
    state.man_free_space += cparam->managed.width * DBLOCK_FREE(fh, 2);
    state.man_free_space += cparam->managed.width * DBLOCK_FREE(fh, 3);
    if(add_obj(fh, dxpl, 10, obj_size, &state, NULL))
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
    if(add_obj(fh, dxpl, 10, obj_size, &state, NULL))
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
    obj_size = DBLOCK_FREE(fh, 3) - (DBLOCK_SIZE(fh, 2) + 1);
    if(add_obj(fh, dxpl, 10, obj_size, &state, NULL))
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
        state.man_alloc_size += DBLOCK_SIZE(fh, 0);
        if(fill_heap(fh, dxpl, 0, SMALL_OBJ_SIZE1, &state, NULL))
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
    if(fill_row(fh, dxpl, 1, SMALL_OBJ_SIZE1, &state, NULL))
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
    if(fill_row(fh, dxpl, 2, SMALL_OBJ_SIZE1, &state, NULL))
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
    state.man_alloc_size += DBLOCK_SIZE(fh, 3);
    if(add_obj(fh, dxpl, 10, SMALL_OBJ_SIZE1, &state, NULL))
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
    size_t      obj_size;               /* Size of object */
    fheap_heap_state_t state;           /* State of fractal heap */

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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test absolute heap
     */
    TESTING("filling first row, then skipping rows, then backfill and extend");

    /* Fill first row of direct blocks */
    if(fill_root_row(fh, dxpl, 0, SMALL_OBJ_SIZE1, &state, NULL))
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
    obj_size = DBLOCK_SIZE(fh, 2) + 1;
    state.heap_size += cparam->managed.width * DBLOCK_SIZE(fh, 1);
    state.heap_size += cparam->managed.width * DBLOCK_SIZE(fh, 2);
    state.heap_size += cparam->managed.width * DBLOCK_SIZE(fh, 3);
    state.man_size += cparam->managed.width * DBLOCK_SIZE(fh, 1);
    state.man_size += cparam->managed.width * DBLOCK_SIZE(fh, 2);
    state.man_size += cparam->managed.width * DBLOCK_SIZE(fh, 3);
    state.man_alloc_size += DBLOCK_SIZE(fh, 3);
    state.man_free_space += cparam->managed.width * DBLOCK_FREE(fh, 1);
    state.man_free_space += cparam->managed.width * DBLOCK_FREE(fh, 2);
    state.man_free_space += cparam->managed.width * DBLOCK_FREE(fh, 3);
    if(add_obj(fh, dxpl, 20, obj_size, &state, NULL))
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
    if(add_obj(fh, dxpl, 20, obj_size, &state, NULL))
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
    if(fill_row(fh, dxpl, 1, SMALL_OBJ_SIZE1, &state, NULL))
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
    if(fill_row(fh, dxpl, 2, SMALL_OBJ_SIZE1, &state, NULL))
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
    state.man_alloc_size += DBLOCK_SIZE(fh, 3);
    if(add_obj(fh, dxpl, 10, obj_size, &state, NULL))
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
    size_t      obj_size;               /* Size of object */
    fheap_heap_state_t state;           /* State of fractal heap */

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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test absolute heap
     */
    TESTING("filling direct blocks and skipping blocks in non-root indirect block, then backfill and extend");

    /* Fill direct blocks in root indirect block */
    if(fill_root_direct(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    obj_size = DBLOCK_SIZE(fh, 2) + 1;
    state.man_alloc_size += DBLOCK_SIZE(fh, 3);
    if(add_obj(fh, dxpl, 20, obj_size, &state, NULL))
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
    if(fill_row(fh, dxpl, 0, SMALL_OBJ_SIZE1, &state, NULL))
        FAIL_STACK_ERROR
    if(fill_row(fh, dxpl, 1, SMALL_OBJ_SIZE1, &state, NULL))
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
    obj_size = DBLOCK_FREE(fh, 3) - obj_size;
    if(add_obj(fh, dxpl, 20, obj_size, &state, NULL))
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
    if(fill_row(fh, dxpl, 2, SMALL_OBJ_SIZE1, &state, NULL))
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
    state.man_alloc_size += DBLOCK_SIZE(fh, 3);
    if(add_obj(fh, dxpl, 10, obj_size, &state, NULL))
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
    unsigned    num_first_indirect_rows;        /* Number of rows (of direct blocks) in each of the first indirect blocks */
    unsigned    row;                    /* Current row in indirect block */
    size_t      obj_size;               /* Size of object */
    fheap_heap_state_t state;           /* State of fractal heap */
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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /* Retrieve info about heap */
    num_first_indirect_rows = IBLOCK_MAX_DROWS(fh, 1);
#ifdef QAK
HDfprintf(stderr, "num_first_indirect_rows = %u\n", num_first_indirect_rows);
#endif /* QAK */

    /*
     * Test absolute heap
     */
    TESTING("filling direct blocks and skipping row of non-root indirect blocks, then backfill and extend");

    /* Fill direct blocks in root indirect block */
    if(fill_root_direct(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    obj_size = DBLOCK_SIZE(fh, num_first_indirect_rows - 1) + 1;
#ifdef QAK
HDfprintf(stderr, "obj_size = %Zu\n", obj_size);
#endif /* QAK */
    state.man_alloc_size += DBLOCK_SIZE(fh, num_first_indirect_rows);
    if(add_obj(fh, dxpl, 20, obj_size, &state, NULL))
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
    if(add_obj(fh, dxpl, 20, obj_size, &state, NULL))
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
    for(row = 0; row < num_first_indirect_rows; row++) {
        /* Fill rows of direct blocks in skipped indirect blocks */
        for(u = 0; u < cparam->managed.width; u++)
            if(fill_row(fh, dxpl, row, SMALL_OBJ_SIZE1, &state, NULL))
                FAIL_STACK_ERROR

        /* Fill row of direct blocks in largest (i.e. non-skipped) indirect block */
        if(fill_row(fh, dxpl, row, SMALL_OBJ_SIZE1, &state, NULL))
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
    state.man_alloc_size += DBLOCK_SIZE(fh, num_first_indirect_rows);
    if(add_obj(fh, dxpl, 10, obj_size, &state, NULL))
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
    unsigned    num_first_indirect_rows;        /* Number of rows (of direct blocks) in each of the first indirect blocks */
    size_t      obj_size;               /* Size of object */
    fheap_heap_state_t state;           /* State of fractal heap */
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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /* Retrieve info about heap */
    num_first_indirect_rows = IBLOCK_MAX_DROWS(fh, 1);
#ifdef QAK
HDfprintf(stderr, "num_first_indirect_rows = %u\n", num_first_indirect_rows);
#endif /* QAK */

    /*
     * Test absolute heap
     */
    TESTING("filling direct blocks, filling 2nd level indirect blocks, except last one, and insert object too large for 2nd level indirect blocks, then backfill and extend");

    /* Fill direct blocks in root indirect block */
    if(fill_root_direct(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    for(u = 0; u < cparam->managed.width - 1; u++)
        /* Fill all rows of 2nd level indirect blocks in root block */
        if(fill_2nd_indirect(fh, dxpl, 1, SMALL_OBJ_SIZE1, &state, NULL))
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
    obj_size = DBLOCK_SIZE(fh, num_first_indirect_rows - 1) + 1;
#ifdef QAK
HDfprintf(stderr, "obj_size = %Zu\n", obj_size);
#endif /* QAK */
    state.man_alloc_size += DBLOCK_SIZE(fh, num_first_indirect_rows);
    if(add_obj(fh, dxpl, 20, obj_size, &state, NULL))
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
    if(add_obj(fh, dxpl, 20, obj_size, &state, NULL))
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
        if(fill_row(fh, dxpl, u, SMALL_OBJ_SIZE1, &state, NULL))
            FAIL_STACK_ERROR

        /* Direct block row in current 2nd level indirect block */
        if(fill_row(fh, dxpl, u, SMALL_OBJ_SIZE1, &state, NULL))
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
    state.man_alloc_size += DBLOCK_SIZE(fh, num_first_indirect_rows);
    if(add_obj(fh, dxpl, 10, obj_size, &state, NULL))
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
    unsigned    num_first_indirect_rows;        /* Number of rows (of direct blocks) in each of the first indirect blocks */
    unsigned    row;                    /* Current row in indirect block */
    size_t      obj_size;               /* Size of object */
    fheap_heap_state_t state;           /* State of fractal heap */
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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /* Retrieve info about heap */
    num_first_indirect_rows = IBLOCK_MAX_DROWS(fh, 1);
#ifdef QAK
HDfprintf(stderr, "num_first_indirect_rows = %u\n", num_first_indirect_rows);
#endif /* QAK */

    /*
     * Test absolute heap
     */
    TESTING("filling direct blocks and skipping row of non-root indirect blocks, then skip row of direct blocks, then backfill and extend");

    /* Fill direct blocks in root indirect block */
    if(fill_root_direct(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    obj_size = DBLOCK_SIZE(fh, num_first_indirect_rows - 1) + 1;
#ifdef QAK
HDfprintf(stderr, "obj_size = %Zu\n", obj_size);
#endif /* QAK */
    state.man_alloc_size += DBLOCK_SIZE(fh, num_first_indirect_rows);
    if(add_obj(fh, dxpl, 20, obj_size, &state, NULL))
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
    if(add_obj(fh, dxpl, 20, obj_size, &state, NULL))
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
    obj_size = DBLOCK_SIZE(fh, 3) + 1;
#ifdef QAK
HDfprintf(stderr, "obj_size = %Zu\n", obj_size);
#endif /* QAK */
    state.man_alloc_size += DBLOCK_SIZE(fh, 4);
    if(add_obj(fh, dxpl, 10, obj_size, &state, NULL))
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
    if(add_obj(fh, dxpl, 20, obj_size, &state, NULL))
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
    for(u = 1; u < cparam->managed.width; u++) {
        state.man_alloc_size += DBLOCK_SIZE(fh, 4);
        if(add_obj(fh, dxpl, 20, obj_size, &state, NULL))
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

    /* Fill all rows of direct blocks that are smaller than large object's block size */
    for(row = 0; row < num_first_indirect_rows; row++) {
        /* Fill rows of direct blocks in skipped indirect blocks */
        for(u = 0; u < cparam->managed.width; u++)
            if(fill_row(fh, dxpl, row, SMALL_OBJ_SIZE1, &state, NULL))
                FAIL_STACK_ERROR

        /* Fill row of direct blocks in largest (i.e. non-skipped) indirect block */
        /* (Skip the row of blocks filled above) */
        if(row != 4)
            if(fill_row(fh, dxpl, row, SMALL_OBJ_SIZE1, &state, NULL))
                FAIL_STACK_ERROR
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
    state.man_alloc_size += DBLOCK_SIZE(fh, num_first_indirect_rows);
    if(add_obj(fh, dxpl, 10, obj_size, &state, NULL))
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
    unsigned    num_first_indirect_rows;        /* Number of rows (of direct blocks) in each of the first indirect blocks */
    unsigned    max_dblock_rows;        /* Max. # of rows (of direct blocks) in the root indirect block */
    size_t      obj_size;               /* Size of object */
    fheap_heap_state_t state;           /* State of fractal heap */
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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /* Retrieve info about heap */
    num_first_indirect_rows = IBLOCK_MAX_DROWS(fh, 1);
    max_dblock_rows = DTABLE_MAX_DROWS(fh);

    /*
     * Test absolute heap
     */
    TESTING("filling direct blocks and skipping two rows of root indirect block, then backfill and extend");

    /* Fill direct blocks in root indirect block */
    if(fill_root_direct(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    obj_size = DBLOCK_SIZE(fh, max_dblock_rows - 2) + 1;
    state.man_alloc_size += DBLOCK_SIZE(fh, max_dblock_rows - 1);
    if(add_obj(fh, dxpl, 20, obj_size, &state, NULL))
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
    obj_size = DBLOCK_FREE(fh, max_dblock_rows - 1) - obj_size;
    if(add_obj(fh, dxpl, 20, obj_size, &state, NULL))
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

    /* Fill rows skipped over in indirect block's direct blocks */
    for(u = 0; u < num_first_indirect_rows; u++) {
        /* Direct block rows in first row of skipped 2nd level indirect blocks */
        for(v = 0; v < cparam->managed.width; v++)
            if(fill_row(fh, dxpl, u, SMALL_OBJ_SIZE1, &state, NULL))
                FAIL_STACK_ERROR

        /* Direct block rows in second row of skipped 2nd level indirect blocks */
        for(v = 0; v < cparam->managed.width; v++)
            if(fill_row(fh, dxpl, u, SMALL_OBJ_SIZE1, &state, NULL))
                FAIL_STACK_ERROR

        /* Direct block row in used 2nd level indirect block */
        if(fill_row(fh, dxpl, u, SMALL_OBJ_SIZE1, &state, NULL))
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
        if(fill_row(fh, dxpl, num_first_indirect_rows, SMALL_OBJ_SIZE1, &state, NULL))
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
    if(fill_row(fh, dxpl, num_first_indirect_rows, SMALL_OBJ_SIZE1, &state, NULL))
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
    state.man_alloc_size += DBLOCK_SIZE(fh, max_dblock_rows - 1);
    if(add_obj(fh, dxpl, 10, obj_size, &state, NULL))
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
    size_t      obj_size;               /* Size of object */
    fheap_heap_state_t state;           /* State of fractal heap */

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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test absolute heap
     */
    TESTING("filling direct blocks, filling 2nd level indirect blocks, and skip first rows of direct blocks of 3rd level indirect block, then backfill and extend");

    /* Fill direct blocks in root indirect block */
    if(fill_root_direct(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    if(fill_all_2nd_indirect_rows(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    obj_size = DBLOCK_SIZE(fh, 2) + 1;
    state.man_alloc_size += DBLOCK_SIZE(fh, 3);
    if(add_obj(fh, dxpl, 10, obj_size, &state, NULL))
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
    if(add_obj(fh, dxpl, 20, obj_size, &state, NULL))
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
    if(fill_row(fh, dxpl, 0, SMALL_OBJ_SIZE1, &state, NULL))
        FAIL_STACK_ERROR
    if(fill_row(fh, dxpl, 1, SMALL_OBJ_SIZE1, &state, NULL))
        FAIL_STACK_ERROR
    if(fill_row(fh, dxpl, 2, SMALL_OBJ_SIZE1, &state, NULL))
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
    state.man_alloc_size += DBLOCK_SIZE(fh, 3);
    if(add_obj(fh, dxpl, 10, obj_size, &state, NULL))
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
    size_t      obj_size;               /* Size of object */
    fheap_heap_state_t state;           /* State of fractal heap */

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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test absolute heap
     */
    TESTING("filling direct blocks, filling 2nd level indirect blocks, filling 3rd level indirect block's direct blocks, and skip first rows of direct blocks of 3rd level indirect block's 2nd level indirect block, then backfill and extend");

    /* Fill direct blocks in root indirect block */
    if(fill_root_direct(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    if(fill_all_2nd_indirect_rows(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    if(fill_all_direct(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    obj_size = DBLOCK_SIZE(fh, 2) + 1;
    state.man_alloc_size += DBLOCK_SIZE(fh, 3);
    if(add_obj(fh, dxpl, 10, obj_size, &state, NULL))
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
    if(add_obj(fh, dxpl, 20, obj_size, &state, NULL))
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
    if(fill_row(fh, dxpl, 0, SMALL_OBJ_SIZE1, &state, NULL))
        FAIL_STACK_ERROR
    if(fill_row(fh, dxpl, 1, SMALL_OBJ_SIZE1, &state, NULL))
        FAIL_STACK_ERROR
    if(fill_row(fh, dxpl, 2, SMALL_OBJ_SIZE1, &state, NULL))
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
    state.man_alloc_size += DBLOCK_SIZE(fh, 3);
    if(add_obj(fh, dxpl, 10, obj_size, &state, NULL))
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
    unsigned    num_first_indirect_rows;        /* Number of rows (of direct blocks) in each of the first indirect blocks */
    size_t      obj_size;               /* Size of object */
    fheap_heap_state_t state;           /* State of fractal heap */
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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /* Retrieve info about heap */
    num_first_indirect_rows = IBLOCK_MAX_DROWS(fh, 1);
#ifdef QAK
HDfprintf(stderr, "num_first_indirect_rows = %u\n", num_first_indirect_rows);
#endif /* QAK */

    /*
     * Test absolute heap
     */
    TESTING("filling direct blocks, filling 2nd level indirect blocks, filling 3rd level indirect block's direct blocks, and skip first row of indirect blocks of 3rd level indirect block, then backfill and extend");

    /* Fill direct blocks in root indirect block */
    if(fill_root_direct(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    if(fill_all_2nd_indirect_rows(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    if(fill_all_direct(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    obj_size = DBLOCK_SIZE(fh, num_first_indirect_rows - 1) + 1;
#ifdef QAK
HDfprintf(stderr, "obj_size = %Zu\n", obj_size);
#endif /* QAK */
    state.man_alloc_size += DBLOCK_SIZE(fh, num_first_indirect_rows);
    if(add_obj(fh, dxpl, 20, obj_size, &state, NULL))
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
    if(add_obj(fh, dxpl, 20, obj_size, &state, NULL))
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
            if(fill_row(fh, dxpl, u, SMALL_OBJ_SIZE1, &state, NULL))
                FAIL_STACK_ERROR

        /* Direct block row in 3rd level indirect block */
        if(fill_row(fh, dxpl, u, SMALL_OBJ_SIZE1, &state, NULL))
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
    state.man_alloc_size += DBLOCK_SIZE(fh, num_first_indirect_rows);
    if(add_obj(fh, dxpl, 10, obj_size, &state, NULL))
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
    unsigned    num_first_indirect_rows;        /* Number of rows (of direct blocks) in each of the first indirect blocks */
    size_t      obj_size;               /* Size of object */
    fheap_heap_state_t state;           /* State of fractal heap */
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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /* Retrieve info about heap */
    num_first_indirect_rows = IBLOCK_MAX_DROWS(fh, 1);
#ifdef QAK
HDfprintf(stderr, "num_first_indirect_rows = %u\n", num_first_indirect_rows);
#endif /* QAK */

    /*
     * Test absolute heap
     */
    TESTING("filling direct blocks, filling 2nd level indirect blocks, filling 3rd level indirect block's direct blocks, and skip first two rows of indirect blocks of 3rd level indirect block, then backfill and extend");

    /* Fill direct blocks in root indirect block */
    if(fill_root_direct(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    if(fill_all_2nd_indirect_rows(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    if(fill_all_direct(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    obj_size = DBLOCK_SIZE(fh, num_first_indirect_rows) + 1;
#ifdef QAK
HDfprintf(stderr, "obj_size = %Zu\n", obj_size);
#endif /* QAK */
    state.man_alloc_size += DBLOCK_SIZE(fh, num_first_indirect_rows + 1);
    if(add_obj(fh, dxpl, 20, obj_size, &state, NULL))
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
    if(add_obj(fh, dxpl, 20, obj_size, &state, NULL))
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
            if(fill_row(fh, dxpl, u, SMALL_OBJ_SIZE1, &state, NULL))
                FAIL_STACK_ERROR

        /* Direct block row in 3rd level indirect block */
        if(fill_row(fh, dxpl, u, SMALL_OBJ_SIZE1, &state, NULL))
            FAIL_STACK_ERROR
    } /* end for */

    /* Fill row of direct blocks in second 3rd level indirect block */
    if(fill_row(fh, dxpl, num_first_indirect_rows, SMALL_OBJ_SIZE1, &state, NULL))
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
    state.man_alloc_size += DBLOCK_SIZE(fh, num_first_indirect_rows + 1);
    if(add_obj(fh, dxpl, 10, obj_size, &state, NULL))
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
 *              blocks in last 3rd level indirect block, then insert object
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
    unsigned    num_first_indirect_rows;        /* Number of rows (of direct blocks) in each of the first indirect blocks */
    size_t      obj_size;               /* Size of object */
    fheap_heap_state_t state;           /* State of fractal heap */
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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /* Retrieve info about heap */
    num_first_indirect_rows = IBLOCK_MAX_DROWS(fh, 1);
#ifdef QAK
HDfprintf(stderr, "num_first_indirect_rows = %u\n", num_first_indirect_rows);
#endif /* QAK */

    /*
     * Test absolute heap
     */
    TESTING("filling direct blocks, filling 2nd level indirect blocks, filling first row of 3rd level indirect blocks, except last one, fill all direct blocks in last 3rd level indirect block, and insert object too large for it's 2nd level indirect blocks, then backfill and extend");

    /* Fill direct blocks in root indirect block */
    if(fill_root_direct(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    if(fill_all_2nd_indirect_rows(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
        if(fill_3rd_indirect(fh, dxpl, 1, SMALL_OBJ_SIZE1, &state, NULL))
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
    if(fill_all_direct(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    obj_size = DBLOCK_SIZE(fh, num_first_indirect_rows - 1) + 1;
#ifdef QAK
HDfprintf(stderr, "obj_size = %Zu\n", obj_size);
#endif /* QAK */
    state.man_alloc_size += DBLOCK_SIZE(fh, num_first_indirect_rows);
    if(add_obj(fh, dxpl, 20, obj_size, &state, NULL))
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
    if(add_obj(fh, dxpl, 20, obj_size, &state, NULL))
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
            if(fill_row(fh, dxpl, u, SMALL_OBJ_SIZE1, &state, NULL))
                FAIL_STACK_ERROR

        /* Direct block row in current 3rd level indirect block */
        if(fill_row(fh, dxpl, u, SMALL_OBJ_SIZE1, &state, NULL))
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
    state.man_alloc_size += DBLOCK_SIZE(fh, num_first_indirect_rows);
    if(add_obj(fh, dxpl, 10, obj_size, &state, NULL))
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
    unsigned    num_first_indirect_rows;        /* Number of rows (of direct blocks) in each of the first indirect blocks */
    size_t      obj_size;               /* Size of object */
    fheap_heap_state_t state;           /* State of fractal heap */
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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /* Retrieve info about heap */
    num_first_indirect_rows = IBLOCK_MAX_DROWS(fh, 1);
#ifdef QAK
HDfprintf(stderr, "num_first_indirect_rows = %u\n", num_first_indirect_rows);
#endif /* QAK */

    /*
     * Test absolute heap
     */
    TESTING("filling direct blocks, filling 2nd level indirect blocks, filling first row of 3rd level indirect blocks, fill all direct blocks in next 3rd level indirect block, fill all 1st row of 2nd level indirect blocks, except last one, and insert object too large for 2nd level indirect block, then backfill and extend");

    /* Fill direct blocks in root indirect block */
    if(fill_root_direct(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    if(fill_all_2nd_indirect_rows(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    if(fill_3rd_indirect_row(fh, dxpl, 1, SMALL_OBJ_SIZE1, &state, NULL))
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
    if(fill_all_direct(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    for(u = 0; u < cparam->managed.width - 1; u++)
        if(fill_2nd_indirect(fh, dxpl, 1, SMALL_OBJ_SIZE1, &state, NULL))
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
    obj_size = DBLOCK_SIZE(fh, num_first_indirect_rows - 1) + 1;
#ifdef QAK
HDfprintf(stderr, "obj_size = %Zu\n", obj_size);
#endif /* QAK */
    state.man_alloc_size += DBLOCK_SIZE(fh, num_first_indirect_rows);
    if(add_obj(fh, dxpl, 20, obj_size, &state, NULL))
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
    if(add_obj(fh, dxpl, 20, obj_size, &state, NULL))
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
        if(fill_row(fh, dxpl, u, SMALL_OBJ_SIZE1, &state, NULL))
            FAIL_STACK_ERROR

        /* Direct block row in current 2nd level indirect block */
        if(fill_row(fh, dxpl, u, SMALL_OBJ_SIZE1, &state, NULL))
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
    state.man_alloc_size += DBLOCK_SIZE(fh, num_first_indirect_rows);
    if(add_obj(fh, dxpl, 10, obj_size, &state, NULL))
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
    unsigned    num_first_indirect_rows;        /* Number of rows (of direct blocks) in each of the first indirect blocks */
    size_t      obj_size;               /* Size of object */
    fheap_heap_state_t state;           /* State of fractal heap */
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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /* Retrieve info about heap */
    num_first_indirect_rows = IBLOCK_MAX_DROWS(fh, 1);
#ifdef QAK
HDfprintf(stderr, "num_first_indirect_rows = %u\n", num_first_indirect_rows);
#endif /* QAK */

    /*
     * Test absolute heap
     */
    TESTING("filling direct blocks, filling 2nd level indirect blocks, filling 3rd level indirect blocks, fill 4th level indirect block's direct blocks, and skip first row of 2nd indirect blocks of 4th level indirect block, then backfill and extend");

    /* Fill direct blocks in root indirect block */
    if(fill_root_direct(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    if(fill_all_2nd_indirect_rows(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    if(fill_all_3rd_indirect_rows(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    if(fill_all_direct(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    obj_size = DBLOCK_SIZE(fh, num_first_indirect_rows - 1) + 1;
#ifdef QAK
HDfprintf(stderr, "obj_size = %Zu\n", obj_size);
#endif /* QAK */
    state.man_alloc_size += DBLOCK_SIZE(fh, num_first_indirect_rows);
    if(add_obj(fh, dxpl, 20, obj_size, &state, NULL))
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
    if(add_obj(fh, dxpl, 20, obj_size, &state, NULL))
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
            if(fill_row(fh, dxpl, u, SMALL_OBJ_SIZE1, &state, NULL))
                FAIL_STACK_ERROR

        /* Direct block row in 2nd level indirect block */
        if(fill_row(fh, dxpl, u, SMALL_OBJ_SIZE1, &state, NULL))
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
    state.man_alloc_size += DBLOCK_SIZE(fh, num_first_indirect_rows);
    if(add_obj(fh, dxpl, 10, obj_size, &state, NULL))
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
 *              fill all direct blocks and 2nd level indirect blocks in 4th
 *              level indirect block, then
 *              insert object that is too large to hold in first row of 2nd
 *              level indirect blocks of 4th level indirect block's first
 *              3rd level indirect block, then
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
    unsigned    num_first_indirect_rows;        /* Number of rows (of direct blocks) in each of the first indirect blocks */
    size_t      obj_size;               /* Size of object */
    fheap_heap_state_t state;           /* State of fractal heap */
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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /* Retrieve info about heap */
    num_first_indirect_rows = IBLOCK_MAX_DROWS(fh, 1);
#ifdef QAK
HDfprintf(stderr, "num_first_indirect_rows = %u\n", num_first_indirect_rows);
#endif /* QAK */

    /*
     * Test absolute heap
     */
    TESTING("filling direct blocks, filling 2nd level indirect blocks, filling 3rd level indirect blocks, fill 4th level indirect block's direct, 2nd level indirect blocks and 3rd level direct block, and skip first row of 2nd indirect blocks of 4th level indirect block's 3rd level indirect block, then backfill and extend");

    /* Fill direct blocks in root indirect block */
    if(fill_root_direct(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    if(fill_all_2nd_indirect_rows(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    if(fill_all_3rd_indirect_rows(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    if(fill_all_direct(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    if(fill_all_2nd_indirect_rows(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    if(fill_all_direct(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    obj_size = DBLOCK_SIZE(fh, num_first_indirect_rows - 1) + 1;
#ifdef QAK
HDfprintf(stderr, "obj_size = %Zu\n", obj_size);
#endif /* QAK */
    state.man_alloc_size += DBLOCK_SIZE(fh, num_first_indirect_rows);
    if(add_obj(fh, dxpl, 20, obj_size, &state, NULL))
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
    if(add_obj(fh, dxpl, 20, obj_size, &state, NULL))
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
            if(fill_row(fh, dxpl, u, SMALL_OBJ_SIZE1, &state, NULL))
                FAIL_STACK_ERROR

        /* Direct block row in 3rd level indirect block */
        if(fill_row(fh, dxpl, u, SMALL_OBJ_SIZE1, &state, NULL))
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
    state.man_alloc_size += DBLOCK_SIZE(fh, num_first_indirect_rows);
    if(add_obj(fh, dxpl, 10, obj_size, &state, NULL))
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
    unsigned    num_first_indirect_rows;        /* Number of rows (of direct blocks) in each of the first indirect blocks */
    size_t      obj_size;               /* Size of object */
    fheap_heap_state_t state;           /* State of fractal heap */
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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /* Retrieve info about heap */
    num_first_indirect_rows = IBLOCK_MAX_DROWS(fh, 1);
#ifdef QAK
HDfprintf(stderr, "num_first_indirect_rows = %u\n", num_first_indirect_rows);
#endif /* QAK */

    /*
     * Test absolute heap
     */
    TESTING("filling direct blocks, filling 2nd level indirect blocks, filling 3rd level indirect blocks, fill first row of 4th level indirect blocks, fill 2nd row 4th level indirect block's direct, 2nd level indirect blocks, first row of 3rd level indirect blocks, 3rd level direct block in 2nd row, and skip first row of 2nd indirect blocks of 4th level indirect block's 3rd level indirect block, then backfill and extend");

    /* Fill direct blocks in root indirect block */
    if(fill_root_direct(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    if(fill_all_2nd_indirect_rows(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    if(fill_all_3rd_indirect_rows(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    if(fill_4th_indirect_row(fh, dxpl, 1, SMALL_OBJ_SIZE1, &state, NULL))
        FAIL_STACK_ERROR

    /* Account for root indirect block doubling # of rows again */
    /* (From 16 rows to the max. # of rows: 22) */
    /* (Note: this is tied to the particular doubling table/heap creation parameters) */
    {
        unsigned max_root_rows;     /* Maximum # of rows in root indirect block */
        unsigned row;               /* Row in heap */

        /* Get some information for the heap */
        max_root_rows = HEAP_MAX_ROOT_ROWS(fh);

        /* Increase heap size & free space */
        for(row = 16; row < max_root_rows; row++) {
            state.heap_size += cparam->managed.width * DBLOCK_SIZE(fh, row);
            state.man_size += cparam->managed.width * DBLOCK_SIZE(fh, row);
            state.man_free_space += cparam->managed.width * DBLOCK_FREE(fh, row);
        } /* end for */
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
    if(fill_all_direct(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    if(fill_all_2nd_indirect_rows(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    if(fill_3rd_indirect_row(fh, dxpl, 1, SMALL_OBJ_SIZE1, &state, NULL))
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
    if(fill_all_direct(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    obj_size = DBLOCK_SIZE(fh, num_first_indirect_rows - 1) + 1;
#ifdef QAK
HDfprintf(stderr, "obj_size = %Zu\n", obj_size);
#endif /* QAK */
    state.man_alloc_size += DBLOCK_SIZE(fh, num_first_indirect_rows);
    if(add_obj(fh, dxpl, 20, obj_size, &state, NULL))
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
    if(add_obj(fh, dxpl, 20, obj_size, &state, NULL))
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
            if(fill_row(fh, dxpl, u, SMALL_OBJ_SIZE1, &state, NULL))
                FAIL_STACK_ERROR

        /* Direct block row in 3rd level indirect block */
        if(fill_row(fh, dxpl, u, SMALL_OBJ_SIZE1, &state, NULL))
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
    state.man_alloc_size += DBLOCK_SIZE(fh, num_first_indirect_rows);
    if(add_obj(fh, dxpl, 10, obj_size, &state, NULL))
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
    unsigned    num_first_indirect_rows;        /* Number of rows (of direct blocks) in each of the first indirect blocks */
    size_t      obj_size;               /* Size of object */
    fheap_heap_state_t state;           /* State of fractal heap */
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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /* Retrieve info about heap */
    num_first_indirect_rows = IBLOCK_MAX_DROWS(fh, 1);
#ifdef QAK
HDfprintf(stderr, "num_first_indirect_rows = %u\n", num_first_indirect_rows);
#endif /* QAK */

    /*
     * Test absolute heap
     */
    TESTING("filling direct blocks, filling 2nd level indirect blocks, filling 3rd level indirect blocks, fill first row of 3rd level indirect blocks in 4th level indirect block except last 3rd level block, fill direct blocks in 3rd level block, and skip row of 2nd indirect blocks of 4th level indirect block's 3rd level indirect block, then backfill and extend");

    /* Fill direct blocks in root indirect block */
    if(fill_root_direct(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    if(fill_all_2nd_indirect_rows(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    if(fill_all_3rd_indirect_rows(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    if(fill_all_direct(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    if(fill_all_2nd_indirect_rows(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
        if(fill_all_direct(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
            FAIL_STACK_ERROR

        /* Fill row of 2nd level indirect blocks in 3rd level indirect block */
        if(fill_2nd_indirect_row(fh, dxpl, 1, SMALL_OBJ_SIZE1, &state, NULL))
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
    if(fill_all_direct(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    obj_size = DBLOCK_SIZE(fh, num_first_indirect_rows - 1) + 1;
#ifdef QAK
HDfprintf(stderr, "obj_size = %Zu\n", obj_size);
#endif /* QAK */
    state.man_alloc_size += DBLOCK_SIZE(fh, num_first_indirect_rows);
    if(add_obj(fh, dxpl, 20, obj_size, &state, NULL))
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
    if(add_obj(fh, dxpl, 20, obj_size, &state, NULL))
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
            if(fill_row(fh, dxpl, u, SMALL_OBJ_SIZE1, &state, NULL))
                FAIL_STACK_ERROR

        /* Direct block row in 4th level indirect block */
        if(fill_row(fh, dxpl, u, SMALL_OBJ_SIZE1, &state, NULL))
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
    state.man_alloc_size += DBLOCK_SIZE(fh, num_first_indirect_rows);
    if(add_obj(fh, dxpl, 10, obj_size, &state, NULL))
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
    unsigned    num_first_indirect_rows;        /* Number of rows (of direct blocks) in each of the first indirect blocks */
    size_t      obj_size;               /* Size of object */
    fheap_heap_state_t state;           /* State of fractal heap */
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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /* Retrieve info about heap */
    num_first_indirect_rows = IBLOCK_MAX_DROWS(fh, 1);
#ifdef QAK
HDfprintf(stderr, "num_first_indirect_rows = %u\n", num_first_indirect_rows);
#endif /* QAK */

    /*
     * Test absolute heap
     */
    TESTING("filling direct blocks, filling 2nd level indirect blocks, filling 3rd level indirect blocks, fill first row of 4th level indirect blocks, except last one, fill first row of 3rd level indirect blocks in last 4th level indirect block except last 3rd level block, fill direct blocks in 3rd level block, and skip row of 2nd indirect blocks of 4th level indirect block's 3rd level indirect block, then backfill and extend");

    /* Fill direct blocks in root indirect block */
    if(fill_root_direct(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    if(fill_all_2nd_indirect_rows(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    if(fill_all_3rd_indirect_rows(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
        if(fill_all_direct(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
            FAIL_STACK_ERROR

        /* Fill all rows of 2nd level indirect blocks in 4th level indirect block */
        if(fill_all_2nd_indirect_rows(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
            FAIL_STACK_ERROR

        /* Fill row of 3rd level indirect blocks in 4th level indirect block */
        if(fill_3rd_indirect_row(fh, dxpl, 1, SMALL_OBJ_SIZE1, &state, NULL))
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
    if(fill_all_direct(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    if(fill_all_2nd_indirect_rows(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
        if(fill_all_direct(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
            FAIL_STACK_ERROR

        /* Fill row of 2nd level indirect blocks in 3rd level indirect block */
        if(fill_2nd_indirect_row(fh, dxpl, 1, SMALL_OBJ_SIZE1, &state, NULL))
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
    if(fill_all_direct(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
        FAIL_STACK_ERROR

    /* Account for root indirect block doubling # of rows again */
    /* (From 16 rows to the max. # of rows: 22) */
    /* (Note: this is tied to the particular doubling table/heap creation parameters) */
    {
        unsigned max_root_rows;     /* Maximum # of rows in root indirect block */
        unsigned row;               /* Row in heap */

        /* Get some information for the heap */
        max_root_rows = HEAP_MAX_ROOT_ROWS(fh);

        /* Increase heap size & free space */
        for(row = 16; row < max_root_rows; row++) {
            state.heap_size += cparam->managed.width * DBLOCK_SIZE(fh, row);
            state.man_size += cparam->managed.width * DBLOCK_SIZE(fh, row);
            state.man_free_space += cparam->managed.width * DBLOCK_FREE(fh, row);
        } /* end for */
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
    obj_size = DBLOCK_SIZE(fh, num_first_indirect_rows - 1) + 1;
#ifdef QAK
HDfprintf(stderr, "obj_size = %Zu\n", obj_size);
#endif /* QAK */
    state.man_alloc_size += DBLOCK_SIZE(fh, num_first_indirect_rows);
    if(add_obj(fh, dxpl, 20, obj_size, &state, NULL))
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
    if(add_obj(fh, dxpl, 20, obj_size, &state, NULL))
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
            if(fill_row(fh, dxpl, u, SMALL_OBJ_SIZE1, &state, NULL))
                FAIL_STACK_ERROR

        /* Direct block row in 4th level indirect block */
        if(fill_row(fh, dxpl, u, SMALL_OBJ_SIZE1, &state, NULL))
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
    state.man_alloc_size += DBLOCK_SIZE(fh, num_first_indirect_rows);
    if(add_obj(fh, dxpl, 10, obj_size, &state, NULL))
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
    size_t      obj_size;               /* Size of object */
    fheap_heap_state_t state;           /* State of fractal heap */
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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /* Compute # direct block rows in root indirect block */
    num_direct_rows = DTABLE_MAX_DROWS(fh);

    /*
     * Test absolute heap
     */
    TESTING("skipping direct blocks to last row and skipping two rows of root indirect block, then backfill and extend");

    /* Compute heap size & free space when half direct blocks allocated */
    row = 0;
    do{
        state.heap_size += cparam->managed.width * DBLOCK_SIZE(fh, row);
        state.man_size += cparam->managed.width * DBLOCK_SIZE(fh, row);
        state.man_free_space += cparam->managed.width * DBLOCK_FREE(fh, row);
        row++;
    } while(row < (num_direct_rows / 2));

    /* Insert object to extend root block to middle of root direct blocks
     */
    obj_size = DBLOCK_SIZE(fh, row - 2) + 1;
    state.man_alloc_size += DBLOCK_SIZE(fh, row -1);
    if(add_obj(fh, dxpl, 10, obj_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Compute heap size & free space when all direct blocks allocated */
    do{
        state.heap_size += cparam->managed.width * DBLOCK_SIZE(fh, row);
        state.man_size += cparam->managed.width * DBLOCK_SIZE(fh, row);
        state.man_free_space += cparam->managed.width * DBLOCK_FREE(fh, row);
        row++;
    } while(row < num_direct_rows);

    /* Insert large objects into last row of direct blocks in root indirect
     * block, to force extension of root indirect block that covers the first
     * row of indirect blocks in root indirect block
     */
    obj_size = DBLOCK_SIZE(fh, num_direct_rows - 2) + 1;
    for(v = 0; v < cparam->managed.width; v++) {
        state.man_alloc_size += DBLOCK_SIZE(fh, num_direct_rows -1);
        if(add_obj(fh, dxpl, 20, obj_size, &state, NULL))
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

    /* Compute heap size & free space when root indirect block doubles again */
    do{
        state.heap_size += cparam->managed.width * DBLOCK_SIZE(fh, row);
        state.man_size += cparam->managed.width * DBLOCK_SIZE(fh, row);
        state.man_free_space += cparam->managed.width * DBLOCK_FREE(fh, row);
        row++;
    } while(row < (2 * num_direct_rows));

    /* Insert large object, to force creation of indirect blocks with
     * range of skipped blocks that are too small to hold the large object
     */
    obj_size = DBLOCK_SIZE(fh, num_direct_rows - 2) + 1;
    state.man_alloc_size += DBLOCK_SIZE(fh, num_direct_rows -1);
    if(add_obj(fh, dxpl, 20, obj_size, &state, NULL))
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
 * Purpose:	Test inserting objects small enough to fit into first row of
 *              direct blocks, but not to share a block with another object,
 *              until start-block-size * 2 blocks are reached.  Then, go back
 *              and fill in the space in the blocks skipped.
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
    size_t      obj_size;               /* Size of object */
    fheap_heap_state_t state;           /* State of fractal heap */
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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test absolute heap
     */
    TESTING("fragmenting small blocks, then backfill and extend");

    /* Insert objects small enough to fit into initial blocks, but not to
     * share them with other objects of the same size, until the next larger
     * block size is reached.
     */
    obj_size = DBLOCK_SIZE(fh, 0) / 2;
    state.heap_size = DBLOCK_SIZE(fh, 0);
    state.man_size = DBLOCK_SIZE(fh, 0);
    state.man_free_space = DBLOCK_FREE(fh, 0);
    for(u = 0; u < cparam->managed.width; u++) {
        state.man_alloc_size += DBLOCK_SIZE(fh, 0);
        if(add_obj(fh, dxpl, 10, obj_size, &state, NULL))
            FAIL_STACK_ERROR
        if(u == 0) {
            state.heap_size = cparam->managed.width * DBLOCK_SIZE(fh, 0);
            state.man_size = cparam->managed.width * DBLOCK_SIZE(fh, 0);
            state.man_free_space += (cparam->managed.width - 1) * DBLOCK_FREE(fh, 0);
        } /* end if */
    } /* end for */
    state.heap_size += DBLOCK_SIZE(fh, 1) * cparam->managed.width;
    state.man_size += DBLOCK_SIZE(fh, 1) * cparam->managed.width;
    state.man_free_space += DBLOCK_FREE(fh, 1) * cparam->managed.width;
    for(u = 0; u < cparam->managed.width; u++) {
        state.man_alloc_size += DBLOCK_SIZE(fh, 1);
        if(add_obj(fh, dxpl, 10, obj_size, &state, NULL))
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

    /* (Account for doubling root indirect block for rows 3-4 */
    for(u = 0; u < 2; u++) {
        state.heap_size += DBLOCK_SIZE(fh, u + 2) * cparam->managed.width;
        state.man_size += DBLOCK_SIZE(fh, u + 2) * cparam->managed.width;
        state.man_free_space += DBLOCK_FREE(fh, u + 2) * cparam->managed.width;
    } /* end for */

    /* Add one more object, to create a 2 * start_block_size block */
    state.man_alloc_size += DBLOCK_SIZE(fh, 2);
    if(add_obj(fh, dxpl, 10, obj_size, &state, NULL))
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
        if(add_obj(fh, dxpl, 20, obj_size, &state, NULL))
            FAIL_STACK_ERROR
    for(u = 0; u < cparam->managed.width; u++)
        if(add_obj(fh, dxpl, 20, obj_size, &state, NULL))
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
    obj_size = DBLOCK_FREE(fh, 2) - (DBLOCK_SIZE(fh, 0) / 2);
    if(add_obj(fh, dxpl, 20, obj_size, &state, NULL))
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
    unsigned    root_direct_rows;       /* Number of rows in root indirect block */
    size_t      obj_size;               /* Size of object */
    fheap_heap_state_t state;           /* State of fractal heap */
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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /* Retrieve # of direct rows in root indirect block */
    root_direct_rows = H5HF_get_dtable_max_drows_test(fh);

    /*
     * Test absolute heap
     */
    TESTING("fragmenting direct blocks, then backfill and extend");

    /* Insert objects small enough to fit into each direct block, but not to
     * share them with other objects of the same size.
     */
    obj_size = DBLOCK_SIZE(fh, 0) / 2;
    state.heap_size = DBLOCK_SIZE(fh, 0);
    state.man_size = DBLOCK_SIZE(fh, 0);
    state.man_free_space = DBLOCK_FREE(fh, 0);
    /* First row */
    for(u = 0; u < cparam->managed.width; u++) {
        state.man_alloc_size += DBLOCK_SIZE(fh, 0);
        if(add_obj(fh, dxpl, 10, obj_size, &state, NULL))
            FAIL_STACK_ERROR
        if(u == 0) {
            state.heap_size = cparam->managed.width * DBLOCK_SIZE(fh, 0);
            state.man_size = cparam->managed.width * DBLOCK_SIZE(fh, 0);
            state.man_free_space += (cparam->managed.width - 1) * DBLOCK_FREE(fh, 0);
        } /* end if */
    } /* end for */
    state.heap_size += DBLOCK_SIZE(fh, 1) * cparam->managed.width;
    state.man_size += DBLOCK_SIZE(fh, 1) * cparam->managed.width;
    state.man_free_space += DBLOCK_FREE(fh, 1) * cparam->managed.width;
    /* Second row */
    for(u = 0; u < cparam->managed.width; u++) {
        state.man_alloc_size += DBLOCK_SIZE(fh, 1);
        if(add_obj(fh, dxpl, 10, obj_size, &state, NULL))
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

    /* (Account for doubling root indirect block for rows 3-4 */
    for(u = 0; u < 2; u++) {
        state.heap_size += DBLOCK_SIZE(fh, u + 2) * cparam->managed.width;
        state.man_size += DBLOCK_SIZE(fh, u + 2) * cparam->managed.width;
        state.man_free_space += DBLOCK_FREE(fh, u + 2) * cparam->managed.width;
    } /* end for */

    /* Rows 3-4 */
    for(u = 0; u < 2; u++) {
        obj_size = DBLOCK_SIZE(fh, u + 2) / 2;
        for(v = 0; v < cparam->managed.width; v++) {
            state.man_alloc_size += DBLOCK_SIZE(fh, u + 2);
            if(add_obj(fh, dxpl, 10, obj_size, &state, NULL))
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

    /* (Account for doubling root indirect block for rows 5-8 */
    for(u = 0; u < 4; u++) {
        state.heap_size += DBLOCK_SIZE(fh, u + 4) * cparam->managed.width;
        state.man_size += DBLOCK_SIZE(fh, u + 4) * cparam->managed.width;
        state.man_free_space += DBLOCK_FREE(fh, u + 4) * cparam->managed.width;
    } /* end for */

    /* Rows 5-8 */
    for(u = 0; u < 4; u++) {
        obj_size = DBLOCK_SIZE(fh, u + 4) / 2;
        for(v = 0; v < cparam->managed.width; v++) {
            state.man_alloc_size += DBLOCK_SIZE(fh, u + 4);
            if(add_obj(fh, dxpl, 10, obj_size, &state, NULL))
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

    /* (Account for doubling root indirect block for rows 9-16 */
    for(u = 0; u < 8; u++) {
        state.heap_size += DBLOCK_SIZE(fh, u + 8) * cparam->managed.width;
        state.man_size += DBLOCK_SIZE(fh, u + 8) * cparam->managed.width;
        state.man_free_space += DBLOCK_FREE(fh, u + 8) * cparam->managed.width;
    } /* end for */

    /* Row 9 (last direct block row) */
    obj_size = DBLOCK_SIZE(fh, 8) / 2;
    for(v = 0; v < cparam->managed.width; v++) {
        state.man_alloc_size += DBLOCK_SIZE(fh, 8);
            if(add_obj(fh, dxpl, 10, obj_size, &state, NULL))
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

    /* Go back and backfill all root block's direct blocks */
    for(u = 0; u < root_direct_rows; u++) {
        obj_size = DBLOCK_FREE(fh, u) - (DBLOCK_SIZE(fh, u) / 2);
        for(v = 0; v < cparam->managed.width; v++)
            if(add_obj(fh, dxpl, 20, obj_size, &state, NULL))
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
    unsigned    num_first_indirect_rows;        /* Number of rows (of direct blocks) in each of the first indirect blocks */
    size_t      obj_size;               /* Size of object */
    fheap_heap_state_t state;           /* State of fractal heap */
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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /* Compute # of bits used in first row */
    num_first_indirect_rows = IBLOCK_MAX_DROWS(fh, 1);
#ifdef QAK
HDfprintf(stderr, "num_first_indirect_rows = %u\n", num_first_indirect_rows);
#endif /* QAK */

    /*
     * Test absolute heap
     */
    TESTING("fill root direct blocks, then fragment 2nd level indirect block's direct blocks, then backfill and extend");

    /* Fill direct blocks in root indirect block */
    if(fill_root_direct(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    for(u = 0; u < num_first_indirect_rows; u++) {
        obj_size = DBLOCK_SIZE(fh, u) / 2;
        for(v = 0; v < cparam->managed.width; v++) {
            state.man_alloc_size += DBLOCK_SIZE(fh, u);
            if(add_obj(fh, dxpl, 10, obj_size, &state, NULL))
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

    /* Go back and backfill all 2nd level indirect block's direct blocks */
    for(u = 0; u < num_first_indirect_rows; u++) {
        obj_size = DBLOCK_FREE(fh, u) - (DBLOCK_SIZE(fh, u) / 2);
        for(v = 0; v < cparam->managed.width; v++)
            if(add_obj(fh, dxpl, 20, obj_size, &state, NULL))
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
    unsigned    root_direct_rows;       /* Number of rows in root indirect block */
    size_t      obj_size;               /* Size of object */
    fheap_heap_state_t state;           /* State of fractal heap */
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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /* Compute # of direct rows in root indirect block */
    root_direct_rows = DTABLE_MAX_DROWS(fh);

    /*
     * Test absolute heap
     */
    TESTING("fill root direct blocks and 2nd level indirect blocks, then fragment 3rd level indirect block's direct blocks, then backfill and extend");

    /* Fill direct blocks in root indirect block */
    if(fill_root_direct(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    if(fill_all_2nd_indirect_rows(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
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
    for(u = 0; u < root_direct_rows; u++) {
        obj_size = DBLOCK_SIZE(fh, u) / 2;
        for(v = 0; v < cparam->managed.width; v++) {
            state.man_alloc_size += DBLOCK_SIZE(fh, u);
            if(add_obj(fh, dxpl, 10, obj_size, &state, NULL))
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

    /* Go back and backfill all 3rd level indirect block's direct blocks */
    for(u = 0; u < root_direct_rows; u++) {
        obj_size = DBLOCK_FREE(fh, u) - (DBLOCK_SIZE(fh, u) / 2);
        for(v = 0; v < cparam->managed.width; v++)
            if(add_obj(fh, dxpl, 20, obj_size, &state, NULL))
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
        if(H5HF_insert(fh, dxpl, obj_size, &shared_wobj_g[obj_loc], &ids[(num_ids - 1) * HEAP_ID_LEN]) < 0)
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

    /* Verify reading the objects written out */
    for(u = 0; u < num_ids; u++) {
        /* Get object length */
        if(H5HF_get_obj_len(fh, &ids[u * HEAP_ID_LEN], &obj_size) < 0)
            FAIL_STACK_ERROR

        /* Clear read buffer */
        HDmemset(shared_robj_g, 0, obj_size);

        /* Read in object */
        if(H5HF_read(fh, dxpl, &ids[u * HEAP_ID_LEN], shared_robj_g) < 0)
            FAIL_STACK_ERROR

        /* Check for correct object */
        obj_loc = cparam->standalone_size - obj_size;
        if(HDmemcmp(&shared_wobj_g[obj_loc], shared_robj_g, obj_size))
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
    H5MM_xfree(ids);
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
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
        if(H5HF_insert(fh, dxpl, obj_size, &shared_wobj_g[obj_loc], &ids[(num_ids - 1) * HEAP_ID_LEN]) < 0)
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

    /* Verify reading the objects written out */
    for(u = 0; u < num_ids; u++) {
        /* Get object length */
        if(H5HF_get_obj_len(fh, &ids[u * HEAP_ID_LEN], &obj_size) < 0)
            FAIL_STACK_ERROR

        /* Clear read buffer */
        HDmemset(shared_robj_g, 0, obj_size);

        /* Read in object */
        if(H5HF_read(fh, dxpl, &ids[u * HEAP_ID_LEN], shared_robj_g) < 0)
            FAIL_STACK_ERROR

        /* Check for correct object */
        obj_loc = cparam->standalone_size - obj_size;
        if(HDmemcmp(&shared_wobj_g[obj_loc], shared_robj_g, obj_size))
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
    H5MM_xfree(ids);
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
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
    hsize_t     obj_off;                /* Offset of object in heap */
    fheap_heap_state_t state;           /* State of fractal heap */
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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));
    if(check_stats(fh, &state))
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
    if(fill_root_direct(fh, dxpl, SMALL_OBJ_SIZE1, &state, NULL))
        FAIL_STACK_ERROR

    /* Get offset of random heap ID */
    if(H5HF_get_id_off_test(fh, heap_id, &obj_off) < 0)
        FAIL_STACK_ERROR

    /* Make certain we can't accidentally use a valid heap ID */
    while(obj_off < state.heap_size) {
        /* Set heap ID to random (non-null) value */
        for(u = 0; u < HEAP_ID_LEN; u++)
            heap_id[u] = HDrandom() + 1;

        /* Get offset of random heap ID */
        if(H5HF_get_id_off_test(fh, heap_id, &obj_off) < 0)
            FAIL_STACK_ERROR
    } /* end while */

    /* Try removing bogus heap ID from heap w/objects */
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
    off_t       empty_size;             /* Size of a file with an empty heap */
    off_t       file_size;              /* Size of file currently */
    fheap_heap_state_t state;           /* State of fractal heap */
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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));
    if(check_stats(fh, &state))
        FAIL_STACK_ERROR

    /* Prepare for querying the size of a file with an empty heap */

    /* Close (empty) heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close file */
    if(H5Fclose(file)<0) TEST_ERROR;

    /* Get the size of a file w/empty heap*/
    if((empty_size = h5_get_file_size(filename)) == 0)
        TEST_ERROR

    /* Re-open the file */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Re-open heap */
    if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
        FAIL_STACK_ERROR

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
    state.heap_size = DBLOCK_SIZE(fh, 0);
    state.man_size = DBLOCK_SIZE(fh, 0);
    state.man_alloc_size = DBLOCK_SIZE(fh, 0);
    state.man_free_space = DBLOCK_FREE(fh, 0) - sizeof(obj);
    state.nobjs = 1;
    if(check_stats(fh, &state))
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
    state.heap_size = 0;
    state.man_size = 0;
    state.man_alloc_size = 0;
    state.man_free_space = 0;
    state.nobjs = 0;
    if(check_stats(fh, &state))
        FAIL_STACK_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* Get the size of the file */
    if((file_size = h5_get_file_size(filename)) == 0)
        TEST_ERROR

    /* Verify the file is correct size */
    if(file_size != empty_size)
        TEST_ERROR

    PASSED()

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
 * Function:	test_abs_remove_two
 *
 * Purpose:	Test removing two objects from heap
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, May 22, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_remove_two(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    unsigned char heap_id1[HEAP_ID_LEN]; /* Heap ID for first object */
    unsigned char heap_id2[HEAP_ID_LEN]; /* Heap ID for second object */
    unsigned char obj[SMALL_OBJ_SIZE1]; /* Buffer for object to insert */
    size_t      id_len;                 /* Size of fractal heap IDs */
    off_t       empty_size;             /* Size of a file with an empty heap */
    off_t       file_size;              /* Size of file currently */
    fheap_heap_state_t state;           /* State of fractal heap */
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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));
    if(check_stats(fh, &state))
        FAIL_STACK_ERROR

    /* Prepare for querying the size of a file with an empty heap */

    /* Close (empty) heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close file */
    if(H5Fclose(file)<0) TEST_ERROR;

    /* Get the size of a file w/empty heap*/
    if((empty_size = h5_get_file_size(filename)) == 0)
        TEST_ERROR

    /* Re-open the file */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Re-open heap */
    if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
        FAIL_STACK_ERROR

    /*
     * Test removing two (small) objects from absolute heap
     */
    TESTING("removing two objects from absolute heap");

    /* Initialize the buffer for objects to insert */
    for(u = 0; u < sizeof(obj); u++)
        obj[u] = u;

    /* Insert first object into heap */
    if(H5HF_insert(fh, dxpl, sizeof(obj), obj, &heap_id1) < 0)
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
    state.heap_size = DBLOCK_SIZE(fh, 0);
    state.man_size = DBLOCK_SIZE(fh, 0);
    state.man_alloc_size = DBLOCK_SIZE(fh, 0);
    state.man_free_space = DBLOCK_FREE(fh, 0) - sizeof(obj);
    state.nobjs = 1;
    if(check_stats(fh, &state))
        FAIL_STACK_ERROR

    /* Insert second object into heap */
    if(H5HF_insert(fh, dxpl, sizeof(obj), obj, &heap_id2) < 0)
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
    state.man_free_space -= sizeof(obj);
    state.nobjs++;
    if(check_stats(fh, &state))
        FAIL_STACK_ERROR

    /* Remove first object from heap */
    if(H5HF_remove(fh, dxpl, heap_id1) < 0)
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
    state.man_free_space += sizeof(obj);
    state.nobjs--;
    if(check_stats(fh, &state))
        FAIL_STACK_ERROR

    /* Remove second object from heap */
    if(H5HF_remove(fh, dxpl, heap_id2) < 0)
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
    state.heap_size = 0;
    state.man_size = 0;
    state.man_alloc_size = 0;
    state.man_free_space = 0;
    state.nobjs = 0;
    if(check_stats(fh, &state))
        FAIL_STACK_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* Get the size of the file */
    if((file_size = h5_get_file_size(filename)) == 0)
        TEST_ERROR

    /* Verify the file is correct size */
    if(file_size != empty_size)
        TEST_ERROR

    PASSED()

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_abs_remove_two() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_remove_one_larger
 *
 * Purpose:	Test removing single larger (but < standalone size) object
 *              from heap
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, June  6, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_remove_one_larger(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    unsigned char heap_id[HEAP_ID_LEN]; /* Heap ID for object */
    unsigned char *obj;                 /* Buffer for object to insert */
    size_t      obj_len;                /* Length of object to insert */
    size_t      id_len;                 /* Size of fractal heap IDs */
    off_t       empty_size;             /* Size of a file with an empty heap */
    off_t       file_size;              /* Size of file currently */
    fheap_heap_state_t state;           /* State of fractal heap */
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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));
    if(check_stats(fh, &state))
        FAIL_STACK_ERROR

    /* Prepare for querying the size of a file with an empty heap */

    /* Close (empty) heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close file */
    if(H5Fclose(file)<0) TEST_ERROR;

    /* Get the size of a file w/empty heap*/
    if((empty_size = h5_get_file_size(filename)) == 0)
        TEST_ERROR

    /* Re-open the file */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Re-open heap */
    if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
        FAIL_STACK_ERROR

    /*
     * Test removing one larger object from absolute heap
     */
    TESTING("removing single larger object from absolute heap");

    /* Set up object to insert */
    obj_len = DBLOCK_SIZE(fh, 2) + 1;
    obj = shared_wobj_g;

    /* Insert object into heap */
    if(H5HF_insert(fh, dxpl, obj_len, obj, &heap_id) < 0)
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
    for(u = 0; u < 4; u++) {
        state.heap_size += DBLOCK_SIZE(fh, u) * cparam->managed.width;
        state.man_size += DBLOCK_SIZE(fh, u) * cparam->managed.width;
        state.man_free_space += DBLOCK_FREE(fh, u) * cparam->managed.width;
    } /* end for */
    state.man_alloc_size = DBLOCK_SIZE(fh, 3);
    state.man_free_space -= obj_len;
    state.nobjs = 1;
    if(check_stats(fh, &state))
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
    state.heap_size = 0;
    state.man_size = 0;
    state.man_alloc_size = 0;
    state.man_free_space = 0;
    state.nobjs = 0;
    if(check_stats(fh, &state))
        FAIL_STACK_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* Get the size of the file */
    if((file_size = h5_get_file_size(filename)) == 0)
        TEST_ERROR

    /* Verify the file is correct size */
    if(file_size != empty_size)
        TEST_ERROR

    PASSED()

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_abs_remove_one_larger() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_remove_two_larger
 *
 * Purpose:	Test removing two larger (but < standalone size) objects
 *              from heap
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Saturday, June 10, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_remove_two_larger(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    unsigned char heap_id1[HEAP_ID_LEN]; /* Heap ID for first object */
    unsigned char heap_id2[HEAP_ID_LEN]; /* Heap ID for second object */
    unsigned char *obj;                 /* Buffer for object to insert */
    size_t      obj_len;                /* Length of object to insert */
    size_t      id_len;                 /* Size of fractal heap IDs */
    off_t       empty_size;             /* Size of a file with an empty heap */
    off_t       file_size;              /* Size of file currently */
    fheap_heap_state_t state;           /* State of fractal heap */
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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));
    if(check_stats(fh, &state))
        FAIL_STACK_ERROR

    /* Prepare for querying the size of a file with an empty heap */

    /* Close (empty) heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close file */
    if(H5Fclose(file)<0) TEST_ERROR;

    /* Get the size of a file w/empty heap*/
    if((empty_size = h5_get_file_size(filename)) == 0)
        TEST_ERROR

    /* Re-open the file */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Re-open heap */
    if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
        FAIL_STACK_ERROR

    /*
     * Test removing two larger objects from absolute heap
     */
    if(tparam->del_dir == HEAP_DEL_FORWARD)
        TESTING("removing two larger objects from absolute heap (forward)")
    else
        TESTING("removing two larger objects from absolute heap (reverse)")

    /* Set up first object to insert */
    obj_len = DBLOCK_SIZE(fh, 2) + 1;
    obj = shared_wobj_g;

    /* Insert object into heap */
    if(H5HF_insert(fh, dxpl, obj_len, obj, &heap_id1) < 0)
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
    for(u = 0; u < 4; u++) {
        state.heap_size += DBLOCK_SIZE(fh, u) * cparam->managed.width;
        state.man_size += DBLOCK_SIZE(fh, u) * cparam->managed.width;
        state.man_free_space += DBLOCK_FREE(fh, u) * cparam->managed.width;
    } /* end for */
    state.man_alloc_size = DBLOCK_SIZE(fh, 3);
    state.man_free_space -= obj_len;
    state.nobjs = 1;
    if(check_stats(fh, &state))
        FAIL_STACK_ERROR

    /* Set up second object to insert */
    obj_len = DBLOCK_SIZE(fh, 4) + 1;
    obj = shared_wobj_g;

    /* Insert object into heap */
    if(H5HF_insert(fh, dxpl, obj_len, obj, &heap_id2) < 0)
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
    /* (Goes to 8 rows because of doubling) */
    for(u = 4; u < 8; u++) {
        state.heap_size += DBLOCK_SIZE(fh, u) * cparam->managed.width;
        state.man_size += DBLOCK_SIZE(fh, u) * cparam->managed.width;
        state.man_free_space += DBLOCK_FREE(fh, u) * cparam->managed.width;
    } /* end for */
    state.man_alloc_size += DBLOCK_SIZE(fh, 5);
    state.man_free_space -= obj_len;
    state.nobjs = 2;
    if(check_stats(fh, &state))
        FAIL_STACK_ERROR

    /* Remove objects in different orders */
    if(tparam->del_dir == HEAP_DEL_FORWARD) {
        /* Remove first object from heap */
        if(H5HF_remove(fh, dxpl, heap_id1) < 0)
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
        state.man_alloc_size -= DBLOCK_SIZE(fh, 3);
        state.man_free_space += DBLOCK_SIZE(fh, 2) + 1;
        state.nobjs = 1;
        if(check_stats(fh, &state))
            FAIL_STACK_ERROR

        /* Remove second object from heap */
        if(H5HF_remove(fh, dxpl, heap_id2) < 0)
            FAIL_STACK_ERROR
    } /* end if */
    else {
        /* Remove second object from heap */
        if(H5HF_remove(fh, dxpl, heap_id2) < 0)
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
        /* (Goes to 4 rows because of halving) */
        for(u = 4; u < 8; u++) {
            state.heap_size -= DBLOCK_SIZE(fh, u) * cparam->managed.width;
            state.man_size -= DBLOCK_SIZE(fh, u) * cparam->managed.width;
            state.man_free_space -= DBLOCK_FREE(fh, u) * cparam->managed.width;
        } /* end for */
        state.man_alloc_size -= DBLOCK_SIZE(fh, 5);
        state.man_free_space += DBLOCK_SIZE(fh, 4) + 1;
        state.nobjs = 1;
        if(check_stats(fh, &state))
            FAIL_STACK_ERROR

        /* Remove first object from heap */
        if(H5HF_remove(fh, dxpl, heap_id1) < 0)
            FAIL_STACK_ERROR
    } /* end else */

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
    state.heap_size = 0;
    state.man_size = 0;
    state.man_alloc_size = 0;
    state.man_free_space = 0;
    state.nobjs = 0;
    if(check_stats(fh, &state))
        FAIL_STACK_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* Get the size of the file */
    if((file_size = h5_get_file_size(filename)) == 0)
        TEST_ERROR

    /* Verify the file is correct size */
#ifdef QAK
HDfprintf(stderr, "empty_size = %lu\n", (unsigned long)empty_size);
HDfprintf(stderr, "file_size = %lu\n", (unsigned long)file_size);
#endif /* QAK */
    if(file_size != empty_size)
        TEST_ERROR

    PASSED()

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_abs_remove_two_larger() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_remove_three_larger
 *
 * Purpose:	Test removing three larger (but < standalone size) objects
 *              from heap
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, June 12, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_remove_three_larger(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    unsigned char heap_id1[HEAP_ID_LEN]; /* Heap ID for first object */
    unsigned char heap_id2[HEAP_ID_LEN]; /* Heap ID for second object */
    unsigned char heap_id3[HEAP_ID_LEN]; /* Heap ID for third object */
    unsigned char *obj;                 /* Buffer for object to insert */
    size_t      obj_len;                /* Length of object to insert */
    size_t      id_len;                 /* Size of fractal heap IDs */
    off_t       empty_size;             /* Size of a file with an empty heap */
    off_t       file_size;              /* Size of file currently */
    fheap_heap_state_t state;           /* State of fractal heap */
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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));
    if(check_stats(fh, &state))
        FAIL_STACK_ERROR

    /* Prepare for querying the size of a file with an empty heap */

    /* Close (empty) heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close file */
    if(H5Fclose(file)<0) TEST_ERROR;

    /* Get the size of a file w/empty heap*/
    if((empty_size = h5_get_file_size(filename)) == 0)
        TEST_ERROR

    /* Re-open the file */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Re-open heap */
    if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
        FAIL_STACK_ERROR

    /*
     * Test removing three larger objects from absolute heap
     */
    if(tparam->del_dir == HEAP_DEL_FORWARD)
        TESTING("removing three larger objects from absolute heap (forward)")
    else
        TESTING("removing three larger objects from absolute heap (reverse)")

    /* Set up first object to insert */
    obj_len = DBLOCK_SIZE(fh, 2) + 1;
    obj = shared_wobj_g;

    /* Insert object into heap */
    if(H5HF_insert(fh, dxpl, obj_len, obj, &heap_id1) < 0)
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
    for(u = 0; u < 4; u++) {
        state.heap_size += DBLOCK_SIZE(fh, u) * cparam->managed.width;
        state.man_size += DBLOCK_SIZE(fh, u) * cparam->managed.width;
        state.man_free_space += DBLOCK_FREE(fh, u) * cparam->managed.width;
    } /* end for */
    state.man_alloc_size = DBLOCK_SIZE(fh, 3);
    state.man_free_space -= obj_len;
    state.nobjs = 1;
    if(check_stats(fh, &state))
        FAIL_STACK_ERROR

    /* Set up second object to insert */
    obj_len = DBLOCK_SIZE(fh, 4) + 1;
    obj = shared_wobj_g;

    /* Insert object into heap */
    if(H5HF_insert(fh, dxpl, obj_len, obj, &heap_id2) < 0)
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
    /* (Goes to 8 rows because of doubling) */
    for(u = 4; u < 8; u++) {
        state.heap_size += DBLOCK_SIZE(fh, u) * cparam->managed.width;
        state.man_size += DBLOCK_SIZE(fh, u) * cparam->managed.width;
        state.man_free_space += DBLOCK_FREE(fh, u) * cparam->managed.width;
    } /* end for */
    state.man_alloc_size += DBLOCK_SIZE(fh, 5);
    state.man_free_space -= obj_len;
    state.nobjs = 2;
    if(check_stats(fh, &state))
        FAIL_STACK_ERROR

    /* Set up third object to insert */
    obj_len = DBLOCK_SIZE(fh, 7) + 1;
    obj = shared_wobj_g;

    /* Insert object into heap */
    if(H5HF_insert(fh, dxpl, obj_len, obj, &heap_id3) < 0)
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
    /* (Goes to 16 rows because of doubling) */
    for(u = 8; u < 16; u++) {
        state.heap_size += DBLOCK_SIZE(fh, u) * cparam->managed.width;
        state.man_size += DBLOCK_SIZE(fh, u) * cparam->managed.width;
        state.man_free_space += DBLOCK_FREE(fh, u) * cparam->managed.width;
    } /* end for */
    state.man_alloc_size += DBLOCK_SIZE(fh, 8);
    state.man_free_space -= obj_len;
    state.nobjs = 3;
    if(check_stats(fh, &state))
        FAIL_STACK_ERROR

    /* Remove objects in different orders */
    if(tparam->del_dir == HEAP_DEL_FORWARD) {
        /* Remove first object from heap */
        if(H5HF_remove(fh, dxpl, heap_id1) < 0)
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
        state.man_alloc_size -= DBLOCK_SIZE(fh, 3);
        state.man_free_space += DBLOCK_SIZE(fh, 2) + 1;
        state.nobjs = 2;
        if(check_stats(fh, &state))
            FAIL_STACK_ERROR

        /* Remove second object from heap */
        if(H5HF_remove(fh, dxpl, heap_id2) < 0)
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
        state.man_alloc_size -= DBLOCK_SIZE(fh, 5);
        state.man_free_space += DBLOCK_SIZE(fh, 4) + 1;
        state.nobjs = 1;
        if(check_stats(fh, &state))
            FAIL_STACK_ERROR

        /* Remove third object from heap */
        if(H5HF_remove(fh, dxpl, heap_id3) < 0)
            FAIL_STACK_ERROR
    } /* end if */
    else {
        /* Remove third object from heap */
        if(H5HF_remove(fh, dxpl, heap_id3) < 0)
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
        /* (Goes to 8 rows because of halving) */
        for(u = 8; u < 16; u++) {
            state.heap_size -= DBLOCK_SIZE(fh, u) * cparam->managed.width;
            state.man_size -= DBLOCK_SIZE(fh, u) * cparam->managed.width;
            state.man_free_space -= DBLOCK_FREE(fh, u) * cparam->managed.width;
        } /* end for */
        state.man_alloc_size -= DBLOCK_SIZE(fh, 8);
        state.man_free_space += DBLOCK_SIZE(fh, 7) + 1;
        state.nobjs = 2;
        if(check_stats(fh, &state))
            FAIL_STACK_ERROR

        /* Remove second object from heap */
        if(H5HF_remove(fh, dxpl, heap_id2) < 0)
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
        /* (Goes to 4 rows because of halving) */
        for(u = 4; u < 8; u++) {
            state.heap_size -= DBLOCK_SIZE(fh, u) * cparam->managed.width;
            state.man_size -= DBLOCK_SIZE(fh, u) * cparam->managed.width;
            state.man_free_space -= DBLOCK_FREE(fh, u) * cparam->managed.width;
        } /* end for */
        state.man_alloc_size -= DBLOCK_SIZE(fh, 5);
        state.man_free_space += DBLOCK_SIZE(fh, 4) + 1;
        state.nobjs = 1;
        if(check_stats(fh, &state))
            FAIL_STACK_ERROR

        /* Remove first object from heap */
        if(H5HF_remove(fh, dxpl, heap_id1) < 0)
            FAIL_STACK_ERROR
    } /* end else */

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
    state.heap_size = 0;
    state.man_size = 0;
    state.man_alloc_size = 0;
    state.man_free_space = 0;
    state.nobjs = 0;
    if(check_stats(fh, &state))
        FAIL_STACK_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* Get the size of the file */
    if((file_size = h5_get_file_size(filename)) == 0)
        TEST_ERROR

    /* Verify the file is correct size */
#ifdef QAK
HDfprintf(stderr, "empty_size = %lu\n", (unsigned long)empty_size);
HDfprintf(stderr, "file_size = %lu\n", (unsigned long)file_size);
#endif /* QAK */
    if(file_size != empty_size)
        TEST_ERROR

    PASSED()

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_abs_remove_three_larger() */
#endif /* QAK */

#ifndef QAK
#ifndef QAK2

/*-------------------------------------------------------------------------
 * Function:	test_abs_remove_root_direct
 *
 * Purpose:	Test filling and removing all objects from root direct block in
 *              heap
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, May 22, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_remove_root_direct(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    fheap_heap_ids_t keep_ids;          /* Structure to retain heap IDs */
    size_t      id_len;                 /* Size of fractal heap IDs */
    off_t       empty_size;             /* Size of a file with an empty heap */
    off_t       file_size;              /* Size of file currently */
    fheap_heap_state_t state;           /* State of fractal heap */
    const char *base_desc = "removing all objects from root direct block of absolute heap %s";       /* Test description */
    char *del_str = NULL;               /* Deletion order description */
    char *test_desc = NULL;             /* Test description */

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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));
    if(check_stats(fh, &state))
        FAIL_STACK_ERROR

    /* Prepare for querying the size of a file with an empty heap */

    /* Close (empty) heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close file */
    if(H5Fclose(file)<0) TEST_ERROR;

    /* Get the size of a file w/empty heap*/
    if((empty_size = h5_get_file_size(filename)) == 0)
        TEST_ERROR

    /* Re-open the file */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Re-open heap */
    if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
        FAIL_STACK_ERROR

    /*
     * Test filling & removing all (small) objects from root direct block of absolute heap
     */
    del_str = get_del_string(tparam);
    HDassert(del_str);
    test_desc = H5MM_malloc(HDstrlen(del_str) + HDstrlen(base_desc));
    sprintf(test_desc, base_desc, del_str);
    TESTING(test_desc);
    H5MM_xfree(del_str);
    H5MM_xfree(test_desc);

    /* Initialize the heap ID structure */
    HDmemset(&keep_ids, 0, sizeof(fheap_heap_ids_t));

    /* Fill the heap up */
    state.heap_size = DBLOCK_SIZE(fh, 0);
    state.man_size = DBLOCK_SIZE(fh, 0);
    state.man_alloc_size = DBLOCK_SIZE(fh, 0);
    state.man_free_space = DBLOCK_FREE(fh, 0);
    if(fill_heap(fh, dxpl, 0, SMALL_OBJ_SIZE1, &state, &keep_ids))
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

    /* Check up on heap... */
    if(check_stats(fh, &state))
        FAIL_STACK_ERROR

    /* Delete objects inserted (either forward or reverse order) */
    if(del_objs(f, dxpl, &fh, tparam, &state, &keep_ids))
        FAIL_STACK_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* Get the size of the file */
    if((file_size = h5_get_file_size(filename)) == 0)
        TEST_ERROR

    /* Verify the file is correct size */
    if(file_size != empty_size)
        TEST_ERROR

    /* Free resources */
    H5MM_xfree(keep_ids.ids);
    H5MM_xfree(keep_ids.lens);
    H5MM_xfree(keep_ids.offs);

    PASSED()

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        H5MM_xfree(keep_ids.ids);
        H5MM_xfree(keep_ids.lens);
        H5MM_xfree(keep_ids.offs);
        H5MM_xfree(del_str);
        H5MM_xfree(test_desc);
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_abs_remove_root_direct() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_remove_two_direct
 *
 * Purpose:	Test filling and removing all objects from (first) two direct
 *              blocks in heap
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, May 22, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_remove_two_direct(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    fheap_heap_ids_t keep_ids;          /* Structure to retain heap IDs */
    size_t      id_len;                 /* Size of fractal heap IDs */
    off_t       empty_size;             /* Size of a file with an empty heap */
    off_t       file_size;              /* Size of file currently */
    fheap_heap_state_t state;           /* State of fractal heap */
    const char *base_desc = "removing all objects from two direct blocks of absolute heap %s";       /* Test description */
    char *del_str = NULL;               /* Deletion order description */
    char *test_desc = NULL;             /* Test description */

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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));
    if(check_stats(fh, &state))
        FAIL_STACK_ERROR

    /* Prepare for querying the size of a file with an empty heap */

    /* Close (empty) heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close file */
    if(H5Fclose(file)<0) TEST_ERROR;

    /* Get the size of a file w/empty heap*/
    if((empty_size = h5_get_file_size(filename)) == 0)
        TEST_ERROR
#ifdef QAK
HDfprintf(stderr, "empty_size = %lu\n", (unsigned long)empty_size);
#endif /* QAK */

    /* Re-open the file */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Re-open heap */
    if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
        FAIL_STACK_ERROR

    /*
     * Test filling & removing all (small) objects from two direct blocks of absolute heap
     */
    del_str = get_del_string(tparam);
    HDassert(del_str);
    test_desc = H5MM_malloc(HDstrlen(del_str) + HDstrlen(base_desc));
    sprintf(test_desc, base_desc, del_str);
    TESTING(test_desc);
    H5MM_xfree(del_str);
    H5MM_xfree(test_desc);

    /* Initialize the heap ID structure */
    HDmemset(&keep_ids, 0, sizeof(fheap_heap_ids_t));

    /* Fill the first block in heap */
    state.heap_size = DBLOCK_SIZE(fh, 0);
    state.man_size = DBLOCK_SIZE(fh, 0);
    state.man_alloc_size = DBLOCK_SIZE(fh, 0);
    state.man_free_space = DBLOCK_FREE(fh, 0);
    if(fill_heap(fh, dxpl, 0, SMALL_OBJ_SIZE1, &state, &keep_ids))
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

    /* Check up on heap... */
    if(check_stats(fh, &state))
        FAIL_STACK_ERROR

    /* Fill the second block in heap */
    state.heap_size = cparam->managed.width * DBLOCK_SIZE(fh, 0);
    state.man_size = cparam->managed.width * DBLOCK_SIZE(fh, 0);
    state.man_alloc_size += DBLOCK_SIZE(fh, 0);
    state.man_free_space = (cparam->managed.width - 1) * DBLOCK_FREE(fh, 0);
    if(fill_heap(fh, dxpl, 0, SMALL_OBJ_SIZE1, &state, &keep_ids))
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

    /* Check up on heap... */
    if(check_stats(fh, &state))
        FAIL_STACK_ERROR

    /* Delete objects inserted (either forward or reverse order) */
    if(del_objs(f, dxpl, &fh, tparam, &state, &keep_ids))
        FAIL_STACK_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR
    fh = NULL;

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* Get the size of the file */
    if((file_size = h5_get_file_size(filename)) == 0)
        TEST_ERROR
#ifdef QAK
HDfprintf(stderr, "file_size = %lu\n", (unsigned long)file_size);
#endif /* QAK */

    /* Verify the file is correct size */
    if(file_size != empty_size)
        TEST_ERROR

    /* Free resources */
    H5MM_xfree(keep_ids.ids);
    H5MM_xfree(keep_ids.lens);
    H5MM_xfree(keep_ids.offs);

    PASSED()

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        H5MM_xfree(keep_ids.ids);
        H5MM_xfree(keep_ids.lens);
        H5MM_xfree(keep_ids.offs);
        H5MM_xfree(del_str);
        H5MM_xfree(test_desc);
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_abs_remove_two_direct() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_remove_first_row
 *
 * Purpose:	Test filling and removing all objects from first row of direct
 *              blocks in heap
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, June  5, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_remove_first_row(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    fheap_heap_ids_t keep_ids;          /* Structure to retain heap IDs */
    size_t      id_len;                 /* Size of fractal heap IDs */
    off_t       empty_size;             /* Size of a file with an empty heap */
    off_t       file_size;              /* Size of file currently */
    fheap_heap_state_t state;           /* State of fractal heap */
    const char *base_desc = "removing all objects from first row of direct blocks of absolute heap %s";       /* Test description */
    char *del_str = NULL;               /* Deletion order description */
    char *test_desc = NULL;             /* Test description */

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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));
    if(check_stats(fh, &state))
        FAIL_STACK_ERROR

    /* Prepare for querying the size of a file with an empty heap */

    /* Close (empty) heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close file */
    if(H5Fclose(file)<0) TEST_ERROR;

    /* Get the size of a file w/empty heap*/
    if((empty_size = h5_get_file_size(filename)) == 0)
        TEST_ERROR
#ifdef QAK
HDfprintf(stderr, "empty_size = %lu\n", (unsigned long)empty_size);
#endif /* QAK */

    /* Re-open the file */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Re-open heap */
    if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
        FAIL_STACK_ERROR

    /*
     * Test filling & removing all (small) objects from first row of direct blocks of absolute heap
     */
    del_str = get_del_string(tparam);
    HDassert(del_str);
    test_desc = H5MM_malloc(HDstrlen(del_str) + HDstrlen(base_desc));
    sprintf(test_desc, base_desc, del_str);
    TESTING(test_desc);
    H5MM_xfree(del_str);
    H5MM_xfree(test_desc);

    /* Initialize the heap ID structure */
    HDmemset(&keep_ids, 0, sizeof(fheap_heap_ids_t));

    /* Fill first row of direct blocks */
    if(fill_root_row(fh, dxpl, 0, SMALL_OBJ_SIZE1, &state, &keep_ids))
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

    /* Check up on heap... */
    if(check_stats(fh, &state))
        FAIL_STACK_ERROR

    /* Delete objects inserted (either forward or reverse order) */
    if(del_objs(f, dxpl, &fh, tparam, &state, &keep_ids))
        FAIL_STACK_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR
    fh = NULL;

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* Get the size of the file */
    if((file_size = h5_get_file_size(filename)) == 0)
        TEST_ERROR
#ifdef QAK
HDfprintf(stderr, "file_size = %lu\n", (unsigned long)file_size);
#endif /* QAK */

    /* Verify the file is correct size */
    if(file_size != empty_size)
        TEST_ERROR

    /* Free resources */
    H5MM_xfree(keep_ids.ids);
    H5MM_xfree(keep_ids.lens);
    H5MM_xfree(keep_ids.offs);

    PASSED()

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        H5MM_xfree(keep_ids.ids);
        H5MM_xfree(keep_ids.lens);
        H5MM_xfree(keep_ids.offs);
        H5MM_xfree(del_str);
        H5MM_xfree(test_desc);
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_abs_remove_first_row() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_remove_first_two_rows
 *
 * Purpose:	Test filling and removing all objects from first two rows of
 *              direct blocks in heap
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, June 12, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_remove_first_two_rows(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    fheap_heap_ids_t keep_ids;          /* Structure to retain heap IDs */
    size_t      id_len;                 /* Size of fractal heap IDs */
    off_t       empty_size;             /* Size of a file with an empty heap */
    off_t       file_size;              /* Size of file currently */
    fheap_heap_state_t state;           /* State of fractal heap */
    const char *base_desc = "removing all objects from first two rows of direct blocks of absolute heap %s";       /* Test description */
    char *del_str = NULL;               /* Deletion order description */
    char *test_desc = NULL;             /* Test description */

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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));
    if(check_stats(fh, &state))
        FAIL_STACK_ERROR

    /* Prepare for querying the size of a file with an empty heap */

    /* Close (empty) heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close file */
    if(H5Fclose(file)<0) TEST_ERROR;

    /* Get the size of a file w/empty heap*/
    if((empty_size = h5_get_file_size(filename)) == 0)
        TEST_ERROR
#ifdef QAK
HDfprintf(stderr, "empty_size = %lu\n", (unsigned long)empty_size);
#endif /* QAK */

    /* Re-open the file */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Re-open heap */
    if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
        FAIL_STACK_ERROR

    /*
     * Test filling & removing all (small) objects from first row of direct blocks of absolute heap
     */
    del_str = get_del_string(tparam);
    HDassert(del_str);
    test_desc = H5MM_malloc(HDstrlen(del_str) + HDstrlen(base_desc));
    sprintf(test_desc, base_desc, del_str);
    TESTING(test_desc);
    H5MM_xfree(del_str);
    H5MM_xfree(test_desc);

    /* Initialize the heap ID structure */
    HDmemset(&keep_ids, 0, sizeof(fheap_heap_ids_t));

    /* Fill first two rows of direct blocks */
    if(fill_root_row(fh, dxpl, 0, SMALL_OBJ_SIZE1, &state, &keep_ids))
        FAIL_STACK_ERROR
    if(fill_root_row(fh, dxpl, 1, SMALL_OBJ_SIZE1, &state, &keep_ids))
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

    /* Check up on heap... */
    if(check_stats(fh, &state))
        FAIL_STACK_ERROR

    /* Delete objects inserted (either forward or reverse order) */
    if(del_objs(f, dxpl, &fh, tparam, &state, &keep_ids))
        FAIL_STACK_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR
    fh = NULL;

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* Get the size of the file */
    if((file_size = h5_get_file_size(filename)) == 0)
        TEST_ERROR
#ifdef QAK
HDfprintf(stderr, "file_size = %lu\n", (unsigned long)file_size);
#endif /* QAK */

    /* Verify the file is correct size */
    if(file_size != empty_size)
        TEST_ERROR

    /* Free resources */
    H5MM_xfree(keep_ids.ids);
    H5MM_xfree(keep_ids.lens);
    H5MM_xfree(keep_ids.offs);

    PASSED()

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        H5MM_xfree(keep_ids.ids);
        H5MM_xfree(keep_ids.lens);
        H5MM_xfree(keep_ids.offs);
        H5MM_xfree(del_str);
        H5MM_xfree(test_desc);
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_abs_remove_first_two_rows() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_remove_first_four_rows
 *
 * Purpose:	Test filling and removing all objects from first four rows of
 *              direct blocks in heap
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, June 13, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_remove_first_four_rows(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    fheap_heap_ids_t keep_ids;          /* Structure to retain heap IDs */
    size_t      id_len;                 /* Size of fractal heap IDs */
    off_t       empty_size;             /* Size of a file with an empty heap */
    off_t       file_size;              /* Size of file currently */
    fheap_heap_state_t state;           /* State of fractal heap */
    const char *base_desc = "removing all objects from first four rows of direct blocks of absolute heap %s";       /* Test description */
    char *del_str = NULL;               /* Deletion order description */
    char *test_desc = NULL;             /* Test description */

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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));
    if(check_stats(fh, &state))
        FAIL_STACK_ERROR

    /* Prepare for querying the size of a file with an empty heap */

    /* Close (empty) heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close file */
    if(H5Fclose(file)<0) TEST_ERROR;

    /* Get the size of a file w/empty heap*/
    if((empty_size = h5_get_file_size(filename)) == 0)
        TEST_ERROR
#ifdef QAK
HDfprintf(stderr, "empty_size = %lu\n", (unsigned long)empty_size);
#endif /* QAK */

    /* Re-open the file */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Re-open heap */
    if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
        FAIL_STACK_ERROR

    /*
     * Test filling & removing all (small) objects from first row of direct blocks of absolute heap
     */
    del_str = get_del_string(tparam);
    HDassert(del_str);
    test_desc = H5MM_malloc(HDstrlen(del_str) + HDstrlen(base_desc));
    sprintf(test_desc, base_desc, del_str);
    TESTING(test_desc);
    H5MM_xfree(del_str);
    H5MM_xfree(test_desc);

    /* Initialize the heap ID structure */
    HDmemset(&keep_ids, 0, sizeof(fheap_heap_ids_t));

    /* Fill first two rows of direct blocks */
    if(fill_root_row(fh, dxpl, 0, SMALL_OBJ_SIZE1, &state, &keep_ids))
        FAIL_STACK_ERROR
    if(fill_root_row(fh, dxpl, 1, SMALL_OBJ_SIZE1, &state, &keep_ids))
        FAIL_STACK_ERROR
    if(fill_root_row(fh, dxpl, 2, SMALL_OBJ_SIZE1, &state, &keep_ids))
        FAIL_STACK_ERROR
    if(fill_root_row(fh, dxpl, 3, SMALL_OBJ_SIZE1, &state, &keep_ids))
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

    /* Check up on heap... */
    if(check_stats(fh, &state))
        FAIL_STACK_ERROR

    /* Delete objects inserted (either forward or reverse order) */
    if(del_objs(f, dxpl, &fh, tparam, &state, &keep_ids))
        FAIL_STACK_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR
    fh = NULL;

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* Get the size of the file */
    if((file_size = h5_get_file_size(filename)) == 0)
        TEST_ERROR
#ifdef QAK
HDfprintf(stderr, "file_size = %lu\n", (unsigned long)file_size);
#endif /* QAK */

    /* Verify the file is correct size */
    if(file_size != empty_size)
        TEST_ERROR

    /* Free resources */
    H5MM_xfree(keep_ids.ids);
    H5MM_xfree(keep_ids.lens);
    H5MM_xfree(keep_ids.offs);

    PASSED()

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        H5MM_xfree(keep_ids.ids);
        H5MM_xfree(keep_ids.lens);
        H5MM_xfree(keep_ids.offs);
        H5MM_xfree(del_str);
        H5MM_xfree(test_desc);
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_abs_remove_first_four_rows() */


/*-------------------------------------------------------------------------
 * Function:	test_abs_remove_all_root_direct
 *
 * Purpose:	Test filling and removing all objects from all direct blocks
 *              in root indirect block of heap
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, June 13, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_remove_all_root_direct(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    fheap_heap_ids_t keep_ids;          /* Structure to retain heap IDs */
    size_t      id_len;                 /* Size of fractal heap IDs */
    off_t       empty_size;             /* Size of a file with an empty heap */
    off_t       file_size;              /* Size of file currently */
    fheap_heap_state_t state;           /* State of fractal heap */
    const char *base_desc = "removing all objects from all direct blocks of root group in absolute heap %s";       /* Test description */
    char *del_str = NULL;               /* Deletion order description */
    char *test_desc = NULL;             /* Test description */

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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));
    if(check_stats(fh, &state))
        FAIL_STACK_ERROR

    /* Prepare for querying the size of a file with an empty heap */

    /* Close (empty) heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close file */
    if(H5Fclose(file)<0) TEST_ERROR;

    /* Get the size of a file w/empty heap*/
    if((empty_size = h5_get_file_size(filename)) == 0)
        TEST_ERROR
#ifdef QAK
HDfprintf(stderr, "empty_size = %lu\n", (unsigned long)empty_size);
#endif /* QAK */

    /* Re-open the file */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Re-open heap */
    if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
        FAIL_STACK_ERROR

    /*
     * Test filling & removing all (small) objects from first row of direct blocks of absolute heap
     */
    del_str = get_del_string(tparam);
    HDassert(del_str);
    test_desc = H5MM_malloc(HDstrlen(del_str) + HDstrlen(base_desc));
    sprintf(test_desc, base_desc, del_str);
    TESTING(test_desc);
    H5MM_xfree(del_str);
    H5MM_xfree(test_desc);

    /* Initialize the heap ID structure */
    HDmemset(&keep_ids, 0, sizeof(fheap_heap_ids_t));

    /* Fill direct blocks in root indirect block */
    if(fill_root_direct(fh, dxpl, SMALL_OBJ_SIZE1, &state, &keep_ids))
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

    /* Check up on heap... */
    if(check_stats(fh, &state))
        FAIL_STACK_ERROR

    /* Delete objects inserted (either forward or reverse order) */
    if(del_objs(f, dxpl, &fh, tparam, &state, &keep_ids))
        FAIL_STACK_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR
    fh = NULL;

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* Get the size of the file */
    if((file_size = h5_get_file_size(filename)) == 0)
        TEST_ERROR
#ifdef QAK
HDfprintf(stderr, "file_size = %lu\n", (unsigned long)file_size);
#endif /* QAK */

    /* Verify the file is correct size */
    if(file_size != empty_size)
        TEST_ERROR

    /* Free resources */
    H5MM_xfree(keep_ids.ids);
    H5MM_xfree(keep_ids.lens);
    H5MM_xfree(keep_ids.offs);

    PASSED()

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        H5MM_xfree(keep_ids.ids);
        H5MM_xfree(keep_ids.lens);
        H5MM_xfree(keep_ids.offs);
        H5MM_xfree(del_str);
        H5MM_xfree(test_desc);
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_abs_remove_all_root_direct() */
#endif /* QAK2 */

#ifdef NOT_YET

/*-------------------------------------------------------------------------
 * Function:	test_abs_remove_2nd_indirect
 *
 * Purpose:	Test filling and removing all objects up to 2nd level indirect
 *              blocks of heap
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, June 13, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_abs_remove_2nd_indirect(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[1024];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    fheap_heap_ids_t keep_ids;          /* Structure to retain heap IDs */
    size_t      id_len;                 /* Size of fractal heap IDs */
    off_t       empty_size;             /* Size of a file with an empty heap */
    off_t       file_size;              /* Size of file currently */
    fheap_heap_state_t state;           /* State of fractal heap */
    const char *base_desc = "removing all objects from 2nd level indirect blocks of absolute heap %s";       /* Test description */
    char *del_str = NULL;               /* Deletion order description */
    char *test_desc = NULL;             /* Test description */

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
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));
    if(check_stats(fh, &state))
        FAIL_STACK_ERROR

    /* Prepare for querying the size of a file with an empty heap */

    /* Close (empty) heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close file */
    if(H5Fclose(file)<0) TEST_ERROR;

    /* Get the size of a file w/empty heap*/
    if((empty_size = h5_get_file_size(filename)) == 0)
        TEST_ERROR
#ifdef QAK
HDfprintf(stderr, "empty_size = %lu\n", (unsigned long)empty_size);
#endif /* QAK */

    /* Re-open the file */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Re-open heap */
    if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
        FAIL_STACK_ERROR

    /*
     * Test filling & removing all (small) objects from first row of direct blocks of absolute heap
     */
    del_str = get_del_string(tparam);
    HDassert(del_str);
    test_desc = H5MM_malloc(HDstrlen(del_str) + HDstrlen(base_desc));
    sprintf(test_desc, base_desc, del_str);
    TESTING(test_desc);
    H5MM_xfree(del_str);
    H5MM_xfree(test_desc);

    /* Initialize the heap ID structure */
    HDmemset(&keep_ids, 0, sizeof(fheap_heap_ids_t));

    /* Fill direct blocks in root indirect block */
    if(fill_root_direct(fh, dxpl, SMALL_OBJ_SIZE1, &state, &keep_ids))
        FAIL_STACK_ERROR

    /* Fill all rows of 2nd level indirect blocks */
    if(fill_all_2nd_indirect_rows(fh, dxpl, SMALL_OBJ_SIZE1, &state, &keep_ids))
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

    /* Check up on heap... */
    if(check_stats(fh, &state))
        FAIL_STACK_ERROR

    /* Delete objects inserted (either forward or reverse order) */
    if(del_objs(f, dxpl, &fh, tparam, &state, &keep_ids))
        FAIL_STACK_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR
    fh = NULL;

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* Get the size of the file */
    if((file_size = h5_get_file_size(filename)) == 0)
        TEST_ERROR
#ifdef QAK
HDfprintf(stderr, "file_size = %lu\n", (unsigned long)file_size);
#endif /* QAK */

    /* Verify the file is correct size */
    if(file_size != empty_size)
        TEST_ERROR

    /* Free resources */
    H5MM_xfree(keep_ids.ids);
    H5MM_xfree(keep_ids.lens);
    H5MM_xfree(keep_ids.offs);

    PASSED()

    /* All tests passed */
    return(0);

error:
    H5E_BEGIN_TRY {
        H5MM_xfree(keep_ids.ids);
        H5MM_xfree(keep_ids.lens);
        H5MM_xfree(keep_ids.offs);
        H5MM_xfree(del_str);
        H5MM_xfree(test_desc);
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_abs_remove_2nd_indirect() */
#endif /* NOT_YET */
#endif /* QAK */


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
    unsigned    u;                      /* Local index variable */
    unsigned	nerrors = 0;            /* Cumulative error count */
    int		ExpressMode;

    /* Reset library */
    h5_reset();
    fapl = h5_fileaccess();
    ExpressMode = GetTestExpress();
    if (ExpressMode > 1)
	printf("***Express test mode on.  Some tests may be skipped\n");

    /* Initialize heap's creation parameters */
    init_small_cparam(&cparam);

    /* Allocate space for the shared objects */
    shared_obj_size_g = cparam.standalone_size + 256;
    shared_wobj_g = H5MM_malloc(shared_obj_size_g);
    shared_robj_g = H5MM_malloc(shared_obj_size_g);

    /* Initialize the shared write buffer for objects */
    for(u = 0; u < shared_obj_size_g; u++)
        shared_wobj_g[u] = (unsigned char)u;

    /* Iterate over the testing parameters */
#ifndef QAK
    for(curr_test = FHEAP_TEST_NORMAL; curr_test < FHEAP_TEST_NTESTS; curr_test++) {
#else /* QAK */
HDfprintf(stderr, "Uncomment test loop!\n");
/* curr_test = FHEAP_TEST_NORMAL; */
curr_test = FHEAP_TEST_REOPEN;
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
	if (ExpressMode > 1)
	    printf("***Express test mode on.  Some tests skipped\n");
	else
            nerrors += test_abs_start_5th_recursive_indirect(fapl, &cparam, &tparam);
#else /* QAK */
HDfprintf(stderr, "Uncomment tests!\n");
#endif /* QAK */

#ifndef QAK
        /* Skip blocks insertion */
	if (ExpressMode > 1)
	    printf("***Express test mode on.  Some tests skipped\n");
	else {
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

            /* Additional skipped block insertion tests */
            nerrors += test_abs_skip_direct_skip_indirect_two_rows_add_skipped(fapl, &cparam, &tparam);
        } /* end else */
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
	if (ExpressMode > 1)
	    printf("***Express test mode on.  Some tests skipped\n");
	else {
            nerrors += test_abs_random_managed((hsize_t)(100*1000*1000), fapl, &cparam, &tparam);
            nerrors += test_abs_random_pow2_managed((hsize_t)(100*1000*1000), fapl, &cparam, &tparam);
        } /* end else */
#else /* QAK */
HDfprintf(stderr, "Uncomment tests!\n");
#endif /* QAK */

        /*
         * Test fractal heap object deletion
         */
        /* Simple removal */
#ifndef QAK
        nerrors += test_abs_remove_bogus(fapl, &cparam, &tparam);
        nerrors += test_abs_remove_one(fapl, &cparam, &tparam);
        nerrors += test_abs_remove_two(fapl, &cparam, &tparam);
        nerrors += test_abs_remove_one_larger(fapl, &cparam, &tparam);
        tparam.del_dir = HEAP_DEL_FORWARD;
        nerrors += test_abs_remove_two_larger(fapl, &cparam, &tparam);
        tparam.del_dir = HEAP_DEL_REVERSE;
        nerrors += test_abs_remove_two_larger(fapl, &cparam, &tparam);
        tparam.del_dir = HEAP_DEL_FORWARD;
        nerrors += test_abs_remove_three_larger(fapl, &cparam, &tparam);
        tparam.del_dir = HEAP_DEL_REVERSE;
        nerrors += test_abs_remove_three_larger(fapl, &cparam, &tparam);
#else /* QAK */
HDfprintf(stderr, "Uncomment tests!\n");
#endif /* QAK */

#ifndef QAK
#ifndef QAK
        {
        fheap_test_del_dir_t del_dir;        /* Deletion direction */
        fheap_test_del_drain_t drain_half;   /* Deletion draining */

        /* More complex removals */
        for(drain_half = HEAP_DEL_DRAIN_ALL; drain_half < HEAP_DEL_DRAIN_N; drain_half++) {
            tparam.drain_half = drain_half;
            for(del_dir = HEAP_DEL_FORWARD; del_dir < HEAP_DEL_NDIRS; del_dir++) {
                tparam.del_dir = del_dir;
#else /* QAK */
HDfprintf(stderr, "Uncomment test loops!\n");
tparam.drain_half = HEAP_DEL_DRAIN_ALL;
/* tparam.drain_half = HEAP_DEL_DRAIN_HALF; */
tparam.del_dir = HEAP_DEL_REVERSE;
#endif /* QAK */

#ifndef QAK
#ifndef QAK2
                nerrors += test_abs_remove_root_direct(fapl, &cparam, &tparam);
                nerrors += test_abs_remove_two_direct(fapl, &cparam, &tparam);
                nerrors += test_abs_remove_first_row(fapl, &cparam, &tparam);
                nerrors += test_abs_remove_first_two_rows(fapl, &cparam, &tparam);
                nerrors += test_abs_remove_first_four_rows(fapl, &cparam, &tparam);
                nerrors += test_abs_remove_all_root_direct(fapl, &cparam, &tparam);
#else /* QAK2 */
HDfprintf(stderr, "Uncomment tests!\n");
#endif /* QAK2 */
#ifdef NOT_YET
                nerrors += test_abs_remove_2nd_indirect(fapl, &cparam, &tparam);
#endif /* NOT_YET */
#else /* QAK */
HDfprintf(stderr, "Uncomment tests!\n");
#endif /* QAK */
#ifndef QAK
            } /* end for */
        } /* end for */
        } /* end block */
#endif /* QAK */
#else /* QAK */
HDfprintf(stderr, "Uncomment tests!\n");
#endif /* QAK */
#ifndef QAK
    } /* end for */
#endif /* QAK */

    if(nerrors)
        goto error;
    puts("All fractal heap tests passed.");

    /* Release space for the shared objects */
    H5MM_xfree(shared_wobj_g);
    H5MM_xfree(shared_robj_g);
    H5MM_xfree(shared_ids_g);
    H5MM_xfree(shared_lens_g);
    H5MM_xfree(shared_offs_g);

    /* Clean up file used */
#ifndef QAK
    h5_cleanup(FILENAME, fapl);
#else /* QAK */
HDfprintf(stderr, "Uncomment cleanup!\n");
#endif /* QAK */

    return 0;

error:
    puts("*** TESTS FAILED ***");
    H5E_BEGIN_TRY {
        H5MM_xfree(shared_wobj_g);
        H5MM_xfree(shared_robj_g);
        H5MM_xfree(shared_ids_g);
        H5MM_xfree(shared_lens_g);
        H5MM_xfree(shared_offs_g);
	H5Pclose(fapl);
    } H5E_END_TRY;
    return 1;
} /* end main() */

