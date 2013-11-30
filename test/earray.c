/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* Programmer:  Quincey Koziol <koziol@hdfgroup.org>
 *              Tuesday, June 17, 2008
 */
#include "h5test.h"

/*
 * This file needs to access private datatypes from the H5EA package.
 * This file also needs to access the extensible array testing code.
 */
#define H5EA_PACKAGE
#define H5EA_TESTING
#include "H5EApkg.h"		/* Extensible Arrays			*/

/* Other private headers that this test requires */
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5Vprivate.h"		/* Vectors and arrays 			*/


/* Local macros */

/* Max. testfile name length */
#define EARRAY_FILENAME_LEN     1024

/* Extensible array creation values */
#define ELMT_SIZE               sizeof(uint64_t)
#define MAX_NELMTS_BITS         32                      /* i.e. 4 giga-elements */
#define IDX_BLK_ELMTS           4
#define SUP_BLK_MIN_DATA_PTRS   4
#define DATA_BLK_MIN_ELMTS      16
#define MAX_DBLOCK_PAGE_NELMTS_BITS     10              /* i.e. 1024 elements per data block page */

/* Convenience macros for computing earray state */
#define EA_HDR_SIZE             72                      /* (hard-coded, current size) */
#define EA_IBLOCK_SIZE          298                     /* (hard-coded, current size) */
#define EA_NELMTS(cparam, tparam, idx, sblk_idx)                \
        (hsize_t)(cparam->idx_blk_elmts +                       \
                tparam->sblk_info[sblk_idx].start_idx +         \
                ((1 + ((idx - (cparam->idx_blk_elmts + tparam->sblk_info[sblk_idx].start_idx)) / tparam->sblk_info[sblk_idx].dblk_nelmts)) \
                    * tparam->sblk_info[sblk_idx].dblk_nelmts))
#define EA_NDATA_BLKS(cparam, tparam, idx, sblk_idx)            \
        (1 + tparam->sblk_info[sblk_idx].start_dblk +           \
                ((idx - (cparam->idx_blk_elmts + tparam->sblk_info[sblk_idx].start_idx)) / tparam->sblk_info[sblk_idx].dblk_nelmts))

/* Iterator parameter values */
#define EA_RND2_SCALE           100
#define EA_CYC_COUNT            4

/* Local typedefs */

/* Types of tests to perform */
typedef enum {
    EARRAY_TEST_NORMAL,         /* "Normal" test, with no testing parameters set */
    EARRAY_TEST_REOPEN,         /* Set the reopen_array flag */
    EARRAY_TEST_NTESTS          /* The number of test types, must be last */
} earray_test_type_t;

/* Types of iteration to perform */
typedef enum {
    EARRAY_ITER_FW,             /* "Forward" iteration */
    EARRAY_ITER_RV,             /* "Reverse" iteration */
    EARRAY_ITER_RND,            /* "Random" iteration */
    EARRAY_ITER_CYC,            /* "Cyclic" iteration */
    EARRAY_ITER_RND2,           /* "Random #2" iteration */
    EARRAY_ITER_NITERS          /* The number of iteration types, must be last */
} earray_iter_type_t;

/* Orders to operate on entries */
typedef enum {
    EARRAY_DIR_FORWARD,          /* Insert objects from 0 -> nobjs */
    EARRAY_DIR_RANDOM,           /* Insert objects randomly from 0 - nobjs */
    EARRAY_DIR_CYCLIC,           /* Insert every n'th object cyclicly: 0, n, 2n, 3n, ..., nobjs/n, 1+nobjs/n, 1+n+nobjs/n, 1+2n+nobjs/n, ..., nobjs */
    EARRAY_DIR_REVERSE,          /* Insert objects from nobjs -> 0 */
    EARRAY_DIR_INWARD,           /* Insert objects from outside to in: 0, nobjs, 1, nobjs-1, 2, nobjs-2, ..., nobjs/2 */
    EARRAY_DIR_OUTWARD,          /* Insert objects from inside to out: nobjs/2, (nobjs/2)-1, (nobjs/2)+1, ..., 0, nobjs */
    EARRAY_DIR_NDIRS             /* The number of different insertion orders, must be last */
} earray_test_dir_t;

/* Whether to compress data blocks */
typedef enum {
    EARRAY_TEST_NO_COMPRESS,    /* Don't compress data blocks */
    EARRAY_TEST_COMPRESS,       /* Compress data blocks */
    EARRAY_TEST_COMP_N          /* The number of different ways to test compressing array blocks, must be last */
} earray_test_comp_t;

/* Extensible array state information */
typedef struct earray_state_t {
    hsize_t hdr_size;           /* Size of header */
    hsize_t nindex_blks;        /* # of index blocks */
    hsize_t index_blk_size;     /* Size of index blocks */
    hsize_t nsuper_blks;        /* # of super blocks */
    hsize_t super_blk_size;     /* Size of super blocks */
    hsize_t ndata_blks;         /* # of data blocks */
    hsize_t data_blk_size;      /* Size of data blocks */
    hsize_t max_idx_set;        /* Highest element index stored (+1 - i.e. if element 0 has been set, this value with be '1', if no elements have been stored, this value will be '0') */
    hsize_t nelmts;             /* # of elements "realized" */
} earray_state_t;

/* Forward decl. */
typedef struct earray_test_param_t earray_test_param_t;

/* Extensible array iterator class */
typedef struct earray_iter_t {
    void *(*init)(const H5EA_create_t *cparam, const earray_test_param_t *tparam,
        hsize_t cnt);           /* Initialize/allocate iterator private info */
    hssize_t (*next)(void *info);       /* Get the next element to test */
    hssize_t (*max_elem)(const void *info);  /* Get the max. element set */
    int (*state)(void *_eiter, const H5EA_create_t *cparam,
            const earray_test_param_t *tparam, earray_state_t *state, hsize_t idx);  /* Get the state of the extensible array */
    herr_t (*term)(void *info); /* Shutdown/free iterator private info */
} earray_iter_t;

/* Testing parameters */
struct earray_test_param_t {
    earray_test_type_t reopen_array;    /* Whether to re-open the array during the test */
    earray_test_comp_t comp;            /* Whether to compress the blocks or not */
    const earray_iter_t *eiter;         /* Iterator to use for this test */

    /* Super block information */
    size_t nsblks;                      /* Number of superblocks needed for array */
    H5EA_sblk_info_t *sblk_info;        /* Array of information for each super block */
};

/* Flush depend test context */
typedef struct earray_flush_depend_ctx_t {
    hbool_t base_obj;                   /* Flag to indicate that base object has been flushed */
    hbool_t idx0_obj;                   /* Flag to indicate that index 0's object has been flushed */
    hbool_t idx0_elem;                  /* Flag to indicate that index 0's element has been flushed */
    hbool_t idx1_obj;                   /* Flag to indicate that index 1's object has been flushed */
    hbool_t idx1_elem;                  /* Flag to indicate that index 1's element has been flushed */
    hbool_t idx10000_obj;               /* Flag to indicate that index 10000's object has been flushed */
    hbool_t idx10000_elem;              /* Flag to indicate that index 10000's element has been flushed */
} earray_flush_depend_ctx_t;

/* Extensible array test cache object */
typedef struct earray_test_t {
    /* Information for H5AC cache functions, _must_ be first field in structure */
    H5AC_info_t cache_info;

    /* Entry information */
    uint64_t idx;                       /* Index that entry corresponds to */
    earray_flush_depend_ctx_t *fd_info; /* Context information for flush depend test */
} earray_test_t;


/* Local prototypes */

/* Metadata cache (H5AC) callbacks */
static earray_test_t *earray_cache_test_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void *udata, void *udata2);
static herr_t earray_cache_test_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, earray_test_t *test, unsigned * flags_ptr);
static herr_t earray_cache_test_clear(H5F_t *f, earray_test_t *test, hbool_t destroy);
static herr_t earray_cache_test_size(const H5F_t *f, const earray_test_t *test, size_t *size_ptr);
static herr_t earray_cache_test_dest(H5F_t *f, earray_test_t *test);


/* Local variables */
const char *FILENAME[] = {
    "earray",
    NULL
};

/* Filename to use for all tests */
char filename_g[EARRAY_FILENAME_LEN];

/* Empty file size */
h5_stat_size_t empty_size_g;

/* H5EA test object inherits cache-like properties from H5AC */
const H5AC_class_t H5AC_EARRAY_TEST[1] = {{
    H5AC_TEST_ID,
    (H5AC_load_func_t)earray_cache_test_load,
    (H5AC_flush_func_t)earray_cache_test_flush,
    (H5AC_dest_func_t)earray_cache_test_dest,
    (H5AC_clear_func_t)earray_cache_test_clear,
    (H5AC_notify_func_t)NULL,
    (H5AC_size_func_t)earray_cache_test_size,
}};



/*-------------------------------------------------------------------------
 * Function:	init_cparam
 *
 * Purpose:	Initialize array creation parameter structure
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:	Quincey Koziol
 *              Thursday, August 21, 2008
 *
 *-------------------------------------------------------------------------
 */
static int
init_cparam(H5EA_create_t *cparam)
{
    /* Wipe out background */
    HDmemset(cparam, 0, sizeof(*cparam));

    /* General parameters */
    cparam->cls = H5EA_CLS_TEST;
    cparam->raw_elmt_size = ELMT_SIZE;
    cparam->max_nelmts_bits = MAX_NELMTS_BITS;
    cparam->idx_blk_elmts = IDX_BLK_ELMTS;
    cparam->sup_blk_min_data_ptrs = SUP_BLK_MIN_DATA_PTRS;
    cparam->data_blk_min_elmts = DATA_BLK_MIN_ELMTS;
    cparam->max_dblk_page_nelmts_bits = MAX_DBLOCK_PAGE_NELMTS_BITS;

    return(0);
} /* init_cparam() */


/*-------------------------------------------------------------------------
 * Function:	init_tparam
 *
 * Purpose:	Initialize array testing parameter structure
 *
 * Note:	This initialization is the same as that in H5EA_hdr_init()
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:	Quincey Koziol
 *              Thursday, September 25, 2008
 *
 *-------------------------------------------------------------------------
 */
static int
init_tparam(earray_test_param_t *tparam, const H5EA_create_t *cparam)
{
    hsize_t start_idx;          /* First element index for each super block */
    hsize_t start_dblk;         /* First data block index for each super block */
    size_t u;                   /* Local index variable */

    /* Wipe out background */
    HDmemset(tparam, 0, sizeof(*tparam));

    /* Compute general information */
    tparam->nsblks = 1 + (cparam->max_nelmts_bits - H5V_log2_of2(cparam->data_blk_min_elmts));

    /* Allocate information for each super block */
    tparam->sblk_info = (H5EA_sblk_info_t *)HDmalloc(sizeof(H5EA_sblk_info_t) * tparam->nsblks);
    HDassert(tparam->sblk_info);

    /* Compute information about each super block */
    start_idx = 0;
    start_dblk = 0;
    for(u = 0; u < tparam->nsblks; u++) {
        tparam->sblk_info[u].ndblks = (size_t)H5_EXP2(u / 2);
        tparam->sblk_info[u].dblk_nelmts = (size_t)H5_EXP2((u + 1) / 2) * cparam->data_blk_min_elmts;
        tparam->sblk_info[u].start_idx = start_idx;
        tparam->sblk_info[u].start_dblk = start_dblk;

        /* Advance starting indices for next super block */
        start_idx += (hsize_t)tparam->sblk_info[u].ndblks * (hsize_t)tparam->sblk_info[u].dblk_nelmts;
        start_dblk += (hsize_t)tparam->sblk_info[u].ndblks;
    } /* end for */

    return(0);
} /* init_tparam() */


/*-------------------------------------------------------------------------
 * Function:	finish_tparam
 *
 * Purpose:	Close down array testing parameter structure
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:	Quincey Koziol
 *              Thursday, September 25, 2008
 *
 *-------------------------------------------------------------------------
 */
static int
finish_tparam(earray_test_param_t *tparam)
{
    /* Release super block information */
    HDfree(tparam->sblk_info);
    tparam->sblk_info = NULL;

    return(0);
} /* finish_tparam() */


/*-------------------------------------------------------------------------
 * Function:	create_file
 *
 * Purpose:	Create file and retrieve pointer to internal file object
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:	Quincey Koziol
 *              Thursday, August 28, 2008
 *
 *-------------------------------------------------------------------------
 */
static int
create_file(unsigned flags, hid_t fapl, hid_t *file, H5F_t **f)
{
    /* Create the file to work on */
    if((*file = H5Fcreate(filename_g, flags, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (*f = (H5F_t *)H5I_object(*file)))
        FAIL_STACK_ERROR

    /* Ignore metadata tags in the file's cache */
    if(H5AC_ignore_tags(*f) < 0) {
        FAIL_STACK_ERROR
    }

    /* Success */
    return(0);

error:
    return(-1);
} /* create_file() */


/*-------------------------------------------------------------------------
 * Function:	check_stats
 *
 * Purpose:	Verify stats for an extensible array
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:	Quincey Koziol
 *              Thursday, August 21, 2008
 *
 *-------------------------------------------------------------------------
 */
static int
check_stats(const H5EA_t *ea, const earray_state_t *state)
{
    H5EA_stat_t earray_stats;           /* Statistics about the array */

    /* Get statistics for extensible array and verify they are correct */
    if(H5EA_get_stats(ea, &earray_stats) < 0)
        FAIL_STACK_ERROR

    /* Compare information */
    if(earray_stats.stored.max_idx_set != state->max_idx_set) {
        HDfprintf(stdout, "earray_stats.stored.max_idx_set = %Hu, state->max_idx_set = %Hu\n", earray_stats.stored.max_idx_set, state->max_idx_set);
        TEST_ERROR
    } /* end if */
    if(earray_stats.stored.nelmts != state->nelmts) {
        HDfprintf(stdout, "earray_stats.stored.nelmts = %Hu, state->nelmts = %Hu\n", earray_stats.stored.nelmts, state->nelmts);
        TEST_ERROR
    } /* end if */
    if(earray_stats.computed.hdr_size != state->hdr_size) {
        HDfprintf(stdout, "earray_stats.computed.hdr_size = %Hu, state->hdr_size = %Hu\n", earray_stats.computed.hdr_size, state->hdr_size);
        TEST_ERROR
    } /* end if */
    if(earray_stats.computed.nindex_blks != state->nindex_blks) {
        HDfprintf(stdout, "earray_stats.computed.nindex_blks = %Hu, state->nindex_blks = %Hu\n", earray_stats.computed.nindex_blks, state->nindex_blks);
        TEST_ERROR
    } /* end if */
    if(earray_stats.computed.index_blk_size != state->index_blk_size) {
        HDfprintf(stdout, "earray_stats.computed.index_blk_size = %Hu, state->index_blk_size = %Hu\n", earray_stats.computed.index_blk_size, state->index_blk_size);
        TEST_ERROR
    } /* end if */
    if(earray_stats.stored.ndata_blks != state->ndata_blks) {
        HDfprintf(stdout, "earray_stats.stored.ndata_blks = %Hu, state->ndata_blks = %Hu\n", earray_stats.stored.ndata_blks, state->ndata_blks);
        TEST_ERROR
    } /* end if */
/* Don't compare this currently, it's very hard to compute */
#ifdef NOT_YET
    if(earray_stats.stored.data_blk_size != state->data_blk_size) {
        HDfprintf(stdout, "earray_stats.stored.data_blk_size = %Hu, state->data_blk_size = %Hu\n", earray_stats.stored.data_blk_size, state->data_blk_size);
        TEST_ERROR
    } /* end if */
#endif /* NOT_YET */
    if(earray_stats.stored.nsuper_blks != state->nsuper_blks) {
        HDfprintf(stdout, "earray_stats.stored.nsuper_blks = %Hu, state->nsuper_blks = %Hu\n", earray_stats.stored.nsuper_blks, state->nsuper_blks);
        TEST_ERROR
    } /* end if */
/* Don't compare this currently, it's very hard to compute */
#ifdef NOT_YET
    if(earray_stats.stored.super_blk_size != state->super_blk_size) {
        HDfprintf(stdout, "earray_stats.stored.super_blk_size = %Hu, state->super_blk_size = %Hu\n", earray_stats.stored.super_blk_size, state->super_blk_size);
        TEST_ERROR
    } /* end if */
#endif /* NOT_YET */
#ifdef QAK
HDfprintf(stderr, "nelmts = %Hu, total EA size = %Hu\n", earray_stats.stored.nelmts,
        (earray_stats.computed.hdr_size + earray_stats.computed.index_blk_size + earray_stats.stored.super_blk_size + earray_stats.stored.data_blk_size));
#endif /* QAK */

    /* All tests passed */
    return(0);

error:
    return(-1);
} /* check_stats() */


/*-------------------------------------------------------------------------
 * Function:	reopen_file
 *
 * Purpose:	Perform common "re-open" operations on file & array for testing
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:	Quincey Koziol
 *              Thursday, August 28, 2008
 *
 *-------------------------------------------------------------------------
 */
static int
reopen_file(hid_t *file, H5F_t **f, hid_t fapl, hid_t dxpl,
    H5EA_t **ea, haddr_t ea_addr, const earray_test_param_t *tparam)
{
    /* Check for closing & re-opening the array */
    /* (actually will close & re-open the file as well) */
    if(tparam->reopen_array) {
        /* Close array, if given */
        if(ea) {
            if(H5EA_close(*ea, dxpl) < 0)
                FAIL_STACK_ERROR
            *ea = NULL;
        } /* end if */

        /* Close file */
        if(H5Fclose(*file) < 0)
            FAIL_STACK_ERROR
        *file = (-1);
        *f = NULL;

        /* Re-open the file */
        if((*file = H5Fopen(filename_g, H5F_ACC_RDWR, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (*f = (H5F_t *)H5I_object(*file)))
            FAIL_STACK_ERROR

        /* Ignore metadata tags in the file's cache */
        if(H5AC_ignore_tags(*f) < 0) {
            FAIL_STACK_ERROR
        }

        /* Re-open array, if given */
        if(ea) {
            if(NULL == (*ea = H5EA_open(*f, dxpl, ea_addr, NULL)))
                FAIL_STACK_ERROR
        } /* end if */
    } /* end if */

    /* Success */
    return(0);

error:
    return(-1);
} /* reopen_file() */


/*-------------------------------------------------------------------------
 * Function:	create_array
 *
 * Purpose:	Create an extensible array and perform initial checks
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:	Quincey Koziol
 *              Thursday, August 28, 2008
 *
 *-------------------------------------------------------------------------
 */
static int
create_array(H5F_t *f, hid_t dxpl, const H5EA_create_t *cparam,
    H5EA_t **ea, haddr_t *ea_addr, H5EA__ctx_cb_t *cb)
{
    hsize_t     nelmts;                 /* Number of elements in array */
    earray_state_t state;               /* State of extensible array */

    /* Create array */
    if(NULL == (*ea = H5EA_create(f, dxpl, cparam, cb)))
        FAIL_STACK_ERROR

    /* Check status of array */
    nelmts = (hsize_t)ULLONG_MAX;
    if(H5EA_get_nelmts(*ea, &nelmts) < 0)
        FAIL_STACK_ERROR
    if(nelmts > 0)
        TEST_ERROR
    if(H5EA_get_addr(*ea, ea_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(*ea_addr))
        TEST_ERROR
    HDmemset(&state, 0, sizeof(state));
    state.hdr_size = EA_HDR_SIZE;
    if(check_stats(*ea, &state))
        TEST_ERROR

    /* Success */
    return(0);

error:
    return(-1);
} /* create_array() */


/*-------------------------------------------------------------------------
 * Function:	verify_cparam
 *
 * Purpose:	Verify creation parameters are correct
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:	Quincey Koziol
 *              Thursday, August 28, 2008
 *
 *-------------------------------------------------------------------------
 */
static int
verify_cparam(const H5EA_t *ea, const H5EA_create_t *cparam)
{
    H5EA_create_t test_cparam;          /* Creation parameters for array */

    /* Retrieve creation parameters */
    HDmemset(&test_cparam, 0, sizeof(H5EA_create_t));
    if(H5EA_get_cparam_test(ea, &test_cparam) < 0)
        FAIL_STACK_ERROR

    /* Verify creation parameters */
    if(H5EA_cmp_cparam_test(cparam, &test_cparam))
        TEST_ERROR

    /* Success */
    return(0);

error:
    return(-1);
} /* verify_cparam() */


/*-------------------------------------------------------------------------
 * Function:	finish
 *
 * Purpose:	Close array, delete array, close file and verify that file
 *              is empty size
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:	Quincey Koziol
 *              Thursday, August 28, 2008
 *
 *-------------------------------------------------------------------------
 */
static int
finish(hid_t file, hid_t fapl, H5F_t *f, H5EA_t *ea, haddr_t ea_addr)
{
    h5_stat_size_t file_size;           /* File size, after deleting array */

    /* Close the extensible array */
    if(H5EA_close(ea, H5P_DATASET_XFER_DEFAULT) < 0)
        FAIL_STACK_ERROR

#ifdef QAK
HDfprintf(stderr, "ea_addr = %a\n", ea_addr);
H5Fflush(file, H5F_SCOPE_GLOBAL);
HDsystem("cp earray.h5 earray.h5.save");
#endif /* QAK */

    /* Delete array */
    if(H5EA_delete(f, H5P_DATASET_XFER_DEFAULT, ea_addr, NULL) < 0)
        FAIL_STACK_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR


    /* Get the size of the file */
    if((file_size = h5_get_file_size(filename_g, fapl)) < 0)
        TEST_ERROR

    /* Verify the file is correct size */
    if(file_size != empty_size_g)
        TEST_ERROR

    /* Success */
    return(0);

error:
    return(-1);
} /* finish() */


/*-------------------------------------------------------------------------
 * Function:	earray_cache_test_load
 *
 * Purpose:	Loads an extensible array test object from the disk.
 *
 * Return:	Success:	Pointer to a new extensible array test object
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		May 26 2009
 *
 *-------------------------------------------------------------------------
 */
static earray_test_t *
earray_cache_test_load(H5F_t UNUSED *f, hid_t UNUSED dxpl_id, haddr_t UNUSED addr, const void UNUSED *udata1, void UNUSED *udata2)
{
    /* Check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));

    /* Should never be called */
    HDassert(0 && "Can't be called!");

    return(NULL);
} /* end earray_cache_test_load() */


/*-------------------------------------------------------------------------
 * Function:	earray_cache_test_flush
 *
 * Purpose:	Flushes a dirty extensible array test object to disk.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		May 26 2009
 *
 *-------------------------------------------------------------------------
 */
static herr_t
earray_cache_test_flush(H5F_t UNUSED *f, hid_t UNUSED dxpl_id, hbool_t destroy, haddr_t UNUSED addr, earray_test_t *test, unsigned UNUSED * flags_ptr)
{
    /* check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(test);

    if(test->cache_info.is_dirty) {
        /* Check for out of order flush */
        if(test->fd_info->base_obj)
            TEST_ERROR

        /* Check which index this entry corresponds to */
        if((uint64_t)0 == test->idx) {
            /* Check for out of order flush */
            if(test->fd_info->idx0_obj || test->fd_info->idx0_elem)
                TEST_ERROR

            /* Set flag for object flush */
            test->fd_info->idx0_obj = TRUE;
        } /* end if */
        else if((uint64_t)1 == test->idx) {
            /* Check for out of order flush */
            if(test->fd_info->idx1_obj || test->fd_info->idx1_elem)
                TEST_ERROR

            /* Set flag for object flush */
            test->fd_info->idx1_obj = TRUE;
        } /* end if */
        else if((uint64_t)10000 == test->idx) {
            /* Check for out of order flush */
            if(test->fd_info->idx10000_obj || test->fd_info->idx10000_elem)
                TEST_ERROR

            /* Set flag for object flush */
            test->fd_info->idx10000_obj = TRUE;
        } /* end if */
        else if((uint64_t)-1 == test->idx) {
            /* Set flag for object flush */
            test->fd_info->base_obj = TRUE;
        } /* end if */

        /* Mark the entry as clean */
	test->cache_info.is_dirty = FALSE;
    } /* end if */

    if(destroy)
        if(earray_cache_test_dest(f, test) < 0)
            TEST_ERROR

    return(SUCCEED);

error:
    return(FAIL);
} /* earray_cache_test_flush() */


/*-------------------------------------------------------------------------
 * Function:	earray_cache_test_dest
 *
 * Purpose:	Destroys an extensible array test object in memory.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		May 26 2009
 *
 *-------------------------------------------------------------------------
 */
herr_t
earray_cache_test_dest(H5F_t UNUSED *f, earray_test_t *test)
{
    /*
     * Check arguments.
     */
    HDassert(test);

    /* Free the shared info itself */
    HDfree(test);

    return(SUCCEED);
} /* end earray_cache_test_dest() */


/*-------------------------------------------------------------------------
 * Function:	earray_cache_test_clear
 *
 * Purpose:	Mark an extensible array test object in memory as non-dirty.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		May 26 2009
 *
 *-------------------------------------------------------------------------
 */
static herr_t
earray_cache_test_clear(H5F_t *f, earray_test_t *test, hbool_t destroy)
{
    /*
     * Check arguments.
     */
    HDassert(test);

    /* Reset the dirty flag.  */
    test->cache_info.is_dirty = FALSE;

    if(destroy)
        if(earray_cache_test_dest(f, test) < 0)
            TEST_ERROR

    return(SUCCEED);

error:
    return(FAIL);
} /* end earray_cache_test_clear() */


/*-------------------------------------------------------------------------
 * Function:	earray_cache_test_size
 *
 * Purpose:	Compute the size in bytes of an extensible array test object
 *		on disk, and return it in *size_ptr.  On failure,
 *		the value of *size_ptr is undefined.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		May 26 2009
 *
 *-------------------------------------------------------------------------
 */
static herr_t
earray_cache_test_size(const H5F_t UNUSED *f, const earray_test_t UNUSED *test, size_t *size_ptr)
{
    /* check arguments */
    HDassert(f);
    HDassert(test);
    HDassert(size_ptr);

    /* Set size value */
    /* (hard-code to 1) */
    *size_ptr = 1;

    return(SUCCEED);
} /* earray_cache_test_size() */


/*-------------------------------------------------------------------------
 * Function:	test_create
 *
 * Purpose:	Test creating extensible array
 *
 * Return:	Success: 0
 *		Failure: 1
 *
 * Programmer:	Quincey Koziol
 *              Thursday, August  7, 2008
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_create(hid_t fapl, H5EA_create_t *cparam, earray_test_param_t UNUSED *tparam)
{
    hid_t	file = -1;              /* File ID */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5EA_t      *ea = NULL;             /* Extensible array wrapper */
    haddr_t     ea_addr = HADDR_UNDEF;  /* Array address in file */

    /* Create file & retrieve pointer to internal file object */
    if(create_file(H5F_ACC_TRUNC, fapl, &file, &f) < 0)
        TEST_ERROR

    /*
     * Display testing message
     */
    TESTING("invalid extensible array creation parameters");

#ifndef NDEBUG
{
    H5EA_create_t test_cparam;          /* Creation parameters for array */

    /* Set invalid element size */
    HDmemcpy(&test_cparam, cparam, sizeof(test_cparam));
    test_cparam.raw_elmt_size = 0;
    H5E_BEGIN_TRY {
        ea = H5EA_create(f, H5P_DATASET_XFER_DEFAULT, &test_cparam, NULL);
    } H5E_END_TRY;
    if(ea) {
        /* Close opened extensible array */
        H5EA_close(ea, H5P_DATASET_XFER_DEFAULT);
        ea = NULL;

        /* Indicate error */
        TEST_ERROR
    } /* end if */

    /* Set invalid max. # of elements bits */
    HDmemcpy(&test_cparam, cparam, sizeof(test_cparam));
    test_cparam.max_nelmts_bits = 0;
    H5E_BEGIN_TRY {
        ea = H5EA_create(f, H5P_DATASET_XFER_DEFAULT, &test_cparam, NULL);
    } H5E_END_TRY;
    if(ea) {
        /* Close opened extensible array */
        H5EA_close(ea, H5P_DATASET_XFER_DEFAULT);
        ea = NULL;

        /* Indicate error */
        TEST_ERROR
    } /* end if */

    HDmemcpy(&test_cparam, cparam, sizeof(test_cparam));
    test_cparam.max_nelmts_bits = 65;
    H5E_BEGIN_TRY {
        ea = H5EA_create(f, H5P_DATASET_XFER_DEFAULT, &test_cparam, NULL);
    } H5E_END_TRY;
    if(ea) {
        /* Close opened extensible array */
        H5EA_close(ea, H5P_DATASET_XFER_DEFAULT);
        ea = NULL;

        /* Indicate error */
        TEST_ERROR
    } /* end if */

    /* Set invalid min. # of data block pointers in super blocks */
    HDmemcpy(&test_cparam, cparam, sizeof(test_cparam));
    test_cparam.sup_blk_min_data_ptrs = 0;
    H5E_BEGIN_TRY {
        ea = H5EA_create(f, H5P_DATASET_XFER_DEFAULT, &test_cparam, NULL);
    } H5E_END_TRY;
    if(ea) {
        /* Close opened extensible array */
        H5EA_close(ea, H5P_DATASET_XFER_DEFAULT);
        ea = NULL;

        /* Indicate error */
        TEST_ERROR
    } /* end if */
    HDmemcpy(&test_cparam, cparam, sizeof(test_cparam));
    test_cparam.sup_blk_min_data_ptrs = 1;
    H5E_BEGIN_TRY {
        ea = H5EA_create(f, H5P_DATASET_XFER_DEFAULT, &test_cparam, NULL);
    } H5E_END_TRY;
    if(ea) {
        /* Close opened extensible array */
        H5EA_close(ea, H5P_DATASET_XFER_DEFAULT);
        ea = NULL;

        /* Indicate error */
        TEST_ERROR
    } /* end if */
    HDmemcpy(&test_cparam, cparam, sizeof(test_cparam));
    test_cparam.sup_blk_min_data_ptrs = 6;
    H5E_BEGIN_TRY {
        ea = H5EA_create(f, H5P_DATASET_XFER_DEFAULT, &test_cparam, NULL);
    } H5E_END_TRY;
    if(ea) {
        /* Close opened extensible array */
        H5EA_close(ea, H5P_DATASET_XFER_DEFAULT);
        ea = NULL;

        /* Indicate error */
        TEST_ERROR
    } /* end if */

    /* Set invalid min. # of elements per data block */
    HDmemcpy(&test_cparam, cparam, sizeof(test_cparam));
    test_cparam.data_blk_min_elmts = 0;
    H5E_BEGIN_TRY {
        ea = H5EA_create(f, H5P_DATASET_XFER_DEFAULT, &test_cparam, NULL);
    } H5E_END_TRY;
    if(ea) {
        /* Close opened extensible array */
        H5EA_close(ea, H5P_DATASET_XFER_DEFAULT);
        ea = NULL;

        /* Indicate error */
        TEST_ERROR
    } /* end if */

    /* Set invalid max. # of elements per data block page bits */
    if(test_cparam.idx_blk_elmts > 0) {
        HDmemcpy(&test_cparam, cparam, sizeof(test_cparam));
        test_cparam.max_dblk_page_nelmts_bits = H5V_log2_gen((uint64_t)test_cparam.idx_blk_elmts) - 1;
        H5E_BEGIN_TRY {
            ea = H5EA_create(f, H5P_DATASET_XFER_DEFAULT, &test_cparam, NULL);
        } H5E_END_TRY;
        if(ea) {
            /* Close opened extensible array */
            H5EA_close(ea, H5P_DATASET_XFER_DEFAULT);
            ea = NULL;

            /* Indicate error */
            TEST_ERROR
        } /* end if */
    } /* end if */
    HDmemcpy(&test_cparam, cparam, sizeof(test_cparam));
    test_cparam.max_dblk_page_nelmts_bits = 4;  /* corresponds to 16 elements in data block page, which is less than the 64 elements for the default settings */
    H5E_BEGIN_TRY {
        ea = H5EA_create(f, H5P_DATASET_XFER_DEFAULT, &test_cparam, NULL);
    } H5E_END_TRY;
    if(ea) {
        /* Close opened extensible array */
        H5EA_close(ea, H5P_DATASET_XFER_DEFAULT);
        ea = NULL;

        /* Indicate error */
        TEST_ERROR
    } /* end if */
    HDmemcpy(&test_cparam, cparam, sizeof(test_cparam));
    test_cparam.max_dblk_page_nelmts_bits = test_cparam.max_nelmts_bits + 1;
    H5E_BEGIN_TRY {
        ea = H5EA_create(f, H5P_DATASET_XFER_DEFAULT, &test_cparam, NULL);
    } H5E_END_TRY;
    if(ea) {
        /* Close opened extensible array */
        H5EA_close(ea, H5P_DATASET_XFER_DEFAULT);
        ea = NULL;

        /* Indicate error */
        TEST_ERROR
    } /* end if */

    PASSED()
}
#else /* NDEBUG */
    SKIPPED();
    puts("    Not tested when assertions are disabled");
#endif /* NDEBUG */

    /*
     * Display testing message
     */
    TESTING("extensible array creation");

    /* Create array */
    if(create_array(f, H5P_DATASET_XFER_DEFAULT, cparam, &ea, &ea_addr, NULL) < 0)
        TEST_ERROR

    PASSED()

    /* Verify the creation parameters */
    TESTING("verify array creation parameters");

    /* Verify the creation parameters */
    if(verify_cparam(ea, cparam) < 0)
        TEST_ERROR

    /* Close array, delete array, close file & verify file is empty */
    if(finish(file, fapl, f, ea, ea_addr) < 0)
        TEST_ERROR

    /* All tests passed */
    PASSED()

    return 0;

error:
    H5E_BEGIN_TRY {
        if(ea)
            H5EA_close(ea, H5P_DATASET_XFER_DEFAULT);
	H5Fclose(file);
    } H5E_END_TRY;

    return 1;
} /* end test_create() */


/*-------------------------------------------------------------------------
 * Function:	test_reopen
 *
 * Purpose:	Create & reopen an extensible array
 *
 * Return:	Success:	0
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Thursday, August 28, 2008
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_reopen(hid_t fapl, H5EA_create_t *cparam, earray_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5EA_t      *ea = NULL;             /* Extensible array wrapper */
    haddr_t     ea_addr = HADDR_UNDEF;  /* Array address in file */

    /* Create file & retrieve pointer to internal file object */
    if(create_file(H5F_ACC_TRUNC, fapl, &file, &f) < 0)
        TEST_ERROR

    /*
     * Display testing message
     */
    TESTING("create, close & reopen extensible array");

    /* Create array */
    if(create_array(f, H5P_DATASET_XFER_DEFAULT, cparam, &ea, &ea_addr, NULL) < 0)
        TEST_ERROR

    /* Close the extensible array */
    if(H5EA_close(ea, H5P_DATASET_XFER_DEFAULT) < 0)
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the file */
    if(reopen_file(&file, &f, fapl, H5P_DATASET_XFER_DEFAULT, NULL, HADDR_UNDEF, tparam) < 0)
        TEST_ERROR

    /* Re-open the array */
    if(NULL == (ea = H5EA_open(f, H5P_DATASET_XFER_DEFAULT, ea_addr, NULL)))
        FAIL_STACK_ERROR

    /* Verify the creation parameters */
    if(verify_cparam(ea, cparam) < 0)
        TEST_ERROR

    /* Close array, delete array, close file & verify file is empty */
    if(finish(file, fapl, f, ea, ea_addr) < 0)
        TEST_ERROR

    /* All tests passed */
    PASSED()

    return 0;

error:
    H5E_BEGIN_TRY {
        if(ea)
            H5EA_close(ea, H5P_DATASET_XFER_DEFAULT);
	H5Fclose(file);
    } H5E_END_TRY;

    return 1;
} /* test_reopen() */


/*-------------------------------------------------------------------------
 * Function:	test_open_twice
 *
 * Purpose:	Open an extensible array twice
 *
 * Return:	Success:	0
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Thursday, August 28, 2008
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_open_twice(hid_t fapl, H5EA_create_t *cparam, earray_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t	file2 = -1;             /* File ID */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5F_t	*f2 = NULL;             /* Internal file object pointer */
    H5EA_t      *ea = NULL;             /* Extensible array wrapper */
    H5EA_t      *ea2 = NULL;            /* Extensible array wrapper */
    haddr_t     ea_addr = HADDR_UNDEF;  /* Array address in file */

    /* Create file & retrieve pointer to internal file object */
    if(create_file(H5F_ACC_TRUNC, fapl, &file, &f) < 0)
        TEST_ERROR

    /*
     * Display testing message
     */
    TESTING("open extensible array twice");

    /* Create array */
    if(create_array(f, H5P_DATASET_XFER_DEFAULT, cparam, &ea, &ea_addr, NULL) < 0)
        TEST_ERROR

    /* Open the array again, through the first file handle */
    if(NULL == (ea2 = H5EA_open(f, H5P_DATASET_XFER_DEFAULT, ea_addr, NULL)))
        FAIL_STACK_ERROR

    /* Verify the creation parameters */
    if(verify_cparam(ea, cparam) < 0)
        TEST_ERROR
    if(verify_cparam(ea2, cparam) < 0)
        TEST_ERROR

    /* Close the second extensible array wrapper */
    if(H5EA_close(ea2, H5P_DATASET_XFER_DEFAULT) < 0)
        FAIL_STACK_ERROR
    ea2 = NULL;

    /* Check for closing & re-opening the file */
    if(reopen_file(&file, &f, fapl, H5P_DATASET_XFER_DEFAULT, &ea, ea_addr, tparam) < 0)
        TEST_ERROR

    /* Re-open the file */
    if((file2 = H5Freopen(file)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f2 = (H5F_t *)H5I_object(file2)))
        FAIL_STACK_ERROR

    /* Open the extensible array through the second file handle */
    if(NULL == (ea2 = H5EA_open(f2, H5P_DATASET_XFER_DEFAULT, ea_addr, NULL)))
        FAIL_STACK_ERROR

    /* Verify the creation parameters */
    if(verify_cparam(ea, cparam) < 0)
        TEST_ERROR

    /* Close the first extensible array wrapper */
    if(H5EA_close(ea, H5P_DATASET_XFER_DEFAULT) < 0)
        FAIL_STACK_ERROR
    ea = NULL;

    /* Close the first file */
    /* (close before second file, to detect error on internal array header's
     *  shared file information)
     */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Close array, delete array, close file & verify file is empty */
    if(finish(file2, fapl, f2, ea2, ea_addr) < 0)
        TEST_ERROR

    /* All tests passed */
    PASSED()

    return 0;

error:
    H5E_BEGIN_TRY {
        if(ea)
            H5EA_close(ea, H5P_DATASET_XFER_DEFAULT);
        if(ea2)
            H5EA_close(ea2, H5P_DATASET_XFER_DEFAULT);
	H5Fclose(file);
	H5Fclose(file2);
    } H5E_END_TRY;

    return 1;
} /* test_open_twice() */


/*-------------------------------------------------------------------------
 * Function:	test_delete_open
 *
 * Purpose:	Delete opened extensible array (& open deleted array)
 *
 * Return:	Success:	0
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Thursday, August 28, 2008
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_delete_open(hid_t fapl, H5EA_create_t *cparam, earray_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5EA_t      *ea = NULL;             /* Extensible array wrapper */
    H5EA_t      *ea2 = NULL;            /* Extensible array wrapper */
    haddr_t     ea_addr = HADDR_UNDEF;  /* Array address in file */
    h5_stat_size_t file_size;           /* File size, after deleting array */

    /* Create file & retrieve pointer to internal file object */
    if(create_file(H5F_ACC_TRUNC, fapl, &file, &f) < 0)
        TEST_ERROR

    /*
     * Display testing message
     */
    TESTING("deleting open extensible array");

    /* Create array */
    if(create_array(f, H5P_DATASET_XFER_DEFAULT, cparam, &ea, &ea_addr, NULL) < 0)
        TEST_ERROR

    /* Open the array again */
    if(NULL == (ea2 = H5EA_open(f, H5P_DATASET_XFER_DEFAULT, ea_addr, NULL)))
        FAIL_STACK_ERROR

    /* Request that the array be deleted */
    if(H5EA_delete(f, H5P_DATASET_XFER_DEFAULT, ea_addr, NULL) < 0)
        FAIL_STACK_ERROR

    /* Verify the creation parameters */
    if(verify_cparam(ea, cparam) < 0)
        TEST_ERROR
    if(verify_cparam(ea2, cparam) < 0)
        TEST_ERROR

    /* Close the second extensible array wrapper */
    if(H5EA_close(ea2, H5P_DATASET_XFER_DEFAULT) < 0)
        FAIL_STACK_ERROR
    ea2 = NULL;

    /* Try re-opening the array again (should fail, as array will be deleted) */
    H5E_BEGIN_TRY {
        ea2 = H5EA_open(f, H5P_DATASET_XFER_DEFAULT, ea_addr, NULL);
    } H5E_END_TRY;
    if(ea2) {
        /* Close opened array */
        H5EA_close(ea2, H5P_DATASET_XFER_DEFAULT);

        /* Indicate error */
        TEST_ERROR
    } /* end if */

    /* Close the first extensible array wrapper */
    if(H5EA_close(ea, H5P_DATASET_XFER_DEFAULT) < 0)
        FAIL_STACK_ERROR
    ea = NULL;

    /* Check for closing & re-opening the file */
    if(reopen_file(&file, &f, fapl, H5P_DATASET_XFER_DEFAULT, NULL, HADDR_UNDEF, tparam) < 0)
        TEST_ERROR

    /* Try re-opening the array again (should fail, as array is now deleted) */
    H5E_BEGIN_TRY {
        ea = H5EA_open(f, H5P_DATASET_XFER_DEFAULT, ea_addr, NULL);
    } H5E_END_TRY;
    if(ea) {
        /* Close opened array */
        H5EA_close(ea, H5P_DATASET_XFER_DEFAULT);

        /* Indicate error */
        TEST_ERROR
    } /* end if */

    /* Close the file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Get the size of the file */
    if((file_size = h5_get_file_size(filename_g, fapl)) < 0)
        TEST_ERROR

    /* Verify the file is correct size */
    if(file_size != empty_size_g)
        TEST_ERROR

    /* All tests passed */
    PASSED()

    return 0;

error:
    H5E_BEGIN_TRY {
        if(ea)
            H5EA_close(ea, H5P_DATASET_XFER_DEFAULT);
        if(ea2)
            H5EA_close(ea2, H5P_DATASET_XFER_DEFAULT);
	H5Fclose(file);
    } H5E_END_TRY;

    return 1;
} /* test_delete_open() */


/*-------------------------------------------------------------------------
 * Function:	test_flush_depend_cb
 *
 * Purpose:	Callback for flush dependency 'depend'/'undepend' and
 *		'support'/'unsupport' routines
 *
 * Return:	Success:	0
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, May 26, 2009
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_flush_depend_cb(const void *_elmt, size_t nelmts, void *udata)
{
    earray_flush_depend_ctx_t *ctx = (earray_flush_depend_ctx_t *)udata;
    const uint64_t *elmt = (const uint64_t *)_elmt;     /* Convenience pointer to native elements */

    /* Check for out of order flush */
    if(ctx->base_obj)
        return(FAIL);

    /* Look for magic values */
    while(nelmts > 0) {
        /* Check for elements of interest */
        if((uint64_t)0 == *elmt) {
            /* Check for out-of-order flush */
            if(!ctx->idx0_obj)
                return(FAIL);

            /* Indicate that the element was flushed */
            ctx->idx0_elem = TRUE;
        } /* end if */
        else if((uint64_t)1 == *elmt) {
            /* Check for out-of-order flush */
            if(!ctx->idx1_obj)
                return(FAIL);

            /* Indicate that the element was flushed */
            ctx->idx1_elem = TRUE;
        } /* end if */
        else if((uint64_t)10000 == *elmt) {
            /* Check for out-of-order flush */
            if(!ctx->idx10000_obj)
                return(FAIL);

            /* Indicate that the element was flushed */
            ctx->idx10000_elem = TRUE;
        } /* end if */

        /* Decrement elements left to inspect */
        nelmts--;
        elmt++;
    } /* end while */

    return(SUCCEED);
} /* end test_flush_depend_cb() */


/*-------------------------------------------------------------------------
 * Function:	test_flush_depend
 *
 * Purpose:	Exercise flush dependency 'depend'/'undepend' routines
 *
 * Return:	Success:	0
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Thursday, May 21, 2009
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_flush_depend(const char *env_h5_drvr, hid_t fapl, H5EA_create_t *cparam, earray_test_param_t UNUSED *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t	fapl_copy = -1;         /* Copy of FAPL */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5EA_t      *ea = NULL;             /* Extensible array wrapper */
    haddr_t     ea_addr = HADDR_UNDEF;  /* Array address in file */
    H5EA__ctx_cb_t cb;                  /* Extensible array context action info */
    earray_flush_depend_ctx_t fd_info;  /* Context information for flush depend test */
    haddr_t     base_addr;              /* Base test entry address */
    earray_test_t *base_entry;          /* Pointer to base test entry */
    haddr_t     addr1;                  /* Test entry #1 address */
    earray_test_t *entry1;              /* Pointer to test entry #1 */
    haddr_t     addr2;                  /* Test entry #2 address */
    earray_test_t *entry2;              /* Pointer to test entry #2 */
    haddr_t     addr3;                  /* Test entry #3 address */
    earray_test_t *entry3;              /* Pointer to test entry #3 */
    uint64_t    welmt;                  /* Element to write */
    hsize_t     idx;                    /* Index value of element */

    /*
     * Display testing message
     */
    TESTING("flush dependencies on array metadata");

    /* Can't run this test with multi-file VFDs */
    if(HDstrcmp(env_h5_drvr, "split") && HDstrcmp(env_h5_drvr, "multi") && HDstrcmp(env_h5_drvr, "family")) {
        /* Copy FAPL and set latest format on it */
        if((fapl_copy = H5Pcopy(fapl)) < 0)
            FAIL_STACK_ERROR
        if(H5Pset_libver_bounds(fapl_copy, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
            FAIL_STACK_ERROR

        /* Create file & retrieve pointer to internal file object */
        if(create_file(H5F_ACC_TRUNC | H5F_ACC_SWMR_WRITE, fapl_copy, &file, &f) < 0)
            TEST_ERROR

        /* Close copy of FAPL */
        if(H5Pclose(fapl_copy) < 0)
            FAIL_STACK_ERROR

        /* Create array */
        cb.encode = test_flush_depend_cb;
        HDmemset(&fd_info, 0, sizeof(earray_flush_depend_ctx_t));
        cb.udata = &fd_info;
        if(create_array(f, H5P_DATASET_XFER_DEFAULT, cparam, &ea, &ea_addr, &cb) < 0)
            TEST_ERROR

        /* Verify the creation parameters */
        if(verify_cparam(ea, cparam) < 0)
            TEST_ERROR

        /* Create base entry to insert */
        if(NULL == (base_entry = (earray_test_t *)HDmalloc(sizeof(earray_test_t))))
            TEST_ERROR
        HDmemset(base_entry, 0, sizeof(earray_test_t));
        base_entry->idx = (uint64_t)-1;
        base_entry->fd_info = &fd_info;

        /* Insert test entry into cache */
        base_addr = HADDR_MAX;
        if(H5AC_insert_entry(f, H5P_DATASET_XFER_DEFAULT, H5AC_EARRAY_TEST, base_addr, base_entry, H5AC__PIN_ENTRY_FLAG) < 0)
            TEST_ERROR

        /* Set the base entry as a flush dependency for the array */
        if(H5EA_depend((H5AC_info_t *)base_entry, ea) < 0)
            TEST_ERROR

        /* Create entry #1 to insert */
        if(NULL == (entry1 = (earray_test_t *)HDmalloc(sizeof(earray_test_t))))
            TEST_ERROR
        HDmemset(entry1, 0, sizeof(earray_test_t));
        entry1->fd_info = &fd_info;

        /* Insert test entry into cache */
        addr1 = HADDR_MAX - 1;
        if(H5AC_insert_entry(f, H5P_DATASET_XFER_DEFAULT, H5AC_EARRAY_TEST, addr1, entry1, H5AC__PIN_ENTRY_FLAG) < 0)
            TEST_ERROR

        /* Set the test entry as a flush dependency for 0th index in the array */
        if(H5EA_support(ea, H5P_DATASET_XFER_DEFAULT, (hsize_t)0, (H5AC_info_t *)entry1) < 0)
            TEST_ERROR

        /* Set element of array */
        welmt = (uint64_t)0;
        idx = 0;
        if(H5EA_set(ea, H5P_DATASET_XFER_DEFAULT, idx, &welmt) < 0)
            FAIL_STACK_ERROR

        /* Create entry #2 to insert */
        if(NULL == (entry2 = (earray_test_t *)HDmalloc(sizeof(earray_test_t))))
            TEST_ERROR
        HDmemset(entry2, 0, sizeof(earray_test_t));
        entry2->idx = (uint64_t)1;
        entry2->fd_info = &fd_info;

        /* Insert test entry into cache */
        addr2 = HADDR_MAX - 2;
        if(H5AC_insert_entry(f, H5P_DATASET_XFER_DEFAULT, H5AC_EARRAY_TEST, addr2, entry2, H5AC__PIN_ENTRY_FLAG) < 0)
            TEST_ERROR

        /* Set the test entry as a flush dependency for 1st index in the array */
        if(H5EA_support(ea, H5P_DATASET_XFER_DEFAULT, (hsize_t)1, (H5AC_info_t *)entry2) < 0)
            TEST_ERROR

        /* Set element of array */
        welmt = (uint64_t)1;
        idx = 1;
        if(H5EA_set(ea, H5P_DATASET_XFER_DEFAULT, idx, &welmt) < 0)
            FAIL_STACK_ERROR

        /* Create entry #3 to insert */
        if(NULL == (entry3 = (earray_test_t *)HDmalloc(sizeof(earray_test_t))))
            TEST_ERROR
        HDmemset(entry3, 0, sizeof(earray_test_t));
        entry3->idx = (uint64_t)10000;
        entry3->fd_info = &fd_info;

        /* Insert test entry into cache */
        addr3 = HADDR_MAX - 3;
        if(H5AC_insert_entry(f, H5P_DATASET_XFER_DEFAULT, H5AC_EARRAY_TEST, addr3, entry3, H5AC__PIN_ENTRY_FLAG) < 0)
            TEST_ERROR

        /* Set the test entry as a flush dependency for 10,000th index in the array */
        if(H5EA_support(ea, H5P_DATASET_XFER_DEFAULT, (hsize_t)10000, (H5AC_info_t *)entry3) < 0)
            TEST_ERROR

        /* Set element of array */
        welmt = (uint64_t)10000;
        idx = 10000;
        if(H5EA_set(ea, H5P_DATASET_XFER_DEFAULT, idx, &welmt) < 0)
            FAIL_STACK_ERROR


        /* Flush the cache */
        if(H5Fflush(file, H5F_SCOPE_GLOBAL) < 0)
            TEST_ERROR

        /* Check that all callback flags have been set */
        if(!fd_info.base_obj)
            TEST_ERROR
        if(!fd_info.idx0_obj)
            TEST_ERROR
        if(!fd_info.idx0_elem)
            TEST_ERROR
        if(!fd_info.idx1_obj)
            TEST_ERROR
        if(!fd_info.idx1_elem)
            TEST_ERROR
        if(!fd_info.idx10000_obj)
            TEST_ERROR
        if(!fd_info.idx10000_elem)
            TEST_ERROR


        /* Remove the base entry as a flush dependency for the array */
        if(H5EA_undepend((H5AC_info_t *)base_entry, ea) < 0)
            TEST_ERROR

        /* Protect the base entry */
        if(NULL == (base_entry = (earray_test_t *)H5AC_protect(f, H5P_DATASET_XFER_DEFAULT, H5AC_EARRAY_TEST, base_addr, NULL, H5AC_WRITE)))
            TEST_ERROR

        /* Unprotect & unpin the base entry */
        if(H5AC_unprotect(f, H5P_DATASET_XFER_DEFAULT, H5AC_EARRAY_TEST, base_addr, base_entry, (H5AC__UNPIN_ENTRY_FLAG | H5AC__DELETED_FLAG)) < 0)
            TEST_ERROR

        /* Remove the test entry as a flush dependency for 0th index in the array */
        if(H5EA_unsupport(ea, H5P_DATASET_XFER_DEFAULT, (hsize_t)0, (H5AC_info_t *)entry1) < 0)
            TEST_ERROR

        /* Protect the test entry */
        if(NULL == (entry1 = (earray_test_t *)H5AC_protect(f, H5P_DATASET_XFER_DEFAULT, H5AC_EARRAY_TEST, addr1, NULL, H5AC_WRITE)))
            TEST_ERROR

        /* Unprotect & unpin the test entry */
        if(H5AC_unprotect(f, H5P_DATASET_XFER_DEFAULT, H5AC_EARRAY_TEST, addr1, entry1, (H5AC__UNPIN_ENTRY_FLAG | H5AC__DELETED_FLAG)) < 0)
            TEST_ERROR

        /* Remove the test entry as a flush dependency for 1st index in the array */
        if(H5EA_unsupport(ea, H5P_DATASET_XFER_DEFAULT, (hsize_t)1, (H5AC_info_t *)entry2) < 0)
            TEST_ERROR

        /* Protect the test entry */
        if(NULL == (entry2 = (earray_test_t *)H5AC_protect(f, H5P_DATASET_XFER_DEFAULT, H5AC_EARRAY_TEST, addr2, NULL, H5AC_WRITE)))
            TEST_ERROR

        /* Unprotect & unpin the test entry */
        if(H5AC_unprotect(f, H5P_DATASET_XFER_DEFAULT, H5AC_EARRAY_TEST, addr2, entry2, (H5AC__UNPIN_ENTRY_FLAG | H5AC__DELETED_FLAG)) < 0)
            TEST_ERROR

        /* Remove the test entry as a flush dependency for 10,000th index in the array */
        if(H5EA_unsupport(ea, H5P_DATASET_XFER_DEFAULT, (hsize_t)10000, (H5AC_info_t *)entry3) < 0)
            TEST_ERROR

        /* Protect the test entry */
        if(NULL == (entry3 = (earray_test_t *)H5AC_protect(f, H5P_DATASET_XFER_DEFAULT, H5AC_EARRAY_TEST, addr3, NULL, H5AC_WRITE)))
            TEST_ERROR

        /* Unprotect & unpin the test entry */
        if(H5AC_unprotect(f, H5P_DATASET_XFER_DEFAULT, H5AC_EARRAY_TEST, addr3, entry3, (H5AC__UNPIN_ENTRY_FLAG | H5AC__DELETED_FLAG)) < 0)
            TEST_ERROR

        /* Close the extensible array */
        if(H5EA_close(ea, H5P_DATASET_XFER_DEFAULT) < 0)
            FAIL_STACK_ERROR
        ea = NULL;

        /* Close the file */
        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* All tests passed */
        PASSED()
    } /* end if */
    else {
	SKIPPED();
	puts("    Current VFD not compatible with SWMR access");
    } /* end else */

    return 0;

error:
    H5E_BEGIN_TRY {
        if(ea)
            H5EA_close(ea, H5P_DATASET_XFER_DEFAULT);
	H5Pclose(fapl_copy);
	H5Fclose(file);
    } H5E_END_TRY;

    return 1;
} /* test_flush_depend() */

/* Extensible array iterator info for forward iteration */
typedef struct eiter_fw_t {
    hsize_t idx;        /* Index of next array location */
    unsigned base_sblk_idx;       /* Starting index for actual superblocks */
} eiter_fw_t;


/*-------------------------------------------------------------------------
 * Function:	eiter_fw_init
 *
 * Purpose:	Initialize element interator (forward iteration)
 *
 * Return:	Success:	Pointer to iteration status object
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *              Thursday, October  2, 2008
 *
 *-------------------------------------------------------------------------
 */
static void *
eiter_fw_init(const H5EA_create_t UNUSED *cparam, const earray_test_param_t UNUSED *tparam,
    hsize_t UNUSED cnt)
{
    eiter_fw_t *eiter;          /* Forward element iteration object */

    /* Allocate space for the element iteration object */
    eiter = (eiter_fw_t *)HDmalloc(sizeof(eiter_fw_t));
    HDassert(eiter);

    /* Initialize the element iteration object */
    eiter->idx = 0;
    eiter->base_sblk_idx = UINT_MAX;

    /* Return iteration object */
    return(eiter);
} /* end eiter_fw_init() */


/*-------------------------------------------------------------------------
 * Function:	eiter_fw_next
 *
 * Purpose:	Get next element index (forward iteration)
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, November  4, 2008
 *
 *-------------------------------------------------------------------------
 */
static hssize_t
eiter_fw_next(void *_eiter)
{
    eiter_fw_t *eiter = (eiter_fw_t *)_eiter;
    hssize_t ret_val;

    /* Sanity check */
    HDassert(eiter);

    /* Get the next array index to test */
    ret_val = (hssize_t)eiter->idx++;

    return(ret_val);
} /* end eiter_fw_next() */


/*-------------------------------------------------------------------------
 * Function:	eiter_fw_max
 *
 * Purpose:	Get max. element index (forward iteration)
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, November  4, 2008
 *
 *-------------------------------------------------------------------------
 */
static hssize_t
eiter_fw_max(const void *_eiter)
{
    const eiter_fw_t *eiter = (const eiter_fw_t *)_eiter;

    /* Sanity check */
    HDassert(eiter);

    /* Return the max. array index used */
    return((hssize_t)(eiter->idx - 1));
} /* end eiter_fw_max() */


/*-------------------------------------------------------------------------
 * Function:	eiter_fw_state
 *
 * Purpose:	Get extensible array state (forward iteration)
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, November  4, 2008
 *
 *-------------------------------------------------------------------------
 */
static int
eiter_fw_state(void *_eiter, const H5EA_create_t *cparam,
    const earray_test_param_t *tparam, earray_state_t *state, hsize_t idx)
{
    eiter_fw_t *eiter = (eiter_fw_t *)_eiter;

    /* Sanity check */
    HDassert(eiter);
    HDassert(cparam);
    HDassert(tparam);
    HDassert(state);

    /* Compute the state of the extensible array */
    state->hdr_size = EA_HDR_SIZE;
    state->nindex_blks = 1;
    state->index_blk_size = EA_IBLOCK_SIZE;
    state->max_idx_set = idx + 1;
    if(idx < cparam->idx_blk_elmts) {
        state->nelmts = (hsize_t)cparam->idx_blk_elmts;
        state->nsuper_blks = state->ndata_blks = (hsize_t)0;
        state->super_blk_size = state->data_blk_size = (hsize_t)0;
    } /* end if */
    else {
        unsigned sblk_idx;      /* Which superblock does this index fall in? */

        /* Compute super block index for element index */
        /* (same eqn. as in H5EA__dblock_sblk_idx()) */
        sblk_idx = H5V_log2_gen((uint64_t)(((idx - cparam->idx_blk_elmts) / cparam->data_blk_min_elmts) + 1));
#ifdef QAK
HDfprintf(stderr, "idx = %Hu, tparam->sblk_info[%u] = {%Zu, %Zu, %Hu, %Hu}\n", idx, sblk_idx, tparam->sblk_info[sblk_idx].ndblks, tparam->sblk_info[sblk_idx].dblk_nelmts, tparam->sblk_info[sblk_idx].start_idx, tparam->sblk_info[sblk_idx].start_dblk);
#endif /* QAK */

        state->nelmts = EA_NELMTS(cparam, tparam, idx, sblk_idx);
#ifdef QAK
HDfprintf(stderr, "state->nelmts = %Hu\n", state->nelmts);
#endif /* QAK */

        state->ndata_blks = EA_NDATA_BLKS(cparam, tparam, idx, sblk_idx);
#ifdef QAK
HDfprintf(stderr, "state->ndata_blks = %Hu\n", state->ndata_blks);
#endif /* QAK */

        /* Check if we have any super blocks yet */
        if(tparam->sblk_info[sblk_idx].ndblks >= cparam->sup_blk_min_data_ptrs) {
            /* Check if this is the first superblock */
            if(sblk_idx < eiter->base_sblk_idx)
                eiter->base_sblk_idx = sblk_idx;

            state->nsuper_blks = (sblk_idx - eiter->base_sblk_idx) + 1;
#ifdef QAK
HDfprintf(stderr, "state->nsuper_blks = %Hu\n", state->nsuper_blks);
#endif /* QAK */
        } /* end if */
        else
            state->nsuper_blks = 0;
    } /* end else */

    return(0);
} /* end eiter_fw_state() */


/*-------------------------------------------------------------------------
 * Function:	eiter_fw_term
 *
 * Purpose:	Shut down element interator (forward iteration)
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:	Quincey Koziol
 *              Thursday, October  2, 2008
 *
 *-------------------------------------------------------------------------
 */
static int
eiter_fw_term(void *eiter)
{
    /* Sanity check */
    HDassert(eiter);

    /* Free iteration object */
    HDfree(eiter);

    return(0);
} /* end eiter_fw_term() */

/* Extensible array iterator class for forward iteration */
static const earray_iter_t ea_iter_fw = {
    eiter_fw_init,              /* Iterator init */
    eiter_fw_next,              /* Next array index */
    eiter_fw_max,               /* Max. array index */
    eiter_fw_state,             /* State of the extensible array */
    eiter_fw_term               /* Iterator term */
};

/* Extensible array iterator info for reverse iteration */
typedef struct eiter_rv_t {
    hsize_t idx;                        /* Index of next array location */
    hsize_t max;                        /* Index of max. array location */
    hsize_t max_sblk_idx;               /* Which superblock does the max. array location fall in? */
    hsize_t max_nelmts;                 /* Max. # of elements for array */
    hsize_t max_ndata_blks;             /* Max. # of data blocks for array */
    hsize_t idx_blk_nsblks;             /* Number of superblocks directly pointed to in the index block */
} eiter_rv_t;


/*-------------------------------------------------------------------------
 * Function:	eiter_rv_init
 *
 * Purpose:	Initialize element interator (reverse iteration)
 *
 * Return:	Success:	Pointer to iteration status object
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, November  4, 2008
 *
 *-------------------------------------------------------------------------
 */
static void *
eiter_rv_init(const H5EA_create_t *cparam, const earray_test_param_t *tparam,
    hsize_t cnt)
{
    eiter_rv_t *eiter;          /* Reverse element iteration object */

    /* Allocate space for the element iteration object */
    eiter = (eiter_rv_t *)HDmalloc(sizeof(eiter_rv_t));
    HDassert(eiter);

    /* Initialize reverse iteration info */
    eiter->idx = cnt - 1;
    eiter->max = cnt - 1;
    if(cnt > cparam->idx_blk_elmts) {
        eiter->max_sblk_idx = H5V_log2_gen((uint64_t)(((eiter->max - cparam->idx_blk_elmts) / cparam->data_blk_min_elmts) + 1));
        eiter->max_nelmts = EA_NELMTS(cparam, tparam, eiter->max, eiter->max_sblk_idx);
        eiter->max_ndata_blks = EA_NDATA_BLKS(cparam, tparam, eiter->max, eiter->max_sblk_idx);
        eiter->idx_blk_nsblks = 2 * H5V_log2_of2((uint32_t)cparam->sup_blk_min_data_ptrs);
    } /* end if */
    else {
        eiter->max_sblk_idx = (hsize_t)0;
        eiter->max_nelmts = (hsize_t)0;
        eiter->max_ndata_blks = (hsize_t)0;
        eiter->idx_blk_nsblks = (hsize_t)0;
    } /* end else */

    /* Return iteration object */
    return(eiter);
} /* end eiter_rv_init() */


/*-------------------------------------------------------------------------
 * Function:	eiter_rv_next
 *
 * Purpose:	Get next element index (reverse iteration)
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, November  4, 2008
 *
 *-------------------------------------------------------------------------
 */
static hssize_t
eiter_rv_next(void *_eiter)
{
    eiter_rv_t *eiter = (eiter_rv_t *)_eiter;
    hssize_t ret_val;

    /* Sanity check */
    HDassert(eiter);

    /* Get the next array index to test */
    ret_val = (hssize_t)eiter->idx--;

    return(ret_val);
} /* end eiter_rv_next() */


/*-------------------------------------------------------------------------
 * Function:	eiter_rv_max
 *
 * Purpose:	Get max. element index (reverse iteration)
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, November  4, 2008
 *
 *-------------------------------------------------------------------------
 */
static hssize_t
eiter_rv_max(const void *_eiter)
{
    const eiter_rv_t *eiter = (const eiter_rv_t *)_eiter;

    /* Sanity check */
    HDassert(eiter);

    /* Return the max. array index used */
    return((hssize_t)eiter->max);
} /* end eiter_rv_max() */


/*-------------------------------------------------------------------------
 * Function:	eiter_rv_state
 *
 * Purpose:	Get extensible array state (reverse iteration)
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, November  4, 2008
 *
 *-------------------------------------------------------------------------
 */
static int
eiter_rv_state(void *_eiter, const H5EA_create_t *cparam,
    const earray_test_param_t *tparam, earray_state_t *state, hsize_t idx)
{
    eiter_rv_t *eiter = (eiter_rv_t *)_eiter;

    /* Sanity check */
    HDassert(eiter);
    HDassert(cparam);
    HDassert(tparam);
    HDassert(state);

    /* Compute the state of the extensible array */
    state->hdr_size = EA_HDR_SIZE;
    state->nindex_blks = 1;
    state->index_blk_size = EA_IBLOCK_SIZE;
    state->max_idx_set = eiter->max + 1;
    if(eiter->max < cparam->idx_blk_elmts) {
        state->nelmts = (hsize_t)cparam->idx_blk_elmts;
        state->nsuper_blks = state->ndata_blks = (hsize_t)0;
    } /* end if */
    else {
        hsize_t idx_nelmts;     /* # of elements for array index */
        hsize_t idx_ndata_blks; /* # of data blocks for array index */
        hsize_t loc_idx = 0;    /* Local index, for computing an offset in next lower data block */
        unsigned idx_sblk_idx;  /* Which superblock does this index fall in? */
        unsigned loc_sblk_idx = 0;  /* Which superblock does the local index fall in? */

        /* Compute super block index for element index */
        /* (same eqn. as in H5EA__dblock_sblk_idx()) */
        if(idx < cparam->idx_blk_elmts + cparam->data_blk_min_elmts)
            idx_sblk_idx = 0;
        else {
            hsize_t tmp_idx;    /* Temporary index in superblock */
            hsize_t dblk_idx;   /* Index of data block within superblock */

            idx_sblk_idx = H5V_log2_gen((uint64_t)(((idx - cparam->idx_blk_elmts) / cparam->data_blk_min_elmts) + 1));
            tmp_idx = idx - (cparam->idx_blk_elmts + tparam->sblk_info[idx_sblk_idx].start_idx);
            dblk_idx = tmp_idx / tparam->sblk_info[idx_sblk_idx].dblk_nelmts;
            if(dblk_idx > 0)
                loc_idx = idx - tparam->sblk_info[idx_sblk_idx].dblk_nelmts;
            else
                loc_idx = cparam->idx_blk_elmts + tparam->sblk_info[idx_sblk_idx].start_idx - 1;
            loc_sblk_idx = H5V_log2_gen((uint64_t)(((loc_idx - cparam->idx_blk_elmts) / cparam->data_blk_min_elmts) + 1));
        } /* end else */
#ifdef QAK
HDfprintf(stderr, "idx = %Hu, loc_idx = %Hu, eiter->max_sblk_idx = %u, idx_sblk_idx = %u, loc_sblk_idx = %u\n", idx, loc_idx, eiter->max_sblk_idx, idx_sblk_idx, loc_sblk_idx);
HDfprintf(stderr, "tparam->sblk_info[%u] = {%Zu, %Zu, %Hu, %Hu}\n", idx_sblk_idx, tparam->sblk_info[idx_sblk_idx].ndblks, tparam->sblk_info[idx_sblk_idx].dblk_nelmts, tparam->sblk_info[idx_sblk_idx].start_idx, tparam->sblk_info[idx_sblk_idx].start_dblk);
HDfprintf(stderr, "tparam->sblk_info[%u] = {%Zu, %Zu, %Hu, %Hu}\n", eiter->max_sblk_idx, tparam->sblk_info[eiter->max_sblk_idx].ndblks, tparam->sblk_info[eiter->max_sblk_idx].dblk_nelmts, tparam->sblk_info[eiter->max_sblk_idx].start_idx, tparam->sblk_info[eiter->max_sblk_idx].start_dblk);
#endif /* QAK */

        if(idx < cparam->idx_blk_elmts + cparam->data_blk_min_elmts)
            idx_nelmts = (hsize_t)cparam->idx_blk_elmts;
        else
            idx_nelmts = EA_NELMTS(cparam, tparam, loc_idx, loc_sblk_idx);
        state->nelmts = (eiter->max_nelmts - idx_nelmts) + cparam->idx_blk_elmts;
#ifdef QAK
HDfprintf(stderr, "eiter->max_nelmts = %Hu, idx_nelmts = %Hu, state->nelmts = %Hu\n", eiter->max_nelmts, idx_nelmts, state->nelmts);
#endif /* QAK */

        if(idx < cparam->idx_blk_elmts + cparam->data_blk_min_elmts)
            idx_ndata_blks = 0;
        else
            idx_ndata_blks = EA_NDATA_BLKS(cparam, tparam, loc_idx, loc_sblk_idx);
        state->ndata_blks = eiter->max_ndata_blks - idx_ndata_blks;
#ifdef QAK
HDfprintf(stderr, "eiter->max_ndata_blks = %Hu, idx_ndata_blks = %Hu, state->ndata_blks = %Hu\n", eiter->max_ndata_blks, idx_ndata_blks, state->ndata_blks);
#endif /* QAK */

        /* Check if we have any super blocks yet */
        if(tparam->sblk_info[eiter->max_sblk_idx].ndblks >= cparam->sup_blk_min_data_ptrs) {
            if(idx_sblk_idx > eiter->idx_blk_nsblks)
                state->nsuper_blks = (eiter->max_sblk_idx - idx_sblk_idx) + 1;
            else
                state->nsuper_blks = (eiter->max_sblk_idx - eiter->idx_blk_nsblks) + 1;
#ifdef QAK
HDfprintf(stderr, "eiter->idx_blk_nsblks = %Hu, state->nsuper_blks = %Hu\n", eiter->idx_blk_nsblks, state->nsuper_blks);
#endif /* QAK */
        } /* end if */
    } /* end else */

    return(0);
} /* end eiter_rv_state() */


/*-------------------------------------------------------------------------
 * Function:	eiter_rv_term
 *
 * Purpose:	Shut down element interator (reverse iteration)
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, November  4, 2008
 *
 *-------------------------------------------------------------------------
 */
static int
eiter_rv_term(void *eiter)
{
    /* Sanity check */
    HDassert(eiter);

    /* Free iteration object */
    HDfree(eiter);

    return(0);
} /* end eiter_rv_term() */

/* Extensible array iterator class for reverse iteration */
static const earray_iter_t ea_iter_rv = {
    eiter_rv_init,              /* Iterator init */
    eiter_rv_next,              /* Next array index */
    eiter_rv_max,               /* Max. array index written */
    eiter_rv_state,             /* State of the extensible array */
    eiter_rv_term               /* Iterator term */
};

/* Extensible array iterator info for random iteration */
typedef struct eiter_rnd_t {
    hsize_t max;                /* Max. array index used */
    hsize_t pos;                /* Position in shuffled array */
    hsize_t *idx;               /* Array of shuffled indices */
} eiter_rnd_t;


/*-------------------------------------------------------------------------
 * Function:	eiter_rnd_init
 *
 * Purpose:	Initialize element interator (random iteration)
 *
 * Return:	Success:	Pointer to iteration status object
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *              Thursday, November  6, 2008
 *
 *-------------------------------------------------------------------------
 */
static void *
eiter_rnd_init(const H5EA_create_t UNUSED *cparam, const earray_test_param_t UNUSED *tparam,
    hsize_t cnt)
{
    eiter_rnd_t *eiter;         /* Random element iteration object */
    size_t u;                   /* Local index variable */

    /* Allocate space for the element iteration object */
    eiter = (eiter_rnd_t *)HDmalloc(sizeof(eiter_rnd_t));
    HDassert(eiter);

    /* Allocate space for the array of shuffled indices */
    eiter->idx = (hsize_t *)HDmalloc(sizeof(hsize_t) * (size_t)cnt);
    HDassert(eiter->idx);

    /* Initialize reverse iteration info */
    eiter->max = 0;
    eiter->pos = 0;
    for(u = 0; u < (size_t)cnt; u++)
        eiter->idx[u] = (hsize_t)u;

    /* Randomly shuffle array indices */
    if(cnt > 1) {
        for(u = 0; u < (size_t)cnt; u++) {
            size_t swap_idx;            /* Location to swap with when shuffling */
            hsize_t temp_idx;           /* Temporary index */

            swap_idx = ((size_t)HDrandom() % ((size_t)cnt - u)) + u;
            temp_idx = eiter->idx[u];
            eiter->idx[u] = eiter->idx[swap_idx];
            eiter->idx[swap_idx] = temp_idx;
        } /* end for */
    } /* end if */

    /* Return iteration object */
    return(eiter);
} /* end eiter_rnd_init() */


/*-------------------------------------------------------------------------
 * Function:	eiter_rnd_next
 *
 * Purpose:	Get next element index (random iteration)
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *              Thursday, November  6, 2008
 *
 *-------------------------------------------------------------------------
 */
static hssize_t
eiter_rnd_next(void *_eiter)
{
    eiter_rnd_t *eiter = (eiter_rnd_t *)_eiter;
    hssize_t ret_val;

    /* Sanity check */
    HDassert(eiter);

    /* Get the next array index to test */
    ret_val = (hssize_t)eiter->idx[eiter->pos];
    eiter->pos++;

    /* Check for new max. value */
    if((hsize_t)ret_val > eiter->max)
        eiter->max = (hsize_t)ret_val;

    return(ret_val);
} /* end eiter_rnd_next() */


/*-------------------------------------------------------------------------
 * Function:	eiter_rnd_max
 *
 * Purpose:	Get max. element index (random iteration)
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, November  6, 2008
 *
 *-------------------------------------------------------------------------
 */
static hssize_t
eiter_rnd_max(const void *_eiter)
{
    const eiter_rnd_t *eiter = (const eiter_rnd_t *)_eiter;

    /* Sanity check */
    HDassert(eiter);

    /* Return the max. array index used */
    return((hssize_t)eiter->max);
} /* end eiter_rnd_max() */


/*-------------------------------------------------------------------------
 * Function:	eiter_rnd_term
 *
 * Purpose:	Shut down element interator (random iteration)
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, November  6, 2008
 *
 *-------------------------------------------------------------------------
 */
static int
eiter_rnd_term(void *_eiter)
{
    eiter_rnd_t *eiter = (eiter_rnd_t *)_eiter;

    /* Sanity check */
    HDassert(eiter);
    HDassert(eiter->idx);

    /* Free shuffled index array */
    HDfree(eiter->idx);

    /* Free iteration object */
    HDfree(eiter);

    return(0);
} /* end eiter_rnd_term() */

/* Extensible array iterator class for random iteration */
static const earray_iter_t ea_iter_rnd = {
    eiter_rnd_init,             /* Iterator init */
    eiter_rnd_next,             /* Next array index */
    eiter_rnd_max,              /* Max. array index written */
    NULL,                       /* State of the extensible array */
    eiter_rnd_term              /* Iterator term */
};


/*-------------------------------------------------------------------------
 * Function:	eiter_rnd2_init
 *
 * Purpose:	Initialize element interator (random #2 iteration)
 *
 * Return:	Success:	Pointer to iteration status object
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *              Thursday, November 11, 2008
 *
 *-------------------------------------------------------------------------
 */
static void *
eiter_rnd2_init(const H5EA_create_t UNUSED *cparam, const earray_test_param_t UNUSED *tparam,
    hsize_t cnt)
{
    eiter_rnd_t *eiter;        /* Random element iteration object */
    size_t u;                  /* Local index variable */

    /* Allocate space for the element iteration object */
    eiter = (eiter_rnd_t *)HDmalloc(sizeof(eiter_rnd_t));
    HDassert(eiter);

    /* Allocate space for the array of shuffled indices */
    eiter->idx = (hsize_t *)HDmalloc(sizeof(hsize_t) * (size_t)cnt);
    HDassert(eiter->idx);

    /* Initialize reverse iteration info */
    eiter->max = 0;
    eiter->pos = 0;

    /* Randomly shuffle array indices */
    if(cnt > 1) {
        hsize_t *tmp_idx;           /* Temporary index array */
        hsize_t sparse_cnt = (hsize_t)(cnt * EA_RND2_SCALE);         /* Sparse range to choose from */

        /* Allocate temporary index array */
        tmp_idx = (hsize_t *)HDmalloc(sizeof(hsize_t) * (size_t)sparse_cnt);
        HDassert(tmp_idx);

        /* Initialize temporary index array, for shuffling */
        for(u = 0; u < (size_t)sparse_cnt; u++)
            tmp_idx[u] = (hsize_t)u;

        /* Shuffle index elements & store in final array */
        for(u = 0; u < (size_t)cnt; u++) {
            size_t swap_idx;            /* Location to swap with when shuffling */

            swap_idx = ((size_t)HDrandom() % ((size_t)sparse_cnt - u)) + u;
            eiter->idx[u] = tmp_idx[swap_idx];
            tmp_idx[swap_idx] = tmp_idx[u];
        } /* end for */

        /* Release temporary array */
        HDfree(tmp_idx);
    } /* end if */
    else {
        for(u = 0; u < (size_t)cnt; u++)
            eiter->idx[u] = (hsize_t)u;
    } /* end else */

    /* Return iteration object */
    return(eiter);
} /* end eiter_rnd2_init() */

/* Extensible array iterator class for random iteration */
static const earray_iter_t ea_iter_rnd2 = {
    eiter_rnd2_init,            /* Iterator init */
    eiter_rnd_next,             /* Next array index */
    eiter_rnd_max,              /* Max. array index written */
    NULL,                       /* State of the extensible array */
    eiter_rnd_term              /* Iterator term */
};

/* Extensible array iterator info for cyclic iteration */
typedef struct eiter_cyc_t {
    hsize_t max;                /* Max. array index used */
    hsize_t pos;                /* Position in shuffled array */
    hsize_t cnt;                /* # of elements to store */
    hsize_t cyc;                /* Cycle of elements to choose from */
} eiter_cyc_t;


/*-------------------------------------------------------------------------
 * Function:	eiter_cyc_init
 *
 * Purpose:	Initialize element interator (cyclic iteration)
 *
 * Return:	Success:	Pointer to iteration status object
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, November 11, 2008
 *
 *-------------------------------------------------------------------------
 */
static void *
eiter_cyc_init(const H5EA_create_t UNUSED *cparam, const earray_test_param_t UNUSED *tparam,
    hsize_t cnt)
{
    eiter_cyc_t *eiter;         /* Cyclic element iteration object */

    /* Allocate space for the element iteration object */
    eiter = (eiter_cyc_t *)HDmalloc(sizeof(eiter_cyc_t));
    HDassert(eiter);

    /* Initialize reverse iteration info */
    eiter->max = 0;
    eiter->pos = 0;
    eiter->cnt = cnt;
    eiter->cyc = 0;

    /* Return iteration object */
    return(eiter);
} /* end eiter_cyc_init() */


/*-------------------------------------------------------------------------
 * Function:	eiter_cyc_next
 *
 * Purpose:	Get next element index (cyclic iteration)
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, November 11, 2008
 *
 *-------------------------------------------------------------------------
 */
static hssize_t
eiter_cyc_next(void *_eiter)
{
    eiter_cyc_t *eiter = (eiter_cyc_t *)_eiter;
    hssize_t ret_val;

    /* Sanity check */
    HDassert(eiter);

    /* Get the next array index to test */
    ret_val = (hssize_t)eiter->pos;
    eiter->pos += EA_CYC_COUNT;
    if(eiter->pos >= eiter->cnt)
        eiter->pos = ++eiter->cyc;

    /* Check for new max. value */
    if((hsize_t)ret_val > eiter->max)
        eiter->max = (hsize_t)ret_val;

    return(ret_val);
} /* end eiter_cyc_next() */


/*-------------------------------------------------------------------------
 * Function:	eiter_cyc_max
 *
 * Purpose:	Get max. element index (cyclic iteration)
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, November 11, 2008
 *
 *-------------------------------------------------------------------------
 */
static hssize_t
eiter_cyc_max(const void *_eiter)
{
    const eiter_cyc_t *eiter = (const eiter_cyc_t *)_eiter;

    /* Sanity check */
    HDassert(eiter);

    /* Return the max. array index used */
    return((hssize_t)eiter->max);
} /* end eiter_cyc_max() */


/*-------------------------------------------------------------------------
 * Function:	eiter_cyc_term
 *
 * Purpose:	Shut down element interator (cyclic iteration)
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, November 11, 2008
 *
 *-------------------------------------------------------------------------
 */
static int
eiter_cyc_term(void *_eiter)
{
    eiter_cyc_t *eiter = (eiter_cyc_t *)_eiter;

    /* Sanity check */
    HDassert(eiter);

    /* Free iteration object */
    HDfree(eiter);

    return(0);
} /* end eiter_cyc_term() */

/* Extensible array iterator class for cyclic iteration */
static const earray_iter_t ea_iter_cyc = {
    eiter_cyc_init,             /* Iterator init */
    eiter_cyc_next,             /* Next array index */
    eiter_cyc_max,              /* Max. array index written */
    NULL,                       /* State of the extensible array */
    eiter_cyc_term              /* Iterator term */
};


/*-------------------------------------------------------------------------
 * Function:	test_set_elmts
 *
 * Purpose:	Set all elements from 0 through 'nelmts' in extensible array
 *
 * Return:	Success:	0
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Thursday, September 22, 2008
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_set_elmts(hid_t fapl, H5EA_create_t *cparam, earray_test_param_t *tparam,
    hsize_t nelmts, const char *test_str)
{
    hid_t	file = -1;              /* File ID */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5EA_t      *ea = NULL;             /* Extensible array wrapper */
    void        *eiter_info;            /* Extensible array iterator info */
    earray_state_t state;               /* State of extensible array */
    uint64_t    welmt;                  /* Element to write */
    uint64_t    relmt;                  /* Element to read */
    hsize_t     nelmts_written;         /* Highest element written in array */
    hsize_t     cnt;                    /* Count of array indices */
    hssize_t    smax;                   /* Index value of max. element set */
    hsize_t     max;                    /* Index value of max. element set */
    hssize_t    sidx;                   /* Index value of first element of first data block */
    hsize_t     idx;                    /* Index value of first element of first data block */
    haddr_t     ea_addr = HADDR_UNDEF;  /* Array address in file */

    /*
     * Display testing message
     */
    TESTING(test_str);

    /* Create file & retrieve pointer to internal file object */
    if(create_file(H5F_ACC_TRUNC, fapl, &file, &f) < 0)
        TEST_ERROR

    /* Create array */
    if(create_array(f, H5P_DATASET_XFER_DEFAULT, cparam, &ea, &ea_addr, NULL) < 0)
        TEST_ERROR

    /* Verify the creation parameters */
    if(verify_cparam(ea, cparam) < 0)
        TEST_ERROR

    /* Check for closing & re-opening the file */
    if(reopen_file(&file, &f, fapl, H5P_DATASET_XFER_DEFAULT, &ea, ea_addr, tparam) < 0)
        TEST_ERROR

    /* Verify high-water # of elements written */
    nelmts_written = (hsize_t)ULLONG_MAX;
    if(H5EA_get_nelmts(ea, &nelmts_written) < 0)
        FAIL_STACK_ERROR
    if(nelmts_written != 0)
        TEST_ERROR

    /* Verify array state */
    HDmemset(&state, 0, sizeof(state));
    state.hdr_size = EA_HDR_SIZE;
    if(check_stats(ea, &state))
        TEST_ERROR

    /* Get all elements from empty array */

    /* Initialize iterator */
    if(NULL == (eiter_info = tparam->eiter->init(cparam, tparam, nelmts)))
        TEST_ERROR

    /* Get elements of array */
    for(cnt = 0; cnt < nelmts; cnt++) {
        /* Get the array index */
        if((sidx = tparam->eiter->next(eiter_info)) < 0)
            TEST_ERROR
        idx = (hsize_t)sidx;

        /* Retrieve element of array (not set yet) */
        relmt = (uint64_t)0;
        if(H5EA_get(ea, H5P_DATASET_XFER_DEFAULT, idx, &relmt) < 0)
            FAIL_STACK_ERROR

        /* Verify element is fill value for array */
        if(relmt != H5EA_TEST_FILL)
            TEST_ERROR
    } /* end for */

    /* Shutdown iterator */
    if(tparam->eiter->term(eiter_info) < 0)
        TEST_ERROR


    /* Set (& get) all elements from empty array */

    /* Initialize iterator */
    if(NULL == (eiter_info = tparam->eiter->init(cparam, tparam, nelmts)))
        TEST_ERROR

    /* Set elements of array */
    for(cnt = 0; cnt < nelmts; cnt++) {
        /* Get the array index */
        if((sidx = tparam->eiter->next(eiter_info)) < 0)
            TEST_ERROR
        idx = (hsize_t)sidx;

        /* Retrieve element of array (not set yet) */
        relmt = (uint64_t)0;
        if(H5EA_get(ea, H5P_DATASET_XFER_DEFAULT, idx, &relmt) < 0)
            FAIL_STACK_ERROR

        /* Verify element is fill value for array */
        if(relmt != H5EA_TEST_FILL)
            TEST_ERROR

        /* Set element of array */
        welmt = (uint64_t)7 + idx;
        if(H5EA_set(ea, H5P_DATASET_XFER_DEFAULT, idx, &welmt) < 0)
            FAIL_STACK_ERROR

        /* Get the max. array index */
        if((smax = tparam->eiter->max_elem(eiter_info)) < 0)
            TEST_ERROR
        max = (hsize_t)smax;

        /* Verify high-water # of elements written */
        nelmts_written = (hsize_t)ULLONG_MAX;
        if(H5EA_get_nelmts(ea, &nelmts_written) < 0)
            FAIL_STACK_ERROR
        if(nelmts_written != (max + 1))
            TEST_ERROR

        /* Check if array state is available */
        if(tparam->eiter->state) {
            /* Get the extensible array state */
            if(tparam->eiter->state(eiter_info, cparam, tparam, &state, idx) < 0)
                TEST_ERROR

            /* Verify array state */
            if(check_stats(ea, &state))
                TEST_ERROR
        } /* end if */

        /* Retrieve element of array (set now) */
        relmt = (uint64_t)0;
        if(H5EA_get(ea, H5P_DATASET_XFER_DEFAULT, idx, &relmt) < 0)
            FAIL_STACK_ERROR

        /* Verify element is value written */
        if(relmt != welmt)
            TEST_ERROR
    } /* end for */

    /* Shutdown iterator */
    if(tparam->eiter->term(eiter_info) < 0)
        TEST_ERROR

    /* Close array, delete array, close file & verify file is empty */
    if(finish(file, fapl, f, ea, ea_addr) < 0)
        TEST_ERROR

    /* All tests passed */
    PASSED()

    return 0;

error:
    H5E_BEGIN_TRY {
        if(ea)
            H5EA_close(ea, H5P_DATASET_XFER_DEFAULT);
	H5Fclose(file);
    } H5E_END_TRY;

    return 1;
} /* test_set_elmts() */


/*-------------------------------------------------------------------------
 * Function:	test_skip_elmts
 *
 * Purpose:	Skip some elements when writing element
 *
 * Return:	Success:	0
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, November 11, 2008
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_skip_elmts(hid_t fapl, H5EA_create_t *cparam, earray_test_param_t *tparam,
    hsize_t skip_elmts, const char *test_str)
{
    hid_t	file = -1;              /* File ID */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5EA_t      *ea = NULL;             /* Extensible array wrapper */
    earray_state_t state;               /* State of extensible array */
    uint64_t    welmt;                  /* Element to write */
    uint64_t    relmt;                  /* Element to read */
    hsize_t     nelmts_written;         /* Highest element written in array */
    hsize_t     idx;                    /* Index value of element to get */
    hsize_t     cnt;                    /* Count of array indices */
    haddr_t     ea_addr = HADDR_UNDEF;  /* Array address in file */

    /*
     * Display testing message
     */
    TESTING(test_str);

    /* Create file & retrieve pointer to internal file object */
    if(create_file(H5F_ACC_TRUNC, fapl, &file, &f) < 0)
        TEST_ERROR

    /* Create array */
    if(create_array(f, H5P_DATASET_XFER_DEFAULT, cparam, &ea, &ea_addr, NULL) < 0)
        TEST_ERROR

    /* Verify the creation parameters */
    if(verify_cparam(ea, cparam) < 0)
        TEST_ERROR

    /* Check for closing & re-opening the file */
    if(reopen_file(&file, &f, fapl, H5P_DATASET_XFER_DEFAULT, &ea, ea_addr, tparam) < 0)
        TEST_ERROR

    /* Verify high-water # of elements written */
    nelmts_written = (hsize_t)ULLONG_MAX;
    if(H5EA_get_nelmts(ea, &nelmts_written) < 0)
        FAIL_STACK_ERROR
    if(nelmts_written != 0)
        TEST_ERROR

    /* Verify array state */
    HDmemset(&state, 0, sizeof(state));
    state.hdr_size = EA_HDR_SIZE;
    if(check_stats(ea, &state))
        TEST_ERROR

    /* Set (& get) element after skipping elements */
    idx = skip_elmts;

    /* Retrieve element of array (not set yet) */
    relmt = (uint64_t)0;
    if(H5EA_get(ea, H5P_DATASET_XFER_DEFAULT, idx, &relmt) < 0)
        FAIL_STACK_ERROR

    /* Verify element is fill value for array */
    if(relmt != H5EA_TEST_FILL)
        TEST_ERROR

    /* Set element of array */
    welmt = (uint64_t)7 + idx;
    if(H5EA_set(ea, H5P_DATASET_XFER_DEFAULT, idx, &welmt) < 0)
        FAIL_STACK_ERROR

    /* Verify high-water # of elements written */
    nelmts_written = (hsize_t)ULLONG_MAX;
    if(H5EA_get_nelmts(ea, &nelmts_written) < 0)
        FAIL_STACK_ERROR
    if(nelmts_written != (idx + 1))
        TEST_ERROR

    /* Set array state */
    HDmemset(&state, 0, sizeof(state));
    state.hdr_size = EA_HDR_SIZE;
    state.nindex_blks = 1;
    state.index_blk_size = EA_IBLOCK_SIZE;
    state.max_idx_set = idx + 1;
    if(1 == skip_elmts) {
        state.nelmts = (hsize_t)cparam->idx_blk_elmts;
        state.nsuper_blks = state.ndata_blks = (hsize_t)0;
    } /* end if */
    else if(cparam->idx_blk_elmts == skip_elmts) {
        state.nelmts = (hsize_t)cparam->idx_blk_elmts + cparam->data_blk_min_elmts;
        state.ndata_blks = (hsize_t)1;
        state.nsuper_blks = (hsize_t)0;
    } /* end if */
    else {
        unsigned sblk_idx;      /* Which superblock does this index fall in? */

        /* Compute super block index for element index */
        /* (same eqn. as in H5EA__dblock_sblk_idx()) */
        sblk_idx = H5V_log2_gen((uint64_t)(((idx - cparam->idx_blk_elmts) / cparam->data_blk_min_elmts) + 1));
        state.nelmts = (hsize_t)cparam->idx_blk_elmts + tparam->sblk_info[sblk_idx].dblk_nelmts;
        state.ndata_blks = (hsize_t)1;
        state.nsuper_blks = (hsize_t)1;
    } /* end if */

    /* Verify array state */
    if(check_stats(ea, &state))
        TEST_ERROR

    /* Retrieve element of array (set now) */
    relmt = (uint64_t)0;
    if(H5EA_get(ea, H5P_DATASET_XFER_DEFAULT, idx, &relmt) < 0)
        FAIL_STACK_ERROR

    /* Verify element is value written */
    if(relmt != welmt)
        TEST_ERROR

    /* Get unset elements of array */
    for(cnt = 0; cnt < skip_elmts; cnt++) {
        /* Retrieve element of array (not set yet) */
        relmt = (uint64_t)0;
        if(H5EA_get(ea, H5P_DATASET_XFER_DEFAULT, cnt, &relmt) < 0)
            FAIL_STACK_ERROR

        /* Verify element is fill value for array */
        if(relmt != H5EA_TEST_FILL)
            TEST_ERROR
    } /* end for */

    /* Close array, delete array, close file & verify file is empty */
    if(finish(file, fapl, f, ea, ea_addr) < 0)
        TEST_ERROR

    /* All tests passed */
    PASSED()

    return 0;

error:
    H5E_BEGIN_TRY {
        if(ea)
            H5EA_close(ea, H5P_DATASET_XFER_DEFAULT);
	H5Fclose(file);
    } H5E_END_TRY;

    return 1;
} /* test_skip_elmts() */


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Test the extensible array code
 *
 * Return:	Success: 0
 *		Failure: 1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, June 17, 2008
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    H5EA_create_t cparam;               /* Creation parameters for extensible array */
    earray_test_param_t tparam;         /* Testing parameters */
    earray_test_type_t curr_test;       /* Current test being worked on */
    earray_iter_type_t curr_iter;       /* Current iteration type being worked on */
    hid_t	fapl = -1;              /* File access property list for data files */
    unsigned	nerrors = 0;            /* Cumulative error count */
    time_t      curr_time;              /* Current time, for seeding random number generator */
    int		ExpressMode;            /* Test express value */
    const char  *env_h5_drvr;      /* File Driver value from environment */

    env_h5_drvr = HDgetenv("HDF5_DRIVER");
    if(env_h5_drvr == NULL)
        env_h5_drvr = "nomatch";

    /* Reset library */
    h5_reset();
    fapl = h5_fileaccess();
    ExpressMode = GetTestExpress();
    if(ExpressMode > 1)
	printf("***Express test mode on.  Some tests may be skipped\n");

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename_g, sizeof(filename_g));

    /* Seed random #'s */
    curr_time = HDtime(NULL);
    HDsrandom((unsigned long)curr_time);

    /* Create an empty file to retrieve size */
    {
        hid_t	file;              /* File ID */

        if((file = H5Fcreate(filename_g, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Close file */
        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of a file w/no array */
        if((empty_size_g = h5_get_file_size(filename_g, fapl)) < 0)
            TEST_ERROR
    }


    /* Initialize extensible array creation parameters */
    init_cparam(&cparam);

    /* Iterate over the testing parameters */
    for(curr_test = EARRAY_TEST_NORMAL; curr_test < EARRAY_TEST_NTESTS; curr_test++) {

        /* Initialize the testing parameters */
        init_tparam(&tparam, &cparam);

        /* Set appropriate testing parameters for each test */
        switch(curr_test) {
            /* "Normal" testing parameters */
            case EARRAY_TEST_NORMAL:
                puts("Testing with normal parameters");
                break;

            /* "Re-open array" testing parameters */
            case EARRAY_TEST_REOPEN:
                puts("Testing with reopen array flag set");
                tparam.reopen_array = EARRAY_TEST_REOPEN;
                break;

            /* An unknown test? */
            case EARRAY_TEST_NTESTS:
            default:
                goto error;
        } /* end switch */

        /* Basic capability tests */
        nerrors += test_create(fapl, &cparam, &tparam);
        nerrors += test_reopen(fapl, &cparam, &tparam);
        nerrors += test_open_twice(fapl, &cparam, &tparam);
        nerrors += test_delete_open(fapl, &cparam, &tparam);
        nerrors += test_flush_depend(env_h5_drvr, fapl, &cparam, &tparam);

        /* Iterate over the type of capacity tests */
        for(curr_iter = EARRAY_ITER_FW; curr_iter < EARRAY_ITER_NITERS; curr_iter++) {
            hsize_t sblk;               /* Super block index */
            hsize_t dblk;               /* Data block index */
            hsize_t nelmts;             /* # of elements to test */
            char test_str[128];         /* String for describing test */
            hsize_t ndblks;             /* # of data blocks tested */

            /* Set appropriate parameters for each type of iteration */
            switch(curr_iter) {
                /* "Forward" testing parameters */
                case EARRAY_ITER_FW:
                    puts("Testing with forward iteration");
                    tparam.eiter = &ea_iter_fw;
                    break;

                /* "Reverse" testing parameters */
                case EARRAY_ITER_RV:
                    puts("Testing with reverse iteration");
                    tparam.eiter = &ea_iter_rv;
                    break;

                /* "Random" testing parameters */
                case EARRAY_ITER_RND:
                    puts("Testing with random iteration");
                    tparam.eiter = &ea_iter_rnd;
                    break;

                /* "Random #2" testing parameters */
                case EARRAY_ITER_RND2:
                    puts("Testing with random #2 iteration");
                    tparam.eiter = &ea_iter_rnd2;
                    break;

                /* "Cyclic" testing parameters */
                case EARRAY_ITER_CYC:
                    puts("Testing with cyclic iteration");
                    tparam.eiter = &ea_iter_cyc;
                    break;

                /* An unknown iteration? */
                case EARRAY_ITER_NITERS:
                default:
                    goto error;
            } /* end switch */

            /* Basic capacity tests */
            nerrors += test_set_elmts(fapl, &cparam, &tparam, (hsize_t)1, "setting first element of array");
            nerrors += test_set_elmts(fapl, &cparam, &tparam, (hsize_t)cparam.idx_blk_elmts, "setting index block elements of array");

            /* Super Block capacity tests */
            ndblks = 0;
            for(sblk = 0; sblk < 9; sblk++) {
                for(dblk = 0; dblk < tparam.sblk_info[sblk].ndblks; dblk ++) {
                    /* Test first element in data block */
                    nelmts = (hsize_t)((hsize_t)1 + cparam.idx_blk_elmts +
                        tparam.sblk_info[sblk].start_idx +
                        (tparam.sblk_info[sblk].dblk_nelmts * dblk));
                    sprintf(test_str, "setting first element of array's data block #%llu", (unsigned long long)ndblks);
                    nerrors += test_set_elmts(fapl, &cparam, &tparam, nelmts, test_str);

                    /* Test all elements in data block */
                    nelmts = (hsize_t)(cparam.idx_blk_elmts +
                        tparam.sblk_info[sblk].start_idx +
                        (tparam.sblk_info[sblk].dblk_nelmts * (dblk + 1)));
                    sprintf(test_str, "setting all elements of array's data block #%llu", (unsigned long long)ndblks);
                    nerrors += test_set_elmts(fapl, &cparam, &tparam, nelmts, test_str);

                    /* Increment data block being tested */
                    ndblks++;
                } /* end for */
            } /* end for */
        } /* end for */

        /* Check skipping elements */
        nerrors += test_skip_elmts(fapl, &cparam, &tparam, (hsize_t)1, "skipping 1st element");
        nerrors += test_skip_elmts(fapl, &cparam, &tparam, (hsize_t)cparam.idx_blk_elmts, "skipping index block elements");
        nerrors += test_skip_elmts(fapl, &cparam, &tparam, (hsize_t)(cparam.idx_blk_elmts + (15 * cparam.data_blk_min_elmts) + 1), "skipping index block & data block elements");
        nerrors += test_skip_elmts(fapl, &cparam, &tparam, (hsize_t)(cparam.idx_blk_elmts + (31 * cparam.data_blk_min_elmts) + 1), "skipping 1st super block elements");
        nerrors += test_skip_elmts(fapl, &cparam, &tparam, (hsize_t)(cparam.idx_blk_elmts + (63 * cparam.data_blk_min_elmts) + 1), "skipping 2nd super block elements");
        nerrors += test_skip_elmts(fapl, &cparam, &tparam, (hsize_t)(cparam.idx_blk_elmts + (127 * cparam.data_blk_min_elmts) + 1), "skipping 3rd super block elements");
        nerrors += test_skip_elmts(fapl, &cparam, &tparam, (hsize_t)(cparam.idx_blk_elmts + (255 * cparam.data_blk_min_elmts) + 1), "skipping 4th super block elements");

        /* Close down testing parameters */
        finish_tparam(&tparam);
    } /* end for */

    /* Verify symbol table messages are cached */
    nerrors += (h5_verify_cached_stabs(FILENAME, fapl) < 0 ? 1 : 0);

    if(nerrors)
        goto error;
    puts("All extensible array tests passed.");

    /* Clean up file used */
    h5_cleanup(FILENAME, fapl);

    return 0;

error:
    puts("*** TESTS FAILED ***");

    H5E_BEGIN_TRY {
	H5Pclose(fapl);
    } H5E_END_TRY;

    return 1;
} /* end main() */

