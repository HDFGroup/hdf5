/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* Programmer:  Quincey Koziol
 *              Tuesday, June 17, 2008
 */
#include "h5test.h"

/*
 * This file needs to access private datatypes from the H5EA package.
 * This file also needs to access the extensible array testing code.
 */
#define H5EA_FRIEND /*suppress error about including H5EApkg      */
#define H5EA_TESTING
#include "H5EApkg.h" /* Extensible Arrays            */

/* Other private headers that this test requires */
#include "H5CXprivate.h" /* API Contexts                         */
#include "H5Iprivate.h"  /* IDs                      */
#include "H5VLprivate.h" /* Virtual Object Layer                     */
#include "H5VMprivate.h" /* Vectors and arrays             */

/* Local macros */

/* Max. testfile name length */
#define EARRAY_FILENAME_LEN 1024

/* Extensible array creation values */
#define ELMT_SIZE                   sizeof(uint64_t)
#define MAX_NELMTS_BITS             32 /* i.e. 4 giga-elements */
#define IDX_BLK_ELMTS               4
#define SUP_BLK_MIN_DATA_PTRS       4
#define DATA_BLK_MIN_ELMTS          16
#define MAX_DBLOCK_PAGE_NELMTS_BITS 10 /* i.e. 1024 elements per data block page */

/* Convenience macros for computing earray state */
#define EA_HDR_SIZE    72  /* (hard-coded, current size) */
#define EA_IBLOCK_SIZE 298 /* (hard-coded, current size) */
#define EA_NELMTS(cparam, tparam, idx, sblk_idx)                                                             \
    (hsize_t)(cparam->idx_blk_elmts + tparam->sblk_info[sblk_idx].start_idx +                                \
              ((1 + ((idx - (cparam->idx_blk_elmts + tparam->sblk_info[sblk_idx].start_idx)) /               \
                     tparam->sblk_info[sblk_idx].dblk_nelmts)) *                                             \
               tparam->sblk_info[sblk_idx].dblk_nelmts))
#define EA_NDATA_BLKS(cparam, tparam, idx, sblk_idx)                                                         \
    (1 + tparam->sblk_info[sblk_idx].start_dblk +                                                            \
     ((idx - (cparam->idx_blk_elmts + tparam->sblk_info[sblk_idx].start_idx)) /                              \
      tparam->sblk_info[sblk_idx].dblk_nelmts))

/* Iterator parameter values */
#define EA_RND2_SCALE 100
#define EA_CYC_COUNT  4

/* Local typedefs */

/* Types of tests to perform */
typedef enum {
    EARRAY_TEST_NORMAL, /* "Normal" test, with no testing parameters set */
    EARRAY_TEST_REOPEN, /* Set the reopen_array flag */
    EARRAY_TEST_NTESTS  /* The number of test types, must be last */
} earray_test_type_t;

/* Types of iteration to perform */
typedef enum {
    EARRAY_ITER_FW,    /* "Forward" iteration */
    EARRAY_ITER_RV,    /* "Reverse" iteration */
    EARRAY_ITER_RND,   /* "Random" iteration */
    EARRAY_ITER_CYC,   /* "Cyclic" iteration */
    EARRAY_ITER_RND2,  /* "Random #2" iteration */
    EARRAY_ITER_NITERS /* The number of iteration types, must be last */
} earray_iter_type_t;

/* Orders to operate on entries */
typedef enum {
    EARRAY_DIR_FORWARD, /* Insert objects from 0 -> nobjs */
    EARRAY_DIR_RANDOM,  /* Insert objects randomly from 0 - nobjs */
    EARRAY_DIR_CYCLIC,  /* Insert every n'th object cyclicly: 0, n, 2n, 3n, ..., nobjs/n, 1+nobjs/n,
                           1+n+nobjs/n, 1+2n+nobjs/n, ..., nobjs */
    EARRAY_DIR_REVERSE, /* Insert objects from nobjs -> 0 */
    EARRAY_DIR_INWARD, /* Insert objects from outside to in: 0, nobjs, 1, nobjs-1, 2, nobjs-2, ..., nobjs/2 */
    EARRAY_DIR_OUTWARD, /* Insert objects from inside to out: nobjs/2, (nobjs/2)-1, (nobjs/2)+1, ..., 0, nobjs
                         */
    EARRAY_DIR_NDIRS    /* The number of different insertion orders, must be last */
} earray_test_dir_t;

/* Whether to compress data blocks */
typedef enum {
    EARRAY_TEST_NO_COMPRESS, /* Don't compress data blocks */
    EARRAY_TEST_COMPRESS,    /* Compress data blocks */
    EARRAY_TEST_COMP_N       /* The number of different ways to test compressing array blocks, must be last */
} earray_test_comp_t;

/* Extensible array state information */
typedef struct earray_state_t {
    hsize_t hdr_size;       /* Size of header */
    hsize_t nindex_blks;    /* # of index blocks */
    hsize_t index_blk_size; /* Size of index blocks */
    hsize_t nsuper_blks;    /* # of super blocks */
    hsize_t super_blk_size; /* Size of super blocks */
    hsize_t ndata_blks;     /* # of data blocks */
    hsize_t data_blk_size;  /* Size of data blocks */
    hsize_t max_idx_set; /* Highest element index stored (+1 - i.e. if element 0 has been set, this value with
                            be '1', if no elements have been stored, this value will be '0') */
    hsize_t nelmts;      /* # of elements "realized" */
} earray_state_t;

/* Forward decl. */
typedef struct earray_test_param_t earray_test_param_t;

/* Extensible array iterator class */
typedef struct earray_iter_t {
    void *(*init)(const H5EA_create_t *cparam, const earray_test_param_t *tparam,
                  hsize_t cnt);             /* Initialize/allocate iterator private info */
    hssize_t (*next)(void *info);           /* Get the next element to test */
    hssize_t (*max_elem)(const void *info); /* Get the max. element set */
    int (*state)(void *in_eiter, const H5EA_create_t *cparam, const earray_test_param_t *tparam,
                 earray_state_t *state, hsize_t idx); /* Get the state of the extensible array */
    herr_t (*term)(void *info);                       /* Shutdown/free iterator private info */
} earray_iter_t;

/* Testing parameters */
struct earray_test_param_t {
    earray_test_type_t   reopen_array; /* Whether to re-open the array during the test */
    earray_test_comp_t   comp;         /* Whether to compress the blocks or not */
    const earray_iter_t *eiter;        /* Iterator to use for this test */

    /* Super block information */
    size_t            nsblks;    /* Number of superblocks needed for array */
    H5EA_sblk_info_t *sblk_info; /* Array of information for each super block */
};

/* Flush depend test context */
typedef struct earray_flush_depend_ctx_t {
    hbool_t base_obj;      /* Flag to indicate that base object has been flushed */
    hbool_t idx0_obj;      /* Flag to indicate that index 0's object has been flushed */
    hbool_t idx0_elem;     /* Flag to indicate that index 0's element has been flushed */
    hbool_t idx1_obj;      /* Flag to indicate that index 1's object has been flushed */
    hbool_t idx1_elem;     /* Flag to indicate that index 1's element has been flushed */
    hbool_t idx10000_obj;  /* Flag to indicate that index 10000's object has been flushed */
    hbool_t idx10000_elem; /* Flag to indicate that index 10000's element has been flushed */
} earray_flush_depend_ctx_t;

/* Extensible array test cache object */
typedef struct earray_test_t {
    /* Information for H5AC cache functions, _must_ be first field in structure */
    H5AC_info_t cache_info;

    /* Entry information */
    uint64_t                   idx;     /* Index that entry corresponds to */
    earray_flush_depend_ctx_t *fd_info; /* Context information for flush depend test */
} earray_test_t;

/* Local prototypes */

/* Local variables */
const char *FILENAME[] = {"earray", "earray_tmp", NULL};

/* Filename to use for all tests */
char filename_g[EARRAY_FILENAME_LEN];

/* Empty file size */
h5_stat_size_t empty_size_g;

/*-------------------------------------------------------------------------
 * Function:    init_cparam
 *
 * Purpose:    Initialize array creation parameter structure
 *
 * Return:    Success:    0
 *        Failure:    -1
 *
 * Programmer:    Quincey Koziol
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
    cparam->cls                       = H5EA_CLS_TEST;
    cparam->raw_elmt_size             = ELMT_SIZE;
    cparam->max_nelmts_bits           = MAX_NELMTS_BITS;
    cparam->idx_blk_elmts             = IDX_BLK_ELMTS;
    cparam->sup_blk_min_data_ptrs     = SUP_BLK_MIN_DATA_PTRS;
    cparam->data_blk_min_elmts        = DATA_BLK_MIN_ELMTS;
    cparam->max_dblk_page_nelmts_bits = MAX_DBLOCK_PAGE_NELMTS_BITS;

    return (0);
} /* init_cparam() */

/*-------------------------------------------------------------------------
 * Function:    init_tparam
 *
 * Purpose:    Initialize array testing parameter structure
 *
 * Note:    This initialization is the same as that in H5EA_hdr_init()
 *
 * Return:    Success:    0
 *        Failure:    -1
 *
 * Programmer:    Quincey Koziol
 *              Thursday, September 25, 2008
 *
 *-------------------------------------------------------------------------
 */
static int
init_tparam(earray_test_param_t *tparam, const H5EA_create_t *cparam)
{
    hsize_t start_idx;  /* First element index for each super block */
    hsize_t start_dblk; /* First data block index for each super block */
    size_t  u;          /* Local index variable */

    /* Wipe out background */
    HDmemset(tparam, 0, sizeof(*tparam));

    /* Compute general information */
    tparam->nsblks = 1 + (cparam->max_nelmts_bits - H5VM_log2_of2(cparam->data_blk_min_elmts));

    /* Allocate information for each super block */
    tparam->sblk_info = (H5EA_sblk_info_t *)HDmalloc(sizeof(H5EA_sblk_info_t) * tparam->nsblks);
    HDassert(tparam->sblk_info);

    /* Compute information about each super block */
    start_idx  = 0;
    start_dblk = 0;
    for (u = 0; u < tparam->nsblks; u++) {
        tparam->sblk_info[u].ndblks      = (size_t)H5_EXP2(u / 2);
        tparam->sblk_info[u].dblk_nelmts = (size_t)H5_EXP2((u + 1) / 2) * cparam->data_blk_min_elmts;
        tparam->sblk_info[u].start_idx   = start_idx;
        tparam->sblk_info[u].start_dblk  = start_dblk;

        /* Advance starting indices for next super block */
        start_idx += (hsize_t)tparam->sblk_info[u].ndblks * (hsize_t)tparam->sblk_info[u].dblk_nelmts;
        start_dblk += (hsize_t)tparam->sblk_info[u].ndblks;
    } /* end for */

    return (0);
} /* init_tparam() */

/*-------------------------------------------------------------------------
 * Function:    finish_tparam
 *
 * Purpose:    Close down array testing parameter structure
 *
 * Return:    Success:    0
 *        Failure:    -1
 *
 * Programmer:    Quincey Koziol
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

    return (0);
} /* finish_tparam() */

/*-------------------------------------------------------------------------
 * Function:    create_file
 *
 * Purpose:    Create file and retrieve pointer to internal file object
 *
 * Return:    Success:    0
 *        Failure:    -1
 *
 * Programmer:    Quincey Koziol
 *              Thursday, August 28, 2008
 *
 *-------------------------------------------------------------------------
 */
static int
create_file(unsigned flags, hid_t fapl, hid_t *file, H5F_t **f)
{
    /* Create the file to work on */
    if ((*file = H5Fcreate(filename_g, flags, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if (NULL == (*f = (H5F_t *)H5VL_object(*file)))
        FAIL_STACK_ERROR

    /* Ignore metadata tags in the file's cache */
    if (H5AC_ignore_tags(*f) < 0)
        FAIL_STACK_ERROR

    /* Success */
    return (0);

error:
    return (-1);
} /* create_file() */

/*-------------------------------------------------------------------------
 * Function:    check_stats
 *
 * Purpose:    Verify stats for an extensible array
 *
 * Return:    Success:    0
 *        Failure:    -1
 *
 * Programmer:    Quincey Koziol
 *              Thursday, August 21, 2008
 *
 *-------------------------------------------------------------------------
 */
static int
check_stats(const H5EA_t *ea, const earray_state_t *state)
{
    H5EA_stat_t earray_stats; /* Statistics about the array */

    /* Get statistics for extensible array and verify they are correct */
    if (H5EA_get_stats(ea, &earray_stats) < 0)
        FAIL_STACK_ERROR

    /* Compare information */
    if (earray_stats.stored.max_idx_set != state->max_idx_set) {
        HDfprintf(stdout,
                  "earray_stats.stored.max_idx_set = %" PRIuHSIZE ", state->max_idx_set = %" PRIuHSIZE "\n",
                  earray_stats.stored.max_idx_set, state->max_idx_set);
        TEST_ERROR
    } /* end if */
    if (earray_stats.stored.nelmts != state->nelmts) {
        HDfprintf(stdout, "earray_stats.stored.nelmts = %" PRIuHSIZE ", state->nelmts = %" PRIuHSIZE "\n",
                  earray_stats.stored.nelmts, state->nelmts);
        TEST_ERROR
    } /* end if */
    if (earray_stats.computed.hdr_size != state->hdr_size) {
        HDfprintf(stdout,
                  "earray_stats.computed.hdr_size = %" PRIuHSIZE ", state->hdr_size = %" PRIuHSIZE "\n",
                  earray_stats.computed.hdr_size, state->hdr_size);
        TEST_ERROR
    } /* end if */
    if (earray_stats.computed.nindex_blks != state->nindex_blks) {
        HDfprintf(stdout,
                  "earray_stats.computed.nindex_blks = %" PRIuHSIZE ", state->nindex_blks = %" PRIuHSIZE "\n",
                  earray_stats.computed.nindex_blks, state->nindex_blks);
        TEST_ERROR
    } /* end if */
    if (earray_stats.computed.index_blk_size != state->index_blk_size) {
        HDfprintf(stdout,
                  "earray_stats.computed.index_blk_size = %" PRIuHSIZE ", state->index_blk_size = %" PRIuHSIZE
                  "\n",
                  earray_stats.computed.index_blk_size, state->index_blk_size);
        TEST_ERROR
    } /* end if */
    if (earray_stats.stored.ndata_blks != state->ndata_blks) {
        HDfprintf(stdout,
                  "earray_stats.stored.ndata_blks = %" PRIuHSIZE ", state->ndata_blks = %" PRIuHSIZE "\n",
                  earray_stats.stored.ndata_blks, state->ndata_blks);
        TEST_ERROR
    } /* end if */
/* Don't compare this currently, it's very hard to compute */
#ifdef NOT_YET
    if (earray_stats.stored.data_blk_size != state->data_blk_size) {
        HDfprintf(stdout,
                  "earray_stats.stored.data_blk_size = %" PRIuHSIZE ", state->data_blk_size = %" PRIuHSIZE
                  "\n",
                  earray_stats.stored.data_blk_size, state->data_blk_size);
        TEST_ERROR
    }  /* end if */
#endif /* NOT_YET */
    if (earray_stats.stored.nsuper_blks != state->nsuper_blks) {
        HDfprintf(stdout,
                  "earray_stats.stored.nsuper_blks = %" PRIuHSIZE ", state->nsuper_blks = %" PRIuHSIZE "\n",
                  earray_stats.stored.nsuper_blks, state->nsuper_blks);
        TEST_ERROR
    } /* end if */
/* Don't compare this currently, it's very hard to compute */
#ifdef NOT_YET
    if (earray_stats.stored.super_blk_size != state->super_blk_size) {
        HDfprintf(stdout,
                  "earray_stats.stored.super_blk_size = %" PRIuHSIZE ", state->super_blk_size = %" PRIuHSIZE
                  "\n",
                  earray_stats.stored.super_blk_size, state->super_blk_size);
        TEST_ERROR
    }  /* end if */
#endif /* NOT_YET */

    /* All tests passed */
    return (0);

error:
    return (-1);
} /* check_stats() */

/*-------------------------------------------------------------------------
 * Function:    reopen_file
 *
 * Purpose:    Perform common "re-open" operations on file & array for testing
 *
 * Return:    Success:    0
 *        Failure:    -1
 *
 * Programmer:    Quincey Koziol
 *              Thursday, August 28, 2008
 *
 *-------------------------------------------------------------------------
 */
static int
reopen_file(hid_t *file, H5F_t **f, hid_t fapl, H5EA_t **ea, haddr_t ea_addr,
            const earray_test_param_t *tparam)
{
    /* Check for closing & re-opening the array */
    /* (actually will close & re-open the file as well) */
    if (tparam->reopen_array) {
        /* Close array, if given */
        if (ea && *ea) {
            if (H5EA_close(*ea) < 0)
                FAIL_STACK_ERROR
            *ea = NULL;
        } /* end if */

        /* Close file */
        if (*file) {
            if (H5Fclose(*file) < 0)
                FAIL_STACK_ERROR
            *file = (-1);
            *f    = NULL;
        } /* end if */

        /* Re-open the file */
        if ((*file = H5Fopen(filename_g, H5F_ACC_RDWR, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if (NULL == (*f = (H5F_t *)H5VL_object(*file)))
            FAIL_STACK_ERROR

        /* Ignore metadata tags in the file's cache */
        if (H5AC_ignore_tags(*f) < 0)
            FAIL_STACK_ERROR

        /* Re-open array, if given */
        if (ea)
            if (NULL == (*ea = H5EA_open(*f, ea_addr, NULL)))
                FAIL_STACK_ERROR
    } /* end if */

    /* Success */
    return (0);

error:
    return (-1);
} /* reopen_file() */

/*-------------------------------------------------------------------------
 * Function:    create_array
 *
 * Purpose:    Create an extensible array and perform initial checks
 *
 * Return:    Success:    0
 *        Failure:    -1
 *
 * Programmer:    Quincey Koziol
 *              Thursday, August 28, 2008
 *
 *-------------------------------------------------------------------------
 */
static int
create_array(H5F_t *f, const H5EA_create_t *cparam, H5EA_t **ea, haddr_t *ea_addr, H5EA__ctx_cb_t *cb)
{
    hsize_t        nelmts; /* Number of elements in array */
    earray_state_t state;  /* State of extensible array */

    /* Create array */
    if (NULL == (*ea = H5EA_create(f, cparam, cb)))
        FAIL_STACK_ERROR

    /* Check status of array */
    nelmts = (hsize_t)ULLONG_MAX;
    if (H5EA_get_nelmts(*ea, &nelmts) < 0)
        FAIL_STACK_ERROR
    if (nelmts > 0)
        TEST_ERROR
    if (H5EA_get_addr(*ea, ea_addr) < 0)
        FAIL_STACK_ERROR
    if (!H5F_addr_defined(*ea_addr))
        TEST_ERROR
    HDmemset(&state, 0, sizeof(state));
    state.hdr_size = EA_HDR_SIZE;
    if (check_stats(*ea, &state))
        TEST_ERROR

    /* Success */
    return (0);

error:
    return (-1);
} /* create_array() */

/*-------------------------------------------------------------------------
 * Function:    verify_cparam
 *
 * Purpose:    Verify creation parameters are correct
 *
 * Return:    Success:    0
 *        Failure:    -1
 *
 * Programmer:    Quincey Koziol
 *              Thursday, August 28, 2008
 *
 *-------------------------------------------------------------------------
 */
static int
verify_cparam(const H5EA_t *ea, const H5EA_create_t *cparam)
{
    H5EA_create_t test_cparam; /* Creation parameters for array */

    /* Retrieve creation parameters */
    HDmemset(&test_cparam, 0, sizeof(H5EA_create_t));
    if (H5EA__get_cparam_test(ea, &test_cparam) < 0)
        FAIL_STACK_ERROR

    /* Verify creation parameters */
    if (H5EA__cmp_cparam_test(cparam, &test_cparam))
        TEST_ERROR

    /* Success */
    return SUCCEED;

error:
    return FAIL;
} /* verify_cparam() */

/*-------------------------------------------------------------------------
 * Function:    finish
 *
 * Purpose:    Close array, delete array, close file and verify that file
 *              is empty size
 *
 * Return:    Success:    0
 *        Failure:    -1
 *
 * Programmer:    Quincey Koziol
 *              Thursday, August 28, 2008
 *
 *-------------------------------------------------------------------------
 */
static int
finish(hid_t file, hid_t fapl, H5F_t *f, H5EA_t *ea, haddr_t ea_addr)
{
    h5_stat_size_t file_size; /* File size, after deleting array */

    /* Close the extensible array */
    if (H5EA_close(ea) < 0)
        FAIL_STACK_ERROR

    /* Delete array */
    if (H5EA_delete(f, ea_addr, NULL) < 0)
        FAIL_STACK_ERROR

    /* Close the file */
    if (H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Get the size of the file */
    if ((file_size = h5_get_file_size(filename_g, fapl)) < 0)
        TEST_ERROR

    /* Verify the file is correct size */
    if (file_size != empty_size_g)
        TEST_ERROR

    /* Success */
    return (0);

error:
    return (-1);
} /* finish() */

/*-------------------------------------------------------------------------
 * Function:    test_create
 *
 * Purpose:    Test creating extensible array
 *
 * Return:    Success: 0
 *        Failure: 1
 *
 * Programmer:    Quincey Koziol
 *              Thursday, August  7, 2008
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_create(hid_t fapl, H5EA_create_t *cparam, earray_test_param_t H5_ATTR_UNUSED *tparam)
{
    hid_t   file    = -1;          /* File ID */
    H5F_t * f       = NULL;        /* Internal file object pointer */
    H5EA_t *ea      = NULL;        /* Extensible array wrapper */
    haddr_t ea_addr = HADDR_UNDEF; /* Array address in file */

    /* Create file & retrieve pointer to internal file object */
    if (create_file(H5F_ACC_TRUNC, fapl, &file, &f) < 0)
        TEST_ERROR

    /*
     * Display testing message
     */
    TESTING("invalid extensible array creation parameters");

#ifndef NDEBUG
    {
        H5EA_create_t test_cparam; /* Creation parameters for array */

        /* Set invalid element size */
        HDmemcpy(&test_cparam, cparam, sizeof(test_cparam));
        test_cparam.raw_elmt_size = 0;
        H5E_BEGIN_TRY
        {
            ea = H5EA_create(f, &test_cparam, NULL);
        }
        H5E_END_TRY;
        if (ea) {
            /* Close opened extensible array */
            H5EA_close(ea);
            ea = NULL;

            /* Indicate error */
            TEST_ERROR
        } /* end if */

        /* Set invalid max. # of elements bits */
        HDmemcpy(&test_cparam, cparam, sizeof(test_cparam));
        test_cparam.max_nelmts_bits = 0;
        H5E_BEGIN_TRY
        {
            ea = H5EA_create(f, &test_cparam, NULL);
        }
        H5E_END_TRY;
        if (ea) {
            /* Close opened extensible array */
            H5EA_close(ea);
            ea = NULL;

            /* Indicate error */
            TEST_ERROR
        } /* end if */

        HDmemcpy(&test_cparam, cparam, sizeof(test_cparam));
        test_cparam.max_nelmts_bits = 65;
        H5E_BEGIN_TRY
        {
            ea = H5EA_create(f, &test_cparam, NULL);
        }
        H5E_END_TRY;
        if (ea) {
            /* Close opened extensible array */
            H5EA_close(ea);
            ea = NULL;

            /* Indicate error */
            TEST_ERROR
        } /* end if */

        /* Set invalid min. # of data block pointers in super blocks */
        HDmemcpy(&test_cparam, cparam, sizeof(test_cparam));
        test_cparam.sup_blk_min_data_ptrs = 0;
        H5E_BEGIN_TRY
        {
            ea = H5EA_create(f, &test_cparam, NULL);
        }
        H5E_END_TRY;
        if (ea) {
            /* Close opened extensible array */
            H5EA_close(ea);
            ea = NULL;

            /* Indicate error */
            TEST_ERROR
        } /* end if */
        HDmemcpy(&test_cparam, cparam, sizeof(test_cparam));
        test_cparam.sup_blk_min_data_ptrs = 1;
        H5E_BEGIN_TRY
        {
            ea = H5EA_create(f, &test_cparam, NULL);
        }
        H5E_END_TRY;
        if (ea) {
            /* Close opened extensible array */
            H5EA_close(ea);
            ea = NULL;

            /* Indicate error */
            TEST_ERROR
        } /* end if */
        HDmemcpy(&test_cparam, cparam, sizeof(test_cparam));
        test_cparam.sup_blk_min_data_ptrs = 6;
        H5E_BEGIN_TRY
        {
            ea = H5EA_create(f, &test_cparam, NULL);
        }
        H5E_END_TRY;
        if (ea) {
            /* Close opened extensible array */
            H5EA_close(ea);
            ea = NULL;

            /* Indicate error */
            TEST_ERROR
        } /* end if */

        /* Set invalid min. # of elements per data block */
        HDmemcpy(&test_cparam, cparam, sizeof(test_cparam));
        test_cparam.data_blk_min_elmts = 0;
        H5E_BEGIN_TRY
        {
            ea = H5EA_create(f, &test_cparam, NULL);
        }
        H5E_END_TRY;
        if (ea) {
            /* Close opened extensible array */
            H5EA_close(ea);
            ea = NULL;

            /* Indicate error */
            TEST_ERROR
        } /* end if */

        /* Set invalid max. # of elements per data block page bits */
        if (test_cparam.idx_blk_elmts > 0) {
            HDmemcpy(&test_cparam, cparam, sizeof(test_cparam));
            test_cparam.max_dblk_page_nelmts_bits =
                (uint8_t)(H5VM_log2_gen((uint64_t)test_cparam.idx_blk_elmts) - 1);
            H5E_BEGIN_TRY
            {
                ea = H5EA_create(f, &test_cparam, NULL);
            }
            H5E_END_TRY;
            if (ea) {
                /* Close opened extensible array */
                H5EA_close(ea);
                ea = NULL;

                /* Indicate error */
                TEST_ERROR
            } /* end if */
        }     /* end if */
        HDmemcpy(&test_cparam, cparam, sizeof(test_cparam));
        test_cparam.max_dblk_page_nelmts_bits = 4; /* corresponds to 16 elements in data block page, which is
                                                      less than the 64 elements for the default settings */
        H5E_BEGIN_TRY
        {
            ea = H5EA_create(f, &test_cparam, NULL);
        }
        H5E_END_TRY;
        if (ea) {
            /* Close opened extensible array */
            H5EA_close(ea);
            ea = NULL;

            /* Indicate error */
            TEST_ERROR
        } /* end if */
        HDmemcpy(&test_cparam, cparam, sizeof(test_cparam));
        test_cparam.max_dblk_page_nelmts_bits = (uint8_t)(test_cparam.max_nelmts_bits + 1);
        H5E_BEGIN_TRY
        {
            ea = H5EA_create(f, &test_cparam, NULL);
        }
        H5E_END_TRY;
        if (ea) {
            /* Close opened extensible array */
            H5EA_close(ea);
            ea = NULL;

            /* Indicate error */
            TEST_ERROR
        } /* end if */

        PASSED();
    }
#else  /* NDEBUG */
    SKIPPED();
    HDputs("    Not tested when assertions are disabled");
#endif /* NDEBUG */

    /*
     * Display testing message
     */
    TESTING("extensible array creation");

    /* Create array */
    if (create_array(f, cparam, &ea, &ea_addr, NULL) < 0)
        TEST_ERROR

    PASSED();

    /* Verify the creation parameters */
    TESTING("verify array creation parameters");

    /* Verify the creation parameters */
    if (verify_cparam(ea, cparam) < 0)
        TEST_ERROR

    /* Close array, delete array, close file & verify file is empty */
    if (finish(file, fapl, f, ea, ea_addr) < 0)
        TEST_ERROR

    /* All tests passed */
    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (ea)
            H5EA_close(ea);
        H5Fclose(file);
    }
    H5E_END_TRY;

    return 1;
} /* end test_create() */

/*-------------------------------------------------------------------------
 * Function:    test_reopen
 *
 * Purpose:    Create & reopen an extensible array
 *
 * Return:    Success:    0
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Thursday, August 28, 2008
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_reopen(hid_t fapl, H5EA_create_t *cparam, earray_test_param_t *tparam)
{
    hid_t   file    = -1;          /* File ID */
    H5F_t * f       = NULL;        /* Internal file object pointer */
    H5EA_t *ea      = NULL;        /* Extensible array wrapper */
    haddr_t ea_addr = HADDR_UNDEF; /* Array address in file */

    /* Create file & retrieve pointer to internal file object */
    if (create_file(H5F_ACC_TRUNC, fapl, &file, &f) < 0)
        TEST_ERROR

    /*
     * Display testing message
     */
    TESTING("create, close & reopen extensible array");

    /* Create array */
    if (create_array(f, cparam, &ea, &ea_addr, NULL) < 0)
        TEST_ERROR

    /* Close the extensible array */
    if (H5EA_close(ea) < 0)
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the file */
    if (reopen_file(&file, &f, fapl, NULL, HADDR_UNDEF, tparam) < 0)
        TEST_ERROR

    /* Re-open the array */
    if (NULL == (ea = H5EA_open(f, ea_addr, NULL)))
        FAIL_STACK_ERROR

    /* Verify the creation parameters */
    if (verify_cparam(ea, cparam) < 0)
        TEST_ERROR

    /* Close array, delete array, close file & verify file is empty */
    if (finish(file, fapl, f, ea, ea_addr) < 0)
        TEST_ERROR

    /* All tests passed */
    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (ea)
            H5EA_close(ea);
        H5Fclose(file);
    }
    H5E_END_TRY;

    return 1;
} /* test_reopen() */

/*-------------------------------------------------------------------------
 * Function:    test_open_twice
 *
 * Purpose:    Open an extensible array twice
 *
 * Return:    Success:    0
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Thursday, August 28, 2008
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_open_twice(hid_t fapl, H5EA_create_t *cparam, earray_test_param_t *tparam)
{
    hid_t   file    = -1;          /* File ID */
    hid_t   file2   = -1;          /* File ID */
    H5F_t * f       = NULL;        /* Internal file object pointer */
    H5F_t * f2      = NULL;        /* Internal file object pointer */
    H5EA_t *ea      = NULL;        /* Extensible array wrapper */
    H5EA_t *ea2     = NULL;        /* Extensible array wrapper */
    haddr_t ea_addr = HADDR_UNDEF; /* Array address in file */

    /* Create file & retrieve pointer to internal file object */
    if (create_file(H5F_ACC_TRUNC, fapl, &file, &f) < 0)
        TEST_ERROR

    /*
     * Display testing message
     */
    TESTING("open extensible array twice");

    /* Create array */
    if (create_array(f, cparam, &ea, &ea_addr, NULL) < 0)
        TEST_ERROR

    /* Open the array again, through the first file handle */
    if (NULL == (ea2 = H5EA_open(f, ea_addr, NULL)))
        FAIL_STACK_ERROR

    /* Verify the creation parameters */
    if (verify_cparam(ea, cparam) < 0)
        TEST_ERROR
    if (verify_cparam(ea2, cparam) < 0)
        TEST_ERROR

    /* Close the second extensible array wrapper */
    if (H5EA_close(ea2) < 0)
        FAIL_STACK_ERROR
    ea2 = NULL;

    /* Check for closing & re-opening the file */
    if (reopen_file(&file, &f, fapl, &ea, ea_addr, tparam) < 0)
        TEST_ERROR

    /* Re-open the file */
    if ((file2 = H5Freopen(file)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if (NULL == (f2 = (H5F_t *)H5VL_object(file2)))
        FAIL_STACK_ERROR

    /* Open the extensible array through the second file handle */
    if (NULL == (ea2 = H5EA_open(f2, ea_addr, NULL)))
        FAIL_STACK_ERROR

    /* Verify the creation parameters */
    if (verify_cparam(ea, cparam) < 0)
        TEST_ERROR

    /* Close the first extensible array wrapper */
    if (H5EA_close(ea) < 0)
        FAIL_STACK_ERROR
    ea = NULL;

    /* Close the first file */
    /* (close before second file, to detect error on internal array header's
     *  shared file information)
     */
    if (H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Close array, delete array, close file & verify file is empty */
    if (finish(file2, fapl, f2, ea2, ea_addr) < 0)
        TEST_ERROR

    /* All tests passed */
    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (ea)
            H5EA_close(ea);
        if (ea2)
            H5EA_close(ea2);
        H5Fclose(file);
        H5Fclose(file2);
    }
    H5E_END_TRY;

    return 1;
} /* test_open_twice() */

/*-------------------------------------------------------------------------
 * Function:    test_open_twice_diff
 *
 * Purpose:    Open an extensible array twice, through different "top" file
 *              handles, with an intermediate file open that takes the "shared"
 *              file handle from the first extensible array's file pointer.
 *
 * Return:    Success:    0
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Friday, December 18, 2015
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_open_twice_diff(hid_t fapl, H5EA_create_t *cparam, earray_test_param_t *tparam)
{
    char    filename_tmp[EARRAY_FILENAME_LEN]; /* Temporary file name */
    hid_t   file    = -1;                      /* File ID */
    hid_t   file2   = -1;                      /* File ID */
    hid_t   file0   = -1;                      /* File ID */
    hid_t   file00  = -1;                      /* File ID */
    H5F_t * f       = NULL;                    /* Internal file object pointer */
    H5F_t * f2      = NULL;                    /* Internal file object pointer */
    H5EA_t *ea      = NULL;                    /* Extensible array wrapper */
    H5EA_t *ea2     = NULL;                    /* Extensible array wrapper */
    haddr_t ea_addr = HADDR_UNDEF;             /* Array address in file */

    /* Create file & retrieve pointer to internal file object */
    if (create_file(H5F_ACC_TRUNC, fapl, &file, &f) < 0)
        TEST_ERROR

    /*
     * Display testing message
     */
    TESTING("open extensible array twice, through different file handles");

    /* Create array */
    if (create_array(f, cparam, &ea, &ea_addr, NULL) < 0)
        TEST_ERROR

    /* Open the array again, through the first file handle */
    if (NULL == (ea2 = H5EA_open(f, ea_addr, NULL)))
        FAIL_STACK_ERROR

    /* Verify the creation parameters */
    if (verify_cparam(ea, cparam) < 0)
        TEST_ERROR
    if (verify_cparam(ea2, cparam) < 0)
        TEST_ERROR

    /* Close the second extensible array wrapper */
    if (H5EA_close(ea2) < 0)
        FAIL_STACK_ERROR
    ea2 = NULL;

    /* Re-open the file */
    /* (So that there is something holding the file open when the extensible
     *  array is closed)
     */
    if ((file0 = H5Fopen(filename_g, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the file */
    if (reopen_file(&file, &f, fapl, &ea, ea_addr, tparam) < 0)
        TEST_ERROR

    /* Verify the creation parameters */
    if (verify_cparam(ea, cparam) < 0)
        TEST_ERROR

    /* Close the first extensible array wrapper */
    if (H5EA_close(ea) < 0)
        FAIL_STACK_ERROR
    ea = NULL;

    /* Close the first file */
    /* (close before second file, to detect error on internal array header's
     *  shared file information)
     */
    if (H5Fclose(file) < 0)
        FAIL_STACK_ERROR
    file = -1;

    /* Open a different file */
    /* (This re-allocates the 'top' file pointer and assigns it a different
     *  'shared' file pointer, making the file pointer in the extensible array's
     *  header stale)
     */
    h5_fixname(FILENAME[1], fapl, filename_tmp, sizeof(filename_tmp));
    if ((file00 = H5Fcreate(filename_tmp, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Re-open the file with the extensible array */
    if ((file2 = H5Fopen(filename_g, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if (NULL == (f2 = (H5F_t *)H5VL_object(file2)))
        FAIL_STACK_ERROR

    /* Open the extensible array through the second file handle */
    if (NULL == (ea2 = H5EA_open(f2, ea_addr, NULL)))
        FAIL_STACK_ERROR

    /* Verify the creation parameters */
    if (verify_cparam(ea2, cparam) < 0)
        TEST_ERROR

    /* Close the extra file handles */
    if (H5Fclose(file0) < 0)
        FAIL_STACK_ERROR
    if (H5Fclose(file00) < 0)
        FAIL_STACK_ERROR

    /* Close array, delete array, close file & verify file is empty */
    if (finish(file2, fapl, f2, ea2, ea_addr) < 0)
        TEST_ERROR

    /* All tests passed */
    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (ea)
            H5EA_close(ea);
        if (ea2)
            H5EA_close(ea2);
        H5Fclose(file);
        H5Fclose(file2);
        H5Fclose(file0);
        H5Fclose(file00);
    }
    H5E_END_TRY;

    return 1;
} /* test_open_twice_diff() */

/*-------------------------------------------------------------------------
 * Function:    test_delete_open
 *
 * Purpose:    Delete opened extensible array (& open deleted array)
 *
 * Return:    Success:    0
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Thursday, August 28, 2008
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_delete_open(hid_t fapl, H5EA_create_t *cparam, earray_test_param_t *tparam)
{
    hid_t          file    = -1;          /* File ID */
    H5F_t *        f       = NULL;        /* Internal file object pointer */
    H5EA_t *       ea      = NULL;        /* Extensible array wrapper */
    H5EA_t *       ea2     = NULL;        /* Extensible array wrapper */
    haddr_t        ea_addr = HADDR_UNDEF; /* Array address in file */
    h5_stat_size_t file_size;             /* File size, after deleting array */

    /* Create file & retrieve pointer to internal file object */
    if (create_file(H5F_ACC_TRUNC, fapl, &file, &f) < 0)
        TEST_ERROR

    /*
     * Display testing message
     */
    TESTING("deleting open extensible array");

    /* Create array */
    if (create_array(f, cparam, &ea, &ea_addr, NULL) < 0)
        TEST_ERROR

    /* Open the array again */
    if (NULL == (ea2 = H5EA_open(f, ea_addr, NULL)))
        FAIL_STACK_ERROR

    /* Request that the array be deleted */
    if (H5EA_delete(f, ea_addr, NULL) < 0)
        FAIL_STACK_ERROR

    /* Verify the creation parameters */
    if (verify_cparam(ea, cparam) < 0)
        TEST_ERROR
    if (verify_cparam(ea2, cparam) < 0)
        TEST_ERROR

    /* Close the second extensible array wrapper */
    if (H5EA_close(ea2) < 0)
        FAIL_STACK_ERROR
    ea2 = NULL;

    /* Try re-opening the array again (should fail, as array will be deleted) */
    H5E_BEGIN_TRY
    {
        ea2 = H5EA_open(f, ea_addr, NULL);
    }
    H5E_END_TRY;
    if (ea2) {
        /* Close opened array */
        H5EA_close(ea2);

        /* Indicate error */
        TEST_ERROR
    } /* end if */

    /* Close the first extensible array wrapper */
    if (H5EA_close(ea) < 0)
        FAIL_STACK_ERROR
    ea = NULL;

    /* Check for closing & re-opening the file */
    if (reopen_file(&file, &f, fapl, NULL, HADDR_UNDEF, tparam) < 0)
        TEST_ERROR

    /* Try re-opening the array again (should fail, as array is now deleted) */
    H5E_BEGIN_TRY
    {
        ea = H5EA_open(f, ea_addr, NULL);
    }
    H5E_END_TRY;
    if (ea) {
        /* Close opened array */
        H5EA_close(ea);

        /* Indicate error */
        TEST_ERROR
    } /* end if */

    /* Close the file */
    if (H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Get the size of the file */
    if ((file_size = h5_get_file_size(filename_g, fapl)) < 0)
        TEST_ERROR

    /* Verify the file is correct size */
    if (file_size != empty_size_g)
        TEST_ERROR

    /* All tests passed */
    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (ea)
            H5EA_close(ea);
        if (ea2)
            H5EA_close(ea2);
        H5Fclose(file);
    }
    H5E_END_TRY;

    return 1;
} /* test_delete_open() */

/* Extensible array iterator info for forward iteration */
typedef struct eiter_fw_t {
    hsize_t  idx;           /* Index of next array location */
    unsigned base_sblk_idx; /* Starting index for actual superblocks */
} eiter_fw_t;

/*-------------------------------------------------------------------------
 * Function:    eiter_fw_init
 *
 * Purpose:    Initialize element interator (forward iteration)
 *
 * Return:    Success:    Pointer to iteration status object
 *        Failure:    NULL
 *
 * Programmer:    Quincey Koziol
 *              Thursday, October  2, 2008
 *
 *-------------------------------------------------------------------------
 */
static void *
eiter_fw_init(const H5EA_create_t H5_ATTR_UNUSED *cparam, const earray_test_param_t H5_ATTR_UNUSED *tparam,
              hsize_t H5_ATTR_UNUSED cnt)
{
    eiter_fw_t *eiter; /* Forward element iteration object */

    /* Allocate space for the element iteration object */
    eiter = (eiter_fw_t *)HDmalloc(sizeof(eiter_fw_t));
    HDassert(eiter);

    /* Initialize the element iteration object */
    eiter->idx           = 0;
    eiter->base_sblk_idx = UINT_MAX;

    /* Return iteration object */
    return (eiter);
} /* end eiter_fw_init() */

/*-------------------------------------------------------------------------
 * Function:    eiter_fw_next
 *
 * Purpose:    Get next element index (forward iteration)
 *
 * Return:    Success:    Non-negative
 *        Failure:    Negative
 *
 * Programmer:    Quincey Koziol
 *              Tuesday, November  4, 2008
 *
 *-------------------------------------------------------------------------
 */
static hssize_t
eiter_fw_next(void *in_eiter)
{
    eiter_fw_t *eiter = (eiter_fw_t *)in_eiter;
    hssize_t    ret_val;

    /* Sanity check */
    HDassert(eiter);

    /* Get the next array index to test */
    ret_val = (hssize_t)eiter->idx++;

    return (ret_val);
} /* end eiter_fw_next() */

/*-------------------------------------------------------------------------
 * Function:    eiter_fw_max
 *
 * Purpose:    Get max. element index (forward iteration)
 *
 * Return:    Success:    Non-negative
 *        Failure:    Negative
 *
 * Programmer:    Quincey Koziol
 *              Tuesday, November  4, 2008
 *
 *-------------------------------------------------------------------------
 */
static H5_ATTR_PURE hssize_t
eiter_fw_max(const void *in_eiter)
{
    const eiter_fw_t *eiter = (const eiter_fw_t *)in_eiter;

    /* Sanity check */
    HDassert(eiter);

    /* Return the max. array index used */
    return ((hssize_t)(eiter->idx - 1));
} /* end eiter_fw_max() */

/*-------------------------------------------------------------------------
 * Function:    eiter_fw_state
 *
 * Purpose:    Get extensible array state (forward iteration)
 *
 * Return:    Success:    Non-negative
 *        Failure:    Negative
 *
 * Programmer:    Quincey Koziol
 *              Tuesday, November  4, 2008
 *
 *-------------------------------------------------------------------------
 */
static int
eiter_fw_state(void *in_eiter, const H5EA_create_t *cparam, const earray_test_param_t *tparam,
               earray_state_t *state, hsize_t idx)
{
    eiter_fw_t *eiter = (eiter_fw_t *)in_eiter;

    /* Sanity check */
    HDassert(eiter);
    HDassert(cparam);
    HDassert(tparam);
    HDassert(state);

    /* Compute the state of the extensible array */
    state->hdr_size       = EA_HDR_SIZE;
    state->nindex_blks    = 1;
    state->index_blk_size = EA_IBLOCK_SIZE;
    state->max_idx_set    = idx + 1;
    if (idx < cparam->idx_blk_elmts) {
        state->nelmts      = (hsize_t)cparam->idx_blk_elmts;
        state->nsuper_blks = state->ndata_blks = (hsize_t)0;
        state->super_blk_size = state->data_blk_size = (hsize_t)0;
    } /* end if */
    else {
        unsigned sblk_idx; /* Which superblock does this index fall in? */

        /* Compute super block index for element index */
        /* (same eqn. as in H5EA__dblock_sblk_idx()) */
        sblk_idx =
            H5VM_log2_gen((uint64_t)(((idx - cparam->idx_blk_elmts) / cparam->data_blk_min_elmts) + 1));
        state->nelmts     = EA_NELMTS(cparam, tparam, idx, sblk_idx);
        state->ndata_blks = EA_NDATA_BLKS(cparam, tparam, idx, sblk_idx);

        /* Check if we have any super blocks yet */
        if (tparam->sblk_info[sblk_idx].ndblks >= cparam->sup_blk_min_data_ptrs) {
            /* Check if this is the first superblock */
            if (sblk_idx < eiter->base_sblk_idx)
                eiter->base_sblk_idx = sblk_idx;

            state->nsuper_blks = (sblk_idx - eiter->base_sblk_idx) + 1;
        } /* end if */
        else
            state->nsuper_blks = 0;
    } /* end else */

    return (0);
} /* end eiter_fw_state() */

/*-------------------------------------------------------------------------
 * Function:    eiter_fw_term
 *
 * Purpose:    Shut down element interator (forward iteration)
 *
 * Return:    Success:    0
 *        Failure:    -1
 *
 * Programmer:    Quincey Koziol
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

    return (0);
} /* end eiter_fw_term() */

/* Extensible array iterator class for forward iteration */
static const earray_iter_t ea_iter_fw = {
    eiter_fw_init,  /* Iterator init */
    eiter_fw_next,  /* Next array index */
    eiter_fw_max,   /* Max. array index */
    eiter_fw_state, /* State of the extensible array */
    eiter_fw_term   /* Iterator term */
};

/* Extensible array iterator info for reverse iteration */
typedef struct eiter_rv_t {
    hsize_t idx;            /* Index of next array location */
    hsize_t max;            /* Index of max. array location */
    hsize_t max_sblk_idx;   /* Which superblock does the max. array location fall in? */
    hsize_t max_nelmts;     /* Max. # of elements for array */
    hsize_t max_ndata_blks; /* Max. # of data blocks for array */
    hsize_t idx_blk_nsblks; /* Number of superblocks directly pointed to in the index block */
} eiter_rv_t;

/*-------------------------------------------------------------------------
 * Function:    eiter_rv_init
 *
 * Purpose:    Initialize element interator (reverse iteration)
 *
 * Return:    Success:    Pointer to iteration status object
 *        Failure:    NULL
 *
 * Programmer:    Quincey Koziol
 *              Tuesday, November  4, 2008
 *
 *-------------------------------------------------------------------------
 */
static void *
eiter_rv_init(const H5EA_create_t *cparam, const earray_test_param_t *tparam, hsize_t cnt)
{
    eiter_rv_t *eiter; /* Reverse element iteration object */

    /* Allocate space for the element iteration object */
    eiter = (eiter_rv_t *)HDmalloc(sizeof(eiter_rv_t));
    HDassert(eiter);

    /* Initialize reverse iteration info */
    eiter->idx = cnt - 1;
    eiter->max = cnt - 1;
    if (cnt > cparam->idx_blk_elmts) {
        eiter->max_sblk_idx = H5VM_log2_gen(
            (uint64_t)(((eiter->max - cparam->idx_blk_elmts) / cparam->data_blk_min_elmts) + 1));
        eiter->max_nelmts     = EA_NELMTS(cparam, tparam, eiter->max, eiter->max_sblk_idx);
        eiter->max_ndata_blks = EA_NDATA_BLKS(cparam, tparam, eiter->max, eiter->max_sblk_idx);
        eiter->idx_blk_nsblks = 2 * H5VM_log2_of2((uint32_t)cparam->sup_blk_min_data_ptrs);
    } /* end if */
    else {
        eiter->max_sblk_idx   = (hsize_t)0;
        eiter->max_nelmts     = (hsize_t)0;
        eiter->max_ndata_blks = (hsize_t)0;
        eiter->idx_blk_nsblks = (hsize_t)0;
    } /* end else */

    /* Return iteration object */
    return (eiter);
} /* end eiter_rv_init() */

/*-------------------------------------------------------------------------
 * Function:    eiter_rv_next
 *
 * Purpose:    Get next element index (reverse iteration)
 *
 * Return:    Success:    Non-negative
 *        Failure:    Negative
 *
 * Programmer:    Quincey Koziol
 *              Tuesday, November  4, 2008
 *
 *-------------------------------------------------------------------------
 */
static hssize_t
eiter_rv_next(void *in_eiter)
{
    eiter_rv_t *eiter = (eiter_rv_t *)in_eiter;
    hssize_t    ret_val;

    /* Sanity check */
    HDassert(eiter);

    /* Get the next array index to test */
    ret_val = (hssize_t)eiter->idx--;

    return (ret_val);
} /* end eiter_rv_next() */

/*-------------------------------------------------------------------------
 * Function:    eiter_rv_max
 *
 * Purpose:    Get max. element index (reverse iteration)
 *
 * Return:    Success:    Non-negative
 *        Failure:    Negative
 *
 * Programmer:    Quincey Koziol
 *              Tuesday, November  4, 2008
 *
 *-------------------------------------------------------------------------
 */
static H5_ATTR_PURE hssize_t
eiter_rv_max(const void *in_eiter)
{
    const eiter_rv_t *eiter = (const eiter_rv_t *)in_eiter;

    /* Sanity check */
    HDassert(eiter);

    /* Return the max. array index used */
    return ((hssize_t)eiter->max);
} /* end eiter_rv_max() */

/*-------------------------------------------------------------------------
 * Function:    eiter_rv_state
 *
 * Purpose:    Get extensible array state (reverse iteration)
 *
 * Return:    Success:    Non-negative
 *        Failure:    Negative
 *
 * Programmer:    Quincey Koziol
 *              Tuesday, November  4, 2008
 *
 *-------------------------------------------------------------------------
 */
static int
eiter_rv_state(void *in_eiter, const H5EA_create_t *cparam, const earray_test_param_t *tparam,
               earray_state_t *state, hsize_t idx)
{
    eiter_rv_t *eiter = (eiter_rv_t *)in_eiter;

    /* Sanity check */
    HDassert(eiter);
    HDassert(cparam);
    HDassert(tparam);
    HDassert(state);

    /* Compute the state of the extensible array */
    state->hdr_size       = EA_HDR_SIZE;
    state->nindex_blks    = 1;
    state->index_blk_size = EA_IBLOCK_SIZE;
    state->max_idx_set    = eiter->max + 1;
    if (eiter->max < cparam->idx_blk_elmts) {
        state->nelmts      = (hsize_t)cparam->idx_blk_elmts;
        state->nsuper_blks = state->ndata_blks = (hsize_t)0;
    } /* end if */
    else {
        hsize_t  idx_nelmts;       /* # of elements for array index */
        hsize_t  idx_ndata_blks;   /* # of data blocks for array index */
        hsize_t  loc_idx = 0;      /* Local index, for computing an offset in next lower data block */
        unsigned idx_sblk_idx;     /* Which superblock does this index fall in? */
        unsigned loc_sblk_idx = 0; /* Which superblock does the local index fall in? */

        /* Compute super block index for element index */
        /* (same eqn. as in H5EA__dblock_sblk_idx()) */
        if (idx < cparam->idx_blk_elmts + cparam->data_blk_min_elmts)
            idx_sblk_idx = 0;
        else {
            hsize_t tmp_idx;  /* Temporary index in superblock */
            hsize_t dblk_idx; /* Index of data block within superblock */

            idx_sblk_idx =
                H5VM_log2_gen((uint64_t)(((idx - cparam->idx_blk_elmts) / cparam->data_blk_min_elmts) + 1));
            tmp_idx  = idx - (cparam->idx_blk_elmts + tparam->sblk_info[idx_sblk_idx].start_idx);
            dblk_idx = tmp_idx / tparam->sblk_info[idx_sblk_idx].dblk_nelmts;
            if (dblk_idx > 0)
                loc_idx = idx - tparam->sblk_info[idx_sblk_idx].dblk_nelmts;
            else
                loc_idx = cparam->idx_blk_elmts + tparam->sblk_info[idx_sblk_idx].start_idx - 1;
            loc_sblk_idx = H5VM_log2_gen(
                (uint64_t)(((loc_idx - cparam->idx_blk_elmts) / cparam->data_blk_min_elmts) + 1));
        } /* end else */

        if (idx < cparam->idx_blk_elmts + cparam->data_blk_min_elmts)
            idx_nelmts = (hsize_t)cparam->idx_blk_elmts;
        else
            idx_nelmts = EA_NELMTS(cparam, tparam, loc_idx, loc_sblk_idx);
        state->nelmts = (eiter->max_nelmts - idx_nelmts) + cparam->idx_blk_elmts;

        if (idx < cparam->idx_blk_elmts + cparam->data_blk_min_elmts)
            idx_ndata_blks = 0;
        else
            idx_ndata_blks = EA_NDATA_BLKS(cparam, tparam, loc_idx, loc_sblk_idx);
        state->ndata_blks = eiter->max_ndata_blks - idx_ndata_blks;

        /* Check if we have any super blocks yet */
        if (tparam->sblk_info[eiter->max_sblk_idx].ndblks >= cparam->sup_blk_min_data_ptrs) {
            if (idx_sblk_idx > eiter->idx_blk_nsblks)
                state->nsuper_blks = (eiter->max_sblk_idx - idx_sblk_idx) + 1;
            else
                state->nsuper_blks = (eiter->max_sblk_idx - eiter->idx_blk_nsblks) + 1;
        } /* end if */
    }     /* end else */

    return (0);
} /* end eiter_rv_state() */

/*-------------------------------------------------------------------------
 * Function:    eiter_rv_term
 *
 * Purpose:    Shut down element interator (reverse iteration)
 *
 * Return:    Success:    0
 *        Failure:    -1
 *
 * Programmer:    Quincey Koziol
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

    return (0);
} /* end eiter_rv_term() */

/* Extensible array iterator class for reverse iteration */
static const earray_iter_t ea_iter_rv = {
    eiter_rv_init,  /* Iterator init */
    eiter_rv_next,  /* Next array index */
    eiter_rv_max,   /* Max. array index written */
    eiter_rv_state, /* State of the extensible array */
    eiter_rv_term   /* Iterator term */
};

/* Extensible array iterator info for random iteration */
typedef struct eiter_rnd_t {
    hsize_t  max; /* Max. array index used */
    hsize_t  pos; /* Position in shuffled array */
    hsize_t *idx; /* Array of shuffled indices */
} eiter_rnd_t;

/*-------------------------------------------------------------------------
 * Function:    eiter_rnd_init
 *
 * Purpose:    Initialize element interator (random iteration)
 *
 * Return:    Success:    Pointer to iteration status object
 *        Failure:    NULL
 *
 * Programmer:    Quincey Koziol
 *              Thursday, November  6, 2008
 *
 *-------------------------------------------------------------------------
 */
static void *
eiter_rnd_init(const H5EA_create_t H5_ATTR_UNUSED *cparam, const earray_test_param_t H5_ATTR_UNUSED *tparam,
               hsize_t cnt)
{
    eiter_rnd_t *eiter; /* Random element iteration object */
    size_t       u;     /* Local index variable */

    /* Allocate space for the element iteration object */
    eiter = (eiter_rnd_t *)HDmalloc(sizeof(eiter_rnd_t));
    HDassert(eiter);

    /* Allocate space for the array of shuffled indices */
    eiter->idx = (hsize_t *)HDmalloc(sizeof(hsize_t) * (size_t)cnt);
    HDassert(eiter->idx);

    /* Initialize reverse iteration info */
    eiter->max = 0;
    eiter->pos = 0;
    for (u = 0; u < (size_t)cnt; u++)
        eiter->idx[u] = (hsize_t)u;

    /* Randomly shuffle array indices */
    if (cnt > 1) {
        for (u = 0; u < (size_t)cnt; u++) {
            size_t  swap_idx; /* Location to swap with when shuffling */
            hsize_t temp_idx; /* Temporary index */

            swap_idx             = ((size_t)HDrandom() % ((size_t)cnt - u)) + u;
            temp_idx             = eiter->idx[u];
            eiter->idx[u]        = eiter->idx[swap_idx];
            eiter->idx[swap_idx] = temp_idx;
        } /* end for */
    }     /* end if */

    /* Return iteration object */
    return (eiter);
} /* end eiter_rnd_init() */

/*-------------------------------------------------------------------------
 * Function:    eiter_rnd_next
 *
 * Purpose:    Get next element index (random iteration)
 *
 * Return:    Success:    Non-negative
 *        Failure:    Negative
 *
 * Programmer:    Quincey Koziol
 *              Thursday, November  6, 2008
 *
 *-------------------------------------------------------------------------
 */
static hssize_t
eiter_rnd_next(void *in_eiter)
{
    eiter_rnd_t *eiter = (eiter_rnd_t *)in_eiter;
    hssize_t     ret_val;

    /* Sanity check */
    HDassert(eiter);

    /* Get the next array index to test */
    ret_val = (hssize_t)eiter->idx[eiter->pos];
    eiter->pos++;

    /* Check for new max. value */
    if ((hsize_t)ret_val > eiter->max)
        eiter->max = (hsize_t)ret_val;

    return (ret_val);
} /* end eiter_rnd_next() */

/*-------------------------------------------------------------------------
 * Function:    eiter_rnd_max
 *
 * Purpose:    Get max. element index (random iteration)
 *
 * Return:    Success:    Non-negative
 *        Failure:    Negative
 *
 * Programmer:    Quincey Koziol
 *              Tuesday, November  6, 2008
 *
 *-------------------------------------------------------------------------
 */
static H5_ATTR_PURE hssize_t
eiter_rnd_max(const void *in_eiter)
{
    const eiter_rnd_t *eiter = (const eiter_rnd_t *)in_eiter;

    /* Sanity check */
    HDassert(eiter);

    /* Return the max. array index used */
    return ((hssize_t)eiter->max);
} /* end eiter_rnd_max() */

/*-------------------------------------------------------------------------
 * Function:    eiter_rnd_term
 *
 * Purpose:    Shut down element interator (random iteration)
 *
 * Return:    Success:    0
 *        Failure:    -1
 *
 * Programmer:    Quincey Koziol
 *              Tuesday, November  6, 2008
 *
 *-------------------------------------------------------------------------
 */
static int
eiter_rnd_term(void *in_eiter)
{
    eiter_rnd_t *eiter = (eiter_rnd_t *)in_eiter;

    /* Sanity check */
    HDassert(eiter);
    HDassert(eiter->idx);

    /* Free shuffled index array */
    HDfree(eiter->idx);

    /* Free iteration object */
    HDfree(eiter);

    return (0);
} /* end eiter_rnd_term() */

/* Extensible array iterator class for random iteration */
static const earray_iter_t ea_iter_rnd = {
    eiter_rnd_init, /* Iterator init */
    eiter_rnd_next, /* Next array index */
    eiter_rnd_max,  /* Max. array index written */
    NULL,           /* State of the extensible array */
    eiter_rnd_term  /* Iterator term */
};

/*-------------------------------------------------------------------------
 * Function:    eiter_rnd2_init
 *
 * Purpose:    Initialize element interator (random #2 iteration)
 *
 * Return:    Success:    Pointer to iteration status object
 *        Failure:    NULL
 *
 * Programmer:    Quincey Koziol
 *              Thursday, November 11, 2008
 *
 *-------------------------------------------------------------------------
 */
static void *
eiter_rnd2_init(const H5EA_create_t H5_ATTR_UNUSED *cparam, const earray_test_param_t H5_ATTR_UNUSED *tparam,
                hsize_t cnt)
{
    eiter_rnd_t *eiter; /* Random element iteration object */
    size_t       u;     /* Local index variable */

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
    if (cnt > 1) {
        hsize_t *tmp_idx;                                     /* Temporary index array */
        hsize_t  sparse_cnt = (hsize_t)(cnt * EA_RND2_SCALE); /* Sparse range to choose from */

        /* Allocate temporary index array */
        tmp_idx = (hsize_t *)HDmalloc(sizeof(hsize_t) * (size_t)sparse_cnt);
        HDassert(tmp_idx);

        /* Initialize temporary index array, for shuffling */
        for (u = 0; u < (size_t)sparse_cnt; u++)
            tmp_idx[u] = (hsize_t)u;

        /* Shuffle index elements & store in final array */
        for (u = 0; u < (size_t)cnt; u++) {
            size_t swap_idx; /* Location to swap with when shuffling */

            swap_idx          = ((size_t)HDrandom() % ((size_t)sparse_cnt - u)) + u;
            eiter->idx[u]     = tmp_idx[swap_idx];
            tmp_idx[swap_idx] = tmp_idx[u];
        } /* end for */

        /* Release temporary array */
        HDfree(tmp_idx);
    } /* end if */
    else {
        for (u = 0; u < (size_t)cnt; u++)
            eiter->idx[u] = (hsize_t)u;
    } /* end else */

    /* Return iteration object */
    return (eiter);
} /* end eiter_rnd2_init() */

/* Extensible array iterator class for random iteration */
static const earray_iter_t ea_iter_rnd2 = {
    eiter_rnd2_init, /* Iterator init */
    eiter_rnd_next,  /* Next array index */
    eiter_rnd_max,   /* Max. array index written */
    NULL,            /* State of the extensible array */
    eiter_rnd_term   /* Iterator term */
};

/* Extensible array iterator info for cyclic iteration */
typedef struct eiter_cyc_t {
    hsize_t max; /* Max. array index used */
    hsize_t pos; /* Position in shuffled array */
    hsize_t cnt; /* # of elements to store */
    hsize_t cyc; /* Cycle of elements to choose from */
} eiter_cyc_t;

/*-------------------------------------------------------------------------
 * Function:    eiter_cyc_init
 *
 * Purpose:    Initialize element interator (cyclic iteration)
 *
 * Return:    Success:    Pointer to iteration status object
 *        Failure:    NULL
 *
 * Programmer:    Quincey Koziol
 *              Tuesday, November 11, 2008
 *
 *-------------------------------------------------------------------------
 */
static void *
eiter_cyc_init(const H5EA_create_t H5_ATTR_UNUSED *cparam, const earray_test_param_t H5_ATTR_UNUSED *tparam,
               hsize_t cnt)
{
    eiter_cyc_t *eiter; /* Cyclic element iteration object */

    /* Allocate space for the element iteration object */
    eiter = (eiter_cyc_t *)HDmalloc(sizeof(eiter_cyc_t));
    HDassert(eiter);

    /* Initialize reverse iteration info */
    eiter->max = 0;
    eiter->pos = 0;
    eiter->cnt = cnt;
    eiter->cyc = 0;

    /* Return iteration object */
    return (eiter);
} /* end eiter_cyc_init() */

/*-------------------------------------------------------------------------
 * Function:    eiter_cyc_next
 *
 * Purpose:    Get next element index (cyclic iteration)
 *
 * Return:    Success:    Non-negative
 *        Failure:    Negative
 *
 * Programmer:    Quincey Koziol
 *              Tuesday, November 11, 2008
 *
 *-------------------------------------------------------------------------
 */
static hssize_t
eiter_cyc_next(void *in_eiter)
{
    eiter_cyc_t *eiter = (eiter_cyc_t *)in_eiter;
    hssize_t     ret_val;

    /* Sanity check */
    HDassert(eiter);

    /* Get the next array index to test */
    ret_val = (hssize_t)eiter->pos;
    eiter->pos += EA_CYC_COUNT;
    if (eiter->pos >= eiter->cnt)
        eiter->pos = ++eiter->cyc;

    /* Check for new max. value */
    if ((hsize_t)ret_val > eiter->max)
        eiter->max = (hsize_t)ret_val;

    return (ret_val);
} /* end eiter_cyc_next() */

/*-------------------------------------------------------------------------
 * Function:    eiter_cyc_max
 *
 * Purpose:    Get max. element index (cyclic iteration)
 *
 * Return:    Success:    Non-negative
 *        Failure:    Negative
 *
 * Programmer:    Quincey Koziol
 *              Tuesday, November 11, 2008
 *
 *-------------------------------------------------------------------------
 */
static H5_ATTR_PURE hssize_t
eiter_cyc_max(const void *in_eiter)
{
    const eiter_cyc_t *eiter = (const eiter_cyc_t *)in_eiter;

    /* Sanity check */
    HDassert(eiter);

    /* Return the max. array index used */
    return ((hssize_t)eiter->max);
} /* end eiter_cyc_max() */

/*-------------------------------------------------------------------------
 * Function:    eiter_cyc_term
 *
 * Purpose:    Shut down element interator (cyclic iteration)
 *
 * Return:    Success:    0
 *        Failure:    -1
 *
 * Programmer:    Quincey Koziol
 *              Tuesday, November 11, 2008
 *
 *-------------------------------------------------------------------------
 */
static int
eiter_cyc_term(void *in_eiter)
{
    eiter_cyc_t *eiter = (eiter_cyc_t *)in_eiter;

    /* Sanity check */
    HDassert(eiter);

    /* Free iteration object */
    HDfree(eiter);

    return (0);
} /* end eiter_cyc_term() */

/* Extensible array iterator class for cyclic iteration */
static const earray_iter_t ea_iter_cyc = {
    eiter_cyc_init, /* Iterator init */
    eiter_cyc_next, /* Next array index */
    eiter_cyc_max,  /* Max. array index written */
    NULL,           /* State of the extensible array */
    eiter_cyc_term  /* Iterator term */
};

/*-------------------------------------------------------------------------
 * Function:    test_set_elmts
 *
 * Purpose:    Set all elements from 0 through 'nelmts' in extensible array
 *
 * Return:    Success:    0
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Thursday, September 22, 2008
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_set_elmts(hid_t fapl, H5EA_create_t *cparam, earray_test_param_t *tparam, hsize_t nelmts,
               const char *test_str)
{
    hid_t          file = -1;             /* File ID */
    H5F_t *        f    = NULL;           /* Internal file object pointer */
    H5EA_t *       ea   = NULL;           /* Extensible array wrapper */
    void *         eiter_info;            /* Extensible array iterator info */
    earray_state_t state;                 /* State of extensible array */
    uint64_t       welmt;                 /* Element to write */
    uint64_t       relmt;                 /* Element to read */
    hsize_t        nelmts_written;        /* Highest element written in array */
    hsize_t        cnt;                   /* Count of array indices */
    hssize_t       smax;                  /* Index value of max. element set */
    hsize_t        max;                   /* Index value of max. element set */
    hssize_t       sidx;                  /* Index value of first element of first data block */
    hsize_t        idx;                   /* Index value of first element of first data block */
    haddr_t        ea_addr = HADDR_UNDEF; /* Array address in file */

    /*
     * Display testing message
     */
    TESTING(test_str);

    /* Create file & retrieve pointer to internal file object */
    if (create_file(H5F_ACC_TRUNC, fapl, &file, &f) < 0)
        TEST_ERROR

    /* Create array */
    if (create_array(f, cparam, &ea, &ea_addr, NULL) < 0)
        TEST_ERROR

    /* Verify the creation parameters */
    if (verify_cparam(ea, cparam) < 0)
        TEST_ERROR

    /* Check for closing & re-opening the file */
    if (reopen_file(&file, &f, fapl, &ea, ea_addr, tparam) < 0)
        TEST_ERROR

    /* Verify high-water # of elements written */
    nelmts_written = (hsize_t)ULLONG_MAX;
    if (H5EA_get_nelmts(ea, &nelmts_written) < 0)
        FAIL_STACK_ERROR
    if (nelmts_written != 0)
        TEST_ERROR

    /* Verify array state */
    HDmemset(&state, 0, sizeof(state));
    state.hdr_size = EA_HDR_SIZE;
    if (check_stats(ea, &state))
        TEST_ERROR

    /* Get all elements from empty array */

    /* Initialize iterator */
    if (NULL == (eiter_info = tparam->eiter->init(cparam, tparam, nelmts)))
        TEST_ERROR

    /* Get elements of array */
    for (cnt = 0; cnt < nelmts; cnt++) {
        /* Get the array index */
        if ((sidx = tparam->eiter->next(eiter_info)) < 0)
            TEST_ERROR
        idx = (hsize_t)sidx;

        /* Retrieve element of array (not set yet) */
        relmt = (uint64_t)0;
        if (H5EA_get(ea, idx, &relmt) < 0)
            FAIL_STACK_ERROR

        /* Verify element is fill value for array */
        if (relmt != H5EA_TEST_FILL)
            TEST_ERROR
    } /* end for */

    /* Shutdown iterator */
    if (tparam->eiter->term(eiter_info) < 0)
        TEST_ERROR

    /* Set (& get) all elements from empty array */

    /* Initialize iterator */
    if (NULL == (eiter_info = tparam->eiter->init(cparam, tparam, nelmts)))
        TEST_ERROR

    /* Set elements of array */
    for (cnt = 0; cnt < nelmts; cnt++) {
        /* Get the array index */
        if ((sidx = tparam->eiter->next(eiter_info)) < 0)
            TEST_ERROR
        idx = (hsize_t)sidx;

        /* Retrieve element of array (not set yet) */
        relmt = (uint64_t)0;
        if (H5EA_get(ea, idx, &relmt) < 0)
            FAIL_STACK_ERROR

        /* Verify element is fill value for array */
        if (relmt != H5EA_TEST_FILL)
            TEST_ERROR

        /* Set element of array */
        welmt = (uint64_t)7 + idx;
        if (H5EA_set(ea, idx, &welmt) < 0)
            FAIL_STACK_ERROR

        /* Get the max. array index */
        if ((smax = tparam->eiter->max_elem(eiter_info)) < 0)
            TEST_ERROR
        max = (hsize_t)smax;

        /* Verify high-water # of elements written */
        nelmts_written = (hsize_t)ULLONG_MAX;
        if (H5EA_get_nelmts(ea, &nelmts_written) < 0)
            FAIL_STACK_ERROR
        if (nelmts_written != (max + 1))
            TEST_ERROR

        /* Check if array state is available */
        if (tparam->eiter->state) {
            /* Get the extensible array state */
            if (tparam->eiter->state(eiter_info, cparam, tparam, &state, idx) < 0)
                TEST_ERROR

            /* Verify array state */
            if (check_stats(ea, &state))
                TEST_ERROR
        } /* end if */

        /* Retrieve element of array (set now) */
        relmt = (uint64_t)0;
        if (H5EA_get(ea, idx, &relmt) < 0)
            FAIL_STACK_ERROR

        /* Verify element is value written */
        if (relmt != welmt)
            TEST_ERROR
    } /* end for */

    /* Shutdown iterator */
    if (tparam->eiter->term(eiter_info) < 0)
        TEST_ERROR

    /* Close array, delete array, close file & verify file is empty */
    if (finish(file, fapl, f, ea, ea_addr) < 0)
        TEST_ERROR

    /* All tests passed */
    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (ea)
            H5EA_close(ea);
        H5Fclose(file);
    }
    H5E_END_TRY;

    return 1;
} /* test_set_elmts() */

/*-------------------------------------------------------------------------
 * Function:    test_skip_elmts
 *
 * Purpose:    Skip some elements when writing element
 *
 * Return:    Success:    0
 *        Failure:    1
 *
 * Programmer:    Quincey Koziol
 *              Tuesday, November 11, 2008
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_skip_elmts(hid_t fapl, H5EA_create_t *cparam, earray_test_param_t *tparam, hsize_t skip_elmts,
                const char *test_str)
{
    hid_t          file = -1;             /* File ID */
    H5F_t *        f    = NULL;           /* Internal file object pointer */
    H5EA_t *       ea   = NULL;           /* Extensible array wrapper */
    earray_state_t state;                 /* State of extensible array */
    uint64_t       welmt;                 /* Element to write */
    uint64_t       relmt;                 /* Element to read */
    hsize_t        nelmts_written;        /* Highest element written in array */
    hsize_t        idx;                   /* Index value of element to get */
    hsize_t        cnt;                   /* Count of array indices */
    haddr_t        ea_addr = HADDR_UNDEF; /* Array address in file */

    /*
     * Display testing message
     */
    TESTING(test_str);

    /* Create file & retrieve pointer to internal file object */
    if (create_file(H5F_ACC_TRUNC, fapl, &file, &f) < 0)
        TEST_ERROR

    /* Create array */
    if (create_array(f, cparam, &ea, &ea_addr, NULL) < 0)
        TEST_ERROR

    /* Verify the creation parameters */
    if (verify_cparam(ea, cparam) < 0)
        TEST_ERROR

    /* Check for closing & re-opening the file */
    if (reopen_file(&file, &f, fapl, &ea, ea_addr, tparam) < 0)
        TEST_ERROR

    /* Verify high-water # of elements written */
    nelmts_written = (hsize_t)ULLONG_MAX;
    if (H5EA_get_nelmts(ea, &nelmts_written) < 0)
        FAIL_STACK_ERROR
    if (nelmts_written != 0)
        TEST_ERROR

    /* Verify array state */
    HDmemset(&state, 0, sizeof(state));
    state.hdr_size = EA_HDR_SIZE;
    if (check_stats(ea, &state))
        TEST_ERROR

    /* Set (& get) element after skipping elements */
    idx = skip_elmts;

    /* Retrieve element of array (not set yet) */
    relmt = (uint64_t)0;
    if (H5EA_get(ea, idx, &relmt) < 0)
        FAIL_STACK_ERROR

    /* Verify element is fill value for array */
    if (relmt != H5EA_TEST_FILL)
        TEST_ERROR

    /* Set element of array */
    welmt = (uint64_t)7 + idx;
    if (H5EA_set(ea, idx, &welmt) < 0)
        FAIL_STACK_ERROR

    /* Verify high-water # of elements written */
    nelmts_written = (hsize_t)ULLONG_MAX;
    if (H5EA_get_nelmts(ea, &nelmts_written) < 0)
        FAIL_STACK_ERROR
    if (nelmts_written != (idx + 1))
        TEST_ERROR

    /* Set array state */
    HDmemset(&state, 0, sizeof(state));
    state.hdr_size       = EA_HDR_SIZE;
    state.nindex_blks    = 1;
    state.index_blk_size = EA_IBLOCK_SIZE;
    state.max_idx_set    = idx + 1;
    if (1 == skip_elmts) {
        state.nelmts      = (hsize_t)cparam->idx_blk_elmts;
        state.nsuper_blks = state.ndata_blks = (hsize_t)0;
    } /* end if */
    else if (cparam->idx_blk_elmts == skip_elmts) {
        state.nelmts      = (hsize_t)cparam->idx_blk_elmts + cparam->data_blk_min_elmts;
        state.ndata_blks  = (hsize_t)1;
        state.nsuper_blks = (hsize_t)0;
    } /* end if */
    else {
        unsigned sblk_idx; /* Which superblock does this index fall in? */

        /* Compute super block index for element index */
        /* (same eqn. as in H5EA__dblock_sblk_idx()) */
        sblk_idx =
            H5VM_log2_gen((uint64_t)(((idx - cparam->idx_blk_elmts) / cparam->data_blk_min_elmts) + 1));
        state.nelmts      = (hsize_t)cparam->idx_blk_elmts + tparam->sblk_info[sblk_idx].dblk_nelmts;
        state.ndata_blks  = (hsize_t)1;
        state.nsuper_blks = (hsize_t)1;
    } /* end if */

    /* Verify array state */
    if (check_stats(ea, &state))
        TEST_ERROR

    /* Retrieve element of array (set now) */
    relmt = (uint64_t)0;
    if (H5EA_get(ea, idx, &relmt) < 0)
        FAIL_STACK_ERROR

    /* Verify element is value written */
    if (relmt != welmt)
        TEST_ERROR

    /* Get unset elements of array */
    for (cnt = 0; cnt < skip_elmts; cnt++) {
        /* Retrieve element of array (not set yet) */
        relmt = (uint64_t)0;
        if (H5EA_get(ea, cnt, &relmt) < 0)
            FAIL_STACK_ERROR

        /* Verify element is fill value for array */
        if (relmt != H5EA_TEST_FILL)
            TEST_ERROR
    } /* end for */

    /* Close array, delete array, close file & verify file is empty */
    if (finish(file, fapl, f, ea, ea_addr) < 0)
        TEST_ERROR

    /* All tests passed */
    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (ea)
            H5EA_close(ea);
        H5Fclose(file);
    }
    H5E_END_TRY;

    return 1;
} /* test_skip_elmts() */

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:    Test the extensible array code
 *
 * Return:    Success: 0
 *        Failure: 1
 *
 * Programmer:    Quincey Koziol
 *              Tuesday, June 17, 2008
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    H5EA_create_t       cparam;                 /* Creation parameters for extensible array */
    earray_test_param_t tparam;                 /* Testing parameters */
    earray_test_type_t  curr_test;              /* Current test being worked on */
    earray_iter_type_t  curr_iter;              /* Current iteration type being worked on */
    hid_t               fapl    = -1;           /* File access property list for data files */
    unsigned            nerrors = 0;            /* Cumulative error count */
    time_t              curr_time;              /* Current time, for seeding random number generator */
    int                 ExpressMode;            /* Test express value */
    hbool_t             api_ctx_pushed = FALSE; /* Whether API context pushed */

    /* Reset library */
    h5_reset();
    fapl        = h5_fileaccess();
    ExpressMode = GetTestExpress();
    if (ExpressMode > 1)
        HDprintf("***Express test mode on.  Some tests may be skipped\n");

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename_g, sizeof(filename_g));

    /* Push API context */
    if (H5CX_push() < 0)
        FAIL_STACK_ERROR
    api_ctx_pushed = TRUE;

    /* Seed random #'s */
    curr_time = HDtime(NULL);
    HDsrandom((unsigned)curr_time);

    /* Create an empty file to retrieve size */
    {
        hid_t file; /* File ID */

        if ((file = H5Fcreate(filename_g, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Close file */
        if (H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of a file w/no array */
        if ((empty_size_g = h5_get_file_size(filename_g, fapl)) < 0)
            TEST_ERROR
    }

    /* Initialize extensible array creation parameters */
    init_cparam(&cparam);

    /* Iterate over the testing parameters */
    for (curr_test = EARRAY_TEST_NORMAL; curr_test < EARRAY_TEST_NTESTS; curr_test++) {

        /* Initialize the testing parameters */
        init_tparam(&tparam, &cparam);

        /* Set appropriate testing parameters for each test */
        switch (curr_test) {
            /* "Normal" testing parameters */
            case EARRAY_TEST_NORMAL:
                HDputs("Testing with normal parameters");
                break;

            /* "Re-open array" testing parameters */
            case EARRAY_TEST_REOPEN:
                HDputs("Testing with reopen array flag set");
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
        nerrors += test_open_twice_diff(fapl, &cparam, &tparam);
        nerrors += test_delete_open(fapl, &cparam, &tparam);

        /* Iterate over the type of capacity tests */
        for (curr_iter = EARRAY_ITER_FW; curr_iter < EARRAY_ITER_NITERS; curr_iter++) {
            hsize_t sblk;          /* Super block index */
            hsize_t dblk;          /* Data block index */
            hsize_t nelmts;        /* # of elements to test */
            char    test_str[128]; /* String for describing test */
            hsize_t ndblks;        /* # of data blocks tested */

            /* Set appropriate parameters for each type of iteration */
            switch (curr_iter) {
                /* "Forward" testing parameters */
                case EARRAY_ITER_FW:
                    HDputs("Testing with forward iteration");
                    tparam.eiter = &ea_iter_fw;
                    break;

                /* "Reverse" testing parameters */
                case EARRAY_ITER_RV:
                    HDputs("Testing with reverse iteration");
                    tparam.eiter = &ea_iter_rv;
                    break;

                /* "Random" testing parameters */
                case EARRAY_ITER_RND:
                    HDputs("Testing with random iteration");
                    tparam.eiter = &ea_iter_rnd;
                    break;

                /* "Random #2" testing parameters */
                case EARRAY_ITER_RND2:
                    HDputs("Testing with random #2 iteration");
                    tparam.eiter = &ea_iter_rnd2;
                    break;

                /* "Cyclic" testing parameters */
                case EARRAY_ITER_CYC:
                    HDputs("Testing with cyclic iteration");
                    tparam.eiter = &ea_iter_cyc;
                    break;

                /* An unknown iteration? */
                case EARRAY_ITER_NITERS:
                default:
                    goto error;
            } /* end switch */

            /* Basic capacity tests */
            nerrors += test_set_elmts(fapl, &cparam, &tparam, (hsize_t)1, "setting first element of array");
            nerrors += test_set_elmts(fapl, &cparam, &tparam, (hsize_t)cparam.idx_blk_elmts,
                                      "setting index block elements of array");

            /* Super Block capacity tests */
            ndblks = 0;
            for (sblk = 0; sblk < 9; sblk++) {
                for (dblk = 0; dblk < tparam.sblk_info[sblk].ndblks; dblk++) {
                    /* Test first element in data block */
                    nelmts = (hsize_t)((hsize_t)1 + cparam.idx_blk_elmts + tparam.sblk_info[sblk].start_idx +
                                       (tparam.sblk_info[sblk].dblk_nelmts * dblk));
                    HDsprintf(test_str, "setting first element of array's data block #%llu",
                              (unsigned long long)ndblks);
                    nerrors += test_set_elmts(fapl, &cparam, &tparam, nelmts, test_str);

                    /* Test all elements in data block */
                    nelmts = (hsize_t)(cparam.idx_blk_elmts + tparam.sblk_info[sblk].start_idx +
                                       (tparam.sblk_info[sblk].dblk_nelmts * (dblk + 1)));
                    HDsprintf(test_str, "setting all elements of array's data block #%llu",
                              (unsigned long long)ndblks);
                    nerrors += test_set_elmts(fapl, &cparam, &tparam, nelmts, test_str);

                    /* Increment data block being tested */
                    ndblks++;
                } /* end for */
            }     /* end for */
        }         /* end for */

        /* Check skipping elements */
        nerrors += test_skip_elmts(fapl, &cparam, &tparam, (hsize_t)1, "skipping 1st element");
        nerrors += test_skip_elmts(fapl, &cparam, &tparam, (hsize_t)cparam.idx_blk_elmts,
                                   "skipping index block elements");
        nerrors += test_skip_elmts(fapl, &cparam, &tparam,
                                   (hsize_t)(cparam.idx_blk_elmts + (15 * cparam.data_blk_min_elmts) + 1),
                                   "skipping index block & data block elements");
        nerrors += test_skip_elmts(fapl, &cparam, &tparam,
                                   (hsize_t)(cparam.idx_blk_elmts + (31 * cparam.data_blk_min_elmts) + 1),
                                   "skipping 1st super block elements");
        nerrors += test_skip_elmts(fapl, &cparam, &tparam,
                                   (hsize_t)(cparam.idx_blk_elmts + (63 * cparam.data_blk_min_elmts) + 1),
                                   "skipping 2nd super block elements");
        nerrors += test_skip_elmts(fapl, &cparam, &tparam,
                                   (hsize_t)(cparam.idx_blk_elmts + (127 * cparam.data_blk_min_elmts) + 1),
                                   "skipping 3rd super block elements");
        nerrors += test_skip_elmts(fapl, &cparam, &tparam,
                                   (hsize_t)(cparam.idx_blk_elmts + (255 * cparam.data_blk_min_elmts) + 1),
                                   "skipping 4th super block elements");

        /* Close down testing parameters */
        finish_tparam(&tparam);
    } /* end for */

    /* Verify symbol table messages are cached */
    nerrors += (h5_verify_cached_stabs(FILENAME, fapl) < 0 ? 1 : 0);

    /* Pop API context */
    if (api_ctx_pushed && H5CX_pop(FALSE) < 0)
        FAIL_STACK_ERROR
    api_ctx_pushed = FALSE;

    if (nerrors)
        goto error;
    HDputs("All extensible array tests passed.");

    /* Clean up file used */
    h5_cleanup(FILENAME, fapl);

    return 0;

error:
    HDputs("*** TESTS FAILED ***");

    H5E_BEGIN_TRY
    {
        H5Pclose(fapl);
    }
    H5E_END_TRY;

    if (api_ctx_pushed)
        H5CX_pop(FALSE);

    return 1;
} /* end main() */
