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


/* Local macros */

/* Max. testfile name length */
#define EARRAY_FILENAME_LEN     1024

/* Extensible array creation values */
#define ELMT_SIZE               sizeof(uint64_t)
#define MAX_NELMTS_BITS         32
#define IDX_BLK_ELMTS           4
#define SUP_BLK_MIN_DATA_PTRS   4
#define DATA_BLK_MIN_ELMTS      16

/* Local typedefs */

/* Types of tests to perform */
typedef enum {
    EARRAY_TEST_NORMAL,         /* "Normal" test, with no testing parameters set */
    EARRAY_TEST_REOPEN,         /* Set the reopen_array flag */
    EARRAY_TEST_NTESTS          /* The number of test types, must be last */
} earray_test_type_t;

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

/* Testing parameters */
typedef struct earray_test_param_t {
    earray_test_type_t reopen_array;    /* Whether to re-open the array during the test */
    earray_test_comp_t comp;            /* Whether to compress the blocks or not */
} earray_test_param_t;

/* Extensible array state information */
typedef struct earray_state_t {
    hsize_t     nsuper_blks;            /* # of super blocks */
    hsize_t     ndata_blks;             /* # of data blocks */
} earray_state_t;

/* Local variables */
const char *FILENAME[] = {
    "earray",
    NULL
};

/* Filename to use for all tests */
char filename_g[EARRAY_FILENAME_LEN];

/* Empty file size */
h5_stat_size_t empty_size_g;


/* Local prototypes */


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

    return(0);
} /* init_cparam() */


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
create_file(hid_t fapl, hid_t *file, H5F_t **f)
{
    /* Create the file to work on */
    if((*file = H5Fcreate(filename_g, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (*f = (H5F_t *)H5I_object(*file)))
        FAIL_STACK_ERROR

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
    if(earray_stats.nsuper_blks != state->nsuper_blks) {
        HDfprintf(stdout, "earray_stats.nsuper_blks = %Hu, state->nsuper_blks = %Hu\n", earray_stats.nsuper_blks, state->nsuper_blks);
        TEST_ERROR
    } /* end if */
    if(earray_stats.ndata_blks != state->ndata_blks) {
        HDfprintf(stdout, "earray_stats.ndata_blks = %Hu, state->ndata_blks = %Hu\n", earray_stats.ndata_blks, state->ndata_blks);
        TEST_ERROR
    } /* end if */

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
    H5EA_t **ea, haddr_t ea_addr, const H5EA_class_t *ea_cls,
    const earray_test_param_t *tparam)
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

        /* Re-open array, if given */
        if(ea) {
            if(NULL == (*ea = H5EA_open(*f, dxpl, ea_addr, ea_cls)))
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
    H5EA_t **ea, haddr_t *ea_addr)
{
    hsize_t     nelmts;                 /* Number of elements in array */
    earray_state_t state;               /* State of extensible array */

    /* Create array */
    if(NULL == (*ea = H5EA_create(f, dxpl, cparam)))
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
 * Function:	shutdown
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
shutdown(hid_t file, H5F_t *f, H5EA_t *ea, haddr_t ea_addr)
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
    if(H5EA_delete(f, H5P_DATASET_XFER_DEFAULT, ea_addr) < 0)
        FAIL_STACK_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR


    /* Get the size of the file */
    if((file_size = h5_get_file_size(filename_g)) < 0)
        TEST_ERROR

    /* Verify the file is correct size */
    if(file_size != empty_size_g)
        TEST_ERROR

    /* Success */
    return(0);

error:
    return(-1);
} /* shutdown() */


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
    if(create_file(fapl, &file, &f) < 0)
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
        ea = H5EA_create(f, H5P_DATASET_XFER_DEFAULT, &test_cparam);
    } H5E_END_TRY;
    if(ea) {
        /* Close opened extensible array */
        H5EA_close(ea, H5P_DATASET_XFER_DEFAULT);

        /* Indicate error */
        TEST_ERROR
    } /* end if */

    /* Set invalid max. # of elements bits */
    HDmemcpy(&test_cparam, cparam, sizeof(test_cparam));
    test_cparam.max_nelmts_bits = 0;
    H5E_BEGIN_TRY {
        ea = H5EA_create(f, H5P_DATASET_XFER_DEFAULT, &test_cparam);
    } H5E_END_TRY;
    if(ea) {
        /* Close opened extensible array */
        H5EA_close(ea, H5P_DATASET_XFER_DEFAULT);

        /* Indicate error */
        TEST_ERROR
    } /* end if */

    HDmemcpy(&test_cparam, cparam, sizeof(test_cparam));
    test_cparam.max_nelmts_bits = 65;
    H5E_BEGIN_TRY {
        ea = H5EA_create(f, H5P_DATASET_XFER_DEFAULT, &test_cparam);
    } H5E_END_TRY;
    if(ea) {
        /* Close opened extensible array */
        H5EA_close(ea, H5P_DATASET_XFER_DEFAULT);

        /* Indicate error */
        TEST_ERROR
    } /* end if */

    /* Set invalid min. # of data block pointers in super blocks */
    HDmemcpy(&test_cparam, cparam, sizeof(test_cparam));
    test_cparam.sup_blk_min_data_ptrs = 0;
    H5E_BEGIN_TRY {
        ea = H5EA_create(f, H5P_DATASET_XFER_DEFAULT, &test_cparam);
    } H5E_END_TRY;
    if(ea) {
        /* Close opened extensible array */
        H5EA_close(ea, H5P_DATASET_XFER_DEFAULT);

        /* Indicate error */
        TEST_ERROR
    } /* end if */

    /* Set invalid min. # of elements per data block */
    HDmemcpy(&test_cparam, cparam, sizeof(test_cparam));
    test_cparam.data_blk_min_elmts = 0;
    H5E_BEGIN_TRY {
        ea = H5EA_create(f, H5P_DATASET_XFER_DEFAULT, &test_cparam);
    } H5E_END_TRY;
    if(ea) {
        /* Close opened extensible array */
        H5EA_close(ea, H5P_DATASET_XFER_DEFAULT);

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
    if(create_array(f, H5P_DATASET_XFER_DEFAULT, cparam, &ea, &ea_addr) < 0)
        TEST_ERROR

    PASSED()

    /* Verify the creation parameters */
    TESTING("verify array creation parameters");

    /* Verify the creation parameters */
    if(verify_cparam(ea, cparam) < 0)
        TEST_ERROR

    /* Close array, delete array, close file & verify file is empty */
    if(shutdown(file, f, ea, ea_addr) < 0)
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
    if(create_file(fapl, &file, &f) < 0)
        TEST_ERROR

    /*
     * Display testing message
     */
    TESTING("create, close & reopen extensible array");

    /* Create array */
    if(create_array(f, H5P_DATASET_XFER_DEFAULT, cparam, &ea, &ea_addr) < 0)
        TEST_ERROR

    /* Close the extensible array */
    if(H5EA_close(ea, H5P_DATASET_XFER_DEFAULT) < 0)
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the file */
    if(reopen_file(&file, &f, fapl, H5P_DATASET_XFER_DEFAULT, NULL, HADDR_UNDEF, NULL, tparam) < 0)
        TEST_ERROR

    /* Re-open the array */
    if(NULL == (ea = H5EA_open(f, H5P_DATASET_XFER_DEFAULT, ea_addr, cparam->cls)))
        FAIL_STACK_ERROR

    /* Verify the creation parameters */
    if(verify_cparam(ea, cparam) < 0)
        TEST_ERROR

    /* Close array, delete array, close file & verify file is empty */
    if(shutdown(file, f, ea, ea_addr) < 0)
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
    if(create_file(fapl, &file, &f) < 0)
        TEST_ERROR

    /*
     * Display testing message
     */
    TESTING("open extensible array twice");

    /* Create array */
    if(create_array(f, H5P_DATASET_XFER_DEFAULT, cparam, &ea, &ea_addr) < 0)
        TEST_ERROR

    /* Open the array again, through the first file handle */
    if(NULL == (ea2 = H5EA_open(f, H5P_DATASET_XFER_DEFAULT, ea_addr, cparam->cls)))
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
    if(reopen_file(&file, &f, fapl, H5P_DATASET_XFER_DEFAULT, &ea, ea_addr, cparam->cls, tparam) < 0)
        TEST_ERROR

    /* Re-open the file */
    if((file2 = H5Freopen(file)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f2 = (H5F_t *)H5I_object(file2)))
        FAIL_STACK_ERROR

    /* Open the extensible array through the second file handle */
    if(NULL == (ea2 = H5EA_open(f2, H5P_DATASET_XFER_DEFAULT, ea_addr, cparam->cls)))
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
    if(shutdown(file2, f2, ea2, ea_addr) < 0)
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
    if(create_file(fapl, &file, &f) < 0)
        TEST_ERROR

    /*
     * Display testing message
     */
    TESTING("deleting open extensible array");

    /* Create array */
    if(create_array(f, H5P_DATASET_XFER_DEFAULT, cparam, &ea, &ea_addr) < 0)
        TEST_ERROR

    /* Open the array again */
    if(NULL == (ea2 = H5EA_open(f, H5P_DATASET_XFER_DEFAULT, ea_addr, cparam->cls)))
        FAIL_STACK_ERROR

    /* Request that the array be deleted */
    if(H5EA_delete(f, H5P_DATASET_XFER_DEFAULT, ea_addr) < 0)
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
        ea2 = H5EA_open(f, H5P_DATASET_XFER_DEFAULT, ea_addr, cparam->cls);
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
    if(reopen_file(&file, &f, fapl, H5P_DATASET_XFER_DEFAULT, NULL, HADDR_UNDEF, NULL, tparam) < 0)
        TEST_ERROR

    /* Try re-opening the array again (should fail, as array is now deleted) */
    H5E_BEGIN_TRY {
        ea = H5EA_open(f, H5P_DATASET_XFER_DEFAULT, ea_addr, cparam->cls);
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
    if((file_size = h5_get_file_size(filename_g)) < 0)
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
 * Function:	test_set_first
 *
 * Purpose:	Set first element in extensible array
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
test_set_first(hid_t fapl, H5EA_create_t *cparam, earray_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5EA_t      *ea = NULL;             /* Extensible array wrapper */
    earray_state_t state;               /* State of extensible array */
    uint64_t    welmt;                  /* Element to write */
    uint64_t    relmt;                  /* Element to read */
    hsize_t     nelmts;                 /* Highest element written in array */
    haddr_t     ea_addr = HADDR_UNDEF;  /* Array address in file */

    /*
     * Display testing message
     */
    TESTING("setting first element of array");

    /* Check for elements in index block */
    if(cparam->idx_blk_elmts > 0) {
        /* Create file & retrieve pointer to internal file object */
        if(create_file(fapl, &file, &f) < 0)
            TEST_ERROR

        /* Create array */
        if(create_array(f, H5P_DATASET_XFER_DEFAULT, cparam, &ea, &ea_addr) < 0)
            TEST_ERROR

        /* Verify the creation parameters */
        if(verify_cparam(ea, cparam) < 0)
            TEST_ERROR

        /* Check for closing & re-opening the file */
        if(reopen_file(&file, &f, fapl, H5P_DATASET_XFER_DEFAULT, &ea, ea_addr, cparam->cls, tparam) < 0)
            TEST_ERROR

        /* Verify high-water # of elements written */
        nelmts = (hsize_t)ULLONG_MAX;
        if(H5EA_get_nelmts(ea, &nelmts) < 0)
            FAIL_STACK_ERROR
        if(nelmts != 0)
            TEST_ERROR

        /* Verify array state */
        HDmemset(&state, 0, sizeof(state));
        if(check_stats(ea, &state))
            TEST_ERROR

        /* Retrieve first element of array (not set yet) */
        relmt = (uint64_t)0;
        if(H5EA_get(ea, H5P_DATASET_XFER_DEFAULT, (hsize_t)0, &relmt) < 0)
            FAIL_STACK_ERROR

        /* Verify first element is fill value for array */
        if(relmt != H5EA_TEST_FILL)
            TEST_ERROR

        /* Set first element of array */
        welmt = (uint64_t)7;
        if(H5EA_set(ea, H5P_DATASET_XFER_DEFAULT, (hsize_t)0, &welmt) < 0)
            FAIL_STACK_ERROR

        /* Verify high-water # of elements written */
        nelmts = (hsize_t)ULLONG_MAX;
        if(H5EA_get_nelmts(ea, &nelmts) < 0)
            FAIL_STACK_ERROR
        if(nelmts != 1)
            TEST_ERROR

        /* Verify array state */
        HDmemset(&state, 0, sizeof(state));
        if(check_stats(ea, &state))
            TEST_ERROR

        /* Retrieve first element of array (set now) */
        relmt = (uint64_t)0;
        if(H5EA_get(ea, H5P_DATASET_XFER_DEFAULT, (hsize_t)0, &relmt) < 0)
            FAIL_STACK_ERROR

        /* Verify first element is value written */
        if(relmt != welmt)
            TEST_ERROR

        /* Close array, delete array, close file & verify file is empty */
        if(shutdown(file, f, ea, ea_addr) < 0)
            TEST_ERROR

        /* All tests passed */
        PASSED()
    } /* end if */
    else {
        SKIPPED();
        puts("    No elements stored in index block");
    } /* end else */

    return 0;

error:
    H5E_BEGIN_TRY {
        if(ea)
            H5EA_close(ea, H5P_DATASET_XFER_DEFAULT);
	H5Fclose(file);
    } H5E_END_TRY;

    return 1;
} /* test_set_first() */


/*-------------------------------------------------------------------------
 * Function:	test_set_iblock
 *
 * Purpose:	Set all elements in extensible array's index block
 *
 * Return:	Success:	0
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Thursday, September 11, 2008
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_set_iblock(hid_t fapl, H5EA_create_t *cparam, earray_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5EA_t      *ea = NULL;             /* Extensible array wrapper */
    earray_state_t state;               /* State of extensible array */
    uint64_t    welmt;                  /* Element to write */
    uint64_t    relmt;                  /* Element to read */
    hsize_t     nelmts;                 /* Highest element written in array */
    haddr_t     ea_addr = HADDR_UNDEF;  /* Array address in file */

    /*
     * Display testing message
     */
    TESTING("setting index block elements of array");

    /* Check for elements in index block */
    if(cparam->idx_blk_elmts > 0) {
        unsigned u;             /* Local index variable */

        /* Create file & retrieve pointer to internal file object */
        if(create_file(fapl, &file, &f) < 0)
            TEST_ERROR

        /* Create array */
        if(create_array(f, H5P_DATASET_XFER_DEFAULT, cparam, &ea, &ea_addr) < 0)
            TEST_ERROR

        /* Verify the creation parameters */
        if(verify_cparam(ea, cparam) < 0)
            TEST_ERROR

        /* Check for closing & re-opening the file */
        if(reopen_file(&file, &f, fapl, H5P_DATASET_XFER_DEFAULT, &ea, ea_addr, cparam->cls, tparam) < 0)
            TEST_ERROR

        /* Verify high-water # of elements written */
        nelmts = (hsize_t)ULLONG_MAX;
        if(H5EA_get_nelmts(ea, &nelmts) < 0)
            FAIL_STACK_ERROR
        if(nelmts != 0)
            TEST_ERROR

        /* Verify array state */
        HDmemset(&state, 0, sizeof(state));
        if(check_stats(ea, &state))
            TEST_ERROR

        /* Retrieve elements of array in index block (not set yet) */
        for(u = 0; u < cparam->idx_blk_elmts; u++) {
            relmt = (uint64_t)0;
            if(H5EA_get(ea, H5P_DATASET_XFER_DEFAULT, (hsize_t)u, &relmt) < 0)
                FAIL_STACK_ERROR

            /* Verify first element is fill value for array */
            if(relmt != H5EA_TEST_FILL)
                TEST_ERROR
        } /* end for */

        /* Set elements of array in index block */
        for(u = 0; u < cparam->idx_blk_elmts; u++) {
            welmt = (uint64_t)(7 + u);
            if(H5EA_set(ea, H5P_DATASET_XFER_DEFAULT, (hsize_t)u, &welmt) < 0)
                FAIL_STACK_ERROR

            /* Verify high-water # of elements written */
            nelmts = (hsize_t)ULLONG_MAX;
            if(H5EA_get_nelmts(ea, &nelmts) < 0)
                FAIL_STACK_ERROR
            if(nelmts != (u + 1))
                TEST_ERROR

            /* Verify array state */
            HDmemset(&state, 0, sizeof(state));
            if(check_stats(ea, &state))
                TEST_ERROR

            /* Retrieve first element of array (set now) */
            relmt = (uint64_t)0;
            if(H5EA_get(ea, H5P_DATASET_XFER_DEFAULT, (hsize_t)u, &relmt) < 0)
                FAIL_STACK_ERROR

            /* Verify first element is value written */
            if(relmt != welmt)
                TEST_ERROR
        } /* end for */

        /* Close array, delete array, close file & verify file is empty */
        if(shutdown(file, f, ea, ea_addr) < 0)
            TEST_ERROR

        /* All tests passed */
        PASSED()
    } /* end if */
    else {
        SKIPPED();
        puts("    No elements stored in index block");
    } /* end else */

    return 0;

error:
    H5E_BEGIN_TRY {
        if(ea)
            H5EA_close(ea, H5P_DATASET_XFER_DEFAULT);
	H5Fclose(file);
    } H5E_END_TRY;

    return 1;
} /* test_set_iblock() */


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
    hid_t	fapl = -1;              /* File access property list for data files */
    unsigned	nerrors = 0;            /* Cumulative error count */
    int		ExpressMode;            /* Test express value */
    const char *envval;                 /* File Driver value from environment */

    /* Reset library */
    h5_reset();
    fapl = h5_fileaccess();
    ExpressMode = GetTestExpress();
    if(ExpressMode > 1)
	printf("***Express test mode on.  Some tests may be skipped\n");

    if(NULL == (envval = HDgetenv("HDF5_DRIVER")))
        envval = "nomatch";

    if(HDstrcmp(envval, "core") && HDstrcmp(envval, "split") && HDstrcmp(envval, "multi") && 
            HDstrcmp(envval, "family")) {
        /* Set the filename to use for this test (dependent on fapl) */
        h5_fixname(FILENAME[0], fapl, filename_g, sizeof(filename_g));


        /* Create an empty file to retrieve size */
        {
            hid_t	file;              /* File ID */

            if((file = H5Fcreate(filename_g, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
                FAIL_STACK_ERROR

            /* Close file */
            if(H5Fclose(file) < 0)
                FAIL_STACK_ERROR

            /* Get the size of a file w/no array */
            if((empty_size_g = h5_get_file_size(filename_g)) < 0)
                TEST_ERROR
        }


        /* Initialize extensible array creation parameters */
        init_cparam(&cparam);

        /* Iterate over the testing parameters */
        for(curr_test = EARRAY_TEST_NORMAL; curr_test < EARRAY_TEST_NTESTS; curr_test++) {

            /* Clear the testing parameters */
            HDmemset(&tparam, 0, sizeof(tparam));

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
                default:
                    goto error;
            } /* end switch */

            /* Basic capability tests */
            nerrors += test_create(fapl, &cparam, &tparam);
            nerrors += test_reopen(fapl, &cparam, &tparam);
            nerrors += test_open_twice(fapl, &cparam, &tparam);
            nerrors += test_delete_open(fapl, &cparam, &tparam);

            /* Basic capacity tests */
            nerrors += test_set_first(fapl, &cparam, &tparam);
            nerrors += test_set_iblock(fapl, &cparam, &tparam);
        } /* end for */

        if(nerrors)
            goto error;
        puts("All extensible array tests passed.");
    } /* end if(HDstrcmp(envval=="...")) */
    else
        printf("All extensible array tests skipped - Incompatible with current Virtual File Driver\n");

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

