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

/*
 * This file is modified based on earray.c.
 */
#include "h5test.h"

/*
 * This file needs to access private datatypes from the H5FA package.
 * This file also needs to access the fixed array testing code.
 */
#define H5FA_FRIEND		/*suppress error about including H5FApkg	  */
#define H5FA_TESTING
#include "H5FApkg.h"		/* Fixed Arrays			*/

/* Other private headers that this test requires */
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5VMprivate.h"		/* Vectors and arrays 			*/


/* Local macros */

/* Max. testfile name length */
#define FARRAY_FILENAME_LEN     1024

/* Fixed array creation values */
#define ELMT_SIZE      	sizeof(uint64_t)
#define MAX_DBLOCK_PAGE_NELMTS_BITS     10      /* 2^10 = 1024 elements per data block page */

/* Testing # of elements in the Fixed Array */
#define TEST_NELMTS 	20000

/* Convenience macros for computing earray state */
#define FA_HDR_SIZE   	28   	/* hard-coded */
#define DBLOCK_PREFIX   18	/* hard-coded */

/* 4 giga-elements: max chunk size */
#define MAX_NELMTS 	((unsigned long long)4*1024*1024*1024) /* 4 giga-elements */

/* Iterator parameter values */
#define FA_CYC_COUNT            4


/* Local typedefs */

/* Types of tests to perform */
typedef enum {
    FARRAY_TEST_NORMAL,         /* "Normal" test, with no testing parameters set */
    FARRAY_TEST_REOPEN,         /* Set the reopen_array flag */
    FARRAY_TEST_NTESTS          /* The number of test types, must be last */
} farray_test_type_t;

/* Types of iteration to perform */
typedef enum {
    FARRAY_ITER_FW,             /* "Forward" iteration */
    FARRAY_ITER_RV,             /* "Reverse" iteration */
    FARRAY_ITER_RND,            /* "Random" iteration */
    FARRAY_ITER_CYC,            /* "Cyclic" iteration */
    FARRAY_ITER_NITERS          /* The number of iteration types, must be last */
} farray_iter_type_t;


/* Fixed array state information */
typedef struct farray_state_t {
    hsize_t hdr_size;           /* Size of header */
    hsize_t dblk_size;          /* Size of data block */
    hsize_t nelmts;             /* # of elements */
} farray_state_t;

/* Forward decl. */
typedef struct farray_test_param_t farray_test_param_t;

/* Fixed array iterator class */
typedef struct farray_iter_t {
    void *(*init)(const H5FA_create_t *cparam, const farray_test_param_t *tparam,
        hsize_t cnt);           /* Initialize/allocate iterator private info */
    hssize_t (*next)(void *info);       /* Get the next element to test */
    herr_t (*term)(void *info); /* Shutdown/free iterator private info */
} farray_iter_t;


/* Testing parameters */
struct farray_test_param_t {
    farray_test_type_t reopen_array;    /* Whether to re-open the array during the test */
    hsize_t	nelmts;			/* # of elements to set for the fixed array */
    const farray_iter_t *fiter;         /* Iterator to use for this test */
};

/* Local variables */
const char *FILENAME[] = {
    "farray",
    "farray_tmp",
    NULL
};

/* Filename to use for all tests */
char filename_g[FARRAY_FILENAME_LEN];

/* Empty file size */
h5_stat_size_t empty_size_g;


/*-------------------------------------------------------------------------
 * Function:	init_cparam
 *
 * Purpose:	Initialize array creation parameter structure
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 *-------------------------------------------------------------------------
 */
static int
init_cparam(H5FA_create_t *cparam, farray_test_param_t *tparam)
{
    /* Wipe out background */
    HDmemset(cparam, 0, sizeof(*cparam));

    cparam->cls = H5FA_CLS_TEST;
    cparam->raw_elmt_size = ELMT_SIZE;
    cparam->max_dblk_page_nelmts_bits = MAX_DBLOCK_PAGE_NELMTS_BITS;
    cparam->nelmts = tparam->nelmts;

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

    /* Ignore metadata tags in the file's cache */
    if(H5AC_ignore_tags(*f) < 0)
        FAIL_STACK_ERROR

    /* Success */
    return(0);

error:
    return(-1);
} /* create_file() */


/*-------------------------------------------------------------------------
 * Function:	check_stats
 *
 * Purpose:	Verify stats for a fixed array
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 *-------------------------------------------------------------------------
 */
static int
check_stats(const H5FA_t *fa, const farray_state_t *state)
{
    H5FA_stat_t farray_stats;           /* Statistics about the array */

    /* Get statistics for fixed array and verify they are correct */
    if(H5FA_get_stats(fa, &farray_stats) < 0)
        FAIL_STACK_ERROR

    /* Compare information */
    if(farray_stats.hdr_size != state->hdr_size) {
        HDfprintf(stdout, "farray_stats.hdr_size = %Hu, state->hdr_size = %Hu\n",
	    farray_stats.hdr_size, state->hdr_size);
        TEST_ERROR
    } /* end if */

    if(farray_stats.dblk_size != state->dblk_size) {
        HDfprintf(stdout, "farray_stats.dblk_size = %Hu, state->dblk_size = %Hu\n",
	    farray_stats.dblk_size, state->dblk_size);
        TEST_ERROR
    } /* end if */

    if(farray_stats.nelmts != state->nelmts) {
        HDfprintf(stdout, "farray_stats.nelmts = %Hu, state->nelmts = %Hu\n",
	    farray_stats.nelmts, state->nelmts);
        TEST_ERROR
    } /* end if */

    /* All tests passed */
    return(0);

error:
    return(-1);
} /* check_stats() */


/*-------------------------------------------------------------------------
 * Function:	set_fa_state
 *
 * Purpose:	Set the state of the Fixed Array
 *
 * Return:	does not fail
 *
 * Programmer:	Vailin Choi; 5th August, 2009
 *
 *-------------------------------------------------------------------------
 */
static int
set_fa_state(const H5FA_create_t *cparam, farray_state_t *state)
{
    size_t dblk_page_nelmts;			/* # of elements per page */

    /* Sanity check */
    HDassert(cparam);
    HDassert(state);

    /* Compute the state of the fixed array */
    state->hdr_size = FA_HDR_SIZE;
    state->nelmts = cparam->nelmts;

    dblk_page_nelmts = (size_t)1 << cparam->max_dblk_page_nelmts_bits;
    if(state->nelmts > dblk_page_nelmts) {
	size_t npages = (size_t)(((state->nelmts + dblk_page_nelmts) - 1) / dblk_page_nelmts);
	size_t dblk_page_init_size = (npages + 7) / 8;
	hsize_t checksum_size = npages * 4;

	state->dblk_size = DBLOCK_PREFIX + dblk_page_init_size + checksum_size +
			    state->nelmts * cparam->raw_elmt_size;
    } else
	state->dblk_size = DBLOCK_PREFIX + state->nelmts * cparam->raw_elmt_size;

    return(0);
} /* end set_fa_state() */


/*-------------------------------------------------------------------------
 * Function:	reopen_file
 *
 * Purpose:	Perform common "re-open" operations on file & array for testing
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 *-------------------------------------------------------------------------
 */
static int
reopen_file(hid_t *file, H5F_t **f, hid_t fapl, hid_t dxpl,
    H5FA_t **fa, haddr_t fa_addr, const farray_test_param_t *tparam)
{
    /* Check for closing & re-opening the array */
    /* (actually will close & re-open the file as well) */
    if(tparam->reopen_array) {
        /* Close array, if given */
        if(fa && *fa) {
            if(H5FA_close(*fa, dxpl) < 0)
                FAIL_STACK_ERROR
            *fa = NULL;
        } /* end if */

        /* Close file */
        if(*file) {
            if(H5Fclose(*file) < 0)
                FAIL_STACK_ERROR
            *file = (-1);
            *f = NULL;
        } /* end if */

        /* Re-open the file */
        if((*file = H5Fopen(filename_g, H5F_ACC_RDWR, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (*f = (H5F_t *)H5I_object(*file)))
            FAIL_STACK_ERROR

        /* Ignore metadata tags in the file's cache */
        if(H5AC_ignore_tags(*f) < 0)
            FAIL_STACK_ERROR

        /* Re-open array, if given */
        if(fa)
            if(NULL == (*fa = H5FA_open(*f, dxpl, fa_addr, NULL)))
                FAIL_STACK_ERROR
    } /* end if */

    /* Success */
    return(0);

error:
    return(-1);
} /* reopen_file() */


/*-------------------------------------------------------------------------
 * Function:	create_array
 *
 * Purpose:	Create a fixed array and perform initial checks
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 *-------------------------------------------------------------------------
 */
static int
create_array(H5F_t *f, hid_t dxpl, const H5FA_create_t *cparam,
    H5FA_t **fa, haddr_t *fa_addr)
{
    farray_state_t state;               /* State of extensible array */

    /* Create array */
    if(NULL == (*fa = H5FA_create(f, dxpl, cparam, NULL)))
        FAIL_STACK_ERROR

    /* Check status of array */
    if(H5FA_get_addr(*fa, fa_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(*fa_addr))
        TEST_ERROR
    HDmemset(&state, 0, sizeof(state));
    state.hdr_size = FA_HDR_SIZE;
    state.nelmts = cparam->nelmts;
    if(check_stats(*fa, &state))
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
 *-------------------------------------------------------------------------
 */
static int
verify_cparam(const H5FA_t *fa, const H5FA_create_t *cparam)
{
    H5FA_create_t test_cparam;          /* Creation parameters for array */

    /* Retrieve creation parameters */
    HDmemset(&test_cparam, 0, sizeof(H5FA_create_t));
    if(H5FA_get_cparam_test(fa, &test_cparam) < 0)
        FAIL_STACK_ERROR

    /* Verify creation parameters */
    if(H5FA_cmp_cparam_test(cparam, &test_cparam))
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
 *-------------------------------------------------------------------------
 */
static int
finish(hid_t file, hid_t fapl, H5F_t *f, H5FA_t *fa, haddr_t fa_addr)
{
    h5_stat_size_t file_size;           /* File size, after deleting array */

    /* Close the fixed array */
    if(H5FA_close(fa, H5AC_ind_read_dxpl_id) < 0)
        FAIL_STACK_ERROR

    /* Delete array */
    if(H5FA_delete(f, H5AC_ind_read_dxpl_id, fa_addr, NULL) < 0)
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
 * Function:	test_create
 *
 * Purpose:	Test creating fixed array
 *
 * Return:	Success: 0
 *		Failure: 1
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_create(hid_t fapl, H5FA_create_t *cparam, farray_test_param_t H5_ATTR_UNUSED *tparam)
{
    hid_t	file = -1;              /* File ID */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5FA_t      *fa = NULL;             /* Fixed array wrapper */
    haddr_t     fa_addr = HADDR_UNDEF;  /* Array address in file */

    /* Create file & retrieve pointer to internal file object */
    if(create_file(fapl, &file, &f) < 0)
        TEST_ERROR

    /*
     * Display testing message
     */
    TESTING("invalid fixed array creation parameters");

#ifndef NDEBUG
{
    H5FA_create_t test_cparam;          /* Creation parameters for array */

    /* Set invalid element size */
    HDmemcpy(&test_cparam, cparam, sizeof(test_cparam));
    test_cparam.raw_elmt_size = 0;
    H5E_BEGIN_TRY {
        fa = H5FA_create(f, H5AC_ind_read_dxpl_id, &test_cparam, NULL);
    } H5E_END_TRY;
    if(fa) {
        /* Close opened fixed array */
        H5FA_close(fa, H5AC_ind_read_dxpl_id);
        fa = NULL;

        /* Indicate error */
        TEST_ERROR
    } /* end if */

    /* Set invalid max. # of elements bits */
    HDmemcpy(&test_cparam, cparam, sizeof(test_cparam));
    test_cparam.max_dblk_page_nelmts_bits = 0;
    H5E_BEGIN_TRY {
        fa = H5FA_create(f, H5AC_ind_read_dxpl_id, &test_cparam, NULL);
    } H5E_END_TRY;
    if(fa) {
        /* Close opened fixed array */
        H5FA_close(fa, H5AC_ind_read_dxpl_id);
        fa = NULL;

        /* Indicate error */
        TEST_ERROR
    } /* end if */

    /* Set invalid max. # of elements */
    HDmemcpy(&test_cparam, cparam, sizeof(test_cparam));
    test_cparam.nelmts = 0;
    H5E_BEGIN_TRY {
        fa = H5FA_create(f, H5AC_ind_read_dxpl_id, &test_cparam, NULL);
    } H5E_END_TRY;
    if(fa) {
        /* Close opened fixed array */
        H5FA_close(fa, H5AC_ind_read_dxpl_id);
        fa = NULL;

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
    TESTING("fixed array creation");

    /* Create array */
    if(create_array(f, H5AC_ind_read_dxpl_id, cparam, &fa, &fa_addr) < 0)
        TEST_ERROR

    PASSED()

    /* Verify the creation parameters */
    TESTING("verify array creation parameters");

    /* Verify the creation parameters */
    if(verify_cparam(fa, cparam) < 0)
        TEST_ERROR

    /* Close array, delete array, close file & verify file is empty */
    if(finish(file, fapl, f, fa, fa_addr) < 0)
        TEST_ERROR

    /* All tests passed */
    PASSED()

    return 0;

error:
    H5E_BEGIN_TRY {
        if(fa)
            H5FA_close(fa, H5AC_ind_read_dxpl_id);
	H5Fclose(file);
    } H5E_END_TRY;

    return 1;
} /* end test_create() */


/*-------------------------------------------------------------------------
 * Function:	test_reopen
 *
 * Purpose:	Create & reopen a fixed array
 *
 * Return:	Success:	0
 *		Failure:	1
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_reopen(hid_t fapl, H5FA_create_t *cparam, farray_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5FA_t      *fa = NULL;             /* Fixed array wrapper */
    haddr_t     fa_addr = HADDR_UNDEF;  /* Array address in file */

    /* Create file & retrieve pointer to internal file object */
    if(create_file(fapl, &file, &f) < 0)
        TEST_ERROR

    /*
     * Display testing message
     */
    TESTING("create, close & reopen fixed array");

    /* Create array */
    if(create_array(f, H5AC_ind_read_dxpl_id, cparam, &fa, &fa_addr) < 0)
        TEST_ERROR

    /* Close the fixed array */
    if(H5FA_close(fa, H5AC_ind_read_dxpl_id) < 0)
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the file */
    if(reopen_file(&file, &f, fapl, H5AC_ind_read_dxpl_id, NULL, HADDR_UNDEF, tparam) < 0)
        TEST_ERROR

    /* Re-open the array */
    if(NULL == (fa = H5FA_open(f, H5AC_ind_read_dxpl_id, fa_addr, NULL)))
        FAIL_STACK_ERROR

    /* Verify the creation parameters */
    if(verify_cparam(fa, cparam) < 0)
        TEST_ERROR

    /* Close array, delete array, close file & verify file is empty */
    if(finish(file, fapl, f, fa, fa_addr) < 0)
        TEST_ERROR

    /* All tests passed */
    PASSED()

    return 0;

error:
    H5E_BEGIN_TRY {
        if(fa)
            H5FA_close(fa, H5AC_ind_read_dxpl_id);
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
 *-------------------------------------------------------------------------
 */
static unsigned
test_open_twice(hid_t fapl, H5FA_create_t *cparam, farray_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t	file2 = -1;             /* File ID */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5F_t	*f2 = NULL;             /* Internal file object pointer */
    H5FA_t      *fa = NULL;             /* Fixed array wrapper */
    H5FA_t      *fa2 = NULL;            /* Fixed array wrapper */
    haddr_t     fa_addr = HADDR_UNDEF;  /* Array address in file */

    /* Create file & retrieve pointer to internal file object */
    if(create_file(fapl, &file, &f) < 0)
        TEST_ERROR

    /*
     * Display testing message
     */
    TESTING("open fixed array twice");

    /* Create array */
    if(create_array(f, H5AC_ind_read_dxpl_id, cparam, &fa, &fa_addr) < 0)
        TEST_ERROR

    /* Open the array again, through the first file handle */
    if(NULL == (fa2 = H5FA_open(f, H5AC_ind_read_dxpl_id, fa_addr, NULL)))
        FAIL_STACK_ERROR

    /* Verify the creation parameters */
    if(verify_cparam(fa, cparam) < 0)
        TEST_ERROR
    if(verify_cparam(fa2, cparam) < 0)
        TEST_ERROR

    /* Close the second fixed array wrapper */
    if(H5FA_close(fa2, H5AC_ind_read_dxpl_id) < 0)
        FAIL_STACK_ERROR
    fa2 = NULL;

    /* Check for closing & re-opening the file */
    if(reopen_file(&file, &f, fapl, H5AC_ind_read_dxpl_id, &fa, fa_addr, tparam) < 0)
        TEST_ERROR

    /* Re-open the file */
    if((file2 = H5Freopen(file)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f2 = (H5F_t *)H5I_object(file2)))
        FAIL_STACK_ERROR

    /* Open the fixed array through the second file handle */
    if(NULL == (fa2 = H5FA_open(f2, H5AC_ind_read_dxpl_id, fa_addr, NULL)))
        FAIL_STACK_ERROR

    /* Verify the creation parameters */
    if(verify_cparam(fa, cparam) < 0)
        TEST_ERROR

    /* Close the first extensible array wrapper */
    if(H5FA_close(fa, H5AC_ind_read_dxpl_id) < 0)
        FAIL_STACK_ERROR
    fa = NULL;

    /* Close the first file */
    /* (close before second file, to detect error on internal array header's
     *  shared file information)
     */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Close array, delete array, close file & verify file is empty */
    if(finish(file2, fapl, f2, fa2, fa_addr) < 0)
        TEST_ERROR

    /* All tests passed */
    PASSED()

    return 0;

error:
    H5E_BEGIN_TRY {
        if(fa)
            H5FA_close(fa, H5AC_ind_read_dxpl_id);
        if(fa2)
            H5FA_close(fa2, H5AC_ind_read_dxpl_id);
	H5Fclose(file);
	H5Fclose(file2);
    } H5E_END_TRY;

    return 1;
} /* test_open_twice() */


/*-------------------------------------------------------------------------
 * Function:	test_open_twice_diff
 *
 * Purpose:	Open a fixed array twice, through different "top" file
 *              handles, with an intermediate file open that takes the "shared"
 *              file handle from the first fixed array's file pointer.
 *
 * Return:	Success:	0
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Friday, December 18, 2015
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_open_twice_diff(hid_t fapl, H5FA_create_t *cparam, farray_test_param_t *tparam)
{
    char        filename_tmp[FARRAY_FILENAME_LEN];      /* Temporary file name */
    hid_t	file = -1;              /* File ID */
    hid_t	file2 = -1;             /* File ID */
    hid_t	file0 = -1;             /* File ID */
    hid_t	file00 = -1;            /* File ID */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5F_t	*f2 = NULL;             /* Internal file object pointer */
    H5FA_t      *fa = NULL;             /* Fixed array wrapper */
    H5FA_t      *fa2 = NULL;            /* Fixed array wrapper */
    haddr_t     fa_addr = HADDR_UNDEF;  /* Array address in file */

    /*
     * Display testing message
     */
    TESTING("open fixed array twice, through different file handles");

    /* Create file & retrieve pointer to internal file object */
    if(create_file(fapl, &file, &f) < 0)
        TEST_ERROR

    /* Create array */
    if(create_array(f, H5AC_ind_read_dxpl_id, cparam, &fa, &fa_addr) < 0)
        TEST_ERROR

    /* Open the array again, through the first file handle */
    if(NULL == (fa2 = H5FA_open(f, H5AC_ind_read_dxpl_id, fa_addr, NULL)))
        FAIL_STACK_ERROR

    /* Verify the creation parameters */
    if(verify_cparam(fa, cparam) < 0)
        TEST_ERROR
    if(verify_cparam(fa2, cparam) < 0)
        TEST_ERROR

    /* Close the second fixed array wrapper */
    if(H5FA_close(fa2, H5AC_ind_read_dxpl_id) < 0)
        FAIL_STACK_ERROR
    fa2 = NULL;

    /* Re-open the file */
    /* (So that there is something holding the file open when the extensible
     *  array is closed)
     */
    if((file0 = H5Fopen(filename_g, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the file */
    if(reopen_file(&file, &f, fapl, H5AC_ind_read_dxpl_id, &fa, fa_addr, tparam) < 0)
        TEST_ERROR

    /* Close the first fixed array wrapper */
    if(H5FA_close(fa, H5AC_ind_read_dxpl_id) < 0)
        FAIL_STACK_ERROR
    fa = NULL;

    /* Close the first file */
    /* (close before second file, to detect error on internal array header's
     *  shared file information)
     */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR
    file = -1;

    /* Open a different file */
    /* (This re-allocates the 'top' file pointer and assigns it a different
     *  'shared' file pointer, making the file pointer in the fixed array's
     *  header stale)
     */
    h5_fixname(FILENAME[1], fapl, filename_tmp, sizeof(filename_tmp));
    if((file00 = H5Fcreate(filename_tmp, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR


    /* Re-open the file with the fixed array */
    if((file2 = H5Fopen(filename_g, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f2 = (H5F_t *)H5I_object(file2)))
        FAIL_STACK_ERROR

    /* Open the fixed array through the second file handle */
    if(NULL == (fa2 = H5FA_open(f2, H5AC_ind_read_dxpl_id, fa_addr, NULL)))
        FAIL_STACK_ERROR

    /* Verify the creation parameters */
    if(verify_cparam(fa2, cparam) < 0)
        TEST_ERROR

    /* Close the extra file handles */
    if(H5Fclose(file0) < 0)
        FAIL_STACK_ERROR
    if(H5Fclose(file00) < 0)
        FAIL_STACK_ERROR

    /* Close array, delete array, close file & verify file is empty */
    if(finish(file2, fapl, f2, fa2, fa_addr) < 0)
        TEST_ERROR

    /* All tests passed */
    PASSED()

    return 0;

error:
    H5E_BEGIN_TRY {
        if(fa)
            H5FA_close(fa, H5AC_ind_read_dxpl_id);
        if(fa2)
            H5FA_close(fa2, H5AC_ind_read_dxpl_id);
	H5Fclose(file);
	H5Fclose(file2);
	H5Fclose(file0);
	H5Fclose(file00);
    } H5E_END_TRY;

    return 1;
} /* test_open_twice_diff() */


/*-------------------------------------------------------------------------
 * Function:	test_delete_open
 *
 * Purpose:	Delete opened fixed array (& open deleted array)
 *
 * Return:	Success:	0
 *		Failure:	1
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_delete_open(hid_t fapl, H5FA_create_t *cparam, farray_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5FA_t      *fa = NULL;             /* Fixed array wrapper */
    H5FA_t      *fa2 = NULL;            /* Fixed array wrapper */
    haddr_t     fa_addr = HADDR_UNDEF;  /* Array address in file */
    h5_stat_size_t file_size;           /* File size, after deleting array */

    /* Create file & retrieve pointer to internal file object */
    if(create_file(fapl, &file, &f) < 0)
        TEST_ERROR

    /*
     * Display testing message
     */
    TESTING("deleting open fixed array");

    /* Create array */
    if(create_array(f, H5AC_ind_read_dxpl_id, cparam, &fa, &fa_addr) < 0)
        TEST_ERROR

    /* Open the array again */
    if(NULL == (fa2 = H5FA_open(f, H5AC_ind_read_dxpl_id, fa_addr, NULL)))
        FAIL_STACK_ERROR

    /* Request that the array be deleted */
    if(H5FA_delete(f, H5AC_ind_read_dxpl_id, fa_addr, NULL) < 0)
        FAIL_STACK_ERROR

    /* Verify the creation parameters */
    if(verify_cparam(fa, cparam) < 0)
        TEST_ERROR
    if(verify_cparam(fa2, cparam) < 0)
        TEST_ERROR

    /* Close the second fixed array wrapper */
    if(H5FA_close(fa2, H5AC_ind_read_dxpl_id) < 0)
        FAIL_STACK_ERROR
    fa2 = NULL;

    /* Try re-opening the array again (should fail, as array will be deleted) */
    H5E_BEGIN_TRY {
        fa2 = H5FA_open(f, H5AC_ind_read_dxpl_id, fa_addr, NULL);
    } H5E_END_TRY;
    if(fa2) {
        /* Close opened array */
        H5FA_close(fa2, H5AC_ind_read_dxpl_id);

        /* Indicate error */
        TEST_ERROR
    } /* end if */

    /* Close the first fixed array wrapper */
    if(H5FA_close(fa, H5AC_ind_read_dxpl_id) < 0)
        FAIL_STACK_ERROR
    fa = NULL;

    /* Check for closing & re-opening the file */
    if(reopen_file(&file, &f, fapl, H5AC_ind_read_dxpl_id, NULL, HADDR_UNDEF, tparam) < 0)
        TEST_ERROR

    /* Try re-opening the array again (should fail, as array is now deleted) */
    H5E_BEGIN_TRY {
        fa = H5FA_open(f, H5AC_ind_read_dxpl_id, fa_addr, NULL);
    } H5E_END_TRY;
    if(fa) {
        /* Close opened array */
        H5FA_close(fa, H5AC_ind_read_dxpl_id);

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
        if(fa)
            H5FA_close(fa, H5AC_ind_read_dxpl_id);
        if(fa2)
            H5FA_close(fa2, H5AC_ind_read_dxpl_id);
	H5Fclose(file);
    } H5E_END_TRY;

    return 1;
} /* test_delete_open() */

/* Fixed array iterator info for forward iteration */
typedef struct fiter_fw_t {
    hsize_t idx;        /* Index of next array location */
} fiter_fw_t;


/*-------------------------------------------------------------------------
 * Function:	fiter_fw_init
 *
 * Purpose:	Initialize element interator (forward iteration)
 *
 * Return:	Success:	Pointer to iteration status object
 *		Failure:	NULL
 *
 *-------------------------------------------------------------------------
 */
static void *
fiter_fw_init(const H5FA_create_t H5_ATTR_UNUSED *cparam, const farray_test_param_t H5_ATTR_UNUSED *tparam,
    hsize_t H5_ATTR_UNUSED cnt)
{
    fiter_fw_t *fiter;          /* Forward element iteration object */

    /* Allocate space for the element iteration object */
    fiter = (fiter_fw_t *)HDmalloc(sizeof(fiter_fw_t));
    HDassert(fiter);

    /* Initialize the element iteration object */
    fiter->idx = 0;

    /* Return iteration object */
    return(fiter);
} /* end fiter_fw_init() */


/*-------------------------------------------------------------------------
 * Function:	fiter_fw_next
 *
 * Purpose:	Get next element index (forward iteration)
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 *-------------------------------------------------------------------------
 */
static hssize_t
fiter_fw_next(void *_fiter)
{
    fiter_fw_t *fiter = (fiter_fw_t *)_fiter;
    hssize_t ret_val;

    /* Sanity check */
    HDassert(fiter);

    /* Get the next array index to test */
    ret_val = (hssize_t)fiter->idx++;

    return(ret_val);
} /* end fiter_fw_next() */


/*-------------------------------------------------------------------------
 * Function:	fiter_term
 *
 * Purpose:	Shut down element interator (simple iterators)
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 *-------------------------------------------------------------------------
 */
static int
fiter_term(void *fiter)
{
    /* Sanity check */
    HDassert(fiter);

    /* Free iteration object */
    HDfree(fiter);

    return(0);
} /* end fiter_term() */

/* Fixed array iterator class for forward iteration */
static const farray_iter_t fa_iter_fw = {
    fiter_fw_init,              /* Iterator init */
    fiter_fw_next,              /* Next array index */
    fiter_term               	/* Iterator term */
};

/* Fixed array iterator info for reverse iteration */
typedef struct fiter_rv_t {
    hsize_t idx;                        /* Index of next array location */
} fiter_rv_t;


/*-------------------------------------------------------------------------
 * Function:	fiter_rv_init
 *
 * Purpose:	Initialize element interator (reverse iteration)
 *
 * Return:	Success:	Pointer to iteration status object
 *		Failure:	NULL
 *
 *-------------------------------------------------------------------------
 */
static void *
fiter_rv_init(const H5FA_create_t *cparam, const farray_test_param_t H5_ATTR_UNUSED *tparam,
    hsize_t H5_ATTR_UNUSED cnt)
{
    fiter_rv_t *fiter;          /* Reverse element iteration object */

    /* Allocate space for the element iteration object */
    fiter = (fiter_rv_t *)HDmalloc(sizeof(fiter_rv_t));
    HDassert(fiter);

    /* Initialize reverse iteration info */
    fiter->idx = cparam->nelmts - 1;

    /* Return iteration object */
    return(fiter);
} /* end fiter_rv_init() */


/*-------------------------------------------------------------------------
 * Function:	fiter_rv_next
 *
 * Purpose:	Get next element index (reverse iteration)
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 *-------------------------------------------------------------------------
 */
static hssize_t
fiter_rv_next(void *_fiter)
{
    fiter_rv_t *fiter = (fiter_rv_t *)_fiter;
    hssize_t ret_val;

    /* Sanity check */
    HDassert(fiter);

    /* Get the next array index to test */
    ret_val = (hssize_t)fiter->idx--;

    return(ret_val);
} /* end fiter_rv_next() */

/* Fixed array iterator class for reverse iteration */
static const farray_iter_t fa_iter_rv = {
    fiter_rv_init,              /* Iterator init */
    fiter_rv_next,              /* Next array index */
    fiter_term               	/* Iterator term */
};

/* Fixed array iterator info for random iteration */
typedef struct fiter_rnd_t {
    hsize_t pos;                /* Position in shuffled array */
    hsize_t *idx;               /* Array of shuffled indices */
} fiter_rnd_t;


/*-------------------------------------------------------------------------
 * Function:	fiter_rnd_init
 *
 * Purpose:	Initialize element interator (random iteration)
 *
 * Return:	Success:	Pointer to iteration status object
 *		Failure:	NULL
 *
 *-------------------------------------------------------------------------
 */
static void *
fiter_rnd_init(const H5FA_create_t H5_ATTR_UNUSED *cparam, const farray_test_param_t H5_ATTR_UNUSED *tparam,
    hsize_t cnt)
{
    fiter_rnd_t *fiter;         /* Random element iteration object */
    size_t u;                   /* Local index variable */

    /* Allocate space for the element iteration object */
    fiter = (fiter_rnd_t *)HDmalloc(sizeof(fiter_rnd_t));
    HDassert(fiter);

    /* Allocate space for the array of shuffled indices */
    fiter->idx = (hsize_t *)HDmalloc(sizeof(hsize_t) * (size_t)cnt);
    HDassert(fiter->idx);

    /* Initialize reverse iteration info */
    fiter->pos = 0;
    for(u = 0; u < (size_t)cnt; u++)
        fiter->idx[u] = (hsize_t)u;

    /* Randomly shuffle array indices */
    if(cnt > 1) {
        for(u = 0; u < (size_t)cnt; u++) {
            size_t swap_idx;            /* Location to swap with when shuffling */
            hsize_t temp_idx;           /* Temporary index */

            swap_idx = ((size_t)HDrandom() % ((size_t)cnt - u)) + u;
            temp_idx = fiter->idx[u];
            fiter->idx[u] = fiter->idx[swap_idx];
            fiter->idx[swap_idx] = temp_idx;
        } /* end for */
    } /* end if */

    /* Return iteration object */
    return(fiter);
} /* end fiter_rnd_init() */


/*-------------------------------------------------------------------------
 * Function:	fiter_rnd_next
 *
 * Purpose:	Get next element index (random iteration)
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 *-------------------------------------------------------------------------
 */
static hssize_t
fiter_rnd_next(void *_fiter)
{
    fiter_rnd_t *fiter = (fiter_rnd_t *)_fiter;
    hssize_t ret_val;

    /* Sanity check */
    HDassert(fiter);

    /* Get the next array index to test */
    ret_val = (hssize_t)fiter->idx[fiter->pos];
    fiter->pos++;

    return(ret_val);
} /* end fiter_rnd_next() */


/*-------------------------------------------------------------------------
 * Function:	fiter_rnd_term
 *
 * Purpose:	Shut down element interator (random iteration)
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 *-------------------------------------------------------------------------
 */
static int
fiter_rnd_term(void *_fiter)
{
    fiter_rnd_t *fiter = (fiter_rnd_t *)_fiter;

    /* Sanity check */
    HDassert(fiter);
    HDassert(fiter->idx);

    /* Free shuffled index array */
    HDfree(fiter->idx);

    /* Free iteration object */
    HDfree(fiter);

    return(0);
} /* end fiter_rnd_term() */

/* Fixed array iterator class for random iteration */
static const farray_iter_t fa_iter_rnd = {
    fiter_rnd_init,             /* Iterator init */
    fiter_rnd_next,             /* Next array index */
    fiter_rnd_term              /* Iterator term */
};

/* Fixed array iterator info for cyclic iteration */
typedef struct fiter_cyc_t {
    hsize_t pos;                /* Position in shuffled array */
    hsize_t cnt;                /* # of elements to store */
    hsize_t cyc;                /* Cycle of elements to choose from */
} fiter_cyc_t;


/*-------------------------------------------------------------------------
 * Function:	fiter_cyc_init
 *
 * Purpose:	Initialize element interator (cyclic iteration)
 *
 * Return:	Success:	Pointer to iteration status object
 *		Failure:	NULL
 *
 *-------------------------------------------------------------------------
 */
static void *
fiter_cyc_init(const H5FA_create_t H5_ATTR_UNUSED *cparam, const farray_test_param_t H5_ATTR_UNUSED *tparam,
    hsize_t cnt)
{
    fiter_cyc_t *fiter;         /* Cyclic element iteration object */

    /* Allocate space for the element iteration object */
    fiter = (fiter_cyc_t *)HDmalloc(sizeof(fiter_cyc_t));
    HDassert(fiter);

    /* Initialize reverse iteration info */
    fiter->pos = 0;
    fiter->cnt = cnt;
    fiter->cyc = 0;

    /* Return iteration object */
    return(fiter);
} /* end fiter_cyc_init() */


/*-------------------------------------------------------------------------
 * Function:	fiter_cyc_next
 *
 * Purpose:	Get next element index (cyclic iteration)
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 *-------------------------------------------------------------------------
 */
static hssize_t
fiter_cyc_next(void *_fiter)
{
    fiter_cyc_t *fiter = (fiter_cyc_t *)_fiter;
    hssize_t ret_val;

    /* Sanity check */
    HDassert(fiter);

    /* Get the next array index to test */
    ret_val = (hssize_t)fiter->pos;
    fiter->pos += FA_CYC_COUNT;
    if(fiter->pos >= fiter->cnt)
        fiter->pos = ++fiter->cyc;

    return(ret_val);
} /* end fiter_cyc_next() */


/* Fixed array iterator class for cyclic iteration */
static const farray_iter_t fa_iter_cyc = {
    fiter_cyc_init,             /* Iterator init */
    fiter_cyc_next,             /* Next array index */
    fiter_term              	/* Iterator term */
};


/*-------------------------------------------------------------------------
 * Function:	check_elmt
 *
 * Purpose:	Check whether _relmt is the same as in _welmt
 *
 * Return:	Success:	0
 *		Failure:	1
 *
 * Programmer:  Vailin Choi; 6th August, 2009
 *
 *-------------------------------------------------------------------------
 */
static int
check_elmt(void *_relmt, void *_welmt)
{
    uint64_t *relmt = (uint64_t *)_relmt;
    uint64_t *welmt = (uint64_t *)_welmt;

    if(welmt == NULL) { /* check for fill value */
        if(*relmt != H5FA_TEST_FILL)
            TEST_ERROR
    } /* end if */
    else {
        if(*relmt != *welmt)
            TEST_ERROR
    } /* end else */

    return(0);

error:
    return(-1);
} /* end check_elmt() */


/*-------------------------------------------------------------------------
 * Function:	test_set_elmts
 *
 * Purpose:	Set all elements from 0 to ('nelmts' - 1) in fixed array
 *		("nelmts" is the # of elements to be set in the fixed array)
 *
 * Return:	Success:	0
 *		Failure:	1
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_set_elmts(hid_t fapl, H5FA_create_t *cparam, farray_test_param_t *tparam,
    hsize_t nelmts, const char *test_str)
{
    hid_t	file = -1;              /* File ID */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5FA_t      *fa = NULL;             /* Fixed array wrapper */
    void        *fiter_info;            /* Fixed array iterator info */
    farray_state_t state;               /* State of fixed array */
    uint64_t    welmt;                  /* Element to write */
    uint64_t    relmt;                  /* Element to read */
    hsize_t     cnt;                    /* Count of array indices */
    hssize_t    sidx;                   /* Index value of next element in the fixed array */
    hsize_t     idx;                    /* Index value of next element in the fixed array */
    hsize_t	fa_nelmts;		/* # of elements in fixed array */
    haddr_t     fa_addr = HADDR_UNDEF;  /* Array address in file */

    HDassert(nelmts);
    /*
     * Display testing message
     */
    TESTING(test_str);

    /* Create file & retrieve pointer to internal file object */
    if(create_file(fapl, &file, &f) < 0)
        TEST_ERROR

    /* Create array */
    if(create_array(f, H5AC_ind_read_dxpl_id, cparam, &fa, &fa_addr) < 0)
        TEST_ERROR

    /* Verify the creation parameters */
    if(verify_cparam(fa, cparam) < 0)
        TEST_ERROR

    /* Check for closing & re-opening the file */
    if(reopen_file(&file, &f, fapl, H5AC_ind_read_dxpl_id, &fa, fa_addr, tparam) < 0)
        TEST_ERROR

    if(H5FA_get_nelmts(fa, &fa_nelmts) < 0)
        FAIL_STACK_ERROR

    if(nelmts > fa_nelmts)
        TEST_ERROR

    /* Verify array state */
    HDmemset(&state, 0, sizeof(state));
    state.hdr_size = FA_HDR_SIZE;
    state.nelmts = cparam->nelmts;
    state.dblk_size = 0;
    if(check_stats(fa, &state))
        TEST_ERROR

    /* Get all elements from empty array */

    /* Initialize iterator */
    if(NULL == (fiter_info = tparam->fiter->init(cparam, tparam, nelmts)))
        TEST_ERROR

    /* Get elements of array */
    for(cnt = 0; cnt < nelmts; cnt++) {
        /* Get the array index */
        if((sidx = tparam->fiter->next(fiter_info)) < 0)
            TEST_ERROR
	idx = (hsize_t)sidx;

        /* Retrieve element of array (not set yet) */
        relmt = (uint64_t)0;
        if(H5FA_get(fa, H5AC_ind_read_dxpl_id, idx, &relmt) < 0)
            FAIL_STACK_ERROR

        /* Verify that the retrieved is correct */
        if(check_elmt(&relmt, NULL))
            TEST_ERROR
    } /* end for */

    /* Shutdown iterator */
    if(tparam->fiter->term(fiter_info) < 0)
        TEST_ERROR


    /* Set (& get) all elements from empty array */

    /* Initialize iterator */
    if(NULL == (fiter_info = tparam->fiter->init(cparam, tparam, nelmts)))
        TEST_ERROR

    /* Set elements of array */
    for(cnt = 0; cnt < nelmts; cnt++) {
        /* Get the array index */
        if((sidx = tparam->fiter->next(fiter_info)) < 0)
            TEST_ERROR
	idx = (hsize_t)sidx;

        relmt = (uint64_t)0;
        if(H5FA_get(fa, H5AC_ind_read_dxpl_id, idx, &relmt) < 0)
            FAIL_STACK_ERROR

        /* Verify that the retrieved element is correct */
        if(check_elmt(&relmt, NULL))
            TEST_ERROR

        /* Set element of array */
        welmt = (uint64_t)7 + idx;
        if(H5FA_set(fa, H5AC_ind_read_dxpl_id, idx, &welmt) < 0)
            FAIL_STACK_ERROR

        /* Retrieve element of array (set now) */
        relmt = (uint64_t)0;
        if(H5FA_get(fa, H5AC_ind_read_dxpl_id, idx, &relmt) < 0)
            FAIL_STACK_ERROR

        /* Verify that the retrieved element is correct */
        if(check_elmt(&relmt, &welmt))
            TEST_ERROR
    } /* end for */

    /* Verify array state */
    HDmemset(&state, 0, sizeof(state));
    set_fa_state(cparam, &state);
    if(check_stats(fa, &state))
        TEST_ERROR

    /* Shutdown iterator */
    if(tparam->fiter->term(fiter_info) < 0)
        TEST_ERROR

    /* Close array, delete array, close file & verify file is empty */
    if(finish(file, fapl, f, fa, fa_addr) < 0)
        TEST_ERROR

    /* All tests passed */
    PASSED()

    return 0;

error:
    H5E_BEGIN_TRY {
        if(fa)
            H5FA_close(fa, H5AC_ind_read_dxpl_id);
	H5Fclose(file);
    } H5E_END_TRY;

    return 1;
} /* test_set_elmts() */


/*-------------------------------------------------------------------------
 * Function:	test_skip_elmts
 *
 * Purpose:	Set the element "skip_elmts" in the fixed array
 *		("skip_elmts" is the index of the fixed array to be set.)
 *
 * Return:	Success:	0
 *		Failure:	1
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_skip_elmts(hid_t fapl, H5FA_create_t *cparam, farray_test_param_t *tparam,
    hsize_t skip_elmts, hbool_t check_rest, const char *test_str)
{
    hid_t	file = -1;              /* File ID */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5FA_t      *fa = NULL;             /* Extensible array wrapper */
    farray_state_t state;               /* State of extensible array */
    uint64_t    welmt;                  /* Element to write */
    uint64_t    relmt;                  /* Element to read */
    hsize_t     idx;                    /* Index value of element to get */
    hsize_t     cnt;                    /* Count of array indices */
    hsize_t	fa_nelmts;		/* # of elements in fixed array */
    haddr_t     fa_addr = HADDR_UNDEF;  /* Array address in file */

    /*
     * Display testing message
     */
    TESTING(test_str);

    /* Create file & retrieve pointer to internal file object */
    if(create_file(fapl, &file, &f) < 0)
        TEST_ERROR

    /* Create array */
    if(create_array(f, H5AC_ind_read_dxpl_id, cparam, &fa, &fa_addr) < 0)
        TEST_ERROR

    /* Verify the creation parameters */
    if(verify_cparam(fa, cparam) < 0)
        TEST_ERROR

    /* Check for closing & re-opening the file */
    if(reopen_file(&file, &f, fapl, H5AC_ind_read_dxpl_id, &fa, fa_addr, tparam) < 0)
        TEST_ERROR

    if(H5FA_get_nelmts(fa, &fa_nelmts) < 0)
        FAIL_STACK_ERROR

    if(skip_elmts >= fa_nelmts)
        TEST_ERROR

    /* Verify array state */
    HDmemset(&state, 0, sizeof(state));
    state.hdr_size = FA_HDR_SIZE;
    state.nelmts = cparam->nelmts;
    state.dblk_size = 0;
    if(check_stats(fa, &state))
        TEST_ERROR

    /* Set (& get) element after skipping elements */
    idx = skip_elmts;

    /* Retrieve element of array (not set yet) */
    relmt = (uint64_t)0;
    if(H5FA_get(fa, H5AC_ind_read_dxpl_id, idx, &relmt) < 0)
        FAIL_STACK_ERROR

    /* Verify that the retrieved is correct */
    if(check_elmt(&relmt, NULL))
        TEST_ERROR

    /* Set element of array */
    welmt = (uint64_t)7 + idx;
    if(H5FA_set(fa, H5AC_ind_read_dxpl_id, idx, &welmt) < 0)
        FAIL_STACK_ERROR

    /* Verify array state */
    HDmemset(&state, 0, sizeof(state));
    set_fa_state(cparam, &state);
    if(check_stats(fa, &state))
        TEST_ERROR

    /* Retrieve element of array (set now) */
    relmt = (uint64_t)0;
    if(H5FA_get(fa, H5AC_ind_read_dxpl_id, idx, &relmt) < 0)
        FAIL_STACK_ERROR

    /* Verify that the retrieved is correct */
    if(check_elmt(&relmt, &welmt))
        TEST_ERROR

    if(check_rest) {
        /* Get unset elements of array */
        for(cnt = 0; cnt < skip_elmts; cnt++) {
            /* Retrieve element of array (not set yet) */
            relmt = (uint64_t)0;
            if(H5FA_get(fa, H5AC_ind_read_dxpl_id, cnt, &relmt) < 0)
                FAIL_STACK_ERROR

            /* Verify that the retrieved is correct */
            if(check_elmt(&relmt, NULL))
                TEST_ERROR
        } /* end for */
    } /* end if */

    /* Close array, delete array, close file & verify file is empty */
    if(finish(file, fapl, f, fa, fa_addr) < 0)
        TEST_ERROR

    /* All tests passed */
    PASSED()

    return 0;

error:
    H5E_BEGIN_TRY {
        if(fa)
            H5FA_close(fa, H5AC_ind_read_dxpl_id);
	H5Fclose(file);
    } H5E_END_TRY;

    return 1;
} /* test_skip_elmts() */


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Test the fixed array code
 *
 * Return:	Success: 0
 *		Failure: 1
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    H5FA_create_t cparam;               /* Creation parameters for fixed array */
    farray_test_param_t tparam;         /* Testing parameters */
    farray_test_type_t curr_test;       /* Current test being worked on */
    farray_iter_type_t curr_iter;       /* Current iteration type being worked on */
    hid_t	fapl = -1;              /* File access property list for data files */
    unsigned	nerrors = 0;            /* Cumulative error count */
    time_t      curr_time;              /* Current time, for seeding random number generator */
    int		ExpressMode;            /* Test express value */

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
    HDsrandom((unsigned)curr_time);

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

    /* Iterate over the testing parameters */
    for(curr_test = FARRAY_TEST_NORMAL; curr_test < FARRAY_TEST_NTESTS; H5_INC_ENUM(farray_test_type_t, curr_test)) {

        /* Initialize the testing parameters */
        HDmemset(&tparam, 0, sizeof(tparam));
        tparam.nelmts = TEST_NELMTS;

        /* Set appropriate testing parameters for each test */
        switch(curr_test) {
            /* "Normal" testing parameters */
            case FARRAY_TEST_NORMAL:
                puts("Testing with NORMAL PARAMETERS");
                break;

            /* "Re-open array" testing parameters */
            case FARRAY_TEST_REOPEN:
                puts("Testing with reopen array flag set");
                tparam.reopen_array = FARRAY_TEST_REOPEN;
                break;

            /* An unknown test? */
            case FARRAY_TEST_NTESTS:
            default:
                goto error;
        } /* end switch */

	/* Initialize fixed array creation parameters */
	init_cparam(&cparam, &tparam);

        /* Basic capability tests */
        nerrors += test_create(fapl, &cparam, &tparam);
        nerrors += test_reopen(fapl, &cparam, &tparam);
        nerrors += test_open_twice(fapl, &cparam, &tparam);
        nerrors += test_open_twice_diff(fapl, &cparam, &tparam);
        nerrors += test_delete_open(fapl, &cparam, &tparam);

	/* Iterate over the type of capacity tests */
	for(curr_iter = FARRAY_ITER_FW; curr_iter < FARRAY_ITER_NITERS; H5_INC_ENUM(farray_iter_type_t, curr_iter)) {

            /* Set appropriate parameters for each type of iteration */
            switch(curr_iter) {
                /* "Forward" testing parameters */
                case FARRAY_ITER_FW:
                    puts("Testing with forward iteration");
                    tparam.fiter = &fa_iter_fw;
                    break;

                /* "Reverse" testing parameters */
                case FARRAY_ITER_RV:
                    puts("Testing with reverse iteration");
                    tparam.fiter = &fa_iter_rv;
                    break;

                /* "Random" testing parameters */
                case FARRAY_ITER_RND:
                    puts("Testing with random iteration");
                    tparam.fiter = &fa_iter_rnd;
                    break;

                /* "Cyclic" testing parameters */
                case FARRAY_ITER_CYC:
                    puts("Testing with cyclic iteration");
                    tparam.fiter = &fa_iter_cyc;
                    break;

                /* An unknown iteration? */
                case FARRAY_ITER_NITERS:
                default:
                    goto error;
            } /* end switch */

            /* Basic capacity tests */
            nerrors += test_set_elmts(fapl, &cparam, &tparam, (hsize_t)1, "setting 1 element of the array");
            nerrors += test_set_elmts(fapl, &cparam, &tparam, (hsize_t)(tparam.nelmts/2), "setting half of the array's elements ");
            nerrors += test_set_elmts(fapl, &cparam, &tparam, (hsize_t)tparam.nelmts, "setting all the array elements");
        } /* end for */

	/* Check skipping elements */
        nerrors += test_skip_elmts(fapl, &cparam, &tparam, (hsize_t)1, TRUE, "skipping to first element");
        nerrors += test_skip_elmts(fapl, &cparam, &tparam, ((hsize_t)1 << cparam.max_dblk_page_nelmts_bits), TRUE, "skipping to first element in data block page");
        nerrors += test_skip_elmts(fapl, &cparam, &tparam, (hsize_t)(tparam.nelmts - 1), TRUE, "skipping to last element");

	/* Create Fixed Array of MAX_NELMTS elements */
	/*
	 * MAX_NELMTS succeeds on jam and smirom.
	 * The value was adjusted for linew due to the following:
	    Linew failed with "H5FD_sec2_truncate(): unable to extend file properly"
	    Linew failed with "H5FD_sec2_truncate(): File too large"
	 */
        tparam.nelmts = MAX_NELMTS/17;
	init_cparam(&cparam, &tparam);

	/* Set the last element in the Fixed Array */
        nerrors += test_skip_elmts(fapl, &cparam, &tparam, (hsize_t)(tparam.nelmts - 1), FALSE, "skipping to last element");
    } /* end for */

    /* Verify symbol table messages are cached */
    nerrors += (h5_verify_cached_stabs(FILENAME, fapl) < 0 ? 1 : 0);

    if(nerrors)
        goto error;
    puts("All fixed array tests passed.");

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

