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
#define ELMT_SIZE               sizeof(haddr_t)
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

/* Local routines */


/*-------------------------------------------------------------------------
 * Function:	init_cparam
 *
 * Purpose:	Initialize array creation parameter structure 
 *
 * Return:	Success:	0
 *		Failure:	1
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
    cparam->elmt_size = ELMT_SIZE;
    cparam->idx_blk_elmts = IDX_BLK_ELMTS;
    cparam->sup_blk_min_data_ptrs = SUP_BLK_MIN_DATA_PTRS;
    cparam->data_blk_min_elmts = DATA_BLK_MIN_ELMTS;

    return(0);
} /* init_cparam() */


/*-------------------------------------------------------------------------
 * Function:	check_stats
 *
 * Purpose:	Verify stats for an extensible array
 *
 * Return:	Success:	0
 *		Failure:	1
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
    return(1);
} /* check_stats() */


/*-------------------------------------------------------------------------
 * Function:	test_basic
 *
 * Purpose:	Basic tests for extensible arrays
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
    char	filename[EARRAY_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5EA_create_t test_cparam;          /* Creation parameters for array */
    H5EA_t      *ea = NULL;             /* Extensible array wrapper */
    haddr_t     ea_addr;                /* Array address in file */
    hsize_t     nelmts;                 /* Number of elements in array */
    earray_state_t state;               /* State of extensible array */
    h5_stat_size_t empty_size;          /* File size, w/o array */
    h5_stat_size_t file_size;           /* File size, after deleting array */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Get the size of a file w/empty heap*/
    if((empty_size = h5_get_file_size(filename)) < 0)
        TEST_ERROR

    /* Re-open the file */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = (H5F_t *)H5I_object(file)))
        FAIL_STACK_ERROR

    /*
     * Display testing message
     */
    TESTING("invalid extensible array creation parameters");

#ifndef NDEBUG
    /* Set invalid element size */
    HDmemcpy(&test_cparam, cparam, sizeof(test_cparam));
    test_cparam.elmt_size = 0;
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
#else /* NDEBUG */
    SKIPPED();
    puts("    Not tested when assertions are disabled");
#endif /* NDEBUG */

    /*
     * Display testing message
     */
    TESTING("extensible array creation");

    if(NULL == (ea = H5EA_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    nelmts = 0;
    if(H5EA_get_nelmts(ea, &nelmts) < 0)
        FAIL_STACK_ERROR
    if(nelmts > 0)
        TEST_ERROR
    if(H5EA_get_addr(ea, &ea_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(ea_addr))
        TEST_ERROR
    HDmemset(&state, 0, sizeof(state));
    if(check_stats(ea, &state))
        TEST_ERROR
    PASSED()

    /* Query the type of address mapping */
    TESTING("query array creation parameters");
    HDmemset(&test_cparam, 0, sizeof(H5EA_create_t));
    if(H5EA_get_cparam_test(ea, &test_cparam) < 0)
        FAIL_STACK_ERROR
    if(H5EA_cmp_cparam_test(cparam, &test_cparam))
        TEST_ERROR

    /* Close the extensible array */
    if(H5EA_close(ea, H5P_DATASET_XFER_DEFAULT) < 0)
        FAIL_STACK_ERROR

    /* Delete array */
    if(H5EA_delete(f, H5P_DATASET_XFER_DEFAULT, ea_addr) < 0)
        FAIL_STACK_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR


    /* Get the size of the file */
    if((file_size = h5_get_file_size(filename)) < 0)
        TEST_ERROR

    /* Verify the file is correct size */
    if(file_size != empty_size)
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

    /* Initialize extensible array creation parameters */
    init_cparam(&cparam);

    /* Clear the testing parameters */
    HDmemset(&tparam, 0, sizeof(tparam));

    /* Tests */
    nerrors = test_create(fapl, &cparam, &tparam);

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

