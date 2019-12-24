/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 *  Tests for file memory management consist of 3 parts:
 *    test_mf_eoa_*()      tests for file meomory that interact with file allocation
 *    test_mf_fs_*()       tests for file memory that interact with the free-space manager
 *    test_mf_aggr_*()  tests for file memory that interact with the aggregators
 *    test_mf_align_*() tests for file memory with alignment setting
 *    test_filespace_*() tests for file space management
 *     test_page_*() tests for file space paging
 */

#include "h5test.h"

#define H5MF_FRIEND        /*suppress error about including H5MFpkg      */
#include "H5MFpkg.h"

#define H5FS_FRIEND        /*suppress error about including H5FSpkg      */
#include "H5FSpkg.h"

#define H5F_FRIEND        /*suppress error about including H5Fpkg      */
#define H5F_TESTING
#include "H5Fpkg.h"

#include "H5CXprivate.h"        /* API Contexts                         */
#include "H5FLprivate.h"
#include "H5Iprivate.h"
#include "H5VMprivate.h"

#define FILENAME_LEN        1024

#define TBLOCK_SIZE1    1
#define TBLOCK_SIZE2    2
#define TBLOCK_SIZE3    3
#define TBLOCK_SIZE4    4
#define TBLOCK_SIZE5    5
#define TBLOCK_SIZE6    6
#ifdef PB_OUT
#define TBLOCK_SIZE7    7
#define TBLOCK_SIZE8    8
#endif /* PB_OUT */
#define TBLOCK_SIZE10   10
#define TBLOCK_SIZE11   11
#define TBLOCK_SIZE20   20
#define TBLOCK_SIZE30   30
#define TBLOCK_SIZE36   36
#define TBLOCK_SIZE40   40
#define TBLOCK_SIZE50   50
#define TBLOCK_SIZE80   80
#define TBLOCK_SIZE90   90
#define TBLOCK_SIZE98   98
#define TBLOCK_SIZE100  100
#define TBLOCK_SIZE150  150
#define TBLOCK_SIZE200  200
#define TBLOCK_SIZE600  600
#define TBLOCK_SIZE700  700
#define TBLOCK_SIZE1034 1034
#define TBLOCK_SIZE1970 1970
#define TBLOCK_SIZE2048 2048
#define TBLOCK_SIZE2058 2058
#define TBLOCK_SIZE2192 2192
#define TBLOCK_SIZE3080 3080
#define TBLOCK_SIZE3088 3088
#define TBLOCK_SIZE3198 3198
#define TBLOCK_SIZE3286 3286
#define TBLOCK_SIZE3248 3248
#define TBLOCK_SIZE3900 3900
#define TBLOCK_SIZE4020 4020
#define TBLOCK_SIZE4086 4086
#define TBLOCK_SIZE4096 4096
#define TBLOCK_SIZE4106 4106
#define TBLOCK_SIZE5000 5000
#define TBLOCK_SIZE6000 6000
#define TBLOCK_SIZE8000 8000
#define TBLOCK_SIZE8100 8100
#define TBLOCK_SIZE8192 8192
#define TBLOCK_SIZE8190 8190
#define TBLOCK_SIZE12000    12000

#define TBLOCK_ADDR70   70
#define TBLOCK_ADDR100  100

#define TEST_ALIGN16        16
#define TEST_ALIGN1024      1024
#define TEST_ALIGN4096      4096

#define TEST_THRESHOLD10    10
#define TEST_THRESHOLD3     3

const char *FILENAME[] = {
    "mf",
    NULL
};

typedef enum {
    TEST_NORMAL,            /* size of aggregator is >= alignment size */
    TEST_AGGR_SMALL,            /* size of aggregator is smaller than alignment size */
    TEST_NTESTS             /* The number of test types, must be last */
} test_type_t;

static int check_stats(const H5F_t *f, const H5FS_t *frsp, H5FS_stat_t *state);

static unsigned test_mf_eoa(const char *env_h5_drvr, hid_t fapl);
static unsigned test_mf_eoa_shrink(const char *env_h5_drvr, hid_t fapl);
static unsigned test_mf_eoa_extend(const char *env_h5_drvr, hid_t fapl);
static unsigned test_dichotomy(hid_t fapl);
static unsigned test_mf_fs_start(hid_t fapl);
static unsigned test_mf_fs_alloc_free(hid_t fapl);
static unsigned test_mf_fs_extend(hid_t fapl);
static unsigned test_mf_fs_absorb(const char *env_h5_drvr, hid_t fapl);
static unsigned test_mf_aggr_alloc1(const char *env_h5_drvr, hid_t fapl);
static unsigned test_mf_aggr_alloc2(const char *env_h5_drvr, hid_t fapl);
static unsigned test_mf_aggr_alloc3(const char *env_h5_drvr, hid_t fapl);
static unsigned test_mf_aggr_alloc4(const char *env_h5_drvr, hid_t fapl);
static unsigned test_mf_aggr_alloc5(const char *env_h5_drvr, hid_t fapl);
static unsigned test_mf_aggr_alloc6(const char *env_h5_drvr, hid_t fapl);
static unsigned test_mf_aggr_alloc7(const char *env_h5_drvr, hid_t fapl);
static unsigned test_mf_aggr_extend(const char *env_h5_drvr, hid_t fapl);
static unsigned test_mf_aggr_absorb(const char *env_h5_drvr, hid_t fapl);
static unsigned test_mf_align_eoa(const char *env_h5_drvr, hid_t fapl, hid_t new_fapl);
static unsigned test_mf_align_fs(const char *env_h5_drvr, hid_t fapl, hid_t new_fapl);
static unsigned test_mf_align_alloc1(const char *env_h5_drvr, hid_t fapl, hid_t new_fapl);
static unsigned test_mf_align_alloc2(const char *env_h5_drvr, hid_t fapl, hid_t new_fapl);
static unsigned test_mf_align_alloc3(const char *env_h5_drvr, hid_t fapl, hid_t new_fapl);
static unsigned test_mf_align_alloc4(const char *env_h5_drvr, hid_t fapl, hid_t new_fapl);
static unsigned test_mf_align_alloc5(const char *env_h5_drvr, hid_t fapl, hid_t new_fapl);
static unsigned test_mf_align_alloc6(const char *env_h5_drvr, hid_t fapl, hid_t new_fapl);
static unsigned test_mf_tmp(const char *env_h5_drvr, hid_t fapl, hbool_t new_format);
static unsigned test_mf_fs_gone(const char *env_h5_drvr, hid_t fapl, hbool_t new_format);
static unsigned test_mf_strat_thres_gone(const char *env_h5_drvr, hid_t fapl, hbool_t new_format);
static unsigned test_mf_fs_persist(const char *env_h5_drvr, hid_t fapl, hbool_t new_format);
static unsigned test_mf_strat_thres_persist(const char *env_h5_drvr, hid_t fapl, hbool_t new_format);
#ifdef PB_OUT
static unsigned test_mf_fs_persist_split(void);
static unsigned test_mf_fs_persist_multi(void);
#endif
static unsigned test_page_alloc_xfree(const char *env_h5_drvr, hid_t fapl);
static unsigned test_page_small(const char *env_h5_drvr, hid_t fapl);
static unsigned test_page_large(const char *env_h5_drvr, hid_t fapl);
static unsigned test_page_large_try_extend(const char *env_h5_drvr, hid_t fapl);
static unsigned test_page_small_try_extend(const char *env_h5_drvr, hid_t fapl);
static unsigned test_page_try_shrink(const char *env_h5_drvr, hid_t fapl);
static unsigned test_page_alignment(const char *env_h5_drvr, hid_t fapl);

/*
 * Verify statistics for the free-space manager
 *
 */
static int
check_stats(const H5F_t *f, const H5FS_t *frsp, H5FS_stat_t *state)
{
    H5FS_stat_t frspace_stats;             /* Statistics about the heap */

    HDassert(f);
    HDassert(frsp);

    /* Get statistics for free-space and verify they are correct */
    if(H5FS_stat_info(f, frsp, &frspace_stats) < 0)
        FAIL_STACK_ERROR

    if(frspace_stats.tot_space != state->tot_space) {
        HDfprintf(stdout, "frspace_stats.tot_space = %Hu, state->tot_space = %Zu\n",
                  frspace_stats.tot_space, state->tot_space);
        TEST_ERROR
    } /* end if */
    if(frspace_stats.tot_sect_count != state->tot_sect_count) {
        HDfprintf(stdout, "frspace_stats.tot_sect_count = %Hu, state->tot_sect_count = %Hu\n",
                  frspace_stats.tot_sect_count, state->tot_sect_count);
        TEST_ERROR
    } /* end if */
    if(frspace_stats.serial_sect_count != state->serial_sect_count) {
        HDfprintf(stdout, "frspace_stats.serial_sect_count = %Hu, state->serial_sect_count = %Hu\n",
                  frspace_stats.serial_sect_count, state->serial_sect_count);
        TEST_ERROR
    } /* end if */
    if(frspace_stats.ghost_sect_count != state->ghost_sect_count) {
        HDfprintf(stdout, "frspace_stats.ghost_sect_count = %Hu, state->ghost_sect_count = %Hu\n",
                  frspace_stats.ghost_sect_count, state->ghost_sect_count);
        TEST_ERROR
    } /* end if */

    /* All tests passed */
    return(0);

error:
    return(1);
} /* check_stats() */

/*
 *-------------------------------------------------------------------------
 * To verify that blocks are allocated from file allocation
 *
 * Set up:
 *  Turn off using meta/small data aggregator
 *  There is nothing in free-space manager
 *
 * Allocate two blocks which should be from file allocation
 *-------------------------------------------------------------------------
 */
static unsigned
test_mf_eoa(const char *env_h5_drvr, hid_t fapl)
{
    hid_t        file = -1;              /* File ID */
    hid_t        fapl_new = -1;        /* copy of fapl */
    char        filename[FILENAME_LEN]; /* Filename to use */
    H5F_t        *f = NULL;              /* Internal file object pointer */
    h5_stat_size_t      file_size, new_file_size;      /* file size */
    H5FD_mem_t         type;
    haddr_t        addr1, addr2;
    haddr_t         ma_addr=HADDR_UNDEF, new_ma_addr=HADDR_UNDEF;
    hsize_t         ma_size=0;
    hbool_t             contig_addr_vfd;        /* Whether VFD used has a contigous address space */

    TESTING("H5MM_alloc() of file allocation");

    /* Skip test when using VFDs that has different address spaces for each
     *  type of metadata allocation.
     */
    contig_addr_vfd = (hbool_t)(HDstrcmp(env_h5_drvr, "split") && HDstrcmp(env_h5_drvr, "multi"));
    if(contig_addr_vfd) {
        /* Set the filename to use for this test */
        h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

        if((fapl_new = H5Pcopy(fapl)) < 0) TEST_ERROR

        /* Create the file to work on */
        if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Close file */
        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Turn off using meta/small data aggregator */
        H5Pset_meta_block_size(fapl_new, (hsize_t)0);
        H5Pset_small_data_block_size(fapl_new, (hsize_t)0);

        /* Re-open the file with meta/small data setting */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl_new)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        H5MF__aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        type = H5FD_MEM_SUPER;
        addr1 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE30);

        /* nothing should be changed in meta_aggr */
        H5MF__aggr_query(f, &(f->shared->meta_aggr), &new_ma_addr, &ma_size);
        if (new_ma_addr != ma_addr)
            TEST_ERROR

        if (addr1 < (haddr_t)file_size)
            TEST_ERROR

        addr2 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE50);

        /* nothing should be changed in meta_aggr */
        H5MF__aggr_query(f, &(f->shared->meta_aggr), &new_ma_addr, &ma_size);
        if (new_ma_addr != ma_addr)
            TEST_ERROR

        if (addr2 < (haddr_t)file_size)
            TEST_ERROR

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((new_file_size = h5_get_file_size(filename, fapl_new)) < 0)
            TEST_ERROR

        /* Verify the file is the correct size */
        if (new_file_size != (file_size+TBLOCK_SIZE30+TBLOCK_SIZE50))
            TEST_ERROR

        /* Re-open the file */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl_new)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        H5MF_xfree(f, type, addr1, (hsize_t)TBLOCK_SIZE30);
        H5MF_xfree(f, type, addr2, (hsize_t)TBLOCK_SIZE50);

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((new_file_size = h5_get_file_size(filename, fapl_new)) < 0)
            TEST_ERROR

        /* Verify the file is the correct size */
        if(new_file_size != file_size)
            TEST_ERROR

        if(H5Pclose(fapl_new) < 0)
            FAIL_STACK_ERROR

        PASSED()
    } /* end if */
    else {
        SKIPPED();
        HDputs("    Current VFD doesn't support continuous address space");
    } /* end else */

    return(0);

error:
    H5E_BEGIN_TRY {
        H5Pclose(fapl_new);
    H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_mf_eoa() */

/*
 *-------------------------------------------------------------------------
 * To verify that an allocated block from file allocation is shrunk.
 *
 * Set up:
 *     Turn off using meta/small data aggregator
 *     There is nothing in free-space manager
 *
 *    Test 1: Allocate a block of 30 from file allocation
 *         H5MF_try_shrink() the block by 30 : succeed
 *    Test 2: Allocate a block of 30 from file allocation
 *         H5MF_try_shrink() the block by 20 : fail
 *    Test 3: Allocate a block of 30 from file allocation
 *         H5MF_try_shrink() the block by 40 : fail
 *    Test 4: Allocate a block of 30 from file allocation
 *         H5MF_try_shrink() the block by 20 from the end: succeed
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_mf_eoa_shrink(const char *env_h5_drvr, hid_t fapl)
{
    hid_t        file = -1;              /* File ID */
    hid_t        fapl_new = -1;        /* copy of fapl */
    char        filename[FILENAME_LEN]; /* Filename to use */
    H5F_t        *f = NULL;              /* Internal file object pointer */
    h5_stat_size_t      file_size = 0, new_file_size; /* file size */
    H5FD_mem_t         type;
    haddr_t        addr = 0;
    haddr_t         ma_addr=HADDR_UNDEF, new_ma_addr=HADDR_UNDEF;
    hsize_t         ma_size=0, new_ma_size=0;
    hbool_t             contig_addr_vfd;        /* Whether VFD used has a contigous address space */

    TESTING("H5MF_try_shrink() of file allocation: test 1");

    /* Skip test when using VFDs that has different address spaces for each
     *  type of metadata allocation.
     */
    contig_addr_vfd = (hbool_t)(HDstrcmp(env_h5_drvr, "split") && HDstrcmp(env_h5_drvr, "multi"));
    if(contig_addr_vfd) {
        /* Set the filename to use for this test (dependent on fapl) */
        h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

        if((fapl_new = H5Pcopy(fapl)) < 0) TEST_ERROR

        /* Create the file to work on */
        if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Close file */
        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Turn off using meta/small data aggregator */
        H5Pset_meta_block_size(fapl_new, (hsize_t)0);
        H5Pset_small_data_block_size(fapl_new, (hsize_t)0);

        /* Re-open the file with meta/small data setting */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl_new)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        H5MF__aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        type = H5FD_MEM_SUPER;
        addr = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE30);

        if (addr < (haddr_t)file_size)
            TEST_ERROR

        /* nothing should be changed in meta_aggr */
        H5MF__aggr_query(f, &(f->shared->meta_aggr), &new_ma_addr, &new_ma_size);
        if (new_ma_addr != ma_addr) TEST_ERROR
        if (new_ma_size != ma_size) TEST_ERROR

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((new_file_size = h5_get_file_size(filename, fapl_new)) < 0)
            TEST_ERROR

        /* Verify the file is the correct size */
        if (new_file_size != (file_size+TBLOCK_SIZE30))
            TEST_ERROR

        /* Re-open the file */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl_new)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        H5MF__aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        /* should succeed */
        if(H5MF_try_shrink(f, type, addr, (hsize_t)TBLOCK_SIZE30) <= 0)
            TEST_ERROR

        /* nothing should be changed in meta_aggr */
        H5MF__aggr_query(f, &(f->shared->meta_aggr), &new_ma_addr, &ma_size);
        if(new_ma_addr != ma_addr)
            TEST_ERROR
        if(new_ma_size != ma_size)
            TEST_ERROR

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((new_file_size = h5_get_file_size(filename, fapl_new)) < 0)
            TEST_ERROR

        /* Verify the file is the correct size */
        if(new_file_size != file_size)
            TEST_ERROR

        PASSED()
    } /* end if */
    else {
        SKIPPED();
        HDputs("    Current VFD doesn't support metadata aggregator");
    } /* end else */

    TESTING("H5MF_try_shrink() of file allocation: test 2");

    /* Skip test when using VFDs that has different address spaces for each
     *  type of metadata allocation.
     */
    if(contig_addr_vfd) {
        /* Re-open the file with meta/small data setting */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl_new)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        H5MF__aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        addr = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE30);

        if (addr < (haddr_t)file_size)
            TEST_ERROR

        /* should not succeed in shrinking */
        if(H5MF_try_shrink(f, type, addr, (hsize_t)TBLOCK_SIZE30 - 10) > 0)
            TEST_ERROR

        /* nothing should be changed in meta_aggr */
        H5MF__aggr_query(f, &(f->shared->meta_aggr), &new_ma_addr, &ma_size);
        if (new_ma_addr != ma_addr) TEST_ERROR
        if (new_ma_size != ma_size) TEST_ERROR

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((new_file_size = h5_get_file_size(filename, fapl_new)) < 0)
            TEST_ERROR

        /* Verify the file is the correct size */
        if(new_file_size != (file_size + TBLOCK_SIZE30))
            TEST_ERROR

        PASSED()
    } /* end if */
    else {
        SKIPPED();
        HDputs("    Current VFD doesn't support metadata aggregator");
    } /* end else */


    TESTING("H5MF_try_shrink() of file allocation: test 3");

    /* Skip test when using VFDs that has different address spaces for each
     *  type of metadata allocation.
     */
    if(contig_addr_vfd) {
        /* Re-open the file with meta/small data setting */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl_new)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        H5MF__aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        /* should not succeed in shrinking */
        if(H5MF_try_shrink(f, type, addr, (hsize_t)TBLOCK_SIZE30 + 10) > 0)
            TEST_ERROR

        /* nothing should be changed in meta_aggr */
        H5MF__aggr_query(f, &(f->shared->meta_aggr), &new_ma_addr, &ma_size);
        if (new_ma_addr != ma_addr) TEST_ERROR
        if (new_ma_size != ma_size) TEST_ERROR

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((new_file_size = h5_get_file_size(filename, fapl_new)) < 0)
            TEST_ERROR

        /* Verify the file is the correct size */
        if(new_file_size != (file_size + TBLOCK_SIZE30))
            TEST_ERROR

        PASSED()
    } /* end if */
    else {
        SKIPPED();
        HDputs("    Current VFD doesn't support metadata aggregator");
    } /* end else */

    TESTING("H5MF_try_shrink() of file allocation: test 4");

    /* Skip test when using VFDs that has different address spaces for each
     *  type of metadata allocation.
     */
    if(contig_addr_vfd) {
        /* Re-open the file with meta/small data setting */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl_new)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        H5MF__aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        /* should succeed in shrinking */
        if(H5MF_try_shrink(f, type, addr+10, (hsize_t)(TBLOCK_SIZE30 - 10)) <= 0)
            TEST_ERROR

        /* nothing should be changed in meta_aggr */
        H5MF__aggr_query(f, &(f->shared->meta_aggr), &new_ma_addr, &ma_size);
        if(new_ma_addr != ma_addr)
            TEST_ERROR
        if(new_ma_size != ma_size)
            TEST_ERROR

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((new_file_size = h5_get_file_size(filename, fapl_new)) < 0)
            TEST_ERROR

        /* Verify the file is the correct size */
        if(new_file_size != (file_size + 10))
            TEST_ERROR

        if(H5Pclose(fapl_new) < 0)
            FAIL_STACK_ERROR

        PASSED()
    } /* end if */
    else {
        SKIPPED();
        HDputs("    Current VFD doesn't support metadata aggregator");
    } /* end else */

    return(0);

error:
    H5E_BEGIN_TRY {
        H5Pclose(fapl_new);
        H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_mf_eoa_shrink() */

/*
 *-------------------------------------------------------------------------
 * To verify that an allocated block from file allocation is extended.
 *
 * Set up:
 *     Turn off using meta/small data aggregator
 *     There is nothing in free-space manager
 *
 * Test 1: Allocate a block of 30
 *    H5MF_try_extend() the block of size 30 by 50: succeed
 *
 * Test 2: Allocate a block of 30
 *     H5MF_try_extend() the block of size 20 by 50: fail
 *-------------------------------------------------------------------------
 */
static unsigned
test_mf_eoa_extend(const char *env_h5_drvr, hid_t fapl)
{
    hid_t        file = -1;                  /* File ID */
    hid_t        fapl_new = -1;            /* copy of fapl */
    char        filename[FILENAME_LEN];     /* Filename to use */
    H5F_t        *f = NULL;                  /* Internal file object pointer */
    h5_stat_size_t      file_size, new_file_size;      /* File size */
    H5FD_mem_t  type;
    haddr_t        addr;
    htri_t      was_extended;
    haddr_t     ma_addr=HADDR_UNDEF, new_ma_addr=HADDR_UNDEF;
    hsize_t     ma_size=0, new_ma_size=0;
    hbool_t     contig_addr_vfd;        /* Whether VFD used has a contigous address space */

    TESTING("H5MF_try_extend() of file allocation: test 1");

    /* Skip test when using VFDs that has different address spaces for each
     *  type of metadata allocation.
     */
    contig_addr_vfd = (hbool_t)(HDstrcmp(env_h5_drvr, "split") && HDstrcmp(env_h5_drvr, "multi"));
    if(contig_addr_vfd) {
        /* Set the filename to use for this test (dependent on fapl) */
        h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

        if((fapl_new = H5Pcopy(fapl)) < 0) TEST_ERROR

        /* Create the file to work on */
        if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Close file */
        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of a file */
        if((file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Turn off using meta/small data aggregator */
        if(H5Pset_meta_block_size(fapl_new, (hsize_t)0) < 0)
            FAIL_STACK_ERROR
        if(H5Pset_small_data_block_size(fapl_new, (hsize_t)0) < 0)
            FAIL_STACK_ERROR

        /* Re-open the file with meta/small data setting */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl_new)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        H5MF__aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        type = H5FD_MEM_SUPER;
        addr = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE30);
        if (addr < (haddr_t)file_size)
            TEST_ERROR

        /* nothing should be changed in meta_aggr */
        H5MF__aggr_query(f, &(f->shared->meta_aggr), &new_ma_addr, &new_ma_size);
        if (new_ma_addr != ma_addr)
            TEST_ERROR

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((new_file_size = h5_get_file_size(filename, fapl_new)) < 0)
            TEST_ERROR

        /* Verify the file is the correct size */
        if(new_file_size != (file_size + TBLOCK_SIZE30))
            TEST_ERROR

        /* Re-open the file */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl_new)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        /* should succeed */
        was_extended = H5MF_try_extend(f, type, (haddr_t)addr, (hsize_t)TBLOCK_SIZE30, (hsize_t)TBLOCK_SIZE50);

        if(was_extended <= 0)
            TEST_ERROR

        /* nothing should be changed in meta_aggr */
        H5MF__aggr_query(f, &(f->shared->meta_aggr), &new_ma_addr, &new_ma_size);
        if (new_ma_addr != ma_addr)
            TEST_ERROR

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((new_file_size = h5_get_file_size(filename, fapl_new)) < 0)
            TEST_ERROR

        /* Verify the file is the correct size */
        if(new_file_size != (file_size + TBLOCK_SIZE30 + TBLOCK_SIZE50))
            TEST_ERROR

        PASSED()
    } /* end if */
    else {
        SKIPPED();
        HDputs("    Current VFD doesn't support metadata aggregator");
    } /* end else */

    TESTING("H5MF_try_extend() of file allocation: test 2");

    /* Skip test when using VFDs that has different address spaces for each
     *  type of metadata allocation.
     */
    if(contig_addr_vfd) {
        /* Re-open the file with meta/small data setting */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl_new)) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((file_size = h5_get_file_size(filename, fapl_new)) < 0)
            TEST_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        H5MF__aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        type = H5FD_MEM_SUPER;
        addr = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE30);

        if(addr < (haddr_t)file_size)
            TEST_ERROR

        /* nothing should be changed in meta_aggr */
        H5MF__aggr_query(f, &(f->shared->meta_aggr), &new_ma_addr, &new_ma_size);
        if(new_ma_addr != ma_addr)
            TEST_ERROR

        was_extended = H5MF_try_extend(f, type, (haddr_t)addr, (hsize_t)(TBLOCK_SIZE30-10), (hsize_t)(TBLOCK_SIZE50));

        /* should not succeed */
        if(was_extended > 0)
            TEST_ERROR

        /* nothing should be changed in meta_aggr */
        H5MF__aggr_query(f, &(f->shared->meta_aggr), &new_ma_addr, &new_ma_size);
        if (new_ma_addr != ma_addr)
            TEST_ERROR

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((new_file_size = h5_get_file_size(filename, fapl_new)) < 0)
            TEST_ERROR

        /* Verify the file is the correct size */
        if(new_file_size != file_size + TBLOCK_SIZE30)
            TEST_ERROR

        if(H5Pclose(fapl_new) < 0)
            FAIL_STACK_ERROR

        PASSED()
    } /* end if */
    else {
        SKIPPED();
        HDputs("    Current VFD doesn't support metadata aggregator");
    } /* end else */

    return(0);

error:
    H5E_BEGIN_TRY {
        H5Pclose(fapl_new);
        H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_mf_eoa_extend() */

/*
 *-------------------------------------------------------------------------
 * To verify that temporary blocks are allocated correctly
 *
 * Set up:
 *     There is nothing in free-space manager
 *
 * Tests:
 *      Allocate a reasonable-sized temporary block
 *      Check that the temporary address is high enough
 *      Check that file I/O with the temporary address fails
 *      Check that freeing a temporary address fails
 *      Check that closing the file doesn't change the file's size
 *      Check that overlapping normal & temporary address space fails:
 *         - Reopen the file
 *         - Allocate enough temporary space to use ~1/3 of the file
 *         - Allocate enough 'normal' space to use ~1/3 of the file
 *         - Check that allocating another 1/2 of the file as temporary address
 *              space fails
 *         - Check that allocating another 1/2 of the file as normal address
 *              space fails
 *-------------------------------------------------------------------------
 */
static unsigned
test_mf_tmp(const char *env_h5_drvr, hid_t fapl, hbool_t new_format)
{
    hid_t   file = -1;      /* File ID */
    hid_t   fapl2 = -1;     /* File access property list */
    hid_t   fcpl = -1;      /* File creation property list */

    if(new_format)
        TESTING("'temporary' file space allocation with new library format")
    else
        TESTING("'temporary' file space allocation with old library format")

    /* Can't run this test with multi-file VFDs */
    if(HDstrcmp(env_h5_drvr, "split") && HDstrcmp(env_h5_drvr, "multi") && HDstrcmp(env_h5_drvr, "family")) {
        char        filename[FILENAME_LEN]; /* Filename to use */
        H5F_t        *f = NULL;              /* Internal file object pointer */
        h5_stat_size_t  file_size, new_file_size;      /* file size */
        haddr_t         maxaddr;                /* File's max. address */
        haddr_t        tmp_addr;               /* Temporary space file address */
        haddr_t        norm_addr;              /* Normal space file address */
        haddr_t        check_addr;             /* File address for checking for errors */
        unsigned char   buf = 0;                /* Buffer to read/write with */
        herr_t          status;                 /* Generic status value */

        /* Set the filename to use for this test */
        h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

        if((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
            FAIL_STACK_ERROR

        if(new_format) {
            /* Copy the file access property list */
            if((fapl2 = H5Pcopy(fapl)) < 0) FAIL_STACK_ERROR

            /* Set the "use the latest version of the format" bounds for creating objects in the file */
            if(H5Pset_libver_bounds(fapl2, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
                FAIL_STACK_ERROR

            H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_PAGE, FALSE, (hsize_t)1);
        } /* end if */

        /* Create the file to work on */
        if((file = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, new_format?fapl2:fapl)) < 0)
            FAIL_STACK_ERROR

        /* Close file */
        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR


        /* Re-open the file */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        /* Retrieve the file's maxaddr */
        if(H5F_get_maxaddr_test(file, &maxaddr) < 0)
            FAIL_STACK_ERROR

        /* Allocate some temporary address space */
        if(HADDR_UNDEF == (tmp_addr = H5MF_alloc_tmp(f, (hsize_t)TBLOCK_SIZE30)))
            FAIL_STACK_ERROR

        /* Check if temporary file address is valid */
        if(!H5F_IS_TMP_ADDR(f, tmp_addr))
            TEST_ERROR
        if(tmp_addr < (haddr_t)(maxaddr - TBLOCK_SIZE30))
            TEST_ERROR

        /* Reading & writing with a temporary address value should fail */
        H5E_BEGIN_TRY {
            status = H5F_block_read(f, H5FD_MEM_SUPER, tmp_addr, sizeof(buf), &buf);
        } H5E_END_TRY;
        if(status >= 0)
            TEST_ERROR
        H5E_BEGIN_TRY {
            status = H5F_block_write(f, H5FD_MEM_SUPER, tmp_addr, sizeof(buf), &buf);
        } H5E_END_TRY;
        if(status >= 0)
            TEST_ERROR

        /* Freeing a temporary address value should fail */
        H5E_BEGIN_TRY {
            status = H5MF_xfree(f, H5FD_MEM_SUPER, tmp_addr, (hsize_t)TBLOCK_SIZE30);
        } H5E_END_TRY;
        if(status >= 0)
            TEST_ERROR

        /* Close the file */
        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((new_file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Verify the file is the correct size */
        if(new_file_size != file_size)
            TEST_ERROR


        /* Re-open the file */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        /* Allocate 1/3 of the file as temporary address space */
        if(HADDR_UNDEF == (tmp_addr = H5MF_alloc_tmp(f, (hsize_t)(maxaddr / 3))))
            FAIL_STACK_ERROR
        if(!H5F_IS_TMP_ADDR(f, tmp_addr))
            TEST_ERROR

        /* Allocate 1/3 of the file as normal address space */
        if(HADDR_UNDEF == (norm_addr = H5MF_alloc(f, H5FD_MEM_DRAW, (hsize_t)(maxaddr / 3))))
            FAIL_STACK_ERROR
        if(H5F_IS_TMP_ADDR(f, norm_addr))
            TEST_ERROR

        /* Test that pushing temporary space allocation into normal space fails */
        H5E_BEGIN_TRY {
            check_addr = H5MF_alloc_tmp(f, (hsize_t)(maxaddr / 3));
        } H5E_END_TRY;
        if(H5F_addr_defined(check_addr))
            TEST_ERROR

        /* Test that pushing normal space allocation into temporary space fails */
        H5E_BEGIN_TRY {
            check_addr = H5MF_alloc(f, H5FD_MEM_DRAW, (hsize_t)(maxaddr / 3));
        } H5E_END_TRY;
        if(H5F_addr_defined(check_addr))
            TEST_ERROR

        /* Free the normal block (so the file doesn't blow up to a huge size) */
        if(H5MF_xfree(f, H5FD_MEM_DRAW, norm_addr, (hsize_t)(maxaddr / 3)) < 0)
            FAIL_STACK_ERROR

        /* Close the file */
        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((new_file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Verify the file is the correct size */
        if(new_file_size != file_size)
            TEST_ERROR

        PASSED()
    } /* end if */
    else {
        SKIPPED();
        HDputs("    Current VFD doesn't support continuous address space");
    } /* end else */

    return(0);

error:
    H5E_BEGIN_TRY {
        H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_mf_tmp() */

/*
 *-------------------------------------------------------------------------
 * To verify that the free-space manager is created or opened
 *
 * Set up:
 *     Turn off using meta/small data aggregator
 *-------------------------------------------------------------------------
 */
static unsigned
test_mf_fs_start(hid_t fapl)
{
    hid_t        file = -1;              /* File ID */
    hid_t        fapl_new = -1;        /* copy of fapl */
    char        filename[FILENAME_LEN]; /* Filename to use */
    H5F_t        *f = NULL;              /* Internal file object pointer */
    h5_stat_size_t      file_size, new_file_size; /* file size */
    H5FS_stat_t         state;


    TESTING("H5MF_create_fstype()/H5MF__open_fstype() of free-space manager");

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    if((fapl_new = H5Pcopy(fapl)) < 0) TEST_ERROR

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Get the size of the file  */
    if((file_size = h5_get_file_size(filename, fapl)) < 0)
        TEST_ERROR

    /* Turn off using meta/small data aggregator */
    H5Pset_meta_block_size(fapl_new, (hsize_t)0);
    H5Pset_small_data_block_size(fapl_new, (hsize_t)0);

    /* Re-open the file with meta/small data setting */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl_new)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = (H5F_t *)H5I_object(file)))
        FAIL_STACK_ERROR

    /* Start up H5FD_MEM_SUPER free-space manager */
    if(H5MF__start_fstype(f, (H5F_mem_page_t)H5FD_MEM_SUPER) < 0)
        FAIL_STACK_ERROR

    if(f->shared->fs_state[H5FD_MEM_SUPER] != H5F_FS_STATE_OPEN)
        TEST_ERROR
    if(f->shared->fs_man[H5FD_MEM_SUPER]->client != H5FS_CLIENT_FILE_ID)
        TEST_ERROR

    HDmemset(&state, 0, sizeof(H5FS_stat_t));

    if(check_stats(f, f->shared->fs_man[H5FD_MEM_SUPER], &state))
        TEST_ERROR

    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Get the size of the file */
    if((new_file_size = h5_get_file_size(filename, fapl_new)) < 0)
        TEST_ERROR

    /* Verify the file is the correct size */
    if(new_file_size != file_size)
        TEST_ERROR

    if(H5Pclose(fapl_new) < 0)
        FAIL_STACK_ERROR

    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        H5Pclose(fapl_new);
        H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_mf_fs_start() */


/*
 *-------------------------------------------------------------------------
 * To verify that a block is allocated/freed from/to the free-space manager
 *
 * Set up:
 *     Turn off using meta/small data aggregator
 *
 * Test 1:
 *    Add section A to free-space manager (addr=70, size=30)
 *    Allocate a block of size=30
 *    The returned space's address should be same as section A's address
 *    Deallocate the block which will be returned to the free-space manager
 * Test 2:
 *    Add section A to free-space manager (addr=70, size=30)
 *    Allocate a block of size=20
 *    The returned space's address should be same as section A's address
 *    There should still be space of 10 left in the free-space manager
 *    Deallocate the block which will be returned to free-space manager
 * Test 3:
 *    Add section A to free-space manager (addr=70, size=30)
 *    Allocate a block of size=40
 *    The free-space manager is unable to fulfill the request
 *    The block is allocated from file allocation
 *    Deallocate the block which will be returned to free-space manager
 *    (the space is shrunk and freed since it is at end of file)
 *-------------------------------------------------------------------------
 */
static unsigned
test_mf_fs_alloc_free(hid_t fapl)
{
    hid_t        file = -1;              /* File ID */
    hid_t        fapl_new = -1;        /* copy of fapl */
    char        filename[FILENAME_LEN]; /* Filename to use */
    H5F_t        *f = NULL;              /* Internal file object pointer */
    h5_stat_size_t      file_size, new_file_size;     /* file size */
    H5MF_free_section_t *sect_node = NULL;
    haddr_t     addr;
    haddr_t     tmp;
    H5FS_stat_t     state;

    TESTING("H5MF_alloc()/H5MF_xfree() of free-space manager:test 1");

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    if((fapl_new = H5Pcopy(fapl)) < 0) TEST_ERROR

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Get the size of a file */
    if((file_size = h5_get_file_size(filename, fapl)) < 0)
        TEST_ERROR

    /* Turn off using meta/small data aggregator */
    H5Pset_meta_block_size(fapl_new, (hsize_t)0);
    H5Pset_small_data_block_size(fapl_new, (hsize_t)0);

    /* Re-open the file with meta/small data setting */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl_new)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = (H5F_t *)H5I_object(file)))
        FAIL_STACK_ERROR

    /* Start up H5FD_MEM_SUPER free-space manager */
    if(H5MF__start_fstype(f, (H5F_mem_page_t)H5FD_MEM_SUPER) < 0)
        FAIL_STACK_ERROR

    if(f->shared->fs_state[H5FD_MEM_SUPER] != H5F_FS_STATE_OPEN)
        TEST_ERROR
    if(f->shared->fs_man[H5FD_MEM_SUPER]->client != H5FS_CLIENT_FILE_ID)
        TEST_ERROR

    /* Create section A */
    sect_node = H5MF__sect_new(H5MF_FSPACE_SECT_SIMPLE, (haddr_t)TBLOCK_ADDR70, (hsize_t)TBLOCK_SIZE30);

    /* Add section A to free-space manager */
    if(H5MF__add_sect(f, H5FD_MEM_SUPER, f->shared->fs_man[H5FD_MEM_SUPER], sect_node))
        FAIL_STACK_ERROR

    HDmemset(&state, 0, sizeof(H5FS_stat_t));
    state.tot_space += TBLOCK_SIZE30;
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;

    if(check_stats(f, f->shared->fs_man[H5FD_MEM_SUPER], &state))
        TEST_ERROR

    /* Allocate a block of 30 */
    addr = H5MF_alloc(f, H5FD_MEM_SUPER, (hsize_t)TBLOCK_SIZE30);

    /* Verify that the allocated block is section A in free-space */
    if(addr != TBLOCK_ADDR70)
        TEST_ERROR

    state.tot_space -= TBLOCK_SIZE30;
    state.tot_sect_count -= 1;
    state.serial_sect_count -= 1;

    if(check_stats(f, f->shared->fs_man[H5FD_MEM_SUPER], &state))
        TEST_ERROR

    /* Free the block to free-space */
    H5MF_xfree(f, H5FD_MEM_SUPER, addr, (hsize_t)TBLOCK_SIZE30);

    state.tot_space += TBLOCK_SIZE30;
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;
    if(check_stats(f, f->shared->fs_man[H5FD_MEM_SUPER], &state))
        TEST_ERROR

    /* Remove section A from free-space */
    if(H5MF__find_sect(f, H5FD_MEM_SUPER, (hsize_t)TBLOCK_SIZE30, f->shared->fs_man[H5FD_MEM_SUPER], &tmp) != TRUE)
        TEST_ERROR

    if(tmp != TBLOCK_ADDR70)
        TEST_ERROR

    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Get the size of the file */
    if((new_file_size = h5_get_file_size(filename, fapl_new)) < 0)
        TEST_ERROR

    /* Verify the file is the correct size */
    if (new_file_size != file_size)
        TEST_ERROR

    PASSED()

    TESTING("H5MF_alloc()/H5MF_xfree() of free-space manager:test 2");

    /* Re-open the file with meta/small data setting */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl_new)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = (H5F_t *)H5I_object(file)))
        FAIL_STACK_ERROR

    /* Start up H5FD_MEM_SUPER free-space manager */
    if(H5MF__start_fstype(f, (H5F_mem_page_t)H5FD_MEM_SUPER) < 0)
        FAIL_STACK_ERROR

    if(f->shared->fs_state[H5FD_MEM_SUPER] != H5F_FS_STATE_OPEN)
        TEST_ERROR
    if(f->shared->fs_man[H5FD_MEM_SUPER]->client != H5FS_CLIENT_FILE_ID)
        TEST_ERROR

    /* Create section A */
    sect_node = H5MF__sect_new(H5MF_FSPACE_SECT_SIMPLE, (haddr_t)TBLOCK_ADDR70, (hsize_t)TBLOCK_SIZE30);

     /* Add section A to free-space manager */
    if(H5MF__add_sect(f, H5FD_MEM_SUPER, f->shared->fs_man[H5FD_MEM_SUPER], sect_node))
        FAIL_STACK_ERROR

    HDmemset(&state, 0, sizeof(H5FS_stat_t));
    state.tot_space += TBLOCK_SIZE30;
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;

    if(check_stats(f, f->shared->fs_man[H5FD_MEM_SUPER], &state))
        TEST_ERROR

    /* Allocate a block of 20 */
    addr = H5MF_alloc(f, H5FD_MEM_SUPER, (hsize_t)(TBLOCK_SIZE20));

    /* Verify that the allocated block is section A in free-space manager */
    if(addr != TBLOCK_ADDR70)
        TEST_ERROR

    /* should still have 1 section of size 10 left in free-space manager */
    state.tot_space -= (TBLOCK_SIZE20);

    if(check_stats(f, f->shared->fs_man[H5FD_MEM_SUPER], &state))
        TEST_ERROR

    /* Free the block to free-space manager */
    H5MF_xfree(f, H5FD_MEM_SUPER, addr, (hsize_t)(TBLOCK_SIZE20));

    /* Still 1 section in free-space because of merging */
    state.tot_space += TBLOCK_SIZE20;
    if(check_stats(f, f->shared->fs_man[H5FD_MEM_SUPER], &state))
        TEST_ERROR

    /* Remove section A from free-space */
    if(H5MF__find_sect(f, H5FD_MEM_SUPER, (hsize_t)TBLOCK_SIZE30, f->shared->fs_man[H5FD_MEM_SUPER], &tmp) != TRUE)
        TEST_ERROR

    if(tmp != TBLOCK_ADDR70)
        TEST_ERROR

    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Get the size of the file */
    if((new_file_size = h5_get_file_size(filename, fapl_new)) < 0)
        TEST_ERROR

    /* Verify the file is the correct size */
    if (new_file_size != file_size)
        TEST_ERROR

    PASSED()

    TESTING("H5MF_alloc()/H5MF_xfree() of free-space manager:test 3");

    /* Re-open the file with meta/small data setting */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl_new)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = (H5F_t *)H5I_object(file)))
        FAIL_STACK_ERROR

    /* Start up H5FD_MEM_SUPER free-space manager */
    if(H5MF__start_fstype(f, (H5F_mem_page_t)H5FD_MEM_SUPER) < 0)
        FAIL_STACK_ERROR

    if(f->shared->fs_state[H5FD_MEM_SUPER] != H5F_FS_STATE_OPEN)
        TEST_ERROR
    if(f->shared->fs_man[H5FD_MEM_SUPER]->client != H5FS_CLIENT_FILE_ID)
        TEST_ERROR

    /* Create section A */
    sect_node = H5MF__sect_new(H5MF_FSPACE_SECT_SIMPLE, (haddr_t)TBLOCK_ADDR70, (hsize_t)TBLOCK_SIZE30);

    /* Add section A to free-space manager */
    if(H5MF__add_sect(f, H5FD_MEM_SUPER, f->shared->fs_man[H5FD_MEM_SUPER], sect_node))
        FAIL_STACK_ERROR

    HDmemset(&state, 0, sizeof(H5FS_stat_t));
    state.tot_space += TBLOCK_SIZE30;
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;

    if(check_stats(f, f->shared->fs_man[H5FD_MEM_SUPER], &state))
        TEST_ERROR

    /*
     * Allocate a block of 40.
     * Since free-space manager cannot fulfull the request,
     * the block is obtained from file allocation
     */
    addr = H5MF_alloc(f, H5FD_MEM_SUPER, (hsize_t)(TBLOCK_SIZE40));

    /* Verify that the allocated block is not section A in free-space */
    if(addr == TBLOCK_ADDR70)
        TEST_ERROR

    /* free-space info should be the same  */
    if(check_stats(f, f->shared->fs_man[H5FD_MEM_SUPER], &state))
        TEST_ERROR

    /* Remove section A from free-space */
    if(H5MF__find_sect(f, H5FD_MEM_SUPER, (hsize_t)TBLOCK_SIZE30, f->shared->fs_man[H5FD_MEM_SUPER], &tmp) != TRUE)
        TEST_ERROR

    /* Verify that the block is section A in free-space */
    if(tmp != TBLOCK_ADDR70)
        TEST_ERROR

    HDmemset(&state, 0, sizeof(H5FS_stat_t));
    if(check_stats(f, f->shared->fs_man[H5FD_MEM_SUPER], &state))
        TEST_ERROR

    /* Free the block of size 40 to free-space */
    H5MF_xfree(f, H5FD_MEM_SUPER, addr, (hsize_t)(TBLOCK_SIZE40));

    /*
     * Free-space info is the same.
     * The block is returned to free-space.
     * It is shrunk and freed because it is at end of file.
     */
    if(check_stats(f, f->shared->fs_man[H5FD_MEM_SUPER], &state))
        TEST_ERROR

    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Get the size of the file */
    if((new_file_size = h5_get_file_size(filename, fapl_new)) < 0)
        TEST_ERROR

    /* Verify the file is the correct size */
    if(new_file_size != file_size)
        TEST_ERROR

    if(H5Pclose(fapl_new) < 0)
        FAIL_STACK_ERROR

    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        H5Pclose(fapl_new);
        H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_mf_fs_alloc_free() */


/*
 *-------------------------------------------------------------------------
 * To verify that a block allocated from the free-space manager can be extended
 *
 * Set up:
 *     Turn off using meta/small data aggregator
 *
 * Test 1:
 *    Add section A to free-space manager: addr=70, size=30
 *    Allocate a block of size 30 from free-space manager
 *    Add section B to free-space manager: addr=100, size=50
 *    Try to extend the allocated block by requested-size=50
 *    Succeed: section A adjoins section B (70+30=100 which is section B's address) and
 *         requested-size (50) is equal to the size of section B
 * Test 2:
 *    Add section A to free-space manager: addr=70, size=30
 *    Allocate a block of size 30 from free-space manager
 *    Add section B to free-space manager: addr=100, size=50
 *    Try to extend the allocated block by requested-size=60
 *    Fail: section A adjoins section B (70+30=100 which is section B's address) but
 *          requested-size (60) > size of section B (50)
 *
 * Test 3:
 *    Add section A to free-space manager: addr=70, size=30
 *    Allocate a block of size 30 from free-space manager
 *    Add section B to free-space manager: addr=100, size=50
 *    Try to extend the allocated block by requested-size=40
 *    Succeed: section A adjoins section B (70+30=100 which is section B's address) and
 *         requested-size (40) < size of section B (50), therefore,
 *         a section of 10 is left in the free-space manager
 * Test 4:
 *    Add section A to free-space manager: addr=70, size=20
 *    Allocate a block of size 20 from free-space manager
 *    Add section B to free-space manager: addr=100, size=50
 *     Try to extend the allocated block by 50 from the free-space_manager:
 *    Fail: section A does not adjoin section B (70+20 != address of section B) even though
 *          the requested-size (50) equal to size of section B (50)
 *-------------------------------------------------------------------------
 */
static unsigned
test_mf_fs_extend(hid_t fapl)
{
    hid_t        file = -1;              /* File ID */
    hid_t        fapl_new = -1;        /* copy of fapl */
    char        filename[FILENAME_LEN]; /* Filename to use */
    H5F_t        *f = NULL;              /* Internal file object pointer */
    h5_stat_size_t      file_size, new_file_size; /* file size */
    H5MF_free_section_t *sect_node1 = NULL, *sect_node2=NULL;
    haddr_t     addr;
    haddr_t     tmp;
    H5FS_stat_t     state;              /* State of free space*/
    htri_t          was_extended;

    TESTING("H5MF_try_extend() of free-space manager:test 1");

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    if((fapl_new = H5Pcopy(fapl)) < 0) TEST_ERROR

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Get the size of a file */
    if((file_size = h5_get_file_size(filename, fapl)) < 0)
        TEST_ERROR

    /* Turn off using meta/small data aggregator */
    H5Pset_meta_block_size(fapl_new, (hsize_t)0);
    H5Pset_small_data_block_size(fapl_new, (hsize_t)0);

    /* Re-open the file with meta/small data setting */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl_new)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = (H5F_t *)H5I_object(file)))
        FAIL_STACK_ERROR

    /* Start up H5FD_MEM_SUPER free-space manager */
    if(H5MF__start_fstype(f, (H5F_mem_page_t)H5FD_MEM_SUPER) < 0)
        FAIL_STACK_ERROR

    if(f->shared->fs_state[H5FD_MEM_SUPER] != H5F_FS_STATE_OPEN)
        TEST_ERROR
    if(f->shared->fs_man[H5FD_MEM_SUPER]->client != H5FS_CLIENT_FILE_ID)
        TEST_ERROR

    /* Create section A */
    sect_node1 = H5MF__sect_new(H5MF_FSPACE_SECT_SIMPLE, (haddr_t)TBLOCK_ADDR70, (hsize_t)TBLOCK_SIZE30);

    /* Add section A to free-space manager */
    if(H5MF__add_sect(f, H5FD_MEM_SUPER, f->shared->fs_man[H5FD_MEM_SUPER], sect_node1))
        FAIL_STACK_ERROR

    HDmemset(&state, 0, sizeof(H5FS_stat_t));
    state.tot_space += TBLOCK_SIZE30;
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;

    if(check_stats(f, f->shared->fs_man[H5FD_MEM_SUPER], &state))
        TEST_ERROR

    /* Allocate a block of 30 */
    addr = H5MF_alloc(f, H5FD_MEM_SUPER, (hsize_t)TBLOCK_SIZE30);

    /* Verify that the allocated block is section A in free-space manager */
    if(addr != TBLOCK_ADDR70)
        TEST_ERROR

    state.tot_space -= TBLOCK_SIZE30;
    state.tot_sect_count -= 1;
    state.serial_sect_count -= 1;

    if(check_stats(f, f->shared->fs_man[H5FD_MEM_SUPER], &state))
        TEST_ERROR

    /* Create section B */
    sect_node2 = H5MF__sect_new(H5MF_FSPACE_SECT_SIMPLE, (haddr_t)TBLOCK_ADDR100, (hsize_t)TBLOCK_SIZE50);

    /* Add section B to free-space manager */
    if(H5MF__add_sect(f, H5FD_MEM_SUPER, f->shared->fs_man[H5FD_MEM_SUPER], sect_node2))
        FAIL_STACK_ERROR

    state.tot_space += TBLOCK_SIZE50;
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;

    if(check_stats(f, f->shared->fs_man[H5FD_MEM_SUPER], &state))
        TEST_ERROR

    /* Try to extend the allocated block */
    was_extended = H5MF_try_extend(f, H5FD_MEM_SUPER, (haddr_t)TBLOCK_ADDR70, (hsize_t)TBLOCK_SIZE30, (hsize_t)TBLOCK_SIZE50);

    /* should succeed */
    if(was_extended <= 0)
        TEST_ERROR

    /* Section B is removed from free-space manager */
    state.tot_space -= TBLOCK_SIZE50;
    state.tot_sect_count -= 1;
    state.serial_sect_count -= 1;

    if(check_stats(f, f->shared->fs_man[H5FD_MEM_SUPER], &state))
        TEST_ERROR

    /* Free the extended block to free-space manager */
    H5MF_xfree(f, H5FD_MEM_SUPER, addr, (hsize_t)(TBLOCK_SIZE30+TBLOCK_SIZE50));

    /* Verify that the extended block is back into free-space */
    state.tot_space += (TBLOCK_SIZE30+TBLOCK_SIZE50);
    state.tot_sect_count = 1;
    state.serial_sect_count = 1;

    if(check_stats(f, f->shared->fs_man[H5FD_MEM_SUPER], &state))
        TEST_ERROR

    /* Remove the extended block */
    if(H5MF__find_sect(f, H5FD_MEM_SUPER, (hsize_t)(TBLOCK_SIZE30+TBLOCK_SIZE50), f->shared->fs_man[H5FD_MEM_SUPER], &tmp) != TRUE)
        TEST_ERROR

    if(tmp != TBLOCK_ADDR70)
        TEST_ERROR

    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Get the size of the file */
    if((new_file_size = h5_get_file_size(filename, fapl_new)) < 0)
        TEST_ERROR

    /* Verify the file is the correct size */
    if (new_file_size != file_size)
        TEST_ERROR

    PASSED()

    TESTING("H5MF_try_extend() of free-space manager:test 2");

    /* Re-open the file with meta/small data setting */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl_new)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = (H5F_t *)H5I_object(file)))
        FAIL_STACK_ERROR

    /* Start up H5FD_MEM_SUPER free-space manager */
    if(H5MF__start_fstype(f, (H5F_mem_page_t)H5FD_MEM_SUPER) < 0)
        FAIL_STACK_ERROR

    if(f->shared->fs_state[H5FD_MEM_SUPER] != H5F_FS_STATE_OPEN)
        TEST_ERROR
    if(f->shared->fs_man[H5FD_MEM_SUPER]->client != H5FS_CLIENT_FILE_ID)
        TEST_ERROR

    /* Create section A */
    sect_node1 = H5MF__sect_new(H5MF_FSPACE_SECT_SIMPLE, (haddr_t)TBLOCK_ADDR70, (hsize_t)TBLOCK_SIZE30);

    /* Add section A to free-space manager */
    if(H5MF__add_sect(f, H5FD_MEM_SUPER, f->shared->fs_man[H5FD_MEM_SUPER], sect_node1))
        FAIL_STACK_ERROR

    HDmemset(&state, 0, sizeof(H5FS_stat_t));
    state.tot_space += TBLOCK_SIZE30;
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;

    if(check_stats(f, f->shared->fs_man[H5FD_MEM_SUPER], &state))
        TEST_ERROR

    /* Allocate a block of 30 */
    addr = H5MF_alloc(f, H5FD_MEM_SUPER, (hsize_t)TBLOCK_SIZE30);

    /* Verify that the allocated block is section A in free-space manager */
    if(addr != TBLOCK_ADDR70)
        TEST_ERROR

    state.tot_space -= TBLOCK_SIZE30;
    state.tot_sect_count -= 1;
    state.serial_sect_count -= 1;

    if(check_stats(f, f->shared->fs_man[H5FD_MEM_SUPER], &state))
        TEST_ERROR

    /* Create section B */
    sect_node2 = H5MF__sect_new(H5MF_FSPACE_SECT_SIMPLE, (haddr_t)TBLOCK_ADDR100, (hsize_t)TBLOCK_SIZE50);

    /* Add section B to free-space manager */
    if(H5MF__add_sect(f, H5FD_MEM_SUPER, f->shared->fs_man[H5FD_MEM_SUPER], sect_node2))
        FAIL_STACK_ERROR

    state.tot_space += TBLOCK_SIZE50;
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;

    if(check_stats(f, f->shared->fs_man[H5FD_MEM_SUPER], &state))
        TEST_ERROR

    /* Try to extend the allocated block */
    was_extended = H5MF_try_extend(f, H5FD_MEM_SUPER, (haddr_t)TBLOCK_ADDR70, (hsize_t)TBLOCK_SIZE30, (hsize_t)(TBLOCK_SIZE50+10));

    /* Should not be able to extend the allocated block */
    if(was_extended)
        TEST_ERROR

    /* free-space info should remain the same */
    if(check_stats(f, f->shared->fs_man[H5FD_MEM_SUPER], &state))
        TEST_ERROR

    /* Free the allocated block A to free-space */
    H5MF_xfree(f, H5FD_MEM_SUPER, addr, (hsize_t)TBLOCK_SIZE30);

    /* the returned section A is merged with section B in free-space */
    /* rest of the info remains the same */
    state.tot_space += TBLOCK_SIZE30;

    if(check_stats(f, f->shared->fs_man[H5FD_MEM_SUPER], &state))
        TEST_ERROR

    /* Remove the merged sections A & B from free-space */
    if(H5MF__find_sect(f, H5FD_MEM_SUPER, (hsize_t)(TBLOCK_SIZE30+TBLOCK_SIZE50), f->shared->fs_man[H5FD_MEM_SUPER], &tmp) != TRUE)
        TEST_ERROR

    if(tmp != addr) TEST_ERROR

    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Get the size of the file */
    if((new_file_size = h5_get_file_size(filename, fapl_new)) < 0)
        TEST_ERROR

    /* Verify the file is the correct size */
    if (new_file_size != file_size)
        TEST_ERROR

    PASSED()

    TESTING("H5MF_try_extend() of free-space manager:test 3");

    /* Re-open the file with meta/small data setting */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl_new)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = (H5F_t *)H5I_object(file)))
        FAIL_STACK_ERROR

    /* Start up H5FD_MEM_SUPER free-space manager */
    if(H5MF__start_fstype(f, (H5F_mem_page_t)H5FD_MEM_SUPER) < 0)
        FAIL_STACK_ERROR

    if(f->shared->fs_state[H5FD_MEM_SUPER] != H5F_FS_STATE_OPEN)
        TEST_ERROR
    if(f->shared->fs_man[H5FD_MEM_SUPER]->client != H5FS_CLIENT_FILE_ID)
        TEST_ERROR

    /* Create section A */
    sect_node1 = H5MF__sect_new(H5MF_FSPACE_SECT_SIMPLE, (haddr_t)TBLOCK_ADDR70, (hsize_t)TBLOCK_SIZE30);

    /* Add section A to free-space manager */
    if(H5MF__add_sect(f, H5FD_MEM_SUPER, f->shared->fs_man[H5FD_MEM_SUPER], sect_node1))
        FAIL_STACK_ERROR

    HDmemset(&state, 0, sizeof(H5FS_stat_t));
    state.tot_space += TBLOCK_SIZE30;
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;

    if(check_stats(f, f->shared->fs_man[H5FD_MEM_SUPER], &state))
        TEST_ERROR

    /* Allocate a block of 30 */
    addr = H5MF_alloc(f, H5FD_MEM_SUPER, (hsize_t)TBLOCK_SIZE30);

    /* Verify that the allocated block is section A in free-space manager */
    if(addr != TBLOCK_ADDR70)
        TEST_ERROR

    state.tot_space -= TBLOCK_SIZE30;
    state.tot_sect_count -= 1;
    state.serial_sect_count -= 1;

    if(check_stats(f, f->shared->fs_man[H5FD_MEM_SUPER], &state))
        TEST_ERROR

    /* Create section B */
    sect_node2 = H5MF__sect_new(H5MF_FSPACE_SECT_SIMPLE, (haddr_t)TBLOCK_ADDR100, (hsize_t)TBLOCK_SIZE50);

    /* Add section B to free-space manager */
    if(H5MF__add_sect(f, H5FD_MEM_SUPER, f->shared->fs_man[H5FD_MEM_SUPER], sect_node2))
        FAIL_STACK_ERROR

    state.tot_space += TBLOCK_SIZE50;
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;

    if(check_stats(f, f->shared->fs_man[H5FD_MEM_SUPER], &state))
        TEST_ERROR

    /* Try to extend the allocated block */
    was_extended = H5MF_try_extend(f, H5FD_MEM_SUPER, (haddr_t)TBLOCK_ADDR70, (hsize_t)TBLOCK_SIZE30, (hsize_t)(TBLOCK_SIZE40));

    /* Should succeed in extending the allocated block */
    if(was_extended <=0)
        TEST_ERROR

    /* Should have 1 section of size=10 left in free-space manager */
    state.tot_space -= (TBLOCK_SIZE40);
    if(check_stats(f, f->shared->fs_man[H5FD_MEM_SUPER], &state))
        TEST_ERROR

    /* Free the extended block  */
    H5MF_xfree(f, H5FD_MEM_SUPER, addr, (hsize_t)(TBLOCK_SIZE30+TBLOCK_SIZE40));

    /* rest info is same, the extended section returned is merged with the section in free-space */
    state.tot_space += (TBLOCK_SIZE30+TBLOCK_SIZE40);

    if(check_stats(f, f->shared->fs_man[H5FD_MEM_SUPER], &state))
        TEST_ERROR

    /* Remove the merged sections A & B from free-space */
    if(H5MF__find_sect(f, H5FD_MEM_SUPER, (hsize_t)(TBLOCK_SIZE30+TBLOCK_SIZE50), f->shared->fs_man[H5FD_MEM_SUPER], &tmp) != TRUE)
        TEST_ERROR

    if(tmp != addr) TEST_ERROR

    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Get the size of the file */
    if((new_file_size = h5_get_file_size(filename, fapl_new)) < 0)
        TEST_ERROR

    /* Verify the file is the correct size */
    if (new_file_size != file_size)
        TEST_ERROR

    PASSED()

    TESTING("H5MF_try_extend() of free-space manager:test 4");

    /* Re-open the file with meta/small data setting */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl_new)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = (H5F_t *)H5I_object(file)))
        FAIL_STACK_ERROR

    /* Start up H5FD_MEM_SUPER free-space manager */
    if(H5MF__start_fstype(f, (H5F_mem_page_t)H5FD_MEM_SUPER) < 0)
        FAIL_STACK_ERROR

    if(f->shared->fs_state[H5FD_MEM_SUPER] != H5F_FS_STATE_OPEN)
        TEST_ERROR
    if(f->shared->fs_man[H5FD_MEM_SUPER]->client != H5FS_CLIENT_FILE_ID)
        TEST_ERROR

    /* Create section A */
    sect_node1 = H5MF__sect_new(H5MF_FSPACE_SECT_SIMPLE, (haddr_t)TBLOCK_ADDR70, (hsize_t)(TBLOCK_SIZE30-10));

    /* Add section A of size=20 to free-space */
    if(H5MF__add_sect(f, H5FD_MEM_SUPER, f->shared->fs_man[H5FD_MEM_SUPER], sect_node1))
        FAIL_STACK_ERROR

    HDmemset(&state, 0, sizeof(H5FS_stat_t));
    state.tot_space += (TBLOCK_SIZE30-10);
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;

    if(check_stats(f, f->shared->fs_man[H5FD_MEM_SUPER], &state))
        TEST_ERROR

    /* Allocate a block of size=20 */
    addr = H5MF_alloc(f, H5FD_MEM_SUPER, (hsize_t)(TBLOCK_SIZE30-10));

    /* Verify that the allocated block is section A in free-space manager */
    if(addr != TBLOCK_ADDR70)
        TEST_ERROR

    state.tot_space -= (TBLOCK_SIZE30-10);
    state.tot_sect_count -= 1;
    state.serial_sect_count -= 1;

    if(check_stats(f, f->shared->fs_man[H5FD_MEM_SUPER], &state))
        TEST_ERROR

    /* Create section B */
    sect_node2 = H5MF__sect_new(H5MF_FSPACE_SECT_SIMPLE, (haddr_t)TBLOCK_ADDR100, (hsize_t)TBLOCK_SIZE50);

    /* Add section B to free-space manager */
    if(H5MF__add_sect(f, H5FD_MEM_SUPER, f->shared->fs_man[H5FD_MEM_SUPER], sect_node2))
        FAIL_STACK_ERROR

    state.tot_space += TBLOCK_SIZE50;
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;

    if(check_stats(f, f->shared->fs_man[H5FD_MEM_SUPER], &state))
        TEST_ERROR

    /* Try to extend the allocated block */
    was_extended = H5MF_try_extend(f, H5FD_MEM_SUPER, (haddr_t)TBLOCK_ADDR70, (hsize_t)(TBLOCK_SIZE30-10), (hsize_t)TBLOCK_SIZE50);

    /* Should not succeed in extending the allocated block */
    if(was_extended)
        TEST_ERROR

    /* Free-space info should be the same */
    if(check_stats(f, f->shared->fs_man[H5FD_MEM_SUPER], &state))
        TEST_ERROR

    /* Free the allocated block */
    H5MF_xfree(f, H5FD_MEM_SUPER, addr, (hsize_t)(TBLOCK_SIZE30-10));

    state.tot_space += (TBLOCK_SIZE30-10);
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;

    if(check_stats(f, f->shared->fs_man[H5FD_MEM_SUPER], &state))
        TEST_ERROR

    /* Remove section A from free-space manger */
    if(H5MF__find_sect(f, H5FD_MEM_SUPER, (hsize_t)(TBLOCK_SIZE30-10), f->shared->fs_man[H5FD_MEM_SUPER], &tmp) != TRUE)
        TEST_ERROR

    if(tmp != addr) TEST_ERROR

    /* Remove section B from free-space manager */
    if(H5MF__find_sect(f, H5FD_MEM_SUPER, (hsize_t)TBLOCK_SIZE50, f->shared->fs_man[H5FD_MEM_SUPER], &tmp) != TRUE)
        TEST_ERROR

    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Get the size of the file */
    if((new_file_size = h5_get_file_size(filename, fapl_new)) < 0)
        TEST_ERROR

    /* Verify the file is the correct size */
    if(new_file_size != file_size)
        TEST_ERROR

    if(H5Pclose(fapl_new) < 0)
        FAIL_STACK_ERROR

    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        H5Pclose(fapl_new);
        H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_mf_fs_extend() */

/*
 *-------------------------------------------------------------------------
 * To verify that an aggregator is absorbed into a section.
 *
 *    Test 1: To aborb the aggregator onto the beginning of the section
 *        Allocate block A from meta_aggr
 *        Create a free-space section node with an address that adjoins
 *            the end of meta_aggr and a size to make the aggregator
 *            get absorbed into the section.
 *        The adding of the section to free-space will call H5MF_aggr_absorb(),
 *            which will absorb meta_aggr to the section:
 *              section size + remaining size of aggregator is > aggr->alloc_size,
 *              section is allowed to absorb an aggregator (allow_sect_absorb is true)
 *
 *    Test 2: To absorb the aggregator onto the end of the section
 *        Allocate block A from meta_aggr
 *        Allocate block B from sdata_aggr
 *        Create a free-space section node with an address that adjoins
 *            the beginning of meta_aggr and a size to make the
 *            aggregator get absorbed into the section.
 *        The adding of the section to free-space will call H5MF_aggr_absorb(),
 *            which will absorb meta_aggr to the section:
 *              section size + remaining size of aggregator is > aggr->alloc_size,
 *              section is allowed to absorb an aggregator (allow_sect_absorb is true)
 *-------------------------------------------------------------------------
 */
static unsigned
test_mf_fs_absorb(const char *env_h5_drvr, hid_t fapl)
{
    hid_t       file = -1;              /* File ID */
    char        filename[FILENAME_LEN]; /* Filename to use */
    H5F_t       *f = NULL;              /* Internal file object pointer */
    haddr_t     addr, saddr;
    haddr_t     tmp;
    haddr_t     ma_addr=HADDR_UNDEF;
    hsize_t     ma_size=0;
    H5MF_free_section_t *sect_node=NULL;
    hbool_t    contig_addr_vfd;         /* Whether VFD used has a contigous address space */

    TESTING("A free-space section absorbs an aggregator: test 1");

    /* Skip test when using VFDs that don't use the metadata aggregator */
    contig_addr_vfd = (hbool_t)(HDstrcmp(env_h5_drvr, "split") && HDstrcmp(env_h5_drvr, "multi"));
    if(contig_addr_vfd) {
        /* Set the filename to use for this test (dependent on fapl) */
        h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

        /* Create the file to work on */
        if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Close file */
        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Re-open the file */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        /* Start up H5FD_MEM_SUPER free-space manager */
        if(H5MF__start_fstype(f, (H5F_mem_page_t)H5FD_MEM_SUPER) < 0)
            FAIL_STACK_ERROR

        if(f->shared->fs_state[H5FD_MEM_SUPER] != H5F_FS_STATE_OPEN)
            TEST_ERROR
        if(f->shared->fs_man[H5FD_MEM_SUPER]->client != H5FS_CLIENT_FILE_ID)
            TEST_ERROR

        /* Allocate a section from meta_aggr */
        addr = H5MF_alloc(f, H5FD_MEM_SUPER, (hsize_t)TBLOCK_SIZE30);

        H5MF__aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

    /* Create a section */
    sect_node = H5MF__sect_new(H5MF_FSPACE_SECT_SIMPLE, (haddr_t)(ma_addr+ma_size), (hsize_t)TBLOCK_SIZE2048);

        /* Add a section to free-space that adjoins end of the aggregator */
    if(H5MF__add_sect(f, H5FD_MEM_SUPER, f->shared->fs_man[H5FD_MEM_SUPER], sect_node))
        FAIL_STACK_ERROR

        /* Verify that the section did absorb the aggregator */
    if(H5MF__find_sect(f, H5FD_MEM_SUPER, (hsize_t)(ma_addr+ma_size), f->shared->fs_man[H5FD_MEM_SUPER], &tmp) != TRUE)
            TEST_ERROR

        if(tmp != ma_addr)  TEST_ERROR

        /* Restore info for aggregator */
        f->shared->meta_aggr.addr = ma_addr;
        f->shared->meta_aggr.size = ma_size;

        /* Remove section from meta_aggr */
        H5MF_xfree(f, H5FD_MEM_SUPER, addr, (hsize_t)TBLOCK_SIZE30);

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        PASSED()
    } /* end if */
    else {
        SKIPPED();
        HDputs("    Current VFD doesn't support metadata aggregator");
    } /* end else */

    TESTING("A free-space section absorbs an aggregator: test 2");

    /* Skip test when using VFDs that don't use the metadata aggregator */
    if(contig_addr_vfd) {
        /* Re-open the file */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        /* Start up H5FD_MEM_SUPER free-space manager */
        if(H5MF__start_fstype(f, (H5F_mem_page_t)H5FD_MEM_SUPER) < 0)
            FAIL_STACK_ERROR

        if(f->shared->fs_state[H5FD_MEM_SUPER] != H5F_FS_STATE_OPEN)
            TEST_ERROR
        if(f->shared->fs_man[H5FD_MEM_SUPER]->client != H5FS_CLIENT_FILE_ID)
            TEST_ERROR

        /* Allocate a section from meta_aggr */
        addr = H5MF_alloc(f, H5FD_MEM_SUPER, (hsize_t)TBLOCK_SIZE30);
        H5MF__aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        /* Allocate a section from sdata_aggr */
        saddr = H5MF_alloc(f, H5FD_MEM_DRAW, (hsize_t)TBLOCK_SIZE50);

        /* Add a section to free-space that adjoins the beginning of meta_aggr */
        sect_node = H5MF__sect_new(H5MF_FSPACE_SECT_SIMPLE, (haddr_t)addr, (hsize_t)TBLOCK_SIZE30);

        /* When adding, meta_aggr is absorbed onto the end of the section */
        if(H5MF__add_sect(f, H5FD_MEM_SUPER, f->shared->fs_man[H5FD_MEM_SUPER], sect_node))
            FAIL_STACK_ERROR

        /* Verify that the section did absorb the aggregator */
        if(H5MF__find_sect(f, H5FD_MEM_SUPER, (hsize_t)(ma_size+TBLOCK_SIZE30), f->shared->fs_man[H5FD_MEM_SUPER], &tmp) != TRUE)
            TEST_ERROR

        if((tmp + TBLOCK_SIZE30) != ma_addr)
            TEST_ERROR

        /* restore info to meta_aggr */
        f->shared->meta_aggr.addr = ma_addr;
        f->shared->meta_aggr.size = ma_size;

        /* Remove section from meta_aggr */
        H5MF_xfree(f, H5FD_MEM_SUPER, addr, (hsize_t)TBLOCK_SIZE30);
        /* Remove section from sdata_aggr */
        H5MF_xfree(f, H5FD_MEM_DRAW, saddr, (hsize_t)TBLOCK_SIZE50);

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        PASSED()
    } /* end if */
    else {
        SKIPPED();
        HDputs("    Current VFD doesn't support metadata aggregator");
    } /* end else */

    return(0);

error:
    H5E_BEGIN_TRY {
        H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_mf_fs_absorb() */

/*
 *-------------------------------------------------------------------------
 * To verify that blocks are allocated from the aggregator
 *
 *    Allocate first block (30) from meta_aggr: (nothing in the aggregator)
 *        request-size is > what is left in meta_aggr and < meta_aggr->alloc_size
 *    Result:
 *        A block of meta_aggr->alloc_size is allocated from file allocation
 *        The first block of 30 is allocated from meta_aggr
 *        There is space of 2018 left in meta_aggr
 *
 *    Allocate second block (50) from meta_aggr:
 *        request-size is <= what is left in meta_aggr
 *    Result:
 *        The second block of 50 is allocated from meta_aggr
 *        There is space of 1968 left in meta_aggr
 *-------------------------------------------------------------------------
 */
static unsigned
test_mf_aggr_alloc1(const char *env_h5_drvr, hid_t fapl)
{
    hid_t        file = -1;              /* File ID */
    char        filename[FILENAME_LEN]; /* Filename to use */
    H5F_t        *f = NULL;              /* Internal file object pointer */
    hid_t       fcpl;                   /* File creation property list */
    h5_stat_size_t      file_size, new_file_size; /* file size */
    H5FD_mem_t         type;
    haddr_t        addr1, addr2;
    haddr_t         ma_addr=HADDR_UNDEF;
    hsize_t         ma_size=0;
    hbool_t             contig_addr_vfd;        /* Whether VFD used has a contigous address space */

    TESTING("H5MF_alloc() of meta/sdata aggregator:test 1");

    /* Skip test when using VFDs that don't use the metadata aggregator */
    contig_addr_vfd = (hbool_t)(HDstrcmp(env_h5_drvr, "split") && HDstrcmp(env_h5_drvr, "multi"));
    if(contig_addr_vfd) {
        /* Set the filename to use for this test (dependent on fapl) */
        h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

        /* File creation property list template */
        if((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
            TEST_ERROR

        /* Set to H5F_FSPACE_STRATEGY_AGGR strategy */
        if(H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_AGGR, FALSE, (hsize_t)1) < 0)
            TEST_ERROR

        /* Create the file to work on */
        if((file = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Close file */
        if(H5Fclose(file) < 0)
            TEST_ERROR

        /* Get the size of a file  */
        if((file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Re-open the file */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
            TEST_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            TEST_ERROR

        /* Allocate first block from meta_aggr */
        type = H5FD_MEM_SUPER;
        if((addr1 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE30)) == HADDR_UNDEF)
            TEST_ERROR

        H5MF__aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);
        if((addr1+TBLOCK_SIZE30) != ma_addr)
            TEST_ERROR

        /* Allocate second block from meta_aggr */
        if((addr2 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE50)) == HADDR_UNDEF)
            TEST_ERROR

        H5MF__aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);
        if((addr2+TBLOCK_SIZE50) != ma_addr)
            TEST_ERROR
        if(ma_size != (TBLOCK_SIZE2048 - (TBLOCK_SIZE30 + TBLOCK_SIZE50)))
            TEST_ERROR

        if(H5Fclose(file) < 0)
            TEST_ERROR

        /* Get the size of the file */
        if((new_file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Verify the file is the correct size */
        if(new_file_size != (file_size+TBLOCK_SIZE30+TBLOCK_SIZE50))
            TEST_ERROR

        /* Re-open the file */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
            TEST_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            TEST_ERROR

        /* Free the two blocks: order matters because of H5F_FSPACE_STRATEGY_AGGR strategy */
        if(H5MF_xfree(f, type, addr2, (hsize_t)TBLOCK_SIZE50) < 0)
            TEST_ERROR
        if(H5MF_xfree(f, type, addr1, (hsize_t)TBLOCK_SIZE30) < 0)
            TEST_ERROR

        if(H5Fclose(file) < 0)
            TEST_ERROR

        /* Get the size of the file */
        if((new_file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Verify the file is the correct size */
        if (new_file_size != file_size)
            TEST_ERROR

        if(H5Pclose(fcpl) < 0)
            TEST_ERROR

        PASSED()
    } /* end if */
    else {
        SKIPPED();
        HDputs("    Current VFD doesn't support metadata aggregator");
    } /* end else */

    return(0);

error:
    H5E_BEGIN_TRY {
        H5Fclose(file);
        H5Pclose(fcpl);
    } H5E_END_TRY;
    return(1);
} /* test_mf_aggr_alloc1() */

/*
 *-------------------------------------------------------------------------
 * To verify that blocks are allocated from the aggregator
 *
 *    Allocate first block (30) from meta_aggr: (nothing in the aggregator)
 *        request-size is > what is left in meta_aggr and < meta_aggr->alloc_size
 *    Result:
 *        A block of meta_aggr->alloc_size is obtained via file allocation
 *        There is space of 2018 left in meta_aggr
 *
 *    Allocate second block (50) from meta_aggr:
 *        request-size is <= what is left in meta_aggr
 *    Result:
 *        The second block of 50 is allocated from what is left in meta_aggr
 *        There is space of 1968 left in meta_aggr
 *
 *    Allocate third block (2058) from meta_aggr:
 *        request-size is > what is left in meta_aggr and is >= meta_aggr->alloc_size
 *        meta_aggr is at EOA
 *    Result:
 *        A block of request-size is extended via file allocation and is merged with meta_aggr
 *        The block of 2058 is allocated out of meta_aggr
 *        There is space of 1968 left in meta_aggr
 *-------------------------------------------------------------------------
 */
static unsigned
test_mf_aggr_alloc2(const char *env_h5_drvr, hid_t fapl)
{
    hid_t        file = -1;              /* File ID */
    char        filename[FILENAME_LEN]; /* Filename to use */
    H5F_t        *f = NULL;              /* Internal file object pointer */
    h5_stat_size_t      file_size, new_file_size; /* file size */
    H5FD_mem_t         type;
    haddr_t        addr1, addr2, addr3;
    haddr_t         ma_addr=HADDR_UNDEF;
    hsize_t         ma_size=0;
    hbool_t             contig_addr_vfd;        /* Whether VFD used has a contigous address space */

    TESTING("H5MF_alloc() of meta/sdata aggregator:test 2");

    /* Skip test when using VFDs that don't use the metadata aggregator */
    contig_addr_vfd = (hbool_t)(HDstrcmp(env_h5_drvr, "split") && HDstrcmp(env_h5_drvr, "multi"));
    if(contig_addr_vfd) {
        /* Set the filename to use for this test (dependent on fapl) */
        h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

        /* Create the file to work on */
        if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Close file */
        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of a file */
        if((file_size= h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Re-open the file */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        type = H5FD_MEM_SUPER;
        addr1 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE30);

        H5MF__aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);
        if((addr1+TBLOCK_SIZE30) != ma_addr)
            TEST_ERROR

        addr2 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE50);

        H5MF__aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);
        if((addr2+TBLOCK_SIZE50) != ma_addr)
            TEST_ERROR

        if (ma_size != (TBLOCK_SIZE2048 - (TBLOCK_SIZE30 + TBLOCK_SIZE50)))
            TEST_ERROR

        addr3 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE2058);

        H5MF__aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        if((addr3+TBLOCK_SIZE2058) != ma_addr)
            TEST_ERROR

        if(ma_size != (TBLOCK_SIZE2048 - (TBLOCK_SIZE30 + TBLOCK_SIZE50)))
            TEST_ERROR

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((new_file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Verify the file is the correct size */
        /* Unused space is freed from the end of the file */
        if(new_file_size != (file_size+TBLOCK_SIZE30+TBLOCK_SIZE50+TBLOCK_SIZE2058))
            TEST_ERROR

        /* Re-open the file */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        H5MF_xfree(f, type, addr1, (hsize_t)TBLOCK_SIZE30+TBLOCK_SIZE50+TBLOCK_SIZE2058);

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((new_file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Verify the file is the correct size */
        if (new_file_size != file_size)
            TEST_ERROR

        PASSED()
    } /* end if */
    else {
        SKIPPED();
        HDputs("    Current VFD doesn't support metadata aggregator");
    } /* end else */

    return(0);

error:
    H5E_BEGIN_TRY {
        H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_mf_aggr_alloc2() */

/*
 *-------------------------------------------------------------------------
 * To verify that blocks are allocated from the aggregator
 *
 *    Allocate first block (30) from meta_aggr : (nothing in the aggregator)
 *        request-size is > what is left in meta_aggr and < meta_aggr->alloc_size
 *    Result:
 *        A block of meta_aggr->alloc_size is obtained via file allocation
 *        The first block of 30 is allocated from there
 *        There is space of 2018 left in meta_aggr
 *
 *    Allocate second block (50) from meta_aggr:
 *        request-size is <= what is left in meta_aggr
 *    Result:
 *        The second block of 50 is allocated from what is left in meta_aggr
 *        There is space of 1968 left in meta_aggr
 *
 *    Allocate first block (30) from sdata_aggr: (nothing in sdata_aggr)
 *        request-size is > what is left in other_aggr and is < sdata_aggr->alloc_size
 *    Result:
 *        A block of sdata_aggr->alloc_size is obtained via file allocation
 *        The first block of 30 is allocated from there
 *        There is space of 2018 left in sdata_aggr
 *
 *    Allocate the third block (2058) from meta_aggr:
 *        request-size is > what is left in meta_aggr and >= meta_aggr->alloc_size
 *        sdata_aggr is at EOA but has not used up more than sdata_aggr->alloc_size
 *    Result: A block of request-size is obtained via file allocation
 *        The new block's address is returned
 *        Nothing is changed in meta_aggr and sdata_aggr
 *
 *    Allocate fourth block (50) from meta_aggr:
 *        request-size is <= what is left in meta_aggr and < meta_aggr->alloc_size
 *    Result:
 *        The fourth block of 50 is allocated from what is left in meta_aggr
 *        There is space of 1968 left in meta_aggr
 *-------------------------------------------------------------------------
 */
static unsigned
test_mf_aggr_alloc3(const char *env_h5_drvr, hid_t fapl)
{
    hid_t        file = -1;              /* File ID */
    char        filename[FILENAME_LEN]; /* Filename to use */
    H5F_t        *f = NULL;              /* Internal file object pointer */
    h5_stat_size_t      file_size, new_file_size; /* file size */
    H5FD_mem_t         type, stype;
    haddr_t        addr1, addr2, addr3, addr4, saddr1;
    haddr_t         ma_addr=HADDR_UNDEF, new_ma_addr=HADDR_UNDEF;
    hsize_t         ma_size=0, new_ma_size=0;
    haddr_t         sdata_addr=HADDR_UNDEF;
    hsize_t         sdata_size=0;
    hbool_t             contig_addr_vfd;        /* Whether VFD used has a contigous address space */

    TESTING("H5MF_alloc() of meta/sdata aggregator: test 3");

    /* Skip test when using VFDs that don't use the metadata aggregator */
    contig_addr_vfd = (hbool_t)(HDstrcmp(env_h5_drvr, "split") && HDstrcmp(env_h5_drvr, "multi"));
    if(contig_addr_vfd) {
        /* Set the filename to use for this test (dependent on fapl) */
        h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

        /* Create the file to work on */
        if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Close file */
        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Re-open the file */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        /* Allocate first block from meta_aggr */
        type = H5FD_MEM_SUPER;
        addr1 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE30);

        H5MF__aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);
        if ((addr1+TBLOCK_SIZE30) != ma_addr)
            TEST_ERROR

        /* Allocate second block from meta_aggr */
        addr2 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE50);

        H5MF__aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        if((addr2+TBLOCK_SIZE50) != ma_addr)
            TEST_ERROR
        if(ma_size != (TBLOCK_SIZE2048 - (TBLOCK_SIZE30 + TBLOCK_SIZE50)))
            TEST_ERROR

        /* Allocate first block from sdata_aggr */
        stype = H5FD_MEM_DRAW;
        saddr1 = H5MF_alloc(f, stype, (hsize_t)TBLOCK_SIZE30);
        H5MF__aggr_query(f, &(f->shared->sdata_aggr), &sdata_addr, &sdata_size);
        if((saddr1+TBLOCK_SIZE30) != sdata_addr)
            TEST_ERROR
        if(sdata_size != (TBLOCK_SIZE2048 - TBLOCK_SIZE30)) TEST_ERROR

        /* Allocate third block, which is from  file allocation not from meta_aggr */
        addr3 = H5MF_alloc(f, type, (hsize_t)(TBLOCK_SIZE2058));

        H5MF__aggr_query(f, &(f->shared->meta_aggr), &new_ma_addr, &new_ma_size);

        /* info for meta_aggr shouldn't be changed */
        if(addr3 != (sdata_addr+sdata_size)) TEST_ERROR
        if((addr3+TBLOCK_SIZE2058) == new_ma_addr) TEST_ERROR
        if((new_ma_addr != ma_addr) || (new_ma_size != ma_size)) TEST_ERROR

        /* Allocate fourth block, which should be from meta_aggr */
        addr4 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE50);
        H5MF__aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        if((addr4+TBLOCK_SIZE50) != ma_addr)
            TEST_ERROR
        if(ma_size != (TBLOCK_SIZE2048 - (TBLOCK_SIZE30 + TBLOCK_SIZE50 + TBLOCK_SIZE50)))
            TEST_ERROR

        /* Free all the allocated blocks */
        H5MF_xfree(f, type, addr1, (hsize_t)TBLOCK_SIZE30);
        H5MF_xfree(f, type, addr2, (hsize_t)TBLOCK_SIZE50);
        H5MF_xfree(f, type, addr3, (hsize_t)TBLOCK_SIZE2058);
        H5MF_xfree(f, type, addr4, (hsize_t)TBLOCK_SIZE50);
        H5MF_xfree(f, stype, saddr1, (hsize_t)TBLOCK_SIZE30);

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((new_file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Verify the file is the correct size */
        if(new_file_size != file_size)
            TEST_ERROR

        PASSED()
    } /* end if */
    else {
        SKIPPED();
        HDputs("    Current VFD doesn't support metadata aggregator");
    } /* end else */

    return(0);

error:
    H5E_BEGIN_TRY {
        H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_mf_aggr_alloc3() */


/*
 *-------------------------------------------------------------------------
 * To verify that blocks are allocated from the aggregator
 *
 *    Allocate first block (30) from meta_aggr: (nothing in the aggregator)
 *        request-size is > what is left in meta_aggr and < meta_aggr->alloc_size
 *    Result:
 *        A block of meta_aggr->alloc_size is obtained via file allocation
 *        There is space of 2018 left in meta_aggr
 *        The first block of 30 is allocated from there
 *
 *    Allocate first block (30) from sdata_aggr: (nothing in sdata_aggr)
 *        request-size is > what is left in sdata_aggr and < sdata_aggr->alloc_size
 *    Result:
 *        A block of sdata_aggr->alloc_size is obtained via file allocation
 *        The first block of 30 is allocated from there
 *
 *    Allocate the second block (2018) from sdata_aggr:
 *        request-size is <= what is left in sdata_aggr and < sdata_aggr->alloc_size
 *        request-size is < sdata_aggr->alloc_size
 *    Result:
 *        The block is allocated from what is left in sdata_aggr (all used up)
 *
 *    Allocate third block (50) from sdata_aggr :
 *        request-size is > what is left in sdata_aggr and < sdata_aggr->alloc_size
 *    Result:
 *        A block of sdata_aggr->alloc_size is extended via file allocation
 *        The third block of 50 is allocated from there
 *        There is space of 1998 left in the sdata_aggr
 *
 *    Allocate second block (2058) from meta_aggr:
 *        request-size is > what is left in meta_aggr and >= meta_aggr->alloc_size
 *        sdata_aggr is at EOA and has used up more than sdata_aggr->alloc_size
 *    Result:
 *        unused spaced in sdata_aggr is freed to free-space and is shrunk
 *        sdata_aggr is reset to 0
 *        A block of request-size is obtained via file allocation
 *        The new block's address is returned
 *        The block does not adjoin meta_aggr
 *        meta_aggr's info is unchanged
 *-------------------------------------------------------------------------
 */
static unsigned
test_mf_aggr_alloc4(const char *env_h5_drvr, hid_t fapl)
{
    hid_t        file = -1;              /* File ID */
    char        filename[FILENAME_LEN]; /* Filename to use */
    H5F_t        *f = NULL;              /* Internal file object pointer */
    h5_stat_size_t      file_size, new_file_size;    /* File size */
    H5FD_mem_t         type, stype;
    haddr_t        addr1, addr2, saddr1, saddr2, saddr3;
    haddr_t         ma_addr=HADDR_UNDEF, new_ma_addr=HADDR_UNDEF, sdata_addr=HADDR_UNDEF;
    hsize_t         ma_size=0, new_ma_size=0, sdata_size=0;
    hbool_t             contig_addr_vfd;        /* Whether VFD used has a contigous address space */

    TESTING("H5MF_alloc() of meta/sdata aggregator:test 4");

    /* Skip test when using VFDs that don't use the metadata aggregator */
    contig_addr_vfd = (hbool_t)(HDstrcmp(env_h5_drvr, "split") && HDstrcmp(env_h5_drvr, "multi"));
    if(contig_addr_vfd) {
        /* Set the filename to use for this test (dependent on fapl) */
        h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

        /* Create the file to work on */
        if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Close file */
        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file  */
        if((file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Re-open the file */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        /* Allocate first block from meta_aggr */
        type = H5FD_MEM_SUPER;
        addr1 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE30);

        H5MF__aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        if((addr1+TBLOCK_SIZE30) != ma_addr)
            TEST_ERROR
        if(ma_size != (TBLOCK_SIZE2048 - TBLOCK_SIZE30))
            TEST_ERROR

        /* Allocate first block from sdata_aggr */
        stype = H5FD_MEM_DRAW;
        saddr1 = H5MF_alloc(f, stype, (hsize_t)TBLOCK_SIZE30);
        H5MF__aggr_query(f, &(f->shared->sdata_aggr), &sdata_addr, &sdata_size);
        if((saddr1+TBLOCK_SIZE30) != sdata_addr)
            TEST_ERROR

        /* Allocate second block from sdata_aggr */
        saddr2 = H5MF_alloc(f, stype, (hsize_t)(TBLOCK_SIZE2048 - TBLOCK_SIZE30));
        H5MF__aggr_query(f, &(f->shared->sdata_aggr), &sdata_addr, &sdata_size);
        if(saddr2+(TBLOCK_SIZE2048 - TBLOCK_SIZE30) != sdata_addr)
            TEST_ERROR

        /* Allocate third block from sdata_aggr */
        saddr3 = H5MF_alloc(f, stype, (hsize_t)TBLOCK_SIZE50);
        H5MF__aggr_query(f, &(f->shared->sdata_aggr), &sdata_addr, &sdata_size);

        if((saddr3+TBLOCK_SIZE50) != sdata_addr)
            TEST_ERROR
        if(sdata_size != (TBLOCK_SIZE2048 - TBLOCK_SIZE50))
            TEST_ERROR

        /* Allocate second block of 2058, which is from file allocation, not from meta_aggr */
        addr2 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE2058);

        if(addr2 != sdata_addr)
            TEST_ERROR

        /* sdata_aggr is reset 0 */
        H5MF__aggr_query(f, &(f->shared->sdata_aggr), &sdata_addr, &sdata_size);
        if((sdata_addr != 0) && (sdata_size != 0))
            TEST_ERROR

        /* info is unchanged in meta_aggr */
        H5MF__aggr_query(f, &(f->shared->meta_aggr), &new_ma_addr, &new_ma_size);
        if((new_ma_addr != ma_addr) && (new_ma_size != ma_size))
            TEST_ERROR

        /* Free all the allocated blocks */
        H5MF_xfree(f, type, addr1, (hsize_t)TBLOCK_SIZE30);
        H5MF_xfree(f, type, addr2, (hsize_t)TBLOCK_SIZE2058);
        H5MF_xfree(f, stype, saddr1, (hsize_t)TBLOCK_SIZE30);
        H5MF_xfree(f, stype, saddr2, (hsize_t)TBLOCK_SIZE2048 - TBLOCK_SIZE30);
        H5MF_xfree(f, stype, saddr3, (hsize_t)TBLOCK_SIZE50);

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((new_file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Verify the file is the correct size */
        if(new_file_size != file_size)
            TEST_ERROR

        PASSED()
    } /* end if */
    else {
        SKIPPED();
        HDputs("    Current VFD doesn't support metadata aggregator");
    } /* end else */

    return(0);

error:
    H5E_BEGIN_TRY {
        H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_mf_aggr_alloc4() */

/*
 *-------------------------------------------------------------------------
 * To verify that blocks are allocated from the aggregator
 *
 *    Allocate first block (30) from meta_aggr: (nothing in the aggregator)
 *        request-size is > what is left in meta_aggr and < meta_aggr->alloc_size
 *    Result:
 *        A block of meta_aggr->alloc_size is obtained via file allocation
 *        The first block of 30 is allocate from there
 *
 *    Allocate second block (50) from meta_aggr:
 *        request-size is < what is left in meta_aggr
 *    Result:
 *        The second block of 50 is allocated from what is left there
 *        There is space of 1968 left in the meta_aggr
 *
 *    Allocate third block (1970) from meta_aggr:
 *        request-size is > what is left in meta_aggr and is < meta_aggr->alloc_size
 *    Result: A block of meta_aggr->alloc_size is extended via file allocation and is absorbed into the meta_aggr
 *        The block of 1970 is allocated from there
 *        There is space of 2046 left in meta_aggr
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_mf_aggr_alloc5(const char *env_h5_drvr, hid_t fapl)
{
    hid_t        file = -1;              /* File ID */
    char        filename[FILENAME_LEN]; /* Filename to use */
    H5F_t        *f = NULL;              /* Internal file object pointer */
    h5_stat_size_t      file_size, new_file_size;  /* File size */
    H5FD_mem_t         type;
    haddr_t        addr1, addr2, addr3;
    haddr_t         ma_addr=HADDR_UNDEF, new_ma_addr=HADDR_UNDEF;
    hsize_t         ma_size=0, new_ma_size=0;
    hbool_t             contig_addr_vfd;        /* Whether VFD used has a contigous address space */

    TESTING("H5MF_alloc() of meta/sdata aggregator:test 5");

    /* Skip test when using VFDs that don't use the metadata aggregator */
    contig_addr_vfd = (hbool_t)(HDstrcmp(env_h5_drvr, "split") && HDstrcmp(env_h5_drvr, "multi"));
    if(contig_addr_vfd) {
        /* Set the filename to use for this test (dependent on fapl) */
        h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

        /* Create the file to work on */
        if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Close file */
        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Re-open the file */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        /* Allocate first block from meta_aggr */
        type = H5FD_MEM_SUPER;
        addr1 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE30);

        H5MF__aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);
        if((addr1+TBLOCK_SIZE30) != ma_addr)
            TEST_ERROR

        /* Allocate second block from meta_aggr */
        addr2 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE50);
        H5MF__aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        if(addr2+TBLOCK_SIZE50 != ma_addr)
            TEST_ERROR
        if(ma_size != (TBLOCK_SIZE2048 - (TBLOCK_SIZE30+TBLOCK_SIZE50)))
            TEST_ERROR

        /* Allocate third block from meta_aggr */
        addr3 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE1970);
        H5MF__aggr_query(f, &(f->shared->meta_aggr), &new_ma_addr, &new_ma_size);

        if(addr3 != ma_addr) TEST_ERROR
        if((addr3+TBLOCK_SIZE1970) != new_ma_addr) TEST_ERROR
        if(new_ma_size != (TBLOCK_SIZE2048 - (TBLOCK_SIZE1970 - ma_size)))
            TEST_ERROR

        /* Free all the allocated blocks */
        H5MF_xfree(f, type, addr1, (hsize_t)TBLOCK_SIZE30);
        H5MF_xfree(f, type, addr2, (hsize_t)TBLOCK_SIZE50);
        H5MF_xfree(f, type, addr3, (hsize_t)TBLOCK_SIZE1970);

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((new_file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Verify the file is the correct size */
        if(new_file_size != file_size)
            TEST_ERROR

        PASSED()
    } /* end if */
    else {
        SKIPPED();
        HDputs("    Current VFD doesn't support metadata aggregator");
    } /* end else */

    return(0);

error:
    H5E_BEGIN_TRY {
        H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_mf_aggr_alloc5() */

/*
 *-------------------------------------------------------------------------
 * To verify that blocks are allocated from the aggregator
 *
 *    Allocate first block (30) from meta_aggr: (nothing in the aggregator)
 *        request-size is > what is left in meta_aggr and < meta_aggr->alloc_size
 *    Result:
 *        A block of meta_aggr->alloc_size is obtained via file allocation
 *        The first block of 30 is allocated from there
 *
 *    Allocate second block (50) from meta_aggr:
 *        request-size is <= what is left in meta_aggr
 *    Result:
 *        The second block of 50 is allocated from what is left in meta_aggr
 *        There is space of 1968 left in meta_aggr
 *
 *    Allocate first block (30) from sdata_aggr: (nothing in sdata_aggr)
 *        request-size is > what is left in sdata_aggr and is < sdata_aggr->alloc_size
 *    Result:
 *        A block of sdata_aggr->alloc_size is obtained via file allocation
 *        The first block of 30 is allocated from there
 *        There is space of 2018 left in sdata_aggr
 *
 *    Allocate third block (1970) from meta_aggr:
 *        request-size is > what is left in meta_aggr and < meta_aggr->alloc_size
 *        sdata_aggr is at EOA but has not used up more than sdata_aggr->alloc_size
 *    Result:
 *        A block of meta_aggr->alloc_size is obtained via file allocation.
 *        The block does not adjoin meta_aggr
 *        sdata_aggr is untouched
 *        meta_aggr's unused space of [880, 1968] is freed to free-space
 *        meta_aggr is updated to point to the new block
 *-------------------------------------------------------------------------
 */
static unsigned
test_mf_aggr_alloc6(const char *env_h5_drvr, hid_t fapl)
{
    hid_t        file = -1;              /* File ID */
    char        filename[FILENAME_LEN]; /* Filename to use */
    H5F_t        *f = NULL;              /* Internal file object pointer */
    h5_stat_size_t      file_size, new_file_size;  /* file size */
    H5FD_mem_t         type, stype;
    haddr_t        addr1, addr2, addr3, saddr1;
    haddr_t         ma_addr=HADDR_UNDEF, new_ma_addr=HADDR_UNDEF, sdata_addr=HADDR_UNDEF;
    hsize_t         ma_size=0, new_ma_size=0, sdata_size=0;
    H5FS_stat_t state;
    hbool_t             contig_addr_vfd;        /* Whether VFD used has a contigous address space */

    TESTING("H5MF_alloc() of meta/sdata aggregator:test 6");

    /* Skip test when using VFDs that don't use the metadata aggregator */
    contig_addr_vfd = (hbool_t)(HDstrcmp(env_h5_drvr, "split") && HDstrcmp(env_h5_drvr, "multi"));
    if(contig_addr_vfd) {
        /* Set the filename to use for this test (dependent on fapl) */
        h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

        /* Create the file to work on */
        if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Close file */
        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Re-open the file */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        type = H5FD_MEM_SUPER;

        /* Allocate first block from meta_aggr */
        addr1 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE30);

        H5MF__aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);
        if((addr1+TBLOCK_SIZE30) != ma_addr)
            TEST_ERROR

        /* Allocate second block from meta_aggr */
        addr2 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE50);
        H5MF__aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        if(addr2+TBLOCK_SIZE50 != ma_addr)
            TEST_ERROR
        if(ma_size != (TBLOCK_SIZE2048 - (TBLOCK_SIZE30 + TBLOCK_SIZE50)))
            TEST_ERROR

        /* Allocate first block from sdata_aggr */
        stype = H5FD_MEM_DRAW;
        saddr1 = H5MF_alloc(f, stype, (hsize_t)TBLOCK_SIZE30);

        H5MF__aggr_query(f, &(f->shared->sdata_aggr), &sdata_addr, &sdata_size);
        if((saddr1+TBLOCK_SIZE30) != sdata_addr) TEST_ERROR
        if(sdata_size != (TBLOCK_SIZE2048 - TBLOCK_SIZE30)) TEST_ERROR

        /* Allocate third block from meta_aggr */
        addr3 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE1970);

        H5MF__aggr_query(f, &(f->shared->meta_aggr), &new_ma_addr, &new_ma_size);

        if((addr3+TBLOCK_SIZE1970) != new_ma_addr) TEST_ERROR
        if(addr3 != (sdata_addr+sdata_size)) TEST_ERROR

        if((ma_addr+TBLOCK_SIZE1970) == new_ma_addr) TEST_ERROR
        if(new_ma_size != (TBLOCK_SIZE2048 - TBLOCK_SIZE1970))
            TEST_ERROR

        /* Verify that meta_aggr's unused space of 1968 is freed to free-space */
        HDmemset(&state, 0, sizeof(H5FS_stat_t));
        state.tot_space += (TBLOCK_SIZE2048 - (TBLOCK_SIZE30+TBLOCK_SIZE50));
        state.tot_sect_count += 1;
        state.serial_sect_count += 1;

        if(check_stats(f, f->shared->fs_man[type], &state))
            TEST_ERROR

        /* Free all the allocated blocks */
        H5MF_xfree(f, type, addr1, (hsize_t)TBLOCK_SIZE30);
        H5MF_xfree(f, type, addr2, (hsize_t)TBLOCK_SIZE50);
        H5MF_xfree(f, type, addr3, (hsize_t)TBLOCK_SIZE1970);
        H5MF_xfree(f, stype, saddr1, (hsize_t)TBLOCK_SIZE30);

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((new_file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Verify the file is the correct size */
        if(new_file_size != file_size)
            TEST_ERROR

        PASSED()
    } /* end if */
    else {
        SKIPPED();
        HDputs("    Current VFD doesn't support metadata aggregator");
    } /* end else */

    return(0);

error:
    H5E_BEGIN_TRY {
        H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_mf_aggr_alloc6() */

/*
 *-------------------------------------------------------------------------
 * To verify that blocks are allocated from the aggregator
 *
 *  Allocate first block (30) from meta_aggr: (nothing in meta_aggr)
 *      request-size is > what is left in meta_aggr and < meta_aggr->alloc_size
 *  Result:
 *      A block of meta_aggr->alloc_size is obtained via file allocation
 *      The first block of 30 is allocated from there
 *
 *    Allocate second block (50) from meta_aggr:
 *        request-size is <= what is left in meta_aggr
 *    Result:
 *        The second block of 50 is allocated from what is left in the aggregator
 *        There is space of 1968 left in the meta_aggr
 *
 *    Allocate first block (30) from sdata_aggr: (nothing in sdata_aggr)
 *        request-size is > what is left in sdata_aggr->size and < sdata_aggr->alloc_size
 *     Result:
 *        A block of sdata_aggr->alloc_size is obtained via file allocation
 *        The first block of 30 is allocate from there
 *
 *    Allocate second block (2018) from sdata_aggr:
 *        request-size is <= what is left in sdata_aggr and is < sdata_aggr->alloc_size
 *    Result:
 *        The second block of 2018 is allocated from what is left in sdata_aggr (all used up)
 *
 *    Allocate third block (50) from sdata_aggr:
 *        request-size is > what is left in sdata_aggr and < sdata_aggr->alloc_size
 *    Result:
 *        A block of sdata_aggr->alloc_size is extended via file allocation
 *        The third block of 50 is allocated from there
 *
 *    Allocate third block (1970) from meta_aggr:
 *        request-size is > what is left in meta_aggr and is < meta_aggr->alloc_size
 *        sdata_aggr is at EOA and has used up more than sdata_aggr->alloc_size
 *    Result:
 *        unused space in sdata_aggr is freed to free-space and is shrunk
 *        sdata_aggr is reset to 0
 *        A block of meta_aggr->alloc_size is obtained via file allocation
 *        The block does not adjoin meta_aggr
 *        meta_aggr's unused space of [880, 1968] is freed to free-space
 *        meta_aggr is updated to point to the new block
 *-------------------------------------------------------------------------
 */
static unsigned
test_mf_aggr_alloc7(const char *env_h5_drvr, hid_t fapl)
{
    hid_t        file = -1;              /* File ID */
    char        filename[FILENAME_LEN]; /* Filename to use */
    H5F_t        *f = NULL;              /* Internal file object pointer */
    h5_stat_size_t      empty_size, file_size;
    H5FD_mem_t         type, stype;
    haddr_t        addr1, addr2, addr3, saddr1, saddr2, saddr3;
    haddr_t         ma_addr=HADDR_UNDEF, sdata_addr=HADDR_UNDEF;
    hsize_t         ma_size=0, sdata_size=0;
    H5FS_stat_t         state;
    hbool_t             contig_addr_vfd;        /* Whether VFD used has a contigous address space */

    TESTING("H5MF_alloc() of meta/sdata aggregator:test 7");

    /* Skip test when using VFDs that don't use the metadata aggregator */
    contig_addr_vfd = (hbool_t)(HDstrcmp(env_h5_drvr, "split") && HDstrcmp(env_h5_drvr, "multi"));
    if(contig_addr_vfd) {
        /* Set the filename to use for this test (dependent on fapl) */
        h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

        /* Create the file to work on */
        if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Close file */
        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((empty_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Re-open the file */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        /* Allocate the first block from meta_aggr */
        type = H5FD_MEM_SUPER;
        addr1 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE30);
        H5MF__aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);
        if ((addr1+TBLOCK_SIZE30) != ma_addr)
            TEST_ERROR

        /* Allocate the second block from meta_aggr */
        addr2 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE50);
        H5MF__aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        if (addr2+TBLOCK_SIZE50 != ma_addr)
            TEST_ERROR
        if (ma_size != (TBLOCK_SIZE2048 - (TBLOCK_SIZE30 + TBLOCK_SIZE50)))
            TEST_ERROR

        /* Allocate the first block from sdata_aggr */
        stype = H5FD_MEM_DRAW;
        saddr1 = H5MF_alloc(f, stype, (hsize_t)TBLOCK_SIZE30);
        H5MF__aggr_query(f, &(f->shared->sdata_aggr), &sdata_addr, &sdata_size);
        if ((saddr1+TBLOCK_SIZE30) != sdata_addr)
            TEST_ERROR

        /* Allocate the second block from sdata_aggr */
        saddr2 = H5MF_alloc(f, stype, (hsize_t)TBLOCK_SIZE2048 - TBLOCK_SIZE30);

        H5MF__aggr_query(f, &(f->shared->sdata_aggr), &sdata_addr, &sdata_size);
        if ((saddr2+(TBLOCK_SIZE2048 - TBLOCK_SIZE30)) != sdata_addr)
            TEST_ERROR
        if (sdata_size != 0) TEST_ERROR

        /* Allocate the third block from sdata_aggr */
        saddr3 = H5MF_alloc(f, stype, (hsize_t)TBLOCK_SIZE50);

        H5MF__aggr_query(f, &(f->shared->sdata_aggr), &sdata_addr, &sdata_size);
        if ((saddr3+TBLOCK_SIZE50) != sdata_addr)
            TEST_ERROR
        if (sdata_size != (TBLOCK_SIZE2048 - TBLOCK_SIZE50))
            TEST_ERROR

        /* Allocate the third block from meta_aggr */
        addr3 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE1970);
        H5MF__aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        if (addr3 != sdata_addr) TEST_ERROR
        if ((addr3 + TBLOCK_SIZE1970) != ma_addr) TEST_ERROR
        if (ma_size != (TBLOCK_SIZE2048 - TBLOCK_SIZE1970)) TEST_ERROR

        /* sdata_aggr info is reset to 0 */
        H5MF__aggr_query(f, &(f->shared->sdata_aggr), &sdata_addr, &sdata_size);
        if (sdata_addr != HADDR_UNDEF) TEST_ERROR
        if (sdata_size != 0) TEST_ERROR

        /* Verify that meta_aggr's unused space of 1968 is freed to free-space */
        HDmemset(&state, 0, sizeof(H5FS_stat_t));
        state.tot_space += (TBLOCK_SIZE2048 - (TBLOCK_SIZE30 + TBLOCK_SIZE50));
        state.tot_sect_count += 1;
        state.serial_sect_count += 1;

        if(check_stats(f, f->shared->fs_man[type], &state))
            TEST_ERROR

        /* Free all the allocated blocks */
        H5MF_xfree(f, type, addr1, (hsize_t)TBLOCK_SIZE30);
        H5MF_xfree(f, type, addr2, (hsize_t)TBLOCK_SIZE50);
        H5MF_xfree(f, type, addr3, (hsize_t)TBLOCK_SIZE1970);
        H5MF_xfree(f, stype, saddr1, (hsize_t)TBLOCK_SIZE30);
        H5MF_xfree(f, stype, saddr2, (hsize_t)(TBLOCK_SIZE2048 - TBLOCK_SIZE30));
        H5MF_xfree(f, stype, saddr3, (hsize_t)TBLOCK_SIZE50);

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Verify the file is the correct size */
        if (file_size != empty_size)
            TEST_ERROR

        PASSED()
    } /* end if */
    else {
        SKIPPED();
        HDputs("    Current VFD doesn't support metadata aggregator");
    } /* end else */

    return(0);

error:
    H5E_BEGIN_TRY {
        H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_mf_aggr_alloc7() */

/*
 *-------------------------------------------------------------------------
 * To verify that a block can be extended from the aggregator
 *
 *    Test 1: Allocate block A from meta_aggr which is at end of file
 *        Try to extend the block which adjoins the aggregator that is at end of file
 *          a. block size < (% * aggr->alloc_size)
 *             The block is allocated from the aggregator
 *          b. block size > (% * aggr->alloc_size) but block size < aggr->alloc_size
 *             The block is extended by aggr->alloc_size and the block is allocated from the aggregator
 *          c. block size > (% * aggr->alloc_size) but block size > aggr->alloc_size
 *             The block is extended by extended-request and the block is allocated from the aggregator
 *
 *    Test 2: Allocate block A from meta_aggr
 *        Allocate block B from sdata_aggr so that meta_aggr is not at end of file
 *        Try to extend a block which adjoins meta_aggr and meta_aggr can fulfill the extended-request
 *        H5MF_try_extend() succeeds: the block is extended into the aggregator
 *
 *    Test 3: Allocate block A from meta_aggr
 *        Allocate block B from sdata_aggr so that meta_aggr is not at end of file
 *        Try to extend a block which adjoins meta_aggr but meta_aggr cannot fulfill the extended-request
 *        H5MF_try_extend() fails
 *-------------------------------------------------------------------------
 */
static unsigned
test_mf_aggr_extend(const char *env_h5_drvr, hid_t fapl)
{
    hid_t        file = -1;              /* File ID */
    char        filename[FILENAME_LEN]; /* Filename to use */
    H5F_t        *f = NULL;              /* Internal file object pointer */
    h5_stat_size_t      empty_size = 0, file_size;
    H5FD_mem_t         type, stype;
    haddr_t        new_addr, addr, saddr;
    haddr_t         ma_addr=HADDR_UNDEF, new_ma_addr=HADDR_UNDEF, sdata_addr=HADDR_UNDEF;
    hsize_t         ma_size=0, new_ma_size=0, sdata_size=0;
    htri_t          was_extended;
    hbool_t             contig_addr_vfd;        /* Whether VFD used has a contigous address space */

    TESTING("H5MF_try_extend() of meta/sdata aggregator: test 1");

    /* Skip test when using VFDs that don't use the metadata aggregator */
    contig_addr_vfd = (hbool_t)(HDstrcmp(env_h5_drvr, "split") && HDstrcmp(env_h5_drvr, "multi"));
    if(contig_addr_vfd) {
        /* Set the filename to use for this test (dependent on fapl) */
        h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

        /* Create the file to work on */
        if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Close file */
        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((empty_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Re-open the file */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        /* Allocate the first block from meta_aggr */
        type = H5FD_MEM_SUPER;
        addr = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE30);
        H5MF__aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);
        if((addr+TBLOCK_SIZE30) != ma_addr)
            TEST_ERROR

        /* Adjust meta_aggr's info info for testing */
        f->shared->meta_aggr.addr = addr;
        f->shared->meta_aggr.size = f->shared->meta_aggr.alloc_size;

        new_addr = addr - 10;

        /* Try to extend the block by an amount < (% * aggr->alloc_size) */
        was_extended = H5MF_try_extend(f, type, (haddr_t)new_addr, (hsize_t)10, (hsize_t)(TBLOCK_SIZE50));

        /* should succeed */
        if(!was_extended)
            TEST_ERROR

        H5MF__aggr_query(f, &(f->shared->meta_aggr), &new_ma_addr, &new_ma_size);

        if(new_ma_addr != (addr+TBLOCK_SIZE50))
            TEST_ERROR
        if(new_ma_size != (f->shared->meta_aggr.alloc_size - TBLOCK_SIZE50)) TEST_ERROR

        /* Free the allocated blocks */
        H5MF_xfree(f, type, addr, (hsize_t)TBLOCK_SIZE50);

        /* Try to extend the block by an amount > (% * aggr->alloc_size) but amount < aggr->alloc_size */
        was_extended = H5MF_try_extend(f, type, (haddr_t)new_addr, (hsize_t)10, (hsize_t)(TBLOCK_SIZE700));

        /* should succeed */
        if(!was_extended)
            TEST_ERROR

        H5MF__aggr_query(f, &(f->shared->meta_aggr), &new_ma_addr, &new_ma_size);

        if(new_ma_addr != (addr + TBLOCK_SIZE700))
            TEST_ERROR
        if(new_ma_size != (f->shared->meta_aggr.alloc_size * 2 - TBLOCK_SIZE700)) TEST_ERROR

        /* Free the allocated blocks */
        H5MF_xfree(f, type, addr, (hsize_t)TBLOCK_SIZE700);

        /* Try to extend the block by an amount > (% * aggr->alloc_size) but amount > aggr->alloc_size */
        was_extended = H5MF_try_extend(f, type, (haddr_t)new_addr, (hsize_t)10, (hsize_t)(TBLOCK_SIZE2058));

        /* should succeed */
        if(!was_extended)
            TEST_ERROR

        H5MF__aggr_query(f, &(f->shared->meta_aggr), &new_ma_addr, &new_ma_size);

        if (new_ma_addr != (addr + TBLOCK_SIZE2058))
            TEST_ERROR
        if (new_ma_size != f->shared->meta_aggr.size) TEST_ERROR

        /* Free the allocated blocks */
        H5MF_xfree(f, type, addr, (hsize_t)TBLOCK_SIZE2058);

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Verify the file is the correct size */
        if (file_size != empty_size)
            TEST_ERROR

        PASSED()
    } /* end if */
    else {
        SKIPPED();
        HDputs("    Current VFD doesn't support metadata aggregator");
    } /* end else */

    TESTING("H5MF_try_extend() of meta/sdata aggregator: test 2");

    /* Skip test when using VFDs that don't use the metadata aggregator */
    if(contig_addr_vfd) {
        /* Re-open the file */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        /* Allocate the first block from meta_aggr */
        type = H5FD_MEM_SUPER;
        addr = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE30);
        H5MF__aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);
        if((addr+TBLOCK_SIZE30) != ma_addr)
            TEST_ERROR

        /* Allocate the first block from sdata_aggr */
        stype = H5FD_MEM_DRAW;
        saddr = H5MF_alloc(f, stype, (hsize_t)TBLOCK_SIZE50);
        H5MF__aggr_query(f, &(f->shared->sdata_aggr), &sdata_addr, &sdata_size);
        if((saddr+TBLOCK_SIZE50) != sdata_addr)
            TEST_ERROR

        /* Adjust meta_aggr's info info for testing */
        f->shared->meta_aggr.addr = addr;
        f->shared->meta_aggr.size = f->shared->meta_aggr.alloc_size;

        new_addr = addr - 10;

        /* should be able to fulfill request from the aggreqator itself */
        was_extended = H5MF_try_extend(f, type, (haddr_t)new_addr, (hsize_t)10, (hsize_t)(TBLOCK_SIZE50));

        if(!was_extended)
            TEST_ERROR

        H5MF__aggr_query(f, &(f->shared->meta_aggr), &new_ma_addr, &new_ma_size);

        if(new_ma_addr != (addr+TBLOCK_SIZE50))
            TEST_ERROR
        if(new_ma_size != (f->shared->meta_aggr.alloc_size-TBLOCK_SIZE50))
            TEST_ERROR

        /* Restore info for meta_aggr */
        f->shared->meta_aggr.addr = ma_addr;
        f->shared->meta_aggr.size = ma_size;

        H5MF_xfree(f, type, addr, (hsize_t)TBLOCK_SIZE30);
        H5MF_xfree(f, stype, saddr, (hsize_t)TBLOCK_SIZE50);

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Verify the file is the correct size */
        if(file_size != empty_size)
            TEST_ERROR

        PASSED()
    } /* end if */
    else {
        SKIPPED();
        HDputs("    Current VFD doesn't support metadata aggregator");
    } /* end else */

    TESTING("H5MF_try_extend() of meta/sdata aggregator: test 3");

    /* Skip test when using VFDs that don't use the metadata aggregator */
    if(contig_addr_vfd) {
        /* Re-open the file */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        /* Allocate first block from meta_aggr */
        type = H5FD_MEM_SUPER;
        addr = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE30);
        H5MF__aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);
        if ((addr+TBLOCK_SIZE30) != ma_addr)
            TEST_ERROR

        /* Allocate first block from sdata_aggr */
        stype = H5FD_MEM_DRAW;
        saddr = H5MF_alloc(f, stype, (hsize_t)TBLOCK_SIZE50);
        H5MF__aggr_query(f, &(f->shared->sdata_aggr), &sdata_addr, &sdata_size);
        if((saddr+TBLOCK_SIZE50) != sdata_addr)
            TEST_ERROR

        /* Adjust meta_aggr's info info for testing */
        f->shared->meta_aggr.addr = addr;
        f->shared->meta_aggr.size = 0;

        new_addr = addr - 10;

        /* unable to fulfill request from the aggreqator itself */
        was_extended = H5MF_try_extend(f, type, (haddr_t)new_addr, (hsize_t)10, (hsize_t)(TBLOCK_SIZE50));

        if(was_extended)
            TEST_ERROR

        H5MF__aggr_query(f, &(f->shared->meta_aggr), &new_ma_addr, &new_ma_size);

        if (new_ma_addr != addr) TEST_ERROR
        if (new_ma_size != 0) TEST_ERROR

        /* restore info for meta_aggr */
        f->shared->meta_aggr.addr = ma_addr;
        f->shared->meta_aggr.size = ma_size;

        H5MF_xfree(f, type, addr, (hsize_t)TBLOCK_SIZE30);
        H5MF_xfree(f, stype, saddr, (hsize_t)TBLOCK_SIZE50);

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Verify the file is the correct size */
        if(file_size != empty_size)
            TEST_ERROR

        PASSED()
    } /* end if */
    else {
        SKIPPED();
        HDputs("    Current VFD doesn't support metadata aggregator");
    } /* end else */

    return(0);

error:
    H5E_BEGIN_TRY {
        H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_mf_aggr_extend() */

/*
 *-------------------------------------------------------------------------
 * To verify that a block is absorbed into an aggregator
 *
 * MF_try_shrink() only allows blocks to be absorbed into an aggregator
 *
 *    Test 1: H5MF_alloc() block A from meta_aggr
 *        H5MF_try_shrink() block A should merge it back into meta_aggr
 *            since block A adjoins the beginning of meta_aggr
 *
 *    Test 2: H5MF_alloc() block A from meta_aggr
 *        H5MF_alloc() block B from sdata_aggr
 *        H5MF_try_shrink() block B should merge it back to the end of sdata_aggr
 *            because sec2 driver is FLMAP_DICHOTOMY by default
 *
 *    Test 3: H5MF_alloc() block A from meta_aggr
 *        H5MF_alloc() block B from meta_aggr
 *        H5MF_alloc() block C from meta_aggr
 *        H5MF_try_shrink() block B should fail since it does not adjoin the
 *            beginning nor the end of meta_aggr
 *-------------------------------------------------------------------------
 */
static unsigned
test_mf_aggr_absorb(const char *env_h5_drvr, hid_t fapl)
{
    hid_t        file = -1;              /* File ID */
    char        filename[FILENAME_LEN]; /* Filename to use */
    H5F_t        *f = NULL;              /* Internal file object pointer */
    h5_stat_size_t      empty_size = 0, file_size;
    H5FD_mem_t         type, stype;
    haddr_t        addr1, addr2, addr3, saddr1;
    haddr_t         ma_addr=HADDR_UNDEF, new_ma_addr=HADDR_UNDEF;
    haddr_t         new_sdata_addr=HADDR_UNDEF;
    hsize_t         ma_size=0, new_ma_size=0;
    hsize_t         sdata_size=0, new_sdata_size=0;
    hbool_t             contig_addr_vfd;        /* Whether VFD used has a contigous address space */

    TESTING("H5MF_try_shrink() of meta/sdata aggregator: test 1");

    /* Skip test when using VFDs that don't use the metadata aggregator */
    contig_addr_vfd = (hbool_t)(HDstrcmp(env_h5_drvr, "split") && HDstrcmp(env_h5_drvr, "multi"));
    if(contig_addr_vfd) {
        /* Set the filename to use for this test (dependent on fapl) */
        h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

        /* Create the file to work on */
        if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Close file */
        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((empty_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Re-open the file */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        /* Allocate block A from meta_aggr */
        type = H5FD_MEM_SUPER;
        addr1 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE30);
        H5MF__aggr_query(f, &(f->shared->meta_aggr), &new_ma_addr, &new_ma_size);
        ma_addr = new_ma_addr - TBLOCK_SIZE30;

        if((addr1 + TBLOCK_SIZE30) != new_ma_addr)
            TEST_ERROR

        /* should succeed */
        if(H5MF_try_shrink(f, type, addr1, (hsize_t)TBLOCK_SIZE30) <= 0)
            TEST_ERROR

        H5MF__aggr_query(f, &(f->shared->meta_aggr), &new_ma_addr, &new_ma_size);

        if(new_ma_addr != ma_addr) TEST_ERROR

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Verify the file is the correct size */
        if(file_size != empty_size)
            TEST_ERROR

        PASSED()
    } /* end if */
    else {
        SKIPPED();
        HDputs("    Current VFD doesn't support metadata aggregator");
    } /* end else */

    TESTING("H5MF_try_shrink() of meta/sdata aggregator: test 2");

    /* Skip test when using VFDs that don't use the metadata aggregator */
    if(contig_addr_vfd) {
        /* Re-open the file */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        /* Allocate block A from meta_aggr */
        type = H5FD_MEM_SUPER;
        addr1 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE30);
        H5MF__aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        if((addr1+TBLOCK_SIZE30) != ma_addr) TEST_ERROR
        if(ma_size != (TBLOCK_SIZE2048 - TBLOCK_SIZE30)) TEST_ERROR

        /* Allocate block B from sdata_aggr */
        stype = H5FD_MEM_DRAW;
        saddr1 = H5MF_alloc(f, stype, (hsize_t)TBLOCK_SIZE50);

        H5MF__aggr_query(f, &(f->shared->sdata_aggr), NULL, &sdata_size);

        /* should succeed */
        if(H5MF_try_shrink(f, stype, saddr1, (hsize_t)TBLOCK_SIZE50) <= 0)
            TEST_ERROR

        H5MF__aggr_query(f, &(f->shared->sdata_aggr), &new_sdata_addr, &new_sdata_size);
        if(new_sdata_addr != saddr1) TEST_ERROR
        if(new_sdata_size != sdata_size + TBLOCK_SIZE50) TEST_ERROR

        /* meta_aggr info should be updated because the block is absorbed into the meta_aggr */
        H5MF__aggr_query(f, &(f->shared->meta_aggr), &new_ma_addr, &new_ma_size);
        if (new_ma_addr != ma_addr) TEST_ERROR
        if (new_ma_size != (ma_size)) TEST_ERROR

        H5MF_xfree(f, type, addr1, (hsize_t)TBLOCK_SIZE30);

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Verify the file is the correct size */
        if(file_size != empty_size)
            TEST_ERROR

        PASSED()
    } /* end if */
    else {
        SKIPPED();
        HDputs("    Current VFD doesn't support metadata aggregator");
    } /* end else */

    TESTING("H5MF_try_shrink() of meta/sdata aggregator: test 3");

    /* Skip test when using VFDs that don't use the metadata aggregator */
    if(contig_addr_vfd) {
        /* Re-open the file */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        /* Allocate block A from meta_aggr */
        type = H5FD_MEM_SUPER;
        addr1 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE30);
        H5MF__aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        if((addr1+TBLOCK_SIZE30) != ma_addr)
            TEST_ERROR

        /* Allocate block B from meta_aggr */
        addr2 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE50);
        H5MF__aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);
        if((addr2+TBLOCK_SIZE50) != ma_addr)
            TEST_ERROR

        /* Allocate block C from meta_aggr */
        addr3 = H5MF_alloc(f, type, (hsize_t)(TBLOCK_SIZE30+TBLOCK_SIZE50));
        H5MF__aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);
        if((addr3+TBLOCK_SIZE30+TBLOCK_SIZE50) != ma_addr)
            TEST_ERROR

        /* should not succeed */
        if(H5MF_try_shrink(f, type, addr2, (hsize_t)TBLOCK_SIZE50) > 0)
            TEST_ERROR

        /* aggregator info should be the same as before */
        H5MF__aggr_query(f, &(f->shared->meta_aggr), &new_ma_addr, &new_ma_size);
        if(new_ma_addr != ma_addr) TEST_ERROR

        H5MF_xfree(f, type, addr1, (hsize_t)TBLOCK_SIZE30);
        H5MF_xfree(f, type, addr2, (hsize_t)TBLOCK_SIZE50);
        H5MF_xfree(f, type, addr3, (hsize_t)(TBLOCK_SIZE30+TBLOCK_SIZE50));

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Verify the file is the correct size */
        if(file_size != empty_size)
            TEST_ERROR

        PASSED()
    } /* end if */
    else {
        SKIPPED();
        HDputs("    Current VFD doesn't support metadata aggregator");
    } /* end else */

    return(0);

error:
    H5E_BEGIN_TRY {
    H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_mf_aggr_absorb() */

/*
 *-------------------------------------------------------------------------
 * To verify that a block allocated from file allocation is aligned, can be shrunk and extended
 *
 * Alignment = 1024 or 4096
 *
 * Test 1:
 *     Turn off using metadata aggregator
 *     Allocate a block of 30 which should be from file allocation
 *    Result:
 *        The return address should be aligned
 *        A fragment [800, 224] or [800, 3296] is freed to free-space
 *        EOA is 1054 or 4126
 *
 *     Allocate a block of 50 which should be from file allocation
 *    Result:
 *        The return address should be aligned
 *        A fragment [1054, 994] or [4126, 4066] is freed to free-space
 *        EOA is 2098 or 8242
 * Test 2:
 *     Turn off using metadata aggregator
 *     Allocate a block which should be from file allocation
 *    The return address should be aligned
 *    H5MF_try_shrink() the block with aligned address should succeed
 *
 * Test 3:
 *     Turn off using metadata aggregator
 *     Allocate a block which should be from file allocation
 *    The return address should be aligned
 *    H5MF_try_extend() the block with aligned address should succeed
 *-------------------------------------------------------------------------
 */
static unsigned
test_mf_align_eoa(const char *env_h5_drvr, hid_t fapl, hid_t new_fapl)
{
    hid_t        file = -1;              /* File ID */
    hid_t        fapl1 = -1;
    char        filename[FILENAME_LEN]; /* Filename to use */
    H5F_t        *f = NULL;              /* Internal file object pointer */
    h5_stat_size_t      file_size, new_file_size;
    H5FD_mem_t         type;
    haddr_t        addr1, addr2;
    haddr_t         ma_addr=HADDR_UNDEF;
    hsize_t         ma_size=0;
    htri_t         was_extended;
    H5FS_stat_t state;
    hsize_t        alignment=0, mis_align=0, tmp=0, accum=0;
    hbool_t             have_alloc_vfd;        /* Whether VFD used has an 'alloc' callback */

    TESTING("H5MM_alloc() of file allocation with alignment: test 1");

    /* Skip test when using VFDs that have their own 'alloc' callback, which
     *  don't push mis-aligned space fragments on the file free space list
     */
    have_alloc_vfd = (hbool_t)(HDstrcmp(env_h5_drvr, "stdio")
            && HDstrcmp(env_h5_drvr, "split") && HDstrcmp(env_h5_drvr, "multi"));
    if(have_alloc_vfd) {
        /* Set the filename to use for this test (dependent on fapl) */
        h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

        /* Turn off using meta/small data aggregator */
        if((fapl1 = H5Pcopy(new_fapl)) < 0) TEST_ERROR

        H5Pset_meta_block_size(fapl1, (hsize_t)0);
        H5Pset_small_data_block_size(fapl1, (hsize_t)0);

        /* Create the file to work on (without alignment) */
        if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Close file */
        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* get alignment setting */
        if(H5Pget_alignment(fapl1, NULL, &alignment) < 0)
            TEST_ERROR

        /* Re-open the file with alignment and meta/sdata setting */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl1)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        /* calculate fragment for alignment of block 30 */
        if((tmp = (hsize_t)file_size % alignment))
             mis_align = alignment - tmp;

        accum = mis_align + TBLOCK_SIZE30;

        /* Allocate a block of 30 from file allocation */
        type = H5FD_MEM_SUPER;
        addr1 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE30);

        /* Verify that the allocated block is aligned */
        if(addr1 % alignment) TEST_ERROR

        /* there should be nothing in the aggregator */
        H5MF__aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);
        if(ma_addr || ma_size) TEST_ERROR

        HDmemset(&state, 0, sizeof(H5FS_stat_t));
        if (mis_align) {
            state.tot_space += mis_align;
            state.tot_sect_count += 1;
            state.serial_sect_count += 1;
            if(check_stats(f, f->shared->fs_man[type], &state))
                TEST_ERROR
        }

        /* calculate fragment for alignment of block 50 */
        mis_align = 0;
        if ((tmp = ((hsize_t)file_size + accum) % alignment))
             mis_align = alignment - tmp;
        accum += (mis_align + TBLOCK_SIZE50);

        addr2 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE50);

        /* Verify that the allocated block is aligned */
        if (addr2 % alignment) TEST_ERROR

        /* there should be nothing in the aggregator */
        H5MF__aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);
        if(ma_addr || ma_size) TEST_ERROR

        if(mis_align) {
            state.tot_space += mis_align;
            state.tot_sect_count += 1;
            state.serial_sect_count += 1;
            if(check_stats(f, f->shared->fs_man[type], &state))
                TEST_ERROR
        }

        H5MF_xfree(f, type, addr1, (hsize_t)TBLOCK_SIZE30);
        H5MF_xfree(f, type, addr2, (hsize_t)TBLOCK_SIZE50);

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        if((new_file_size = h5_get_file_size(filename, fapl1)) < 0)
            TEST_ERROR

        if (new_file_size != file_size)
            TEST_ERROR

        PASSED()
    } /* end if */
    else {
        SKIPPED();
        HDputs("    Current VFD doesn't support mis-aligned fragments");
    } /* end else */

    TESTING("H5MF_try_shrink() of file allocation with alignment: test 2");

    /* Skip test when using VFDs that have their own 'alloc' callback, which
     *  don't push mis-aligned space fragments on the file free space list
     */
    if(have_alloc_vfd) {
        /* Re-open the file with alignment and meta/sdata setting */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl1)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        /* allocate a block of 50 from meta_aggr */
        type = H5FD_MEM_SUPER;
        addr1 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE50);

        /* address should be aligned */
        if (addr1 % alignment) TEST_ERROR

        /* Close file */
        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        if((file_size = h5_get_file_size(filename, fapl1)) < 0)
            TEST_ERROR

        /* Re-open the file */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl1)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        /* shrink the block */
        if(H5MF_try_shrink(f, type, addr1, (hsize_t)TBLOCK_SIZE50) <= 0)
            TEST_ERROR

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        if((new_file_size = h5_get_file_size(filename, fapl1)) < 0)
            TEST_ERROR

        if (new_file_size != (file_size-TBLOCK_SIZE50)) TEST_ERROR

        PASSED()
    } /* end if */
    else {
        SKIPPED();
        HDputs("    Current VFD doesn't support mis-aligned fragments");
    } /* end else */

    TESTING("H5MF_try_extend() of file allocation with alignment: test 3");

    /* Skip test when using VFDs that have their own 'alloc' callback, which
     *  don't push mis-aligned space fragments on the file free space list
     */
    if(have_alloc_vfd) {
        /* Re-open the file with alignment and meta/sdata setting */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl1)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        /* allocate a block of 50 */
        type = H5FD_MEM_SUPER;
        addr1 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE50);

        /* address should be aligned */
        if(addr1 % alignment) TEST_ERROR

        /* Close file */
        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        if((file_size = h5_get_file_size(filename, fapl1)) < 0)
            TEST_ERROR

        /* Re-open the file */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl1)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        /* try to extend the block */
        was_extended = H5MF_try_extend(f, type, (haddr_t)addr1, (hsize_t)TBLOCK_SIZE50, (hsize_t)TBLOCK_SIZE30);

        if(was_extended <=0) TEST_ERROR

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        if((new_file_size = h5_get_file_size(filename, fapl1)) < 0)
            TEST_ERROR

        if (new_file_size != (file_size+TBLOCK_SIZE30)) TEST_ERROR

        PASSED()
    } /* end if */
    else {
        SKIPPED();
        HDputs("    Current VFD doesn't support mis-aligned fragments");
    } /* end else */

    return(0);

error:
    H5E_BEGIN_TRY {
        H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_mf_align_eoa() */

/*
 *-------------------------------------------------------------------------
 * To verify that a block allocated from the free-space manager is aligned
 *
 * Alignment = 1024 or 4096
 *
 * Test 1:
 *    Add section A with an aligned address to free-space manager (addr=alignment, size=50)
 *    Allocate a block of size=50
 *    The returned space's address should be same as section A's address
 *
 * Test 2:
 *    Add section A to free-space manager (addr=70, size=8000):
 *        section A is mis-aligned but the size is big enough for allocation with alignment
 *    Allocate a block of size=600
 *    The returned space should be allocated from section A with an aligned address:
 *        address=alignment  size=600
 *    There will be 2 sections in free-space: (alignment = 1024 or alignment = 4096)
 *        the fragment left from aligning section A: [70, 954] or [70, 4026]
 *        the section left after allocating block A: [1624, 416] or [4696, 3374]
 *    H5MF_try_extend() the block of size 600 by 200 should succeed:
 *        the existing fragment left from aligning section A: [70, 954] or [70, 4026]
 *        the section left after extending block A: [1824, 216] or [4896, 3174]
 *
 * Test 3:
 *    Add section A to free-space manager (addr=70, size=700):
 *        section A is mis-aligned but the size is not big enough for allocation with alignment
 *    Allocate a block of size=40
 *    The free-space manager is unable to fulfill the request
 *    The block is allocated from file allocation and should be aligned
 *-------------------------------------------------------------------------
 */
static unsigned
test_mf_align_fs(const char *env_h5_drvr, hid_t fapl, hid_t new_fapl)
{
    hid_t        file = -1;              /* File ID */
    char        filename[FILENAME_LEN]; /* Filename to use */
    h5_stat_size_t      file_size;
    H5F_t        *f = NULL;              /* Internal file object pointer */
    H5MF_free_section_t *sect_node = NULL;
    haddr_t        addr;
    H5FS_stat_t state;
    htri_t      was_extended;
    hsize_t        alignment=0, tmp=0, mis_align=0;
    hbool_t             have_alloc_vfd;        /* Whether VFD used has an 'alloc' callback */

    TESTING("H5MF_alloc() of free-space manager with alignment: test 1");

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* get alignment setting */
    if(H5Pget_alignment(new_fapl, NULL, &alignment) < 0)
        TEST_ERROR

    /* Re-open the file with alignment setting */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, new_fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = (H5F_t *)H5I_object(file)))
        FAIL_STACK_ERROR

    /* Start up H5FD_MEM_SUPER free-space manager */
    if(H5MF__start_fstype(f, (H5F_mem_page_t)H5FD_MEM_SUPER) < 0)
        FAIL_STACK_ERROR

    if(f->shared->fs_state[H5FD_MEM_SUPER] != H5F_FS_STATE_OPEN)
    TEST_ERROR
    if(f->shared->fs_man[H5FD_MEM_SUPER]->client != H5FS_CLIENT_FILE_ID)
    TEST_ERROR

    /* Create section A */
    sect_node = H5MF__sect_new(H5MF_FSPACE_SECT_SIMPLE, (haddr_t)alignment, (hsize_t)TBLOCK_SIZE50);

    /* Add section A to free-space manager */
    if(H5MF__add_sect(f, H5FD_MEM_SUPER, f->shared->fs_man[H5FD_MEM_SUPER], sect_node))
        FAIL_STACK_ERROR

    HDmemset(&state, 0, sizeof(H5FS_stat_t));
    state.tot_space += TBLOCK_SIZE50;
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;

    if(check_stats(f, f->shared->fs_man[H5FD_MEM_SUPER], &state))
        TEST_ERROR

    /* Allocate a block of 50 */
    addr = H5MF_alloc(f, H5FD_MEM_SUPER, (hsize_t)TBLOCK_SIZE50);

    /* Verify that the allocated block is section A in free-space */
    if(addr != (haddr_t)alignment) TEST_ERROR
    if(addr % alignment) TEST_ERROR

    state.tot_space -= TBLOCK_SIZE50;
    state.tot_sect_count -= 1;
    state.serial_sect_count -= 1;

    if(check_stats(f, f->shared->fs_man[H5FD_MEM_SUPER], &state))
        TEST_ERROR

    /* Free the block to free-space */
    H5MF_xfree(f, H5FD_MEM_SUPER, addr, (hsize_t)TBLOCK_SIZE50);

    state.tot_space += TBLOCK_SIZE50;
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;
    if(check_stats(f, f->shared->fs_man[H5FD_MEM_SUPER], &state))
        TEST_ERROR

    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    PASSED()

    TESTING("H5MF_alloc() of free-space manager with alignment: test 2");


    /* Re-open the file with alignment setting */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, new_fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = (H5F_t *)H5I_object(file)))
        FAIL_STACK_ERROR

    /* Start up H5FD_MEM_SUPER free-space manager */
    if(H5MF__start_fstype(f, (H5F_mem_page_t)H5FD_MEM_SUPER) < 0)
        FAIL_STACK_ERROR

    if(f->shared->fs_state[H5FD_MEM_SUPER] != H5F_FS_STATE_OPEN)
        TEST_ERROR
    if(f->shared->fs_man[H5FD_MEM_SUPER]->client != H5FS_CLIENT_FILE_ID)
        TEST_ERROR

    /* Create section A */
    sect_node = H5MF__sect_new(H5MF_FSPACE_SECT_SIMPLE, (haddr_t)TBLOCK_ADDR70, (hsize_t)TBLOCK_SIZE8000);

    /* Add section A to free-space manager */
    if(H5MF__add_sect(f, H5FD_MEM_SUPER, f->shared->fs_man[H5FD_MEM_SUPER], sect_node))
        FAIL_STACK_ERROR

    HDmemset(&state, 0, sizeof(H5FS_stat_t));
    state.tot_space += TBLOCK_SIZE8000;
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;

    if(check_stats(f, f->shared->fs_man[H5FD_MEM_SUPER], &state))
        TEST_ERROR

    /* Allocate a block of 600 */
    addr = H5MF_alloc(f, H5FD_MEM_SUPER, (hsize_t)TBLOCK_SIZE600);

    /* Verify that the allocated block is aligned */
    if (addr % alignment) TEST_ERROR

    /* should have 1 more section in free-space */
    state.tot_space -= TBLOCK_SIZE600;
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;

    if(check_stats(f, f->shared->fs_man[H5FD_MEM_SUPER], &state))
        TEST_ERROR

    /* try to extend the block */
    was_extended = H5MF_try_extend(f, H5FD_MEM_SUPER, (haddr_t)addr, (hsize_t)TBLOCK_SIZE600, (hsize_t)TBLOCK_SIZE200);

    if(was_extended <=0) TEST_ERROR

    /* space should be decreased by 200, # of sections remain the same */
    state.tot_space -= TBLOCK_SIZE200;

    if(check_stats(f, f->shared->fs_man[H5FD_MEM_SUPER], &state))
        TEST_ERROR

    /* Free the block to free-space manager */
    H5MF_xfree(f, H5FD_MEM_SUPER, addr, (hsize_t)(TBLOCK_SIZE600+TBLOCK_SIZE200));

    /* only 1 section in free-space because of merging */
    state.tot_space += (TBLOCK_SIZE600+TBLOCK_SIZE200);
    state.tot_sect_count = 1;
    state.serial_sect_count = 1;
    if(check_stats(f, f->shared->fs_man[H5FD_MEM_SUPER], &state))
        TEST_ERROR

    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    PASSED()

    TESTING("H5MF_alloc() of free-space manager with alignment: test 3");

    /* Skip test when using VFDs that have their own 'alloc' callback, which
     *  don't push mis-aligned space fragments on the file free space list
     */
    have_alloc_vfd = (hbool_t)(HDstrcmp(env_h5_drvr, "stdio")
            && HDstrcmp(env_h5_drvr, "split") && HDstrcmp(env_h5_drvr, "multi"));
    if(have_alloc_vfd) {
        if((file_size = h5_get_file_size(filename, new_fapl)) < 0)
            TEST_ERROR

        /* Re-open the file with alignment setting */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, new_fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        /* Start up H5FD_MEM_SUPER free-space manager */
        if(H5MF__start_fstype(f, (H5F_mem_page_t)H5FD_MEM_SUPER) < 0)
            FAIL_STACK_ERROR

        if(f->shared->fs_state[H5FD_MEM_SUPER] != H5F_FS_STATE_OPEN)
            TEST_ERROR
        if(f->shared->fs_man[H5FD_MEM_SUPER]->client != H5FS_CLIENT_FILE_ID)
            TEST_ERROR

        /* Create section A */
        sect_node = H5MF__sect_new(H5MF_FSPACE_SECT_SIMPLE, (haddr_t)TBLOCK_ADDR70, (hsize_t)TBLOCK_SIZE700);

        /* Add section A to free-space manager */
        if(H5MF__add_sect(f, H5FD_MEM_SUPER, f->shared->fs_man[H5FD_MEM_SUPER], sect_node))
            FAIL_STACK_ERROR

        HDmemset(&state, 0, sizeof(H5FS_stat_t));
        state.tot_space += TBLOCK_SIZE700;
        state.tot_sect_count += 1;
        state.serial_sect_count += 1;

        if(check_stats(f, f->shared->fs_man[H5FD_MEM_SUPER], &state))
            TEST_ERROR
        /*
         * Allocate a block of 40
         * Since free-space manager cannot fulfull the request because of alignment,
         * the block is obtained from file allocation
         */
        addr = H5MF_alloc(f, H5FD_MEM_SUPER, (hsize_t)(TBLOCK_SIZE40));

        /* Verify that the allocated block is aligned */
        if(addr % alignment)
            TEST_ERROR

        /* verify that the allocated block is from file allocation, not section A in free-space */
        if (!(addr >= (haddr_t)file_size)) TEST_ERROR

        /* calculate fragment for alignment of block 40 from file allocation */
        if ((tmp = (hsize_t)file_size % alignment))
            mis_align = alignment - tmp;

        if (mis_align) {
            state.tot_space += mis_align;
            state.tot_sect_count += 1;
            state.serial_sect_count += 1;
        }

        /* free-space info should be the same  */
        if(check_stats(f, f->shared->fs_man[H5FD_MEM_SUPER], &state))
            TEST_ERROR

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        PASSED()
    } /* end if */
    else {
        SKIPPED();
        HDputs("    Current VFD doesn't support mis-aligned fragments");
    } /* end else */

    return(0);

error:
    H5E_BEGIN_TRY {
        H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_mf_align_fs() */

/*
 *-------------------------------------------------------------------------
 * To verify that blocks allocated from the aggregator are aligned
 *
 * Alignment = 1024     aggr->alloc_size = 2048
 *
 *    Allocate first block (30) from meta_aggr: (nothing in the aggregator)
 *        request-size > aggr->size and < aggr->alloc_size
 *    Result:
 *        An "aggr->alloc_size" block is allocated from file allocation for the aggregator
 *        EOA is 3072
 *        The first block of 30 is allocated from the aggregator and should be aligned
 *        Fragment from alignment of file allocation is freed to free-space:[800, 224]
 *        There is space of 2018 left in meta_aggr
 *
 *    Allocate second block (50) from meta_aggr:
 *        (request-size + fragment size) <= aggr->size
 *    Result:
 *        The second block of 50 is allocated from the aggregator and should be aligned
 *        Fragment from alignment of aggregator allocation is freed to free-space:[1054, 994]
 *        There is space of 974 left in meta_aggr
 *
 *    Allocate third block (80) from meta_aggr:
 *        (request-size + fragment size) > aggr->size
 *        request-size < meta_aggr->alloc_size
 *        fragment size < (meta_aggr->alloc_size - request-size)
 *        meta_aggr is at EOA
 *    Result:
 *        A block of "meta_aggr->alloc_size" is extended from file allocation for meta_aggr
 *        EOA is 5120
 *        The third block of 80 is allocated from the aggregator and should be aligned
 *        Fragment from alignment of aggregator allocation is freed to free-space:[2098, 974]
 *        There is space of 1968 left in meta_aggr
 *
 *    Allocate fourth block (1970) from meta_aggr:
 *        (request-size + fragment size) is <= aggr->size
 *        fragment size > (aggr->alloc_size - request-size)
 *        meta_aggr is at EOA
 *    Result:
 *        A block of aggr->alloc_size + fragment size - (aggr->alloc_size - request-size))
 *             is extended from file allocation for meta_aggr
 *        The third block of 1970 is allocated from the aggregator and should be aligned
 *        Fragment from alignment of aggregator allocation is freed to free-space:[3152, 944]
 *        There is space of 1968 left in meta_aggr
 *        EOA is at 8034
 *
 *
 * Alignment = 4096     aggr->alloc_size = 2048
 *
 *    Allocate first block (30) from meta_aggr: (aggregator is empty)
 *        request-size is > meta_aggr->size and < meta_aggr->alloc_size
 *    Result:
 *        A meta_aggr->alloc_size block is allocated from file allocation for the aggregator
 *        The first block of 30 is allocated from the aggregator and should be aligned
 *        Fragment from alignment of file allocation is freed to free-space:[800, 3296]
 *        There is space of 2018 left in meta_aggr
 *        EOA is at 6144
 *
 *    Allocate second block (50) from meta_aggr:
 *        (request-size + fragment size) is > meta_aggr->size
 *        request-size < meta_aggr->alloc_size
 *        fragment size > (meta_aggr->alloc_size - request-size)
 *        meta_aggr is at EOA
 *    Result:
 *        A block of meta_aggr->alloc_size + (fragment size - (meta_aggr->alloc_size - request-size))
 *            is extended from file allocation for the aggregator
 *        The second block of 50 is allocated from the aggregator and should be aligned
 *        Fragment from alignment of aggregator allocation is freed to free-space:[4126, 4066]
 *        There is space of 2018 left in meta_aggr
 *        EOA is at 10260
 *
 *    Allocate third block (80) from meta_aggr:
 *        (request-size + fragment size) is > meta_aggr->size
 *        request-size < meta_aggr->alloc_size
 *        fragment size > (meta_aggr->alloc_size - request-size)
 *        meta_aggr is at EOA
 *    Result:
 *        A block of meta_aggr->alloc_size + (fragment size - (meta_aggr->alloc_size - request-size))
 *            is extended from file allocation for the aggregator
 *        The third block of 80 is allocated from the aggregator and should be aligned
 *        Fragment from alignment of aggregator allocation is freed to free-space:[8242, 4046]
 *        There is space of 2018 left in meta_aggr
 *        EOA is at 14386
 *
 *    Allocate fourth block (1970) from meta_aggr:
 *        (request-size + fragment size) > meta_aggr->size
 *        request-size < meta_aggr->alloc_size
 *        fragment size > (meta_aggr->alloc_size - request-size)
 *        meta_aggr is at EOA
 *    Result:
 *        A block of meta_aggr->alloc_size + (fragment size - (meta_aggr->alloc_size - request-size))
 *            is extended from file allocation for the aggregator
 *        The fourth block of 1970 is allocated from the aggregator and should be aligned
 *        Fragment from alignment of aggregator allocation is freed to free-space:[12368, 4016]
 *        There is space of 2018 left in meta_aggr
 *        EOA is at 20372
 *-------------------------------------------------------------------------
 */
static unsigned
test_mf_align_alloc1(const char *env_h5_drvr, hid_t fapl, hid_t new_fapl)
{
    hid_t        file = -1;              /* File ID */
    char        filename[FILENAME_LEN]; /* Filename to use */
    H5F_t        *f = NULL;              /* Internal file object pointer */
    h5_stat_size_t      file_size;              /* File size */

    H5FD_mem_t         type;
    haddr_t        addr1, addr2, addr3, addr4;
    H5FS_stat_t state;
    haddr_t         ma_addr=HADDR_UNDEF;
    hsize_t         ma_size=0, mis_align=0;
    hsize_t        alignment=0, tmp=0;
    hbool_t             have_alloc_vfd;        /* Whether VFD used has an 'alloc' callback */


    TESTING("H5MF_alloc() of meta/sdata aggregator with alignment: test 1");

    /* Skip test when using VFDs that have their own 'alloc' callback, which
     *  don't push mis-aligned space fragments on the file free space list
     */
    have_alloc_vfd = (hbool_t)(HDstrcmp(env_h5_drvr, "stdio")
            && HDstrcmp(env_h5_drvr, "split") && HDstrcmp(env_h5_drvr, "multi"));
    if(have_alloc_vfd) {
        /* Set the filename to use for this test (dependent on fapl) */
        h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

        /* Create the file to work on (without alignment) */
        if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Close file */
        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* get alignment setting */
        if(H5Pget_alignment(new_fapl, NULL, &alignment) < 0)
            TEST_ERROR

        /* Re-open the file with alignment setting */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, new_fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        /* calculate fragment for alignment of block 30 */
        if((tmp = (hsize_t)file_size % alignment))
            mis_align = alignment - tmp;

        /* Allocate a block of 30 from meta_aggr */
        type = H5FD_MEM_SUPER;
        addr1 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE30);

        /* Verify that the allocated block is aligned */
        if(addr1 % alignment) TEST_ERROR

        /* fragment for alignment of block 30 is freed to free-space */
        HDmemset(&state, 0, sizeof(H5FS_stat_t));
        if(mis_align) {
            state.tot_space += mis_align;
            state.tot_sect_count += 1;
            state.serial_sect_count += 1;
        }

        H5MF__aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        if ((addr1 + TBLOCK_SIZE30) != ma_addr)
            TEST_ERROR

        /* calculate fragment for alignment of block 50 */
        mis_align = 0;
        if((tmp = ma_addr % alignment))
            mis_align = alignment - tmp;

        /* Allocate a block of 50 from meta_aggr */
        addr2 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE50);

        /* Verify that the allocated block is aligned */
        if(addr2 % alignment) TEST_ERROR

        /* fragment for alignment of block 50 is freed to free-space */
        if(mis_align) {
            state.tot_space += mis_align;
            state.tot_sect_count += 1;
            state.serial_sect_count += 1;
        }

        H5MF__aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        if((addr2 + TBLOCK_SIZE50) != ma_addr)
            TEST_ERROR

        /* calculate fragment for alignment of block 80 */
        mis_align = 0;
        if ((tmp = ma_addr % alignment))
            mis_align = alignment - tmp;

        addr3 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE80);

        /* Verify that the allocated block is aligned */
        if(addr3 % alignment) TEST_ERROR

        /* fragment for alignment of block 80 is freed to free-space */
        if(mis_align) {
            state.tot_space += mis_align;
            state.tot_sect_count += 1;
            state.serial_sect_count += 1;
        }

        H5MF__aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        if((addr3 + TBLOCK_SIZE80) != ma_addr)
            TEST_ERROR

        /* calculate fragment for alignment of block 1970 */
        mis_align = 0;
        if((tmp = ma_addr % alignment))
            mis_align = alignment - tmp;

        /* Allocate a block of 1970 from meta_aggr */
        addr4 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE1970);

        /* Verify that the allocated block is aligned */
        if(addr4 % alignment) TEST_ERROR

        /* fragment for alignment of block 1970 is freed to free-space */
        if(mis_align) {
            state.tot_space += mis_align;
            state.tot_sect_count += 1;
            state.serial_sect_count += 1;
        }

        H5MF__aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        if((addr4 + TBLOCK_SIZE1970) != ma_addr)
            TEST_ERROR

        /* Verify total size of free space after all the allocations */
        if(check_stats(f, f->shared->fs_man[type], &state))
            TEST_ERROR

        H5MF_xfree(f, type, addr1, (hsize_t)TBLOCK_SIZE30);
        H5MF_xfree(f, type, addr2, (hsize_t)TBLOCK_SIZE50);
        H5MF_xfree(f, type, addr3, (hsize_t)TBLOCK_SIZE80);
        H5MF_xfree(f, type, addr3, (hsize_t)TBLOCK_SIZE1970);

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        PASSED()
    } /* end if */
    else {
        SKIPPED();
        HDputs("    Current VFD doesn't support mis-aligned fragments");
    } /* end else */

    return(0);

error:
    H5E_BEGIN_TRY {
        H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_mf_align_alloc1() */

/*
 *-------------------------------------------------------------------------
 * To verify that blocks allocated from the aggregator are aligned
 *
 * Alignment = 1024     aggr->alloc_size = 2048
 *
 *    Allocate first block (30) from meta_aggr: (meta_aggr is empty)
 *        request-size is > meta_aggr->size and < meta_aggr->alloc_size
 *    Result:
 *        A meta_aggr->alloc_size block is allocated from file allocation for the aggregator
 *        The first block of 30 is allocated from the aggregator and should be aligned
 *        Fragment from alignment of file allocation is freed to free-space:[800, 224]
 *        There is space of 2018 left in meta_aggr
 *        EOA is 3072
 *
 *    Allocate second block (50) from meta_aggr:
 *        (request-size+fragment size) <= aggr->size
 *    Result:
 *        The second block of 50 is allocated from the aggregator and should be aligned
 *        Fragment from alignment of aggregator allocation is freed to free-space:[1054, 994]
 *        There is space of 974 left in meta_aggr
 *
 *    Allocate first block (30) from sdata_aggr: (sdata_aggr is empty)
 *        request-size is > sdata_aggr->size and < sdata_aggr->alloc_size
 *    Result:
 *        A block of sdata_aggr->alloc_size is obtained via file allocation
 *        The first block of 30 is allocated from sdata_aggr and should be aligned
 *        EOA is 5120
 *
 *    Allocate third block (80) from meta_aggr:
 *        request-size+fragment size is > meta_aggr->size
 *        sdata_aggr is at EOA but has not used up more than sdata_aggr->alloc_size
 *    Result:
 *        A block of meta_aggr->alloc_size is allocated from file allocation
 *        The unused space in meta_aggr is freed to free-space [2098, 974]
 *        meta_aggr is updated to point to the new block
 *        The third block of 80 is allocated from meta_aggr and should be aligned
 *        EOA is 7168
 *
 * Alignment = 4096     aggr->alloc_size = 2048
 *
 *    Allocate first block (30) from meta_aggr: (meta_aggr is empty)
 *        request-size is > aggr->size and < aggr->alloc_size
 *    Result:
 *        A meta_aggr->alloc_size block is allocated from file allocation for the aggregator
 *        The first block of 30 is allocated from the aggregator and should be aligned
 *        Fragment from alignment of file allocation is freed to free-space:[800, 3296]
 *        There is space of 2018 left meta_aggr
 *        EOA is at 6144
 *
 *    Allocate second block (50) from meta_aggr:
 *        (request-size + fragment size) > aggr->size
 *        request-size < aggr->alloc_size
 *        fragment size > (aggr->alloc_size - request-size)
 *    Result:
 *        A block of (fragment size  + request-size) is extended from file allocation for the aggregator
 *        The second block of 50 is allocated from the aggregator and should be aligned
 *        Fragment from alignment of aggregator allocation is freed to free-space:[4126, 4066]
 *        There is space of 2018 left in meta_aggr
 *        EOA is at 10260
 *
 *    Allocate first block (30) from sdata_aggr: (sdata_aggr is empty)
 *        request-size is > sdata_aggr->size and < sdata_aggr->alloc_size
 *        meta_aggr is at EOA and has used up more than meta_aggr->alloc_size
 *    Result:
 *        The remaining space in meta_aggr is freed to free-space [8242, 2018] and shrunk since at EOF
 *        meta_aggr is reset to 0
 *        A block of sdata_aggr->alloc_size is obtained via file allocation
 *        Fragment from alignment of file allocation is freed to free-space: [8242, 4046]
 *        The first block of 30 is allocated from sdata_aggr and should be aligned
 *        There is space of 2018 left in sdata_aggr
 *        EOA is 14336
 *
 *    Allocate third block (80) from meta_aggr:
 *        (request-size + fragment size) is > meta_aggr->size
 *        request-size < meta_aggr->alloc_size
 *        sdata_aggr is at EOA but has not used up more than sdata_aggr->alloc_size
 *    Result:
 *        A block of meta_aggr->alloc_size is allocated from file allocation for the aggregator
 *        Fragment from alignment of file allocation is freed to free-space:[14336, 2048]
 *        other_aggr is [12318, 2018]
 *        The third block of 80 is allocated from the aggregator and should be aligned
 *        There is space of 1968 left in meta_aggr
 *        EOA is at 18432
 *-------------------------------------------------------------------------
 */
static unsigned
test_mf_align_alloc2(const char *env_h5_drvr, hid_t fapl, hid_t new_fapl)
{
    hid_t        file = -1;              /* File ID */
    char        filename[FILENAME_LEN]; /* Filename to use */
    H5F_t        *f = NULL;              /* Internal file object pointer */
    h5_stat_size_t      file_size;              /* File size */
    H5FD_mem_t         type, stype;
    haddr_t        addr1, addr2, addr3, saddr1;
    H5FS_stat_t state[H5FD_MEM_NTYPES];
    haddr_t         ma_addr=HADDR_UNDEF, sdata_addr=HADDR_UNDEF;
    hsize_t         ma_size=0, sdata_size=0, mis_align=0;
    hsize_t        alignment=0, tmp=0;
    hbool_t             have_alloc_vfd;        /* Whether VFD used has an 'alloc' callback */

    TESTING("H5MF_alloc() of meta/sdata aggregator with alignment: test 2");

    /* Skip test when using VFDs that have their own 'alloc' callback, which
     *  don't push mis-aligned space fragments on the file free space list
     */
    have_alloc_vfd = (hbool_t)(HDstrcmp(env_h5_drvr, "stdio")
            && HDstrcmp(env_h5_drvr, "split") && HDstrcmp(env_h5_drvr, "multi"));
    if(have_alloc_vfd) {
        /* Set the filename to use for this test (dependent on fapl) */
        h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

        /* Create the file to work on (without alignment) */
        if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Close file */
        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* get alignment setting */
        if(H5Pget_alignment(new_fapl, NULL, &alignment) < 0)
            TEST_ERROR

        /* Re-open the file with alignment setting */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, new_fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        /* calculate fragment for alignment of block 30 */
        if((tmp = (hsize_t)file_size % alignment))
            mis_align = alignment - tmp;

        /* Allocate a block of 30 from meta_aggr */
        type = H5FD_MEM_SUPER;
        addr1 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE30);

        /* Verify that the allocated block is aligned */
        if(addr1 % alignment) TEST_ERROR

        /* fragment for alignment of block 30 is freed to free-space */
        HDmemset(&state, 0, sizeof(H5FS_stat_t) * H5FD_MEM_NTYPES);
        if(mis_align) {
            state[type].tot_space += mis_align;
            state[type].tot_sect_count += 1;
            state[type].serial_sect_count += 1;
        }

        H5MF__aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        if((addr1 + TBLOCK_SIZE30) != ma_addr)
            TEST_ERROR

        /* fragment for alignment of block 50 is freed to free-space */
        mis_align = 0;
        if((tmp = ma_addr % alignment))
            mis_align = alignment - tmp;

        addr2 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE50);

        /* Verify that the allocated block is aligned */
        if(addr2 % alignment) TEST_ERROR

        /* fragment for alignment of block 50 is freed to free-space */
        if(mis_align) {
            state[type].tot_space += mis_align;
            state[type].tot_sect_count += 1;
            state[type].serial_sect_count += 1;
        }

        H5MF__aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        if((addr2 + TBLOCK_SIZE50) != ma_addr)
            TEST_ERROR

        /*
         * Calculate fragment for alignment of block 30 in sdata_aggr:
         *
         * For alignment = 1024, alloc_size = 2048:
         *  block 30 is allocated from (ma_addr + ma_size),
         *    which is already aligned
         *
         * For alignment = 4096, alloc_size = 2048:
         *    since remaining space in meta_aggr is freed and shrunk,
         *    block 30 is allocated from ma_addr
         */
        mis_align = 0;
        if((alignment == TEST_ALIGN1024) && (tmp = ((ma_addr + ma_size) % alignment)))
            mis_align = alignment - tmp;
        else if ((alignment == TEST_ALIGN4096) && (tmp = (ma_addr % alignment)))
            mis_align = alignment - tmp;

        /* Allocate a block of 30 from sdata_aggr */
        stype = H5FD_MEM_DRAW;
        saddr1 = H5MF_alloc(f, stype, (hsize_t)TBLOCK_SIZE30);

        /* fragment for alignment of block 30 for sdata_aggr is freed to free-space */
        if(mis_align) {
            state[stype].tot_space += mis_align;
            state[stype].tot_sect_count += 1;
            state[stype].serial_sect_count += 1;
        }

        /* Verify that the allocated block is aligned */
        if (saddr1 % alignment) TEST_ERROR

        H5MF__aggr_query(f, &(f->shared->sdata_aggr), &sdata_addr, &sdata_size);
        H5MF__aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        if(sdata_addr != (saddr1 + TBLOCK_SIZE30)) TEST_ERROR

        /*
         * Calculate fragment for the allocation of block 80 from meta_aggr:
         *
         * For alignment = 1024, alloc_size = 2048:
         *     fragment for unused space in meta_aggr is freed to free-space
         * For alignment = 4096, alloc_size = 2048:
         *     fragment from alignment of ma_addr is freed
         *    block 30 is allocated from ma_addr
         */
        mis_align = 0;
        if((alignment == TEST_ALIGN1024) && (tmp = (ma_addr % alignment)))
            mis_align = alignment - tmp;
        else if ((alignment == TEST_ALIGN4096) && (tmp = ((sdata_addr + sdata_size) % alignment)))
            mis_align = alignment - tmp;

        /* Allocate a block of 80 from meta_aggr */
        addr3 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE80);

        /* Verify that the allocated block is aligned */
        if (addr3 % alignment) TEST_ERROR

        /* fragment for alignment of block 80 is freed to free-space */
        if(mis_align) {
            state[type].tot_space += mis_align;
            state[type].tot_sect_count += 1;
            state[type].serial_sect_count += 1;
        }

        H5MF__aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        if((addr3 + TBLOCK_SIZE80) != ma_addr)
            TEST_ERROR

        /* Verify total size of free space after all the allocations */
    if(f->shared->fs_man[type]) {
        if(check_stats(f, f->shared->fs_man[type], &(state[type])))
        TEST_ERROR
    }

    if(f->shared->fs_man[stype]) {
        if(check_stats(f, f->shared->fs_man[stype], &(state[stype])))
        TEST_ERROR
    }

        H5MF_xfree(f, type, addr1, (hsize_t)TBLOCK_SIZE30);
        H5MF_xfree(f, type, addr2, (hsize_t)TBLOCK_SIZE50);
        H5MF_xfree(f, type, addr3, (hsize_t)TBLOCK_SIZE80);
        H5MF_xfree(f, stype, saddr1, (hsize_t)TBLOCK_SIZE30);

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        PASSED()
    } /* end if */
    else {
        SKIPPED();
        HDputs("    Current VFD doesn't support mis-aligned fragments");
    } /* end else */

    return(0);

error:
    H5E_BEGIN_TRY {
        H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_mf_align_alloc2() */

/*
 *-------------------------------------------------------------------------
 * To verify that blocks allocated from the aggregator are aligned
 *
 * Alignment = 1024     aggr->alloc_size = 2048
 *
 *    Allocate first block (30) from meta_aggr: (meta_aggr is empty)
 *        request-size is > meta_aggr->size and < meta_aggr->alloc_size
 *    Result:
 *        A block of meta_aggr->alloc_size is allocated from file allocation for the aggregator
 *        Fragment from alignment of file allocation is freed to free-space:[800, 224]
 *        The first block of 30 is allocated from the aggregator and should be aligned
 *        There is space of 2018 left in meta_aggr
 *        EOA is 3072
 *
 *    Allocate second block (50) from meta_aggr:
 *        (request-size+fragment size) is <= aggr->size
 *    Result:
 *        The second block of 50 is allocated from the aggregator and should be aligned
 *        Fragment from alignment of aggregator allocation is freed to free-space:[1054, 994]
 *        There is space of 974 left in the aggregator
 *
 *    Allocate first block (30) from other_aggr: (nothing in other_aggr)
 *        request-size is > what is left in other_aggr->size and < other_aggr->alloc_size
 *    Result:
 *        A "other_aggr->alloc_size" block is allocated from file allocation for other_aggr
 *        The first block of 30 is allocated from other_aggr and should be aligned
 *        There is space of 2018 left in other_aggr->size
 *        EOA is 5120
 *
 *    Allocate second block (50) from sdata_aggr:
 *        (request-size+fragment size) < sdata_aggr->size
 *    Result:
 *        The second block of 50 is allocated from sdata_aggr and should be aligned
 *        Fragment from alignment of aggregator allocation is freed to free-space:[3102, 994]
 *        There is space of 974 left in sdata_aggr
 *
 *    Allocate third block (80) from sdata_aggr:
 *        (request-size+fragment size) is >= sdata_aggr->size
 *        request-size < sdata_aggr->alloc_size
 *        sdata_aggr is at EOA
 *    Result:
 *        Another block of sdata_aggr->alloc_size is extended from file allocation for sdata_aggr
 *        The third block of 80 is allocated from sdata_aggr and should be aligned
 *        Fragment from alignment of aggregator allocation is freed to free-space:[4146, 974]
 *        There is space of 1968 left in sdata_aggr
 *        EOA is 7168
 *
 *    Allocate third block (1034) from meta_aggregator:
 *        (request-size + alignment) > meta_aggr->size but < meta_aggr->alloc_size
 *        sdata_aggr is at EOA and has used up more than sdata_aggr->alloc_size
 *    Result:
 *        The unused space in sdata_aggr is freed to free-space [5200, 1968] then shrunk
 *        sdata_aggr is reset to 0
 *        A block of meta_aggr->alloc_size is allocated from file allocation
 *        Fragment from alignment of file allocation is freed to free-space [5200, 944]
 *        The unused space in meta_aggr is freed to free-space [2098, 974]
 *        The meta_aggr is updated to point to the new space
 *        The block of 1034 is allocated from the new block and should be aligned
 *        There is space of 1014 left in meta_aggr
 *        EOA is 8192
 *
 * Alignment = 4096     aggr->alloc_size = 2048
 *
 *    Allocate first block (30) from meta_aggr: (meta_aggr is empty)
 *        request-size is > what is left in aggr->size and < aggr->alloc_size
 *    Result:
 *        A meta_aggr->alloc block is allocated from file allocation for the aggregator
 *        The first block of 30 is allocated from the aggregator and should be aligned
 *        Fragment from alignment of file allocation is freed to free-space:[800, 3296]
 *        There is space of 2018 left in meta_aggr
 *        EOA is at 6144
 *
 *    Allocate second block (50) from meta_aggr:
 *        (request-size + fragment size) is > what is left in aggr->size
 *        request-size < aggr->alloc_size
 *        fragment size > (aggr->alloc_size - request-size)
 *    Result:
 *        A block of aggr->alloc_size + (fragment size - (aggr->alloc_size - request-size))
 *            is extended from file allocation for the aggregator
 *        The second block of 50 is allocated from the aggregator and should be aligned
 *        Fragment from alignment of aggregator allocation is freed to free-space:[4126, 4066]
 *        There is space of 2018 left in meta_aggr
 *        EOA is at 10260
 *
 *    Allocate first block (30) from sdata_aggr: (sdata_aggr is empty)
 *        request-size > sdata_aggr->size and < sdata_aggr->alloc_size
 *        meta_aggr is at EOA and has used up more than meta_aggr->alloc_size
 *    Result:
 *        The remaining space in meta_aggr is freed to free-space [8242, 2018] and shrunk
 *            since at EOF
 *        meta_aggr is reset to 0
 *        A block of sdata_aggr->alloc_size is obtained via file allocation
 *        Fragment from alignment of file allocation is freed to free-space: [8242, 4046]
 *        The first block of 30 is allocated from sdata_aggr and should be aligned
 *        There is space of 2018 left in sdata_aggr
 *        EOA is 14336
 *
 *    Allocate second block (50) from sdata_aggr:
 *        request-size is > sdata_aggr->size
 *        request-size < sdata_aggr->alloc_size
 *        fragment size > (sdata_aggr->alloc_size - request-size)
 *    Result:
 *        A block of sdata_aggr->alloc_size + (fragment size - (sdata_aggr->alloc_size - request-size))
 *            is extended from file allocation for the aggregator
 *        Fragment from alignment of aggregator allocation is freed to free-space:[12318, 4066]
 *        The second block of 50 is allocated from the aggregator and should be aligned
 *        There is space of 2018 left in the sdata_aggr
 *        EOA is at 18452
 *
 *    Allocate third block (80) from sdata_aggr:
 *        request-size + fragment size is > sdata_aggr->size
 *        request-size < sdata_aggr->alloc_size
 *        fragment size > (sdata_aggr->alloc_size - request-size)
 *    Result:
 *        A block of sdata_aggr->alloc_size + (fragment size - (sdata_aggr->alloc_size - request-size)
 *            is allocated from file allocation for the aggregator
 *        Fragment from alignment of aggregator allocation is freed to free-space:[16434, 4046]
 *        The third block of 80 is allocated from the aggregator and should be aligned
 *        There is space of 2018 left in the sdata_aggr
 *        EOA is at 22578
 *
 *    Allocate third block (1034) from meta_aggregator:
 *        (request-size + fragment size) is > meta_aggr->size but request-size < meta_aggr->alloc_size
 *        sdata_aggr is at EOA and has used up more than sdata_aggr->alloc_size
 *    Result:
 *        The remaining space in sdata_aggr is freed to free-space [20560, 2018] then shrunk
 *        sdata_aggr is reset to 0
 *        There is nothing in meta_aggr
 *        A block of meta_aggr->alloc_size is allocated from file allocation
 *        Fragment from alignment of file allocation is freed to free-space [20560, 4016]
 *        EOA is 26624
 *        The meta_aggr is updated to point to the new space
 *        The block of 1034 is allocated from the new block and should be aligned
 *        There is space of 1014 left in meta_aggr
 *-------------------------------------------------------------------------
 */
static unsigned
test_mf_align_alloc3(const char *env_h5_drvr, hid_t fapl, hid_t new_fapl)
{
    hid_t        file = -1;              /* File ID */
    char        filename[FILENAME_LEN]; /* Filename to use */
    H5F_t        *f = NULL;              /* Internal file object pointer */
    h5_stat_size_t      file_size;
    H5FD_mem_t         type, stype;
    haddr_t        addr1, addr2, addr3;
    haddr_t        saddr1, saddr2, saddr3;
    H5FS_stat_t state[H5FD_MEM_NTYPES];
    haddr_t         ma_addr=HADDR_UNDEF, sdata_addr=HADDR_UNDEF;
    hsize_t         ma_size=0, sdata_size=0, mis_align=0;
    hsize_t        alignment=0, tmp=0;
    hbool_t             have_alloc_vfd;        /* Whether VFD used has an 'alloc' callback */


    TESTING("H5MF_alloc() of meta/sdata aggregator with alignment: test 3");

    /* Skip test when using VFDs that have their own 'alloc' callback, which
     *  don't push mis-aligned space fragments on the file free space list
     */
    have_alloc_vfd = (hbool_t)(HDstrcmp(env_h5_drvr, "stdio")
            && HDstrcmp(env_h5_drvr, "split") && HDstrcmp(env_h5_drvr, "multi"));
    if(have_alloc_vfd) {
        /* Set the filename to use for this test (dependent on fapl) */
        h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

        /* Create the file to work on (without alignment) */
        if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Close file */
        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* get alignment setting */
        if(H5Pget_alignment(new_fapl, NULL, &alignment) < 0)
            TEST_ERROR

        /* Re-open the file with alignment setting */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, new_fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        /* calculate fragment for alignment of block 30 */
        if((tmp = (hsize_t)file_size % alignment))
            mis_align = alignment - tmp;

        /* Allocate a block of 30 from meta_aggr */
        type = H5FD_MEM_SUPER;
        addr1 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE30);

        /* Verify that the allocated block is aligned */
        if(addr1 % alignment) TEST_ERROR

        /* fragment for alignment of block 30 is freed to free-space */
        HDmemset(&state, 0, sizeof(H5FS_stat_t) * H5FD_MEM_NTYPES);
        if(mis_align) {
            state[type].tot_space += mis_align;
            state[type].tot_sect_count += 1;
            state[type].serial_sect_count += 1;
        }

        H5MF__aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);
        if ((addr1 + TBLOCK_SIZE30) != ma_addr)
            TEST_ERROR

        /* calculate fragment for alignment of block 50 */
        mis_align = 0;
        if((tmp = ma_addr % alignment))
            mis_align = alignment - tmp;

        /* Allocate a block of 50 from meta_aggr */
        addr2 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE50);

        /* Verify that the allocated block is aligned */
        if(addr2 % alignment) TEST_ERROR

        /* fragment for alignment of block 50 is freed to free-space */
        if(mis_align) {
            state[type].tot_space += mis_align;
            state[type].tot_sect_count += 1;
            state[type].serial_sect_count += 1;
        }

        H5MF__aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);
        if((addr2 + TBLOCK_SIZE50) != ma_addr)
            TEST_ERROR

        /*
         * Calculate fragment for alignment of block 30 in sdata_aggr:
         *
         * For alignment = 1024, alloc_size = 2048:
         *  block 30 is allocated from (ma_addr + ma_size),
         *  which is already aligned
         *
         * For alignment = 4096, alloc_size = 2048:
         *  since remaining space in meta_aggr is freed and shrunk,
         *  block 30 is allocated from ma_addr
         */
        mis_align = 0;
        if((alignment == TEST_ALIGN1024) && (tmp = ((ma_addr + ma_size) % alignment)))
            mis_align = alignment - tmp;
        else if ((alignment == TEST_ALIGN4096) && (tmp = ma_addr % alignment))
            mis_align = alignment - tmp;

        /* Allocate a block of 30 from sdata_aggr */
        stype = H5FD_MEM_DRAW;
        saddr1 = H5MF_alloc(f, stype, (hsize_t)TBLOCK_SIZE30);

        /* Verify that the allocated block is aligned */
        if(saddr1 % alignment) TEST_ERROR

        /* fragment for alignment of block 30 for sdata_aggr is freed to free-space */
        if(mis_align) {
            state[stype].tot_space += mis_align;
            state[stype].tot_sect_count += 1;
            state[stype].serial_sect_count += 1;
        }

        H5MF__aggr_query(f, &(f->shared->sdata_aggr), &sdata_addr, &sdata_size);
        if(sdata_addr != (saddr1+TBLOCK_SIZE30)) TEST_ERROR

        /* calculate fragment for alignment of block 50 in sdata_aggr */
        mis_align = 0;
        if((tmp = sdata_addr % alignment))
            mis_align = alignment - tmp;

        /* Allocate a block of 50 from sdata_aggr */
        saddr2 = H5MF_alloc(f, stype, (hsize_t)TBLOCK_SIZE50);

        /* Verify that the allocated block is aligned */
        if(saddr2 % alignment) TEST_ERROR

        /* fragment for alignment of block 50 for sdata_aggr is freed to free-space */
        if(mis_align) {
            state[stype].tot_space += mis_align;
            state[stype].tot_sect_count += 1;
            state[stype].serial_sect_count += 1;
        }

        H5MF__aggr_query(f, &(f->shared->sdata_aggr), &sdata_addr, &sdata_size);
        if(sdata_addr != (saddr2 + TBLOCK_SIZE50)) TEST_ERROR

        /* calculate fragment for alignment of block 80 in sdata_aggr */
        mis_align = 0;
        if((tmp = sdata_addr % alignment))
            mis_align = alignment - tmp;

        /* Allocate a block of 80 from sdata_aggr */
        saddr3 = H5MF_alloc(f, stype, (hsize_t)TBLOCK_SIZE80);

        /* Verify that the allocated block is aligned */
        if(saddr3 % alignment) TEST_ERROR

        /* fragment for alignment of block 80 for sdata_aggr is freed to free-space */
        if(mis_align) {
            state[stype].tot_space += mis_align;
            state[stype].tot_sect_count += 1;
            state[stype].serial_sect_count += 1;
        }

        H5MF__aggr_query(f, &(f->shared->sdata_aggr), &sdata_addr, &sdata_size);
        if ((saddr3 + TBLOCK_SIZE80) != sdata_addr)
            TEST_ERROR

        /* calculate fragment for alignment of block 1034 */
        mis_align = 0;
        if((tmp = sdata_addr % alignment))
            mis_align = alignment - tmp;

        /* Allocate a block of 1034 for meta_aggr */
        addr3 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE1034);

        /* Verify that the allocated block is aligned */
        if(addr3 % alignment) TEST_ERROR

        /* fragment for alignment of block 1034 for meta_aggr is freed to free-space */
        if(mis_align) {
            state[type].tot_space += mis_align;
            state[type].tot_sect_count += 1;
            state[type].serial_sect_count += 1;
        }

        /* calculate unused space in meta_aggr that is freed to free-space after block 1034 */
        mis_align = 0;
        if((alignment == TEST_ALIGN1024) && (tmp = (ma_addr % alignment)))
            mis_align = alignment - tmp;

        /* fragment for unused space in meta_aggr after block 1034 is freed to free-space */
        if(mis_align) {
            state[type].tot_space += mis_align;
            state[type].tot_sect_count += 1;
            state[type].serial_sect_count += 1;
        }

        H5MF__aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        if((addr3 + TBLOCK_SIZE1034) != ma_addr)
            TEST_ERROR

        /* Verify total size of free space after all allocations */
    if(f->shared->fs_man[type]) {
        if(check_stats(f, f->shared->fs_man[type], &(state[type])))
        TEST_ERROR
    }

    if(f->shared->fs_man[stype]) {
        if(check_stats(f, f->shared->fs_man[stype], &(state[stype])))
        TEST_ERROR
    }

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        PASSED()
    } /* end if */
    else {
        SKIPPED();
        HDputs("    Current VFD doesn't support mis-aligned fragments");
    } /* end else */

    return(0);

error:
    H5E_BEGIN_TRY {
        H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_mf_align_alloc3() */


/*
 *-------------------------------------------------------------------------
 * To verify that blocks allocated from the aggregator are aligned
 *
 * Alignment = 4096     aggr->alloc_size = 2048
 *
 *    Allocate first block (30) from meta_aggr: (meta_aggr is empty)
 *        request-size > meta_aggr->size and < meta_aggr->alloc_size
 *    Result:
 *        A block of meta_aggr->alloc_size is allocated from file allocation
 *        Fragment from alignment of file allocation is freed to free-space:[800, 224]
 *        The first block of 30 is allocated from meta_aggr and should be aligned
 *        There is space of 2018 left in meta_aggr
 *        EOA is 3072
 *
 *    Allocate second block (2058) from meta_aggr:
 *        (request-size+fragment) is > meta_aggr->size and request-size is > meta_aggr->alloc_size
 *        meta_aggr is at EOA
 *    Result:
 *        The second block of 2058 + fragment is extended and merged together with meta_aggr
 *        The block of 2058 is allocated out of the aggregator
 *        Fragment from alignment of aggregator allocation is freed to free-space:[1054, 994]
 *        There is space of 2018 (same as before) left in meta_aggr
 *        EOA is 6124
 *
 *    Allocate third block (5) from meta_aggr:
 *        request-size+fragment < meta_aggr->size
 *    Result:
 *        A block of 5 is allocated from the aggregator
 *        Fragment from alignment of aggregator allocation is freed to free-space:[4106, 1014]
 *        There is space of 999 left in meta_aggr
 *
 * Alignment = 4096     aggr->alloc_size = 2048
 *
 *    Allocate first block (30) from meta_aggr: (meta_aggr is empty)
 *        request-size is > meta_aggr->size and < meta_aggr->alloc_size
 *    Result:
 *        A block of meta_aggr->alloc_size is allocated from file allocation
 *        Fragment from alignment of file allocation is freed to free-space:[800, 3296]
 *        The first block of 30 is allocated from meta_aggr and should be aligned
 *        There is space of 2018 left in meta_aggr
 *        EOA is 6144
 *
 *    Allocate second block (2058) from meta_aggr:
 *        (request-size+fragment) is > meta_aggr->size and request-size is > meta_aggr->alloc_size
 *        meta_aggr is at EOA
 *    Result:
 *        The second block of 2058 + fragment is extended and merged together with meta_aggr
 *        The block of 2058 is allocated out of the aggregator
 *        Fragment from alignment of aggregator allocation is freed to free-space:[4126, 4066]
 *        There is space of 2018 (same as before) left in meta_aggr
 *        EOA is 12268
 *
 *    Allocate third block (5) from meta_aggr:
 *        request-size+fragment is > meta_aggr->size
 *        request-size < meta_aggr->alloc_size
 *        fragment < (meta_aggr->alloc_size - request-size)
 *        meta_aggr is at EOA
 *    Result:
 *        A block of meta_aggr->alloc_size is extended from file allocation for the aggregator
 *        A block of 5 is allocated from the aggregator
 *        Fragment from alignment of aggregator allocation is freed to free-space:[10250, 2038]
 *        There is space of 2023 left in meta_aggr
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_mf_align_alloc4(const char *env_h5_drvr, hid_t fapl, hid_t new_fapl)
{
    hid_t        file = -1;              /* File ID */
    char        filename[FILENAME_LEN]; /* Filename to use */
    H5F_t        *f = NULL;              /* Internal file object pointer */
    h5_stat_size_t      file_size;
    H5FD_mem_t         type;
    haddr_t        addr1, addr2, addr3;
    H5FS_stat_t     state;
    haddr_t         ma_addr=HADDR_UNDEF;
    hsize_t         ma_size=0, saved_ma_size=0;
    hsize_t        alignment=0, mis_align=0, tmp=0;
    hbool_t             have_alloc_vfd;        /* Whether VFD used has an 'alloc' callback */


    TESTING("H5MF_alloc() of meta/sdata aggregator with alignment: test 4");

    /* Skip test when using VFDs that have their own 'alloc' callback, which
     *  don't push mis-aligned space fragments on the file free space list
     */
    have_alloc_vfd = (hbool_t)(HDstrcmp(env_h5_drvr, "stdio")
            && HDstrcmp(env_h5_drvr, "split") && HDstrcmp(env_h5_drvr, "multi"));
    if(have_alloc_vfd) {
        /* Set the filename to use for this test (dependent on fapl) */
        h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

        /* Create the file to work on (without alignment) */
        if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Close file */
        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Re-open the file with alignment setting and meta/sdata setting */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, new_fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        /* get alignment setting */
        if(H5Pget_alignment(new_fapl, NULL, &alignment) < 0)
            TEST_ERROR

        /* calculate fragment for alignment of block 30 */
        if((tmp = (hsize_t)file_size % alignment))
            mis_align = alignment - tmp;

        /* Allocate a block of 30 from meta_aggr */
        type = H5FD_MEM_SUPER;
        addr1 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE30);

        /* Verify that the allocated block is aligned */
        if(addr1 % alignment) TEST_ERROR

        /* fragment for alignment of block 30 is freed to free-space */
        HDmemset(&state, 0, sizeof(H5FS_stat_t));
        if(mis_align) {
            state.tot_space += mis_align;
            state.tot_sect_count += 1;
            state.serial_sect_count += 1;
        }

        H5MF__aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);
        saved_ma_size = ma_size;
        if((addr1+TBLOCK_SIZE30) != ma_addr) TEST_ERROR

        /* calculate fragment for alignment of block 2058 */
        mis_align = 0;
        if((tmp = ma_addr % alignment))
            mis_align = alignment - tmp;

        /* Allocate a block of 2058 from meta_aggr */
        addr2 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE2058);

        /* Verify that the allocated block is aligned */
        if(addr2 % alignment) TEST_ERROR

        /* fragment for alignment of block 2058 is freed to free-space */
        if(mis_align) {
            state.tot_space += mis_align;
            state.tot_sect_count += 1;
            state.serial_sect_count += 1;
        }

        H5MF__aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        if((addr2 + TBLOCK_SIZE2058) != ma_addr) TEST_ERROR

        /* meta_aggr->size remains the same */
        if(ma_size != saved_ma_size) TEST_ERROR

        /* calculate fragment for alignment of block 5 from meta_aggr */
        mis_align = 0;
        if((tmp = ma_addr % alignment))
            mis_align = alignment - tmp;

        /* Allocate a block of 5 from meta_aggr */
        addr3 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE5);

        /* fragment for alignment of block 5 is freed to free-space */
        if(mis_align) {
            state.tot_space += mis_align;
            state.tot_sect_count += 1;
            state.serial_sect_count += 1;
        }

        /* Verify that the allocated block is aligned */
        if(addr3 % alignment) TEST_ERROR

        /* Verify total size of free space after all allocations */
    if(f->shared->fs_man[type]) {
        if(check_stats(f, f->shared->fs_man[type], &state))
        TEST_ERROR
    }

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        PASSED()
    } /* end if */
    else {
        SKIPPED();
        HDputs("    Current VFD doesn't support mis-aligned fragments");
    } /* end else */

    return(0);

error:
    H5E_BEGIN_TRY {
        H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_mf_align_alloc4() */

/*
 *-------------------------------------------------------------------------
 * To verify that blocks allocated from the aggregator are aligned
 *
 * Alignment = 1024     aggr->alloc_size = 2048
 *
 *    Allocate first block (30) from meta_aggr: (meta_aggr is empty)
 *        request-size > meta_aggr->size and < meta_aggr->alloc_size
 *    Result:
 *        A block of meta_aggr->alloc_size is allocated from file allocation
 *        Fragment from alignment of file allocation is freed to free-space:[800, 224]
 *        The first block of 30 is allocated from meta_aggr and should be aligned
 *        There is space of 2018 left in meta_aggr
 *        EOA is 3072
 *
 *    Allocate first block (30) from sdata_aggr: (nothing in the aggregator)
 *        A block of sdata_aggr->alloc_size is allocated from file allocation
 *        The first block of 30 is allocated from the aggregator and should be aligned
 *        There is space of 2018 left in sdata_aggr
 *        EOA is 5120
 *
 *    Allocate second block (2058) from meta_aggr:
 *        (request-size + fragment size) > meta_aggr->size and > meta_aggr->alloc_size
 *        sdata_aggr is at EOA but has not used up sdata_aggr->alloc_size
 *    Result:
 *        A block of 2058 is allocated from file allocation
 *        EOA is 7178
 *        Nothing is changed in meta_aggr and sdata_aggr
 *
 * Alignment = 4096     aggr->alloc_size = 2048
 *
 *    Allocate first block (30) from meta_aggr: (meta_aggr is empty)
 *        request-size is > meta_aggr->size and < meta_aggr->alloc_size
 *    Result:
 *        A block of meta_aggr->alloc_size is allocated from file allocation
 *        Fragment from alignment of file allocation is freed to free-space:[800, 3296]
 *        The first block of 30 is allocated from meta_aggr and should be aligned
 *        There is space of 2018 left in meta_aggr
 *        EOA is 6144
 *
 *    Allocate first block (30) from sdata_aggr: (meta_aggr is empty)
 *        meta_aggr is at EOA but has not used up more than meta_aggr->alloc_size
 *    Result:
 *        A block of sdata_aggr->alloc_size is allocated from file allocation
 *        Fragment from alignment of file allocation is freed to free-space:[6144, 2048]
 *        This fragment adjoins meta_aggr and fulfills "absorb" condition,
 *            the remaining space left in meta_aggr is absorbed into the fragment and
 *            freed to free-space: [4126, 2018]
 *        meta_aggr is reset to 0
 *        The first block of 30 is allocated from the aggregator and should be aligned
 *        There is space of 2018 left in sdata_aggr
 *        EOA is 10240
 *
 *    Allocate second block (2058) from meta_aggr:
 *        request-size + fragment size is > meta_aggr->size
 *        request_size is > meta_aggr->alloc_size
 *        sdata_aggr is at EOA but has not used up more than sdata_aggr->alloc_size
 *    Result:
 *        A block of 2058 is allocated from file allocation
 *        Fragment from alignment of file allocation is freed to free-space:[10240, 2048]
 *        This fragment adjoins sdata_aggr and fulfills "absorb" condition,
 *            the remaining space left in sdata_aggr is absorbed into the fragment and
 *            freed to free-space: [8222, 2018]
 *        sdata_aggr is reset to 0
 *        EOA is 14346
 *        meta_aggr and sdata_aggr are all 0
 *-------------------------------------------------------------------------
 */
static unsigned
test_mf_align_alloc5(const char *env_h5_drvr, hid_t fapl, hid_t new_fapl)
{
    hid_t        file = -1;              /* File ID */
    char        filename[FILENAME_LEN]; /* Filename to use */
    H5F_t        *f = NULL;              /* Internal file object pointer */
    h5_stat_size_t      file_size;
    H5FD_mem_t         type, stype;
    haddr_t        addr1, addr2, saddr1;
    H5FS_stat_t state[H5FD_MEM_NTYPES];
    haddr_t         ma_addr=HADDR_UNDEF, new_ma_addr=HADDR_UNDEF;
    haddr_t         sdata_addr=HADDR_UNDEF, new_sdata_addr=HADDR_UNDEF;
    hsize_t         ma_size=0, new_ma_size=0, sdata_size=0, new_sdata_size=0;
    hsize_t        alignment=0, mis_align=0, tmp=0;
    hbool_t             have_alloc_vfd;        /* Whether VFD used has an 'alloc' callback */


    TESTING("H5MF_alloc() of meta/sdata aggregator with alignment: test 5");

    /* Skip test when using VFDs that have their own 'alloc' callback, which
     *  don't push mis-aligned space fragments on the file free space list
     */
    have_alloc_vfd = (hbool_t)(HDstrcmp(env_h5_drvr, "stdio")
            && HDstrcmp(env_h5_drvr, "split") && HDstrcmp(env_h5_drvr, "multi"));
    if(have_alloc_vfd) {
        /* Set the filename to use for this test (dependent on fapl) */
        h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

        /* Create the file to work on (without alignment) */
        if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Close file */
        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Re-open the file with alignment setting and meta/sdata setting */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, new_fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        /* get alignment setting */
        if(H5Pget_alignment(new_fapl, NULL, &alignment) < 0)
            TEST_ERROR

        /* calculate fragment for alignment of block 30 */
        if((tmp = (hsize_t)file_size % alignment))
            mis_align = alignment - tmp;

        /* Allocate a block of 30 from meta_aggr */
        type = H5FD_MEM_SUPER;
        addr1 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE30);

        /* Verify that the allocated block is aligned */
        if(addr1 % alignment) TEST_ERROR

        H5MF__aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        if((addr1 + TBLOCK_SIZE30) != ma_addr) TEST_ERROR

        /* fragment for alignment of block 30 is freed to free-space */
        HDmemset(&state, 0, sizeof(H5FS_stat_t) * H5FD_MEM_NTYPES);
        if(mis_align) {
            state[type].tot_space += mis_align;
            state[type].tot_sect_count += 1;
            state[type].serial_sect_count += 1;
        }

        /* calculate fragment for alignment of block 30 from sdata_aggr */
        mis_align = 0;
        if((tmp = (ma_addr + ma_size) % alignment))
            mis_align = alignment - tmp;

        /* Allocate a block of 30 from sdata_aggr */
        stype = H5FD_MEM_DRAW;
        saddr1 = H5MF_alloc(f, stype, (hsize_t)TBLOCK_SIZE30);

        /* Verify that the allocated block is aligned */
        if(saddr1 % alignment) TEST_ERROR

        /* fragment of alignment for block 30 in sdata_aggr is freed to free-space */
        if(mis_align) {
            state[stype].tot_space += mis_align;
            state[stype].tot_sect_count += 1;
            state[stype].serial_sect_count += 1;
        }

        H5MF__aggr_query(f, &(f->shared->sdata_aggr), &sdata_addr, &sdata_size);
        if((saddr1+TBLOCK_SIZE30) != sdata_addr) TEST_ERROR

        /* calculate fragment for alignment of block 2058 from meta_aggr */
        mis_align = 0;
        if((tmp = (sdata_addr + sdata_size) % alignment))
            mis_align = alignment - tmp;

        /* Allocate a block of 2058 from meta_aggr */
        addr2 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE2058);

        /* Verify that the allocated block is aligned */
        if (addr2 % alignment) TEST_ERROR

        /* fragment for alignment of block 2058 is freed to free-space */
        if(mis_align) {
            state[type].tot_space += mis_align;
            state[type].tot_sect_count += 1;
            state[type].serial_sect_count += 1;
        }

        /* Verify total size of free space after all allocations */
    if(f->shared->fs_man[type]) {
        if(check_stats(f, f->shared->fs_man[type], &(state[type])))
        TEST_ERROR
    }

    if(f->shared->fs_man[stype]) {
        if(check_stats(f, f->shared->fs_man[stype], &(state[stype])))
        TEST_ERROR
    }

        /* nothing is changed in meta_aggr */
        H5MF__aggr_query(f, &(f->shared->meta_aggr), &new_ma_addr, &new_ma_size);
        if (new_ma_addr != ma_addr || new_ma_size != ma_size)
            TEST_ERROR

        /* nothing is changed in sdata_aggr */
        H5MF__aggr_query(f, &(f->shared->sdata_aggr), &new_sdata_addr, &new_sdata_size);
        if (new_sdata_addr != sdata_addr || new_sdata_size != sdata_size)
            TEST_ERROR

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        PASSED()
    } /* end if */
    else {
        SKIPPED();
        HDputs("    Current VFD doesn't support mis-aligned fragments");
    } /* end else */

    return(0);

error:
    H5E_BEGIN_TRY {
        H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_mf_align_alloc5() */


/*
 *-------------------------------------------------------------------------
 * To verify that blocks allocated from the aggregator are aligned
 *
 * Alignment = 1024     aggr->alloc_size = 2048
 *
 *    Allocate first block (30) from meta_aggr: (meta_aggr is empty)
 *        request-size is > meta_aggr->size and < meta_aggr->alloc_size
 *    Result:
 *        A block of meta_aggr->alloc_size is allocated from file allocation
 *        Fragment from alignment of file allocation is freed to free-space:[800, 224]
 *        The first block of 30 is allocated from the aggregator and should be aligned
 *        There is space of 2018 left in meta_aggr->size
 *        EOA is 3072
 *
 *    Allocate first block (30) from sdata_aggr: (sdata_aggr is empty)
 *        request_size > sdata_aggr->size and < sdata_aggr->alloc_size
 *    Result:
 *        A block of sdata_aggr->alloc_size is allocated from file allocation
 *        The first block of 30 is allocated from the aggregator and should be aligned
 *        There is space of 2018 left in sdata_aggr
 *        EOA is 5120
 *
 *    Allocate second block (50) from sdata_aggr:
 *        (request-size+fragment size) <= sdata_aggr->size
 *    Result:
 *        The second block of 50 is allocated from sdata_aggr and should be aligned
 *        Fragment from alignment of aggregator allocation is freed to free-space:[3102, 994]
 *        There is space of 974 left in sdata_aggr
 *
 *    Allocate third block (80) from sdata_aggr:
 *        (request-size+fragment size) > sdata_aggr->size
 *        request-size < sdata_aggr->alloc_size
 *        fragment size < (sdata_aggr->alloc_size - request-size)
 *    Result:
 *        Another block of sdata_aggr->alloc_size block is extended from file allocation
 *            for sdata_aggr
 *        The third block of 80 is allocated from sdata_aggr and should be aligned
 *        Fragment from alignment of aggregator allocation is freed to free-space:[4146, 974]
 *        There is space of 1968 left in sdata_aggr
 *        EOA is 7168
 *
 *    Allocate second block (2058) from meta_aggr:
 *        request-size + fragment size is > meta_aggr->size
 *        request-size is > meta_aggr->alloc_size
 *        sdata_aggr is at EOA and has used up more than sdata_aggr->alloc_size
 *    Result:
 *        The remaining space in sdata_aggr is freed to free-space and shrunk
 *        sdata_aggr is reset to 0
 *        A block of 2058 is allocated from file allocation
 *        Fragment from alignment of file allocation is freed to free-space:[5200, 944]
 *        EOA is at 8202
 *        meta_aggr is unchanged
 *
 * Alignment = 4096     aggr->alloc_size = 2048
 *
 *    Allocate first block (30) from meta_aggr: (meta_aggr is emtpy)
 *        request-size is > meta_aggr->size and < meta_aggr->alloc_size
 *    Result:
 *        A block of meta_aggr->alloc_size is allocated from file allocation
 *        Fragment from alignment of file allocation is freed to free-space:[800, 3296]
 *        The first block of 30 is allocated from the aggregator and should be aligned
 *        There is space of 2018 left in meta_aggr
 *        EOA is 6144
 *
 *    Allocate first block (30) from sdata_aggr: (sdata_aggr is empty)
 *        request_size > sdata_aggr->size and < sdata_aggr->alloc_size
 *    Result:
 *        A block of sdata_aggr->alloc_size is allocated from file allocation
 *        Fragment from alignment of file allocation is freed to free-space: [6144, 2048]
 *        The first block of 30 is allocated from the aggregator and should be aligned
 *        There is space of 2018 left in sdata_aggr
 *        EOA is 10240
 *
 *    Allocate second block (50) from sdata_aggr:
 *        (request-size+fragment size) is > sdata_aggr->size
 *        request-size < sdata_aggr->alloc_size
 *        fragment size > (sdata_aggr->alloc_size - request-size)
 *    Result:
 *        A block of (fragment size + request-size) is extended from file allocation for the aggregator
 *        The second block of 50 is allocated from sdata_aggr and should be aligned
 *        Fragment from alignment of aggregator allocation is freed to free-space:[8222, 4066]
 *        There is space of 2018 left in sdata_aggr
 *        EOA is at 14356
 *
 *    Allocate third block (80) from sdata_aggr:
 *        (request-size+fragment size) is > sdata_aggr->size
 *        request-size < sdata_aggr->alloc_size
 *        fragment size > (sdata_aggr->alloc_size - request-size)
 *    Result:
 *        A block of (fragment size + request-size) is extended from file allocation for sdata_aggr
 *        The third block of 80 is allocated from sdata_aggr and should be aligned
 *        Fragment from alignment of aggregator allocation is freed to free-space:[12338, 4046]
 *        There is space of 2018 left in sdata_aggr
 *        EOA is 18482
 *
 *    Allocate second block (2058) from meta_aggr:
 *        request-size + fragment size is > meta_aggr->size
 *        request-size is > meta_aggr->alloc_size
 *        sdata_aggr is at EOA and has used up more than sdata_aggr->alloc_size
 *    Result:
 *        The remaining space in sdata_aggr is freed to free-space and shrunk: [16464, 2018]
 *        sdata_aggr is reset to 0
 *        A block of 2058 is allocated from file allocation
 *        Fragment from alignment of file allocation is freed to free-space:[16464, 4016]
 *        EOA is at 22538
 *        meta_aggr is unchanged
 *-------------------------------------------------------------------------
 */
static unsigned
test_mf_align_alloc6(const char *env_h5_drvr, hid_t fapl, hid_t new_fapl)
{
    hid_t        file = -1;              /* File ID */
    char        filename[FILENAME_LEN]; /* Filename to use */
    H5F_t        *f = NULL;              /* Internal file object pointer */
    h5_stat_size_t      file_size;
    H5FD_mem_t         type, stype;
    haddr_t        addr1, addr2;
    haddr_t        saddr1, saddr2, saddr3;
    H5FS_stat_t state[H5FD_MEM_NTYPES];
    haddr_t         ma_addr=HADDR_UNDEF, new_ma_addr=HADDR_UNDEF, sdata_addr=HADDR_UNDEF;
    hsize_t         ma_size=0, new_ma_size=0, sdata_size=0;
    hsize_t        alignment=0, mis_align=0, tmp=0;
    hbool_t             have_alloc_vfd;        /* Whether VFD used has an 'alloc' callback */

    TESTING("H5MF_alloc() of meta/sdata aggregator with alignment: test 6");

    /* Skip test when using VFDs that have their own 'alloc' callback, which
     *  don't push mis-aligned space fragments on the file free space list
     */
    have_alloc_vfd = (hbool_t)(HDstrcmp(env_h5_drvr, "stdio")
            && HDstrcmp(env_h5_drvr, "split") && HDstrcmp(env_h5_drvr, "multi"));
    if(have_alloc_vfd) {
        /* Set the filename to use for this test (dependent on fapl) */
        h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

        /* Create the file to work on (without alignment) */
        if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Close file */
        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Re-open the file with alignment setting and meta/sdata setting */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, new_fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        /* get alignment setting */
        if(H5Pget_alignment(new_fapl, NULL, &alignment) < 0)
            TEST_ERROR

        /* calculate fragment for alignment of block 30 */
        if((tmp = (hsize_t)file_size % alignment))
            mis_align = alignment - tmp;

        /* Allocate a block of 30 from meta_aggr */
        type = H5FD_MEM_SUPER;
        addr1 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE30);

        /* Verify that the allocated block is aligned */
        if (addr1 % alignment) TEST_ERROR

        /* fragment for alignment of block 30 in meta_aggr is freed to free-space */
        HDmemset(&state, 0, sizeof(H5FS_stat_t) * H5FD_MEM_NTYPES);
        if(mis_align) {
            state[type].tot_space += mis_align;
            state[type].tot_sect_count += 1;
            state[type].serial_sect_count += 1;
        }

        H5MF__aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);
        if ((addr1+TBLOCK_SIZE30) != ma_addr)
            TEST_ERROR

        /* calculate fragment for alignment of block 30 in sdata_aggr */
        mis_align = 0;
        if ((tmp = (ma_addr + ma_size) % alignment))
            mis_align = alignment - tmp;

        /* Allocate a block of 30 from sdata_aggr */
        stype = H5FD_MEM_DRAW;
        saddr1 = H5MF_alloc(f, stype, (hsize_t)TBLOCK_SIZE30);

        /* Verify that the allocated block is aligned */
        if (saddr1 % alignment) TEST_ERROR

        /* fragment for alignment of block 30 in sdata_aggr is freed to free-space */
        if (mis_align) {
            state[stype].tot_space += mis_align;
            state[stype].tot_sect_count += 1;
            state[stype].serial_sect_count += 1;
        }

        H5MF__aggr_query(f, &(f->shared->sdata_aggr), &sdata_addr, &sdata_size);
        if (sdata_addr != (saddr1+TBLOCK_SIZE30)) TEST_ERROR

        /* calculate fragment for alignment of block 50 in sdata_aggr */
        mis_align = 0;
        if ((tmp = sdata_addr % alignment))
            mis_align = alignment - tmp;

        /* Allocate a block of 50 from sdata_aggr */
        saddr2 = H5MF_alloc(f, stype, (hsize_t)TBLOCK_SIZE50);

        /* Verify that the allocated block is aligned */
        if (saddr2 % alignment) TEST_ERROR

        /* fragment for alignment of block 50 in sdata_aggr is freed to free-space */
        if (mis_align) {
            state[stype].tot_space += mis_align;
            state[stype].tot_sect_count += 1;
            state[stype].serial_sect_count += 1;
        }

        H5MF__aggr_query(f, &(f->shared->sdata_aggr), &sdata_addr, &sdata_size);
        if (sdata_addr != (saddr2+TBLOCK_SIZE50)) TEST_ERROR

        /* calculate fragment for alignment of block 80 in sdata_aggr */
        mis_align = 0;
        if ((tmp = sdata_addr % alignment))
            mis_align = alignment - tmp;

        /* Allocate a block of 80 from sdata_aggr */
        saddr3 = H5MF_alloc(f, stype, (hsize_t)TBLOCK_SIZE80);

        /* Verify that the allocated block is aligned */
        if (saddr3 % alignment) TEST_ERROR

        /* fragment for alignment of block 80 in sdata_aggr is freed to free-space */
        if (mis_align) {
            state[stype].tot_space += mis_align;
            state[stype].tot_sect_count += 1;
            state[stype].serial_sect_count += 1;
        }

        H5MF__aggr_query(f, &(f->shared->sdata_aggr), &sdata_addr, &sdata_size);
        if (sdata_addr != (saddr3+TBLOCK_SIZE80)) TEST_ERROR

        /* calculate fragment for alignment of block 2058 */
        /* remaining space in sdata_aggr is freed and shrunk */
        mis_align = 0;
        if ((tmp = sdata_addr % alignment))
            mis_align = alignment - tmp;

        /* Allocate a block of 2058 from meta_aggr */
        addr2 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE2058);

        /* Verify that the allocated block is aligned */
        if (addr2 % alignment) TEST_ERROR

        /* fragment for alignment of block 2058 is freed to free-space */
        if (mis_align) {
            state[type].tot_space += mis_align;
            state[type].tot_sect_count += 1;
            state[type].serial_sect_count += 1;
        }

        H5MF__aggr_query(f, &(f->shared->meta_aggr), &new_ma_addr, &new_ma_size);
        H5MF__aggr_query(f, &(f->shared->sdata_aggr), &sdata_addr, &sdata_size);

        if (new_ma_addr != ma_addr && new_ma_size != ma_size)
            TEST_ERROR

        if (sdata_addr != HADDR_UNDEF || sdata_size != 0)
            TEST_ERROR

    if(f->shared->fs_man[type]) {
        if(check_stats(f, f->shared->fs_man[type], &(state[type])))
        TEST_ERROR
    }


    if(f->shared->fs_man[stype]) {
        if(check_stats(f, f->shared->fs_man[stype], &(state[stype])))
        TEST_ERROR
    }

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        PASSED()
    } /* end if */
    else {
        SKIPPED();
        HDputs("    Current VFD doesn't support mis-aligned fragments");
    } /* end else */

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* test_mf_align_alloc6() */

/*
 * Test a bug that occurs when an allocator with zero size left and an unaligned
 * endpoint is extended to allocate an aligned object
 */
static unsigned
test_mf_bug1(const char *env_h5_drvr, hid_t fapl)
{
    hid_t               file = -1;              /* File ID */
    hid_t               copied_fapl = -1;       /* FAPL to use for this test */
    char                filename[FILENAME_LEN]; /* Filename to use */
    H5F_t               *f = NULL;              /* Internal file object pointer */
    H5FD_mem_t          type;
    haddr_t             addr1, addr2;
    hsize_t             block_size;
    hsize_t             align;
    hbool_t             split = FALSE, multi = FALSE;

    TESTING("H5MF_alloc() bug 1");

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Copy fapl */
    if((copied_fapl = H5Pcopy(fapl)) < 0)
        TEST_ERROR

    /* Get metadata block size */
    if(H5Pget_meta_block_size(copied_fapl, &block_size) < 0)
        TEST_ERROR

    /* Set alignment to equal block size / 2 */
    align = block_size / 2;
    if(H5Pset_alignment(copied_fapl, 0, align) < 0)
        TEST_ERROR

    /* Check for split or multi driver */
    if(!HDstrcmp(env_h5_drvr, "split"))
        split = TRUE;
    else if(!HDstrcmp(env_h5_drvr, "multi"))
        multi = TRUE;

    /* Add alignment to member files for split/multi driver */
    if(split || multi) {
        hid_t memb_fapl;

        /* Creat fapl */
        if((memb_fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
            TEST_ERROR

        /* Set alignment. Note that it is the block size of the parent FAPL that
         * is important here. */
        if(H5Pset_alignment(memb_fapl, 0, align) < 0)
            TEST_ERROR

        if(split) {
            /* Set split driver with new FAPLs */
            if(H5Pset_fapl_split(copied_fapl, "-m.h5", memb_fapl, "-r.h5", memb_fapl) < 0)
                TEST_ERROR
        } /* end if */
        else {
            H5FD_mem_t memb_map[H5FD_MEM_NTYPES];
            hid_t memb_fapl_arr[H5FD_MEM_NTYPES];
            char *memb_name[H5FD_MEM_NTYPES];
            haddr_t memb_addr[H5FD_MEM_NTYPES];
            hbool_t relax;
            H5FD_mem_t  mt;

            /* Get current multi settings */
            HDmemset(memb_name, 0, sizeof memb_name);
            if(H5Pget_fapl_multi(copied_fapl, memb_map, NULL, memb_name, memb_addr, &relax) < 0)
                TEST_ERROR

            /* Populate memb_fapl_arr, patch memb_addr so member file addresses
             * are aligned */
            for(mt = H5FD_MEM_DEFAULT; mt < H5FD_MEM_NTYPES; H5_INC_ENUM(H5FD_mem_t, mt)) {
                memb_fapl_arr[mt] = memb_fapl;
                memb_addr[mt] = ((memb_addr[mt] + align - 1) / align) * align;
            } /* end for */

            /* Set multi driver with new FAPLs */
            if(H5Pset_fapl_multi(copied_fapl, memb_map, memb_fapl_arr, (const char * const *)memb_name, memb_addr, relax) < 0)
                TEST_ERROR

            /* Free memb_name */
            for(mt = H5FD_MEM_DEFAULT; mt < H5FD_MEM_NTYPES; H5_INC_ENUM(H5FD_mem_t, mt))
                free(memb_name[mt]);
        } /* end else */

        /* Close memb_fapl */
        if(H5Pclose(memb_fapl) < 0)
            TEST_ERROR
    } /* end if */

    /* Reopen the file with alignment */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, copied_fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = (H5F_t *)H5I_object(file)))
        TEST_ERROR

    /* Allocate a block of size align from meta_aggr.  This should create an
     * aggregator that extends to the end of the file, with
     * block_size / 2 bytes remaining, and the end of the file aligned */
    type = H5FD_MEM_SUPER;
    addr1 = H5MF_alloc(f, type, align);

    /* Verify that the allocated block is aligned */
    if(addr1 % align) TEST_ERROR

    /* Allocate a block of size align from meta_aggr.  This should force the
     * aggregator to extend to the end of the file, with 0 bytes remaining, and
     * the end of the file aligned */
    type = H5FD_MEM_SUPER;
    addr2 = H5MF_alloc(f, type, align);

    /* Verify that the allocated block is aligned */
    if(addr2 % align) TEST_ERROR

    /* Verify that the allocated block is placed align after the previous */
    if((addr2 - addr1) != align) TEST_ERROR

    /* Allocate a block of size block_size + 1 from meta_aggr.  This should
     * force the aggregator to extend to the end of the file, with 0 bytes
     * remaining, and the end of the file unaligned */
    type = H5FD_MEM_SUPER;
    addr1 = H5MF_alloc(f, type, block_size + (hsize_t)1);

    /* Verify that the allocated block is aligned */
    if(addr1 % align) TEST_ERROR

    /* Verify that the allocated block is placed block_size / 2 after the
     * previous */
    if((addr1 - addr2) != align) TEST_ERROR

    /* Allocate a block of size 1.  This should extend the aggregator from
     * the previous allocation, and align the new block */
    type = H5FD_MEM_SUPER;
    addr2 = H5MF_alloc(f, type, (hsize_t)1);

    /* Verify that the allocated block is aligned */
    if(addr2 % align) TEST_ERROR

    /* Verify that the allocated block is placed 3 * (block_size / 2) after
     * the previous */
    if((addr2 - addr1) != (3 * align)) TEST_ERROR

    PASSED()

    /* Close file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    return(0);

error:
    H5E_BEGIN_TRY {
        H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_mf_bug1() */

/*
 * Verify that the file's free-space manager(s) are persistent for a split-file
 *-------------------------------------------------------------------------
 */
#ifdef PB_OUT
static unsigned
test_mf_fs_persist_split(void)
{
    hid_t   file = -1;                  /* File ID */
    hid_t   fcpl = -1;                  /* File creation property list ID */
    hid_t   fapl = -1;                  /* File access property list ID */
    char    filename[FILENAME_LEN];     /* Filename to use */
    H5F_t   *f = NULL;                  /* Internal file object pointer */
    H5FD_mem_t  type, stype, btype;     /* File allocation type */
    H5FS_stat_t fs_stat;                    /* Information for free-space manager */
    haddr_t addr1, addr2, addr3, addr4; /* File address for H5FD_MEM_SUPER */
    haddr_t saddr1, saddr2, saddr3, saddr4; /* File address for H5FD_MEM_DRAW */
    haddr_t baddr5, baddr6, baddr7, baddr8; /* File address for H5FD_MEM_BTREE */
    haddr_t tmp_addr;                   /* temporary variable for address */

    TESTING("File's free-space managers are persistent for split-file");

    /* for now, we don't support persistent free space managers
     * with the split file driver.
     */
    SKIPPED();
    HDfprintf(stdout, " Persistent FSMs disabled in multi file driver.\n");
    return 0;  /* <========== note return */

    /* File creation property list template */
    if((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)

    /* for now, we don't support persistent free space managers
     * with the split file driver.
     */
    SKIPPED();
    HDfprintf(stdout, " Persistent FSMs disabled in multi file driver.\n");
    return 0;  /* <========== note return */

    /* File creation property list template */
    if((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)

    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        FAIL_STACK_ERROR

    /* Set up split driver */
    if(H5Pset_fapl_split(fapl, "-m.h5", H5P_DEFAULT, "-r.h5", H5P_DEFAULT)<0)
        FAIL_STACK_ERROR

    /* File creation property list template */
    if((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
        FAIL_STACK_ERROR

    if(H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_FSM_AGGR, TRUE, (hsize_t)1) < 0)
        FAIL_STACK_ERROR

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = (H5F_t *)H5I_object(file)))
        FAIL_STACK_ERROR

    /* Allocate 4 blocks of type H5FD_MEM_SUPER */
    type = H5FD_MEM_SUPER;
    if(HADDR_UNDEF == (addr1 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE1)))
        FAIL_STACK_ERROR
    if(HADDR_UNDEF == (addr2 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE2)))
        FAIL_STACK_ERROR
    if(HADDR_UNDEF == (addr3 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE3)))
        FAIL_STACK_ERROR
    if(HADDR_UNDEF == (addr4 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE4)))
        FAIL_STACK_ERROR

    /* Put block #1, #3 into H5FD_MEM_SUPER free-space manager */
    if(H5MF_xfree(f, type, addr1, (hsize_t)TBLOCK_SIZE1) < 0)
        FAIL_STACK_ERROR
    if(H5MF_xfree(f, type, addr3, (hsize_t)TBLOCK_SIZE3) < 0)
        FAIL_STACK_ERROR

    /* Allocate 4 blocks of type H5FD_MEM_DRAW */
    stype = H5FD_MEM_DRAW;
    if(HADDR_UNDEF == (saddr1 = H5MF_alloc(f, stype, (hsize_t)TBLOCK_SIZE1)))
        FAIL_STACK_ERROR
    if(HADDR_UNDEF == (saddr2 = H5MF_alloc(f, stype, (hsize_t)TBLOCK_SIZE2)))
        FAIL_STACK_ERROR
    if(HADDR_UNDEF == (saddr3 = H5MF_alloc(f, stype, (hsize_t)TBLOCK_SIZE3)))
        FAIL_STACK_ERROR
    if(HADDR_UNDEF == (saddr4 = H5MF_alloc(f, stype, (hsize_t)TBLOCK_SIZE4)))
        FAIL_STACK_ERROR

    /* Put block #1, #3 into H5FD_MEM_DRAW free-space manager */
    if(H5MF_xfree(f, stype, saddr1, (hsize_t)TBLOCK_SIZE1) < 0)
        FAIL_STACK_ERROR
    if(H5MF_xfree(f, stype, saddr3, (hsize_t)TBLOCK_SIZE3) < 0)
        FAIL_STACK_ERROR

    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Re-open the file */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = (H5F_t *)H5I_object(file)))
        FAIL_STACK_ERROR

    /* Verify that the H5FD_MEM_SUPER free-space manager is there */
    if(!H5F_addr_defined(f->shared->fs_addr[type]))
        TEST_ERROR

    /* Start up H5FD_MEM_SUPER free-space manager */
    if(H5MF__open_fstype(f, (H5F_mem_page_t)type) < 0)
        FAIL_STACK_ERROR

    /* Get free-space info */
    if(H5FS_stat_info(f, f->shared->fs_man[type], &fs_stat) < 0)
        FAIL_STACK_ERROR

    /* Verify free-space info */
    if(fs_stat.tot_space < (TBLOCK_SIZE1+TBLOCK_SIZE3))
        TEST_ERROR
    if(fs_stat.serial_sect_count < 2)
        TEST_ERROR

    /* Retrieve block #1 from H5FD_MEM_SUPER free-space manager; block #3 still in free-space */
    if(HADDR_UNDEF == (tmp_addr = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE1)))
        FAIL_STACK_ERROR
    if(tmp_addr != addr1)
        TEST_ERROR

    /* Verify that the free-space manager for H5FD_MEM_DRAW is there */
    if(!H5F_addr_defined(f->shared->fs_addr[stype]))
        TEST_ERROR


    /* Start up H5FD_MEM_DRAW free-space manager */
    if(H5MF__open_fstype(f, (H5F_mem_page_t)stype) < 0)
        FAIL_STACK_ERROR

    /* Get free-space info */
    if(H5FS_stat_info(f, f->shared->fs_man[stype], &fs_stat) < 0)
        FAIL_STACK_ERROR

    /* Verify free-space info */
    if(fs_stat.tot_space < (TBLOCK_SIZE1+TBLOCK_SIZE3))
        TEST_ERROR
    if(fs_stat.serial_sect_count < 2)
        TEST_ERROR

    /* Retrieve blocks #1 from H5FD_MEM_DRAW free-space manager */
    if(HADDR_UNDEF == (tmp_addr = H5MF_alloc(f, stype, (hsize_t)TBLOCK_SIZE1)))
        FAIL_STACK_ERROR
    if(tmp_addr != saddr1)
        TEST_ERROR

    /* Retrieve blocks #3 from H5FD_MEM_DRAW free-space manager */
    if(HADDR_UNDEF == (tmp_addr = H5MF_alloc(f, stype, (hsize_t)TBLOCK_SIZE3)))
        FAIL_STACK_ERROR
    if(tmp_addr != saddr3)
        TEST_ERROR
    /* H5FD_MEM_DRAW free-space manager is going away at closing */
    /* works for this one because the freeing of sect_addr is to H5FD_MEM_SUPER fs, not against itself */

    /* Allocate 4 blocks of type H5FD_MEM_BTREE */
    btype = H5FD_MEM_BTREE;
    if(HADDR_UNDEF == (baddr5 = H5MF_alloc(f, btype, (hsize_t)TBLOCK_SIZE5)))
        FAIL_STACK_ERROR
    if(HADDR_UNDEF == (baddr6 = H5MF_alloc(f, btype, (hsize_t)TBLOCK_SIZE6)))
        FAIL_STACK_ERROR
    if(HADDR_UNDEF == (baddr7 = H5MF_alloc(f, btype, (hsize_t)TBLOCK_SIZE7)))
        FAIL_STACK_ERROR
    if(HADDR_UNDEF == (baddr8 = H5MF_alloc(f, btype, (hsize_t)TBLOCK_SIZE8)))
        FAIL_STACK_ERROR

    /* Put block #5 & #7 into H5FD_MEM_BTREE free-space manager */
    if(H5MF_xfree(f, btype, baddr5, (hsize_t)TBLOCK_SIZE5) < 0)
        FAIL_STACK_ERROR
    if(H5MF_xfree(f, btype, baddr7, (hsize_t)TBLOCK_SIZE7) < 0)
        FAIL_STACK_ERROR

    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Re-open the file */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = (H5F_t *)H5I_object(file)))
        FAIL_STACK_ERROR

    /* Verify that the free-space manager for H5FD_MEM_DRAW is not there */
    if(H5F_addr_defined(f->shared->fs_addr[stype]))
        TEST_ERROR

    /* Verify that the free-space manager for H5FD_MEM_SUPER is there */
    if(!H5F_addr_defined(f->shared->fs_addr[type]))
        TEST_ERROR

    /* Start up H5FD_MEM_SUPER free-space manager */
    if(H5MF__open_fstype(f, (H5F_mem_page_t)type) < 0)
        FAIL_STACK_ERROR

    /* Get free-space info */
    if(H5FS_stat_info(f, f->shared->fs_man[type], &fs_stat) < 0)
        FAIL_STACK_ERROR

    /* Verify free-space info */
    if(fs_stat.tot_space < (TBLOCK_SIZE3+TBLOCK_SIZE5+TBLOCK_SIZE7))
        TEST_ERROR

    /* Retrieve block #3 from H5FD_MEM_SUPER free-space manager */
    if(HADDR_UNDEF == (tmp_addr = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE3)))
        FAIL_STACK_ERROR
    if(tmp_addr != addr3)
        TEST_ERROR

    /* Retrieve block #7 from H5FD_MEM_BTREE free-space manager */
    if(HADDR_UNDEF == (tmp_addr = H5MF_alloc(f, btype, (hsize_t)TBLOCK_SIZE7)))
        FAIL_STACK_ERROR
    if(tmp_addr != baddr7)
        TEST_ERROR

    /* There should still be block #5 of H5FD_MEM_BTREE in H5FD_MEM_BTREE free-space manager */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Re-open the file */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = (H5F_t *)H5I_object(file)))
        FAIL_STACK_ERROR

    /* Verify that the H5FD_MEM_SUPER free-space manager is there */
    if(!H5F_addr_defined(f->shared->fs_addr[type]))
        TEST_ERROR

    /* Start up H5FD_MEM_SUPER free-space manager */
    if(H5MF__open_fstype(f, (H5F_mem_page_t)type) < 0)
        FAIL_STACK_ERROR

    /* Get free-space info */
    if(H5FS_stat_info(f, f->shared->fs_man[type], &fs_stat) < 0)
        FAIL_STACK_ERROR

    /* Verify free-space info */
    if(fs_stat.tot_space < TBLOCK_SIZE5)
        TEST_ERROR

    /* Closing */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR
    if(H5Pclose(fapl) < 0)
        FAIL_STACK_ERROR
    if(H5Pclose(fcpl) < 0)
        FAIL_STACK_ERROR

    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        H5Fclose(file);
        H5Pclose(fcpl);
        H5Pclose(fapl);
    } H5E_END_TRY;
    return(1);
} /* test_mf_fs_persist_split() */

#define MULTI_SETUP(memb_map, memb_fapl, memb_name, memb_addr, sv)  {   \
    H5FD_mem_t mt;                              \
    HDmemset(memb_map, 0,  sizeof memb_map);                    \
    HDmemset(memb_fapl, 0, sizeof memb_fapl);                   \
    HDmemset(memb_name, 0, sizeof memb_name);                   \
    HDmemset(memb_addr, 0, sizeof memb_addr);                   \
    HDmemset(sv, 0, sizeof sv);                         \
    for(mt = H5FD_MEM_DEFAULT; mt < H5FD_MEM_NTYPES; H5_INC_ENUM(H5FD_mem_t, mt)) { \
        memb_map[mt] = H5FD_MEM_SUPER;                      \
        memb_fapl[mt] = H5P_DEFAULT;                        \
    }                                       \
    memb_map[H5FD_MEM_BTREE] = H5FD_MEM_BTREE;                  \
    memb_map[H5FD_MEM_DRAW] = H5FD_MEM_DRAW;                    \
    memb_map[H5FD_MEM_GHEAP] = H5FD_MEM_GHEAP;                  \
    memb_map[H5FD_MEM_LHEAP] = H5FD_MEM_LHEAP;                  \
    HDsprintf(sv[H5FD_MEM_SUPER], "%%s-%c.h5", 's');              \
    memb_name[H5FD_MEM_SUPER] = sv[H5FD_MEM_SUPER];             \
    memb_addr[H5FD_MEM_SUPER] = 0;                      \
    HDsprintf(sv[H5FD_MEM_BTREE],  "%%s-%c.h5", 'b');             \
    memb_name[H5FD_MEM_BTREE] = sv[H5FD_MEM_BTREE];             \
    memb_addr[H5FD_MEM_BTREE] = HADDR_MAX/6;                    \
    HDsprintf(sv[H5FD_MEM_DRAW], "%%s-%c.h5", 'r');               \
    memb_name[H5FD_MEM_DRAW] = sv[H5FD_MEM_DRAW];               \
    memb_addr[H5FD_MEM_DRAW] = HADDR_MAX/3;                 \
    HDsprintf(sv[H5FD_MEM_GHEAP], "%%s-%c.h5", 'g');              \
    memb_name[H5FD_MEM_GHEAP] = sv[H5FD_MEM_GHEAP];             \
    memb_addr[H5FD_MEM_GHEAP] = HADDR_MAX/2;                    \
    HDsprintf(sv[H5FD_MEM_LHEAP], "%%s-%c.h5", 'l');              \
    memb_name[H5FD_MEM_LHEAP] = sv[H5FD_MEM_LHEAP];             \
    memb_addr[H5FD_MEM_LHEAP] = HADDR_MAX*2/3;                  \
    HDsprintf(sv[H5FD_MEM_OHDR], "%%s-%c.h5", 'o');               \
    memb_name[H5FD_MEM_OHDR] = sv[H5FD_MEM_OHDR];               \
    memb_addr[H5FD_MEM_OHDR] = HADDR_MAX*5/6;                   \
}

/*
 *-------------------------------------------------------------------------
 * Verify that the file's free-space manager(s) are persistent for a multi-file
 *-------------------------------------------------------------------------
 */
static unsigned
test_mf_fs_persist_multi(void)
{
    hid_t   file = -1;                  /* File ID */
    hid_t   fcpl = -1;                  /* File creation property list ID */
    hid_t   fapl = -1;                  /* File access property list ID */
    char    filename[FILENAME_LEN];     /* Filename to use */
    H5F_t   *f = NULL;                  /* Internal file object pointer */
    H5FD_mem_t  type, stype, btype, gtype;  /* File allocation type */
    H5FS_stat_t fs_stat;                    /* Information for free-space manager */
    haddr_t addr1, addr2, addr3, addr4;     /* File allocation type */
    haddr_t saddr1, saddr2, saddr3, saddr4; /* File address for H5FD_MEM_SUPER */
    haddr_t baddr1, baddr2, baddr3, baddr4; /* File address for H5FD_MEM_DRAW */
    haddr_t gaddr1, gaddr2;             /* File address for H5FD_MEM_GHEAP */
    haddr_t tmp_addr;                   /* Temporary variable for address */
    H5FS_section_info_t *node;          /* Free space section node */
    htri_t      node_found = FALSE;     /* Indicate section is in free-space */
    H5FD_mem_t  memb_map[H5FD_MEM_NTYPES];  /* Memory usage map */
    hid_t       memb_fapl[H5FD_MEM_NTYPES]; /* Member access properties */
    char        sv[H5FD_MEM_NTYPES][64];    /* Name generators */
    const       char *memb_name[H5FD_MEM_NTYPES];       /* Name generators */
    haddr_t     memb_addr[H5FD_MEM_NTYPES]; /* Member starting address */


    TESTING("File's free-space managers are persistent for multi-file");

    /* for now, we don't support persistent free space managers
     * with the multi file driver.
     */
    SKIPPED();
    HDfprintf(stdout, " Persistent FSMs disabled in multi file driver.\n");
    return 0;  /* <========== note return */

    /* for now, we don't support persistent free space managers
     * with the multi file driver.
     */
    SKIPPED();
    HDfprintf(stdout, " Persistent FSMs disabled in multi file driver.\n");
    return 0;  /* <========== note return */

    /* File creation property list template */
    if((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
        FAIL_STACK_ERROR

    if(H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_FSM_AGGR, TRUE, (hsize_t)1) < 0)
        FAIL_STACK_ERROR

    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        FAIL_STACK_ERROR

    MULTI_SETUP(memb_map, memb_fapl, memb_name, memb_addr, sv)

    if(H5Pset_fapl_multi(fapl, memb_map, memb_fapl, memb_name, memb_addr, TRUE) < 0)
        TEST_ERROR;

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = (H5F_t *)H5I_object(file)))
        FAIL_STACK_ERROR

    /* Allocate 4 blocks of type H5FD_MEM_SUPER */
    type = H5FD_MEM_SUPER;
    if(HADDR_UNDEF == (addr1 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE1)))
        FAIL_STACK_ERROR
    if(HADDR_UNDEF == (addr2 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE2)))
        FAIL_STACK_ERROR
    if(HADDR_UNDEF == (addr3 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE3)))
        FAIL_STACK_ERROR
    if(HADDR_UNDEF == (addr4 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE4)))
        FAIL_STACK_ERROR

    /* Put block #1, #3 into H5FD_MEM_SUPER free-space manager */
    if(H5MF_xfree(f, type, addr1, (hsize_t)TBLOCK_SIZE1) < 0)
        FAIL_STACK_ERROR
    if(H5MF_xfree(f, type, addr3, (hsize_t)TBLOCK_SIZE3) < 0)
        FAIL_STACK_ERROR

    /* Allocate 4 blocks of type H5FD_MEM_DRAW */
    stype = H5FD_MEM_DRAW;
    if(HADDR_UNDEF == (saddr1 = H5MF_alloc(f, stype, (hsize_t)TBLOCK_SIZE1)))
        FAIL_STACK_ERROR
    if(HADDR_UNDEF == (saddr2 = H5MF_alloc(f, stype, (hsize_t)TBLOCK_SIZE2)))
        FAIL_STACK_ERROR
    if(HADDR_UNDEF == (saddr3 = H5MF_alloc(f, stype, (hsize_t)TBLOCK_SIZE3)))
        FAIL_STACK_ERROR
    if(HADDR_UNDEF == (saddr4 = H5MF_alloc(f, stype, (hsize_t)TBLOCK_SIZE4)))
        FAIL_STACK_ERROR

    /* Put block #1, #3 into H5FD_MEM_DRAW free-space manager */
    if(H5MF_xfree(f, stype, saddr1, (hsize_t)TBLOCK_SIZE1) < 0)
        FAIL_STACK_ERROR
    if(H5MF_xfree(f, stype, saddr3, (hsize_t)TBLOCK_SIZE3) < 0)
        FAIL_STACK_ERROR

    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Re-open the file */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = (H5F_t *)H5I_object(file)))
        FAIL_STACK_ERROR

    /* Verify that the H5FD_MEM_SUPER free-space manager is there */
    if(!H5F_addr_defined(f->shared->fs_addr[type]))
        TEST_ERROR

    /* Start up H5FD_MEM_SUPER free-space manager */
    if(H5MF__open_fstype(f, (H5F_mem_page_t)type) < 0)
        FAIL_STACK_ERROR

    /* Get free-space info */
    if(H5FS_stat_info(f, f->shared->fs_man[type], &fs_stat) < 0)
        FAIL_STACK_ERROR

    /* Verify free-space info */
    if(fs_stat.tot_space < (TBLOCK_SIZE1+TBLOCK_SIZE3))
        TEST_ERROR
    if(fs_stat.serial_sect_count < 2)
        TEST_ERROR

    /* Retrieve block #1 from H5FD_MEM_SUPER free-space manager; block #3 still in free-space */
    if(HADDR_UNDEF == (tmp_addr = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE1)))
        FAIL_STACK_ERROR
    if(tmp_addr != addr1)
        TEST_ERROR

    /* Verify that the free-space manager for H5FD_MEM_DRAW is there */
    if(!H5F_addr_defined(f->shared->fs_addr[stype]))
        TEST_ERROR

    /* Start up H5FD_MEM_DRAW free-space manager */
    if(H5MF__open_fstype(f, (H5F_mem_page_t)stype) < 0)
        FAIL_STACK_ERROR

    /* Get free-space info */
    if(H5FS_stat_info(f, f->shared->fs_man[stype], &fs_stat) < 0)
        FAIL_STACK_ERROR

    /* Verify free-space info */
    if(fs_stat.tot_space < (TBLOCK_SIZE1+TBLOCK_SIZE3))
        TEST_ERROR
    if(fs_stat.serial_sect_count < 2)
        TEST_ERROR

    /* Retrieve blocks #1 from H5FD_MEM_DRAW free-space manager */
    if(HADDR_UNDEF == (tmp_addr = H5MF_alloc(f, stype, (hsize_t)TBLOCK_SIZE1)))
        FAIL_STACK_ERROR
    if(tmp_addr != saddr1)
        TEST_ERROR

    /* Retrieve blocks #3 from H5FD_MEM_DRAW free-space manager */
    if(HADDR_UNDEF == (tmp_addr = H5MF_alloc(f, stype, (hsize_t)TBLOCK_SIZE3)))
        FAIL_STACK_ERROR
    if(tmp_addr != saddr3)
        TEST_ERROR

    /* Allocate 4 blocks of type H5FD_MEM_BTREE */
    btype = H5FD_MEM_BTREE;
    if(HADDR_UNDEF == (baddr1 = H5MF_alloc(f, btype, (hsize_t)TBLOCK_SIZE1)))
        FAIL_STACK_ERROR
    if(HADDR_UNDEF == (baddr2 = H5MF_alloc(f, btype, (hsize_t)TBLOCK_SIZE2)))
        FAIL_STACK_ERROR
    if(HADDR_UNDEF == (baddr3 = H5MF_alloc(f, btype, (hsize_t)TBLOCK_SIZE3)))
        FAIL_STACK_ERROR
    if(HADDR_UNDEF == (baddr4 = H5MF_alloc(f, btype, (hsize_t)TBLOCK_SIZE4)))
        FAIL_STACK_ERROR

    /* Put block #1 & #3 into H5FD_MEM_BTREE free-space manager */
    if(H5MF_xfree(f, btype, baddr1, (hsize_t)TBLOCK_SIZE1) < 0)
        FAIL_STACK_ERROR
    if(H5MF_xfree(f, btype, baddr3, (hsize_t)TBLOCK_SIZE3) < 0)
        FAIL_STACK_ERROR

    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Re-open the file */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = (H5F_t *)H5I_object(file)))
        FAIL_STACK_ERROR

    /* Verify that the free-space manager for H5FD_MEM_SUPER is there */
    if(!H5F_addr_defined(f->shared->fs_addr[type]))
        TEST_ERROR

    /* Start up H5FD_MEM_SUPER free-space manager */
    if(H5MF__open_fstype(f, (H5F_mem_page_t)type) < 0)
        FAIL_STACK_ERROR

    /* Get free-space info */
    if(H5FS_stat_info(f, f->shared->fs_man[type], &fs_stat) < 0)
        FAIL_STACK_ERROR

    /* Verify free-space info */
    if(fs_stat.tot_space < TBLOCK_SIZE3)
        TEST_ERROR

    /* Retrieve block #3 from H5FD_MEM_SUPER free-space manager */
    if(HADDR_UNDEF == (tmp_addr = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE3)))
        FAIL_STACK_ERROR
    if(tmp_addr != addr3)
        TEST_ERROR

    /* Verify that the free-space manager for H5FD_MEM_DRAW is not there */
    if(H5F_addr_defined(f->shared->fs_addr[stype]))
        TEST_ERROR

    /* Verify that the free-space manager for H5FD_MEM_BTREE is there */
    if(!H5F_addr_defined(f->shared->fs_addr[btype]))
        TEST_ERROR

    /* Start up H5FD_MEM_BTREE free-space manager */
    if(H5MF__open_fstype(f, (H5F_mem_page_t)btype) < 0)
        FAIL_STACK_ERROR

    /* Get free-space info */
    if(H5FS_stat_info(f, f->shared->fs_man[btype], &fs_stat) < 0)
        FAIL_STACK_ERROR

    /* Verify free-space info */
    if(fs_stat.tot_space < (TBLOCK_SIZE1+TBLOCK_SIZE3))
        TEST_ERROR
    if(fs_stat.serial_sect_count < 2)
        TEST_ERROR

    /* Allocate 2 blocks of type H5FD_MEM_GHEAP */
    gtype = H5FD_MEM_GHEAP;
    if(HADDR_UNDEF == (gaddr2 = H5MF_alloc(f, gtype, (hsize_t)TBLOCK_SIZE2)))
        FAIL_STACK_ERROR
    if(HADDR_UNDEF == (gaddr1 = H5MF_alloc(f, gtype, (hsize_t)TBLOCK_SIZE1)))
        FAIL_STACK_ERROR

    /* Put block #2 into H5FD_MEM_GHEAP free-space manager */
    if(H5MF_xfree(f, gtype, gaddr2, (hsize_t)TBLOCK_SIZE2) < 0)
        FAIL_STACK_ERROR

    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Re-open the file */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = (H5F_t *)H5I_object(file)))
        FAIL_STACK_ERROR

    /* If H5FD_MEM_SUPER is there, should not find block #1 & #3 */
    if(H5F_addr_defined(f->shared->fs_addr[type])) {
        /* Start up H5FD_MEM_SUPER free-space manager */
        if(H5MF__open_fstype(f, (H5F_mem_page_t)type) < 0)
            FAIL_STACK_ERROR

        if((node_found = H5FS_sect_find(f, f->shared->fs_man[type],
                         (hsize_t)TBLOCK_SIZE1, (H5FS_section_info_t **)&node)) < 0)
            FAIL_STACK_ERROR
        if(node_found) TEST_ERROR

        if((node_found = H5FS_sect_find(f, f->shared->fs_man[type],
                         (hsize_t)TBLOCK_SIZE3, (H5FS_section_info_t **)&node)) < 0)
            FAIL_STACK_ERROR
        if(node_found) TEST_ERROR
    }

    /* Verify that the H5FD_MEM_GHEAP free-space manager is there */
    if(!H5F_addr_defined(f->shared->fs_addr[gtype]))
        TEST_ERROR

    /* Start up H5FD_MEM_GHEAP free-space manager */
    if(H5MF__open_fstype(f, (H5F_mem_page_t)gtype) < 0)
        FAIL_STACK_ERROR

    /* Get free-space info */
    if(H5FS_stat_info(f, f->shared->fs_man[gtype], &fs_stat) < 0)
        FAIL_STACK_ERROR

    /* Verify free-space info */
    if(fs_stat.tot_space < TBLOCK_SIZE2)
        TEST_ERROR

    /* Closing */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR
    if(H5Pclose(fapl) < 0)
        FAIL_STACK_ERROR
    if(H5Pclose(fcpl) < 0)
        FAIL_STACK_ERROR

    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        H5Fclose(file);
        H5Pclose(fcpl);
        H5Pclose(fapl);
    } H5E_END_TRY;
    return(1);
} /* test_mf_fs_persist_multi() */
#endif /* PB_OUT */

/*
 *-------------------------------------------------------------------------
 * Verify that the file's free-space persists where there are free sections in the manager
 *-------------------------------------------------------------------------
 */
static unsigned
test_mf_fs_persist(const char *env_h5_drvr, hid_t fapl, hbool_t new_format)
{
    hid_t   file = -1;              /* File ID */
    hid_t   fcpl = -1;              /* File creation property list ID */
    hid_t   fapl2 = -1;             /* File access property list ID */
    char    filename[FILENAME_LEN]; /* Filename to use */
    H5F_t   *f = NULL;              /* Internal file object pointer */
    H5FD_mem_t  type;               /* File allocation type */
    H5FD_mem_t  tt;                 /* File allocation type */
    H5FS_stat_t fs_stat;            /* Information for free-space manager */
    haddr_t addr1, addr2, addr3, addr4, addr5, addr6; /* File address for H5FD_MEM_SUPER */
    haddr_t tmp_addr;               /* Temporary variable for address */

    if(new_format)
        TESTING("File's free-space is persistent with new library format")
    else
        TESTING("File's free-space is persistent with old library format")

    if(HDstrcmp(env_h5_drvr, "split") && HDstrcmp(env_h5_drvr, "multi")) {

        /* File creation property list template */
        if((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
            FAIL_STACK_ERROR

        /* Copy the file access property list */
        if((fapl2 = H5Pcopy(fapl)) < 0) FAIL_STACK_ERROR

        if(new_format) {
            /* Latest format */
            if(H5Pset_libver_bounds(fapl2, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
                FAIL_STACK_ERROR
            /* Set to paged aggregation and persisting free-space */
            if(H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_PAGE, TRUE, (hsize_t)1) < 0)
                TEST_ERROR
        } else {
            /* Setting: aggregation with persisting free-space */
            if(H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_FSM_AGGR, TRUE, (hsize_t)1) < 0)
                TEST_ERROR
        }

        /* Set the filename to use for this test (dependent on fapl) */
        h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

        /* Create the file to work on */
        if((file = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl2)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        /* Allocate 6 blocks */
        type = H5FD_MEM_SUPER;
        if(HADDR_UNDEF == (addr1 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE1)))
            FAIL_STACK_ERROR
        if(HADDR_UNDEF == (addr2 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE2)))
            FAIL_STACK_ERROR
        if(HADDR_UNDEF == (addr3 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE3)))
            FAIL_STACK_ERROR
        if(HADDR_UNDEF == (addr4 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE4)))
            FAIL_STACK_ERROR
        if(HADDR_UNDEF == (addr5 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE5)))
            FAIL_STACK_ERROR
        if(HADDR_UNDEF == (addr6 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE6)))
            FAIL_STACK_ERROR

        /* Put block #1, #3, #5 to H5FD_MEM_SUPER free-space manager */
        if(H5MF_xfree(f, type, addr1, (hsize_t)TBLOCK_SIZE1) < 0)
            FAIL_STACK_ERROR
        if(H5MF_xfree(f, type, addr3, (hsize_t)TBLOCK_SIZE3) < 0)
            FAIL_STACK_ERROR
        if(H5MF_xfree(f, type, addr5, (hsize_t)TBLOCK_SIZE5) < 0)
            FAIL_STACK_ERROR

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Re-open the file */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl2)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        H5MF__alloc_to_fs_type(f, type, TBLOCK_SIZE6, (H5F_mem_page_t *)&tt);

        /* Verify that H5FD_MEM_SUPER free-space manager is there */
        if(!H5F_addr_defined(f->shared->fs_addr[tt]))
            TEST_ERROR

        /* Since we are about to open a self referential free space 
         * manager prior to the first file space allocation / deallocation
         * call H5MF_tidy_self_referential_fsm_hack() first so as to avoid
         * assertion failures on the first file space alloc / dealloc.
         */
        if((f->shared->first_alloc_dealloc) &&
           (SUCCEED != 
            H5MF_tidy_self_referential_fsm_hack(f)))
            FAIL_STACK_ERROR

        /* Start up H5FD_MEM_SUPER free-space manager */
        if(!(f->shared->fs_man[tt]))
            if(H5MF__open_fstype(f, (H5F_mem_page_t)tt) < 0)
                FAIL_STACK_ERROR

        /* Get info for free-space manager */
        if(H5FS_stat_info(f, f->shared->fs_man[tt], &fs_stat) < 0)
            FAIL_STACK_ERROR

        /* Verify free-space info */
        if(fs_stat.tot_space < (TBLOCK_SIZE1+TBLOCK_SIZE3+TBLOCK_SIZE5))
            TEST_ERROR

        if(fs_stat.serial_sect_count < 3)
            TEST_ERROR

        /* Retrieve block #3 from H5FD_MEM_SUPER free-space manager */
        if(HADDR_UNDEF == (tmp_addr = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE3)))
            FAIL_STACK_ERROR
        if(tmp_addr != addr3)
            TEST_ERROR

        /* Retrieve block #1 from H5FD_MEM_SUPER free-space manager */
        if(HADDR_UNDEF == (tmp_addr = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE1)))
            FAIL_STACK_ERROR
        if(tmp_addr != addr1)
            TEST_ERROR

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Re-open the file */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        /* Verify that H5FD_MEM_SUPER free-space manager is there */
        if(!H5F_addr_defined(f->shared->fs_addr[tt]))
            TEST_ERROR

        /* Retrieve block #5 from H5FD_MEM_SUPER free-space manager */
        if(HADDR_UNDEF == (tmp_addr = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE5)))
            FAIL_STACK_ERROR
        if(tmp_addr != addr5)
            TEST_ERROR

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR
        if(H5Pclose(fcpl) < 0)
            FAIL_STACK_ERROR
        if(H5Pclose(fapl2) < 0)
            FAIL_STACK_ERROR

        PASSED()

    } else {
        SKIPPED();
        HDputs("    Current VFD doesn't support persisting free-space or paged aggregation strategy");
    }

    return(0);

error:
    H5E_BEGIN_TRY {
        H5Fclose(file);
        H5Pclose(fcpl);
        H5Pclose(fapl2);
    } H5E_END_TRY;
    return(1);
} /* test_mf_fs_persist() */

/*
 *-------------------------------------------------------------------------
 * Verify free-space are merged/shrunk away
 *-------------------------------------------------------------------------
 */
static unsigned
test_mf_fs_gone(const char *env_h5_drvr, hid_t fapl, hbool_t new_format)
{
    hid_t   file = -1;              /* File ID */
    hid_t   fcpl = -1;              /* File creation property list */
    hid_t   fapl2 = -1;             /* File access property list */
    char    filename[FILENAME_LEN]; /* Filename to use */
    H5F_t   *f = NULL;              /* Internal file object pointer */
    H5FD_mem_t  type;               /* File allocation type */
    H5FS_stat_t fs_stat;                    /* Information for free-space manager */
    haddr_t addr1, addr2, addr3, addr4;     /* File address for H5FD_MEM_SUPER */
    haddr_t addrx;
    H5FD_mem_t  fs_type;
    hbool_t contig_addr_vfd;
    hbool_t ran_H5MF_tidy_self_referential_fsm_hack = FALSE;

    if(new_format)
        TESTING("File's free-space is going away with new library format")
    else
        TESTING("File's free-space is going away with old library format")

    /* Current VFD that does not support contigous address space */
    contig_addr_vfd = (hbool_t)(HDstrcmp(env_h5_drvr, "split") && HDstrcmp(env_h5_drvr, "multi"));

    if(contig_addr_vfd) {

        /* File creation property list template */
        if((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
            FAIL_STACK_ERROR

        /* Copy the file access property list */
        if((fapl2 = H5Pcopy(fapl)) < 0) FAIL_STACK_ERROR

        if(new_format)
            if(H5Pset_libver_bounds(fapl2, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
                FAIL_STACK_ERROR

        /* Set to aggregation and persisting free-space */
        if(H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_FSM_AGGR, TRUE, (hsize_t)1) < 0)
            FAIL_STACK_ERROR

        /* Set the filename to use for this test (dependent on fapl) */
        h5_fixname(FILENAME[0], fapl2, filename, sizeof(filename));

        /* Create the file to work on */
        if((file = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl2)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        /* Allocate 4 blocks */
        type = H5FD_MEM_SUPER;
        if(HADDR_UNDEF == (addr1 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE1)))
            FAIL_STACK_ERROR
        if(HADDR_UNDEF == (addr2 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE2)))
            FAIL_STACK_ERROR
        if(HADDR_UNDEF == (addr3 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE3)))
            FAIL_STACK_ERROR
        if(HADDR_UNDEF == (addr4 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE4)))
            FAIL_STACK_ERROR

        /* Put block #1, #3 to H5FD_MEM_SUPER free-space manager */
        if(H5MF_xfree(f, type, addr1, (hsize_t)TBLOCK_SIZE1) < 0)
            FAIL_STACK_ERROR
        if(H5MF_xfree(f, type, addr3, (hsize_t)TBLOCK_SIZE3) < 0)
            FAIL_STACK_ERROR

        /* Retrieve block #1, #3 from H5FD_MEM_SUPER free-space manager */
        if(HADDR_UNDEF == (addr3 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE3)))
            FAIL_STACK_ERROR
        if(HADDR_UNDEF == (addr1 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE1)))
            FAIL_STACK_ERROR

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Re-open the file */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl2)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        H5MF__alloc_to_fs_type(f, type, TBLOCK_SIZE4, (H5F_mem_page_t *)&fs_type);

        /* Verify that the H5FD_MEM_SUPER free-space manager is not there */
        if(H5F_addr_defined(f->shared->fs_addr[fs_type]))
            TEST_ERROR

        /* Put block #3 to H5FD_MEM_SUPER free-space manager */
        if(H5MF_xfree(f, type, addr3, (hsize_t)TBLOCK_SIZE3) < 0)
            FAIL_STACK_ERROR

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Re-open the file */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl2)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        /* Verify that H5FD_MEM_SUPER free-space manager is there */
        if(!H5F_addr_defined(f->shared->fs_addr[fs_type]))
            TEST_ERROR

        /* Since we are about to open a self referential free space 
         * manager prior to the first file space allocation / deallocation
         * call H5MF_tidy_self_referential_fsm_hack() first so as to avoid
         * assertion failures on the first file space alloc / dealloc.
         */
         if(f->shared->first_alloc_dealloc){
             if(SUCCEED!=H5MF_tidy_self_referential_fsm_hack(f))
                FAIL_STACK_ERROR
            ran_H5MF_tidy_self_referential_fsm_hack = TRUE;
        }

        /* Start up H5FD_MEM_SUPER free-space manager */
        if(!(f->shared->fs_man[fs_type]))
            if(H5MF__open_fstype(f, (H5F_mem_page_t)fs_type) < 0)
                FAIL_STACK_ERROR

        /* Get info for H5FD_MEM_SUPER free-space manager */
        if(H5FS_stat_info(f, f->shared->fs_man[fs_type], &fs_stat) < 0)
            FAIL_STACK_ERROR

        /* if we ran H5MF_tidy_self_referential_fsm_hack(), the 
         * H5FD_MEM_SUPER free space manager must be floating.
         * Thus fs_stat.addr must be undefined.
         */
        if((!ran_H5MF_tidy_self_referential_fsm_hack) &&
           (!H5F_addr_defined(fs_stat.addr)))
            TEST_ERROR

        if(fs_stat.tot_space < TBLOCK_SIZE3)
            TEST_ERROR

        /* Put block #4 to H5FD_MEM_SUPER free-space manager */
        if(H5MF_xfree(f, type, addr4, (hsize_t)TBLOCK_SIZE4) < 0)
            FAIL_STACK_ERROR

        if(!new_format) {
            /* Need to take up this space so that the free-space manager will go away */
            if(HADDR_UNDEF == (addrx = H5MF_alloc(f, type, (hsize_t)103)))
                FAIL_STACK_ERROR
        }

        /* The H5FD_MEM_SUPER free-space manager will go away at H5MF_close() */
        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Re-open the file */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl2)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR
        /* Verify that the H5FD_MEM_SUPER free-space manager is not there */
        if(H5F_addr_defined(f->shared->fs_addr[fs_type]))
            TEST_ERROR

        /* Closing */
        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR
        if(H5Pclose(fcpl) < 0)
            FAIL_STACK_ERROR
        if(H5Pclose(fapl2) < 0)
            FAIL_STACK_ERROR

        PASSED()

    } else {
        SKIPPED();
        HDputs("    Current VFD doesn't support persistent free-space manager");
    }

    return(0);

error:
    H5E_BEGIN_TRY {
        H5Fclose(file);
        H5Pclose(fcpl);
        H5Pclose(fapl2);
    } H5E_END_TRY;
    return(1);
} /* test_mf_fs_gone() */

/*
 *-------------------------------------------------------------------------
 * Verify that free-space persist with combinations of
 * file space strategy and free space threshold as specified.
 *-------------------------------------------------------------------------
 */
static unsigned
test_mf_strat_thres_persist(const char *env_h5_drvr, hid_t fapl, hbool_t new_format)
{
    hid_t   file = -1;              /* File ID */
    hid_t   fcpl = -1;              /* File creation property list template */
    hid_t   fapl2 = -1;             /* File access property list template */
    char    filename[FILENAME_LEN]; /* Filename to use */
    H5F_t   *f = NULL;              /* Internal file object pointer */
    H5FD_mem_t  type;               /* File allocation type */
    H5FD_mem_t  tt;                 /* File allocation type */
    haddr_t addr1, addr2, addr3, addr4, addr5, addr6; /* File address for H5FD_MEM_SUPER */
    H5F_fspace_strategy_t fs_type;  /* File space handling strategy */
    hsize_t fs_threshold;           /* Free-space section threshold */
    unsigned    fs_persist;         /* To persist free-space or not */
    hbool_t contig_addr_vfd;

    if(new_format)
        TESTING("File space strategy/persisting/threshold with new library format")
    else
        TESTING("File space strategy/persisting/threshold with old library format")

    /* Current VFD that does not support contigous address space */
    contig_addr_vfd = (hbool_t)(HDstrcmp(env_h5_drvr, "split") && HDstrcmp(env_h5_drvr, "multi"));


    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Copy the file access property list */
    if((fapl2 = H5Pcopy(fapl)) < 0)
         FAIL_STACK_ERROR

    if(new_format)
        if(H5Pset_libver_bounds(fapl2, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
            FAIL_STACK_ERROR

    /* Test with TRUE or FALSE for persisting free-space */
    for(fs_persist = FALSE; fs_persist <= TRUE; fs_persist++) {

        for(fs_threshold = 0; fs_threshold <= TEST_THRESHOLD10; fs_threshold++) {

            /* Testing for H5F_FSPACE_STRATEGY_FSM_AGGR and H5F_FSPACE_STRATEGY_PAGE strategies only */
            for(fs_type = H5F_FSPACE_STRATEGY_FSM_AGGR; fs_type < H5F_FSPACE_STRATEGY_AGGR; H5_INC_ENUM(H5F_fspace_strategy_t, fs_type)) {

                if(!contig_addr_vfd && (fs_persist || fs_type == H5F_FSPACE_STRATEGY_PAGE))
                    continue;

                /* Create file-creation template */
                if((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
                    FAIL_STACK_ERROR

                /* Set default file space information */
                if(H5Pset_file_space_strategy(fcpl, fs_type, (hbool_t)fs_persist, fs_threshold) < 0)
                    FAIL_STACK_ERROR

                /* Create the file to work on */
                if((file = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl2)) < 0)
                    FAIL_STACK_ERROR

                /* Get a pointer to the internal file object */
                if(NULL == (f = (H5F_t *)H5I_object(file)))
                    FAIL_STACK_ERROR

                /* Allocate 6 blocks */
                type = H5FD_MEM_SUPER;
                if(HADDR_UNDEF == (addr1 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE1)))
                    FAIL_STACK_ERROR
                if(HADDR_UNDEF == (addr2 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE2)))
                    FAIL_STACK_ERROR
                if(HADDR_UNDEF == (addr3 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE3)))
                    FAIL_STACK_ERROR
                if(HADDR_UNDEF == (addr4 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE4)))
                    FAIL_STACK_ERROR
                if(HADDR_UNDEF == (addr5 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE5)))
                    FAIL_STACK_ERROR
                if(HADDR_UNDEF == (addr6 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE6)))
                    FAIL_STACK_ERROR

                /* Put block #1, #3, #5 to H5FD_MEM_SUPER free-space manager */
                if(H5MF_xfree(f, type, addr1, (hsize_t)TBLOCK_SIZE1) < 0)
                    FAIL_STACK_ERROR
                if(H5MF_xfree(f, type, addr3, (hsize_t)TBLOCK_SIZE3) < 0)
                    FAIL_STACK_ERROR
                if(H5MF_xfree(f, type, addr5, (hsize_t)TBLOCK_SIZE5) < 0)
                    FAIL_STACK_ERROR


                /* Close the file */
                if(H5Fclose(file) < 0)
                    FAIL_STACK_ERROR

                /* Re-open the file */
                if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl2)) < 0)
                    FAIL_STACK_ERROR

                /* Get a pointer to the internal file object */
                if(NULL == (f = (H5F_t *)H5I_object(file)))
                    FAIL_STACK_ERROR

                H5MF__alloc_to_fs_type(f, type, TBLOCK_SIZE6, (H5F_mem_page_t *)&tt);

                /* Get a pointer to the internal file object */
                if(NULL == (f = (H5F_t *)H5I_object(file)))
                    FAIL_STACK_ERROR

                if(f->shared->fs_persist) {
                    hssize_t nsects;            /* # of free-space sections */
                    int      i;                 /* local index variable */
                    H5F_sect_info_t *sect_info; /* array to hold the free-space information */

                    /* Get the # of free-space sections in the file */
                    if((nsects = H5Fget_free_sections(file, H5FD_MEM_DEFAULT, (size_t)0, NULL)) < 0)
                        FAIL_STACK_ERROR

                    /* Verify no free-space sections */
                    /* paged aggregation has 1 section for last_small */
                    if(fs_threshold > TBLOCK_SIZE5 && nsects && fs_type != H5F_FSPACE_STRATEGY_PAGE)
                        TEST_ERROR

                    if(nsects) {
                        /* Allocate storage for the free space section information */
                        sect_info = (H5F_sect_info_t *)HDcalloc((size_t)nsects, sizeof(H5F_sect_info_t));

                        H5Fget_free_sections(file, H5FD_MEM_DEFAULT, (size_t)nsects, sect_info);

                        /* Verify the size of free-space sections */
                        for(i = 0; i < nsects; i++)
                            if(sect_info[i].size < fs_threshold)
                                TEST_ERROR
                        if(sect_info)
                            HDfree(sect_info);
                    }
                } else {
                    if(H5F_addr_defined(f->shared->fs_addr[tt]))
                        TEST_ERROR
                }

                /* Closing */
                if(H5Fclose(file) < 0)
                    FAIL_STACK_ERROR
                if(H5Pclose(fcpl) < 0)
                    FAIL_STACK_ERROR
            } /* end for fs_type */
        } /* end for fs_threshold */
    } /* end for fs_persist */

    if(H5Pclose(fapl2) < 0)
        FAIL_STACK_ERROR

    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        H5Pclose(fcpl);
        H5Pclose(fapl2);
        H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_mf_strat_thres_persist() */

/*
 *-------------------------------------------------------------------------
 * Verify free-space are merged/shrunk away with file space settings:
 *  --strategy, persist/not persist file space
 *-------------------------------------------------------------------------
 */
static unsigned
test_mf_strat_thres_gone(const char *env_h5_drvr, hid_t fapl, hbool_t new_format)
{
    hid_t   file = -1;              /* File ID */
    hid_t   fcpl = -1;              /* File creation property list template */
    hid_t   fapl2 = -1;             /* File access property list template */
    char    filename[FILENAME_LEN]; /* Filename to use */
    H5F_t   *f = NULL;              /* Internal file object pointer */
    H5FD_mem_t  type;               /* File allocation type */
    H5FD_mem_t  tt;                 /* File allocation type */
    haddr_t addr1, addr2, addr3, addr4, addr5, addr6; /* File address for H5FD_MEM_SUPER */
    H5F_fspace_strategy_t fs_type;  /* File space handling strategy */
    unsigned    fs_persist;         /* To persist free-space or not */
    H5FS_stat_t fs_state;           /* Information for free-space manager */
    H5FS_stat_t fs_state_zero;      /* Information for free-space manager */
    hbool_t contig_addr_vfd;

    if(new_format)
        TESTING("File space merge/shrink for section size < threshold with new library format")
    else
        TESTING("File space merge/shrink for section size < threshold with old library format")

    /* Current VFD that does not support contigous address space */
    contig_addr_vfd = (hbool_t)(HDstrcmp(env_h5_drvr, "split") && HDstrcmp(env_h5_drvr, "multi"));


    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    HDmemset(&fs_state_zero, 0, sizeof(H5FS_stat_t));

    /* Copy the file access property list */
    if((fapl2 = H5Pcopy(fapl)) < 0)
        FAIL_STACK_ERROR

    if(new_format)
        if(H5Pset_libver_bounds(fapl2, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
            FAIL_STACK_ERROR

    /* Test with TRUE or FALSE for persisting free-space */
    for(fs_persist = FALSE; fs_persist <= TRUE; fs_persist++) {
        /* Testing for H5F_FSPACE_STRATEGY_FSM_AGGR and H5F_FSPACE_STRATEGY_PAGE strategies only */
        for(fs_type = H5F_FSPACE_STRATEGY_FSM_AGGR; fs_type < H5F_FSPACE_STRATEGY_AGGR; H5_INC_ENUM(H5F_fspace_strategy_t, fs_type)) {

            /* Skip for multi/split driver: persisting free-space or paged aggregation strategy */
            if(!contig_addr_vfd && (fs_persist || fs_type == H5F_FSPACE_STRATEGY_PAGE))
                continue;

            /* Clear out free-space statistics */
            HDmemset(&fs_state, 0, sizeof(H5FS_stat_t));

            /* Create file-creation template */
            if((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
                FAIL_STACK_ERROR

            /* Set default file space information */
            if(H5Pset_file_space_strategy(fcpl, fs_type, fs_persist, (hsize_t)TEST_THRESHOLD3) < 0)
                FAIL_STACK_ERROR

            /* Create the file to work on */
            if((file = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl2)) < 0)
                FAIL_STACK_ERROR

            /* Get a pointer to the internal file object */
            if(NULL == (f = (H5F_t *)H5I_object(file)))
                FAIL_STACK_ERROR

            /* Allocate 6 blocks */
            type = H5FD_MEM_SUPER;
            if(HADDR_UNDEF == (addr1 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE1)))
                FAIL_STACK_ERROR
            if(HADDR_UNDEF == (addr2 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE2)))
                FAIL_STACK_ERROR
            if(HADDR_UNDEF == (addr3 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE3)))
                FAIL_STACK_ERROR
            if(HADDR_UNDEF == (addr4 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE4)))
                FAIL_STACK_ERROR
            if(HADDR_UNDEF == (addr5 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE5)))
                FAIL_STACK_ERROR
            if(HADDR_UNDEF == (addr6 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE6)))
                FAIL_STACK_ERROR

            H5MF__alloc_to_fs_type(f, type, TBLOCK_SIZE6, (H5F_mem_page_t *)&tt);

            /* For paged aggregation, the section in the page at EOF for small meta fs is not shrunk away */
            if(fs_type == H5F_FSPACE_STRATEGY_PAGE) {
                if(H5FS_stat_info(f, f->shared->fs_man[tt], &fs_state) < 0)
                    FAIL_STACK_ERROR
            }

            /* Put block #3, #5 to H5FD_MEM_SUPER free-space manager */
            if(H5MF_xfree(f, type, addr3, (hsize_t)TBLOCK_SIZE3) < 0)
                FAIL_STACK_ERROR
            if(H5MF_xfree(f, type, addr5, (hsize_t)TBLOCK_SIZE5) < 0)
                FAIL_STACK_ERROR

            fs_state.tot_space += TBLOCK_SIZE3 + TBLOCK_SIZE5;
            fs_state.tot_sect_count += 2;
            fs_state.serial_sect_count += 2;

            if(check_stats(f, f->shared->fs_man[tt], &fs_state))
                TEST_ERROR

            /* section #2 is less than threshold but is merged into section #3 */
            if(H5MF_xfree(f, type, addr2, (hsize_t)TBLOCK_SIZE2) < 0)
                FAIL_STACK_ERROR

            fs_state.tot_space += TBLOCK_SIZE2;
            if(check_stats(f, f->shared->fs_man[tt], &fs_state))
                TEST_ERROR

            if(H5MF_xfree(f, type, addr4, (hsize_t)TBLOCK_SIZE4) < 0)
                FAIL_STACK_ERROR

            if(H5MF_xfree(f, type, addr6, (hsize_t)TBLOCK_SIZE6) < 0)
                FAIL_STACK_ERROR

            /* For paged aggregation, the sections in the page at EOF for small meta fs are merged but are not shrunk away */
            if(fs_type == H5F_FSPACE_STRATEGY_PAGE) {
                fs_state.tot_sect_count = fs_state.serial_sect_count = 1;
                fs_state.tot_space += (TBLOCK_SIZE4 + TBLOCK_SIZE6);
            }

            /* For old format: the sections at EOF are shrunk away */
            if(check_stats(f, f->shared->fs_man[tt], (fs_type == H5F_FSPACE_STRATEGY_PAGE) ? &fs_state:&fs_state_zero))
                TEST_ERROR

            /* section #1 is less than threshold but is shrunk away */
            if(H5MF_xfree(f, type, addr1, (hsize_t)TBLOCK_SIZE1) < 0)
                FAIL_STACK_ERROR

            /* For paged aggregation, the section in the page at EOF for small meta fs is not shrunk away */
            if(fs_type == H5F_FSPACE_STRATEGY_PAGE)
                fs_state.tot_space += TBLOCK_SIZE1;

            /* For old format: the sections at EOF are shrunk away */
            if(check_stats(f, f->shared->fs_man[tt], (fs_type == H5F_FSPACE_STRATEGY_PAGE) ? &fs_state : &fs_state_zero))
                TEST_ERROR

            if(H5Fclose(file) < 0)
                FAIL_STACK_ERROR

            /* Re-open the file */
            if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl2)) < 0)
                FAIL_STACK_ERROR

            /* Get a pointer to the internal file object */
            if(NULL == (f = (H5F_t *)H5I_object(file)))
                FAIL_STACK_ERROR

            /* Free-space manager should be empty */
            if(!(fs_type == H5F_FSPACE_STRATEGY_PAGE && fs_persist))
                if(H5F_addr_defined(f->shared->fs_addr[tt]))
                    TEST_ERROR

            /* Closing */
            if(H5Fclose(file) < 0)
                FAIL_STACK_ERROR

            if(H5Pclose(fcpl) < 0)
                FAIL_STACK_ERROR
        } /* end for fs_type */
    } /* end for fs_persist */

    if(H5Pclose(fapl2) < 0)
        FAIL_STACK_ERROR

    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        H5Pclose(fcpl);
        H5Pclose(fapl2);
        H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_mf_strat_thres_gone() */

/*
 *-------------------------------------------------------------------------
 * To verify that file space is allocated from the corresponding free-space manager
 * because H5FD_FLMAP_DICHOTOMY is used as the default free-list mapping.
 *
 * (1) Allocate the first block (size 30) of type H5FD_MEM_SUPER
 * (2) Allocate the second block (size 50) of type H5FD_MEM_SUPER
 *
 * (3) Allocate the first block (size 30) of type H5FD_MEM_DRAW
 *
 * (4) Free the first block (size 30) of type H5FD_MEM_SUPER
 *
 * (5) Allocate the second block (size 30) of type H5FD_MEM_DRAW
 * (6) Verify that this second block is not the freed block from (3)
 *
 * (7) Allocate the second block (size 30) of type H5FD_MEM_DRAW
 * (8) Free the first block (size 30) of type H5FD_MEM_DRAW
 *
 * (9) Allocate the third block (size 30) of type H5FD_MEM_SUPER
 * (10) Verify that this third block is not freed block from (8)
 *-------------------------------------------------------------------------
 */
static unsigned
test_dichotomy(hid_t fapl)
{
    hid_t        file = -1;              /* File ID */
    char        filename[FILENAME_LEN]; /* Filename to use */
    H5F_t        *f = NULL;              /* Internal file object pointer */
    H5FD_mem_t         type, stype;
    haddr_t        addr1, addr3, saddr1, saddr2;

    TESTING("Allocation from raw or metadata free-space manager");

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = (H5F_t *)H5I_object(file)))
        FAIL_STACK_ERROR

    /* Allocate the first block of type H5FD_MEM_SUPER */
    type = H5FD_MEM_SUPER;
    addr1 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE30);

    /* Allocate the second block of type H5FD_MEM_SUPER */
    H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE50);

    /* Allocate the first block of type H5FD_MEM_DRAW */
    stype = H5FD_MEM_DRAW;
    saddr1 = H5MF_alloc(f, stype, (hsize_t)TBLOCK_SIZE30);

    /* Free the first block of type H5FD_MEM_SUPER */
    H5MF_xfree(f, type, addr1, (hsize_t)TBLOCK_SIZE30);

    /* Allocate the second block of type H5FD_MEM_DRAW */
    saddr2 = H5MF_alloc(f, stype, (hsize_t)TBLOCK_SIZE30);

    /* Verify that saddr1 is not addr1 */
    if(saddr2 == addr1) TEST_ERROR

    /* Free the first block of type H5FD_MEM_DRAW */
    H5MF_xfree(f, stype, saddr1, (hsize_t)TBLOCK_SIZE30);

    /* Allocate the third block of type H5FD_MEM_SUPER */
    addr3 = H5MF_alloc(f, type, (hsize_t)TBLOCK_SIZE30);

    /* Verify that addr3 is not saddr1 */
    if(addr3 == saddr1) TEST_ERROR

    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_dichotomy() */

/*
 *-------------------------------------------------------------------------
 *  set_multi_split():
 *      Internal routine to set up page-aligned address space for multi/split driver
 *      when testing paged aggregation.
 *-------------------------------------------------------------------------
 */
static int
set_multi_split(hid_t fapl, hsize_t pagesize, hbool_t multi, hbool_t split)
{
    H5FD_mem_t memb_map[H5FD_MEM_NTYPES];
    hid_t memb_fapl_arr[H5FD_MEM_NTYPES];
    char *memb_name[H5FD_MEM_NTYPES];
    haddr_t memb_addr[H5FD_MEM_NTYPES];
    hbool_t relax;
    H5FD_mem_t  mt;

    HDassert(multi || split);

    HDmemset(memb_name, 0, sizeof memb_name);

    /* Get current split settings */
    if(H5Pget_fapl_multi(fapl, memb_map, memb_fapl_arr, memb_name, memb_addr, &relax) < 0)
        TEST_ERROR

    if(split) {
        /* Set memb_addr aligned */
        memb_addr[H5FD_MEM_SUPER] = ((memb_addr[H5FD_MEM_SUPER] + pagesize - 1) / pagesize) * pagesize;
        memb_addr[H5FD_MEM_DRAW] = ((memb_addr[H5FD_MEM_DRAW] + pagesize - 1) / pagesize) * pagesize;
    } else {
        /* Set memb_addr aligned */
        for(mt = H5FD_MEM_DEFAULT; mt < H5FD_MEM_NTYPES; H5_INC_ENUM(H5FD_mem_t, mt))
            memb_addr[mt] = ((memb_addr[mt] + pagesize - 1) / pagesize) * pagesize;
    } /* end else */

    /* Set multi driver with new FAPLs */
    if(H5Pset_fapl_multi(fapl, memb_map, memb_fapl_arr, (const char * const *)memb_name, memb_addr, relax) < 0)
        TEST_ERROR

    /* Free memb_name */
    for(mt = H5FD_MEM_DEFAULT; mt < H5FD_MEM_NTYPES; H5_INC_ENUM(H5FD_mem_t, mt))
        free(memb_name[mt]);

    return 0;

error:
    return(-1);
} /* set_multi_split() */

/*-------------------------------------------------------------------------
 * Function:    test_page_alloc_xfree
 *
 * Purpose:     To verify allocations and de-allocations for large/small
 *              sections are done properly when paged aggregation is enabled.
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Vailin Choi; Jan 2013
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_page_alloc_xfree(const char *env_h5_drvr, hid_t fapl)
{

    hid_t fid = -1;             /* File ID */
    hid_t fcpl = -1;            /* File creation property list */
    hid_t fapl_new = -1;        /* File access property list ID */
    H5F_t *f = NULL;            /* Internal file object pointer */
    haddr_t addr2, addr3;       /* Addresses for small metadata blocks */
    haddr_t saddr1;             /* Addresses for small raw data blocks */
    haddr_t gaddr1;             /* Addresses for large data blocks */
    hbool_t split = FALSE, multi = FALSE;
    char filename[FILENAME_LEN];    /* Filename to use */
    haddr_t found_addr;         /* Address of the found section */
    unsigned fs_persist;        /* To persist free-space or not */

    TESTING("Paged aggregation for file space: H5MF_alloc/H5MF_xfree");

    /* Check for split or multi driver */
    if(!HDstrcmp(env_h5_drvr, "split"))
        split = TRUE;
    else if(!HDstrcmp(env_h5_drvr, "multi"))
        multi = TRUE;

    if(!multi && !split) {

        /* Set the filename to use for this test (dependent on fapl) */
        h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

        if((fapl_new = H5Pcopy(fapl)) < 0) TEST_ERROR

        if(multi || split)
            if(set_multi_split(fapl_new, 4096, multi, split) <  0)
                TEST_ERROR;

        /* Test with TRUE or FALSE for persisting free-space */
        for(fs_persist = FALSE; fs_persist <= TRUE; fs_persist++) {
            H5F_mem_page_t fs_type;

            /* File creation property list */
            if((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
                TEST_ERROR

            /* Set the strategy to paged aggregation */
            if(H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_PAGE, fs_persist, (hsize_t)1) < 0)
                TEST_ERROR

            /* Create the file to work on */
            if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl_new)) < 0)
                TEST_ERROR

            /* Get a pointer to the internal file object */
            if(NULL == (f = (H5F_t *)H5I_object(fid)))
                TEST_ERROR

            /* Allocate 3 small metadata blocks: addr1, addr2, addr3 */
            H5MF_alloc(f, H5FD_MEM_OHDR, (hsize_t)TBLOCK_SIZE30);
            addr2 = H5MF_alloc(f, H5FD_MEM_OHDR, (hsize_t)TBLOCK_SIZE1034);
            addr3 = H5MF_alloc(f, H5FD_MEM_OHDR, (hsize_t)TBLOCK_SIZE50);

            /* Free the block with addr2 */
            H5MF_xfree(f, H5FD_MEM_OHDR, addr2, (hsize_t)TBLOCK_SIZE1034);

            if(!fs_persist) {

                H5MF__alloc_to_fs_type(f, H5FD_MEM_OHDR, TBLOCK_SIZE1034, (H5F_mem_page_t *)&fs_type);

                /* Verify that the freed block with addr2 is found from the small metadata manager */
                if(H5MF__find_sect(f, H5FD_MEM_OHDR, (hsize_t)TBLOCK_SIZE1034, f->shared->fs_man[fs_type], &found_addr) < 0)
                    TEST_ERROR
                if(found_addr != addr2)
                    TEST_ERROR
            } /* end if */

            /* Allocate 2 small raw data blocks: saddr1, saddr2 */
            saddr1 = H5MF_alloc(f, H5FD_MEM_DRAW, (hsize_t)TBLOCK_SIZE30);
            H5MF_alloc(f, H5FD_MEM_DRAW, (hsize_t)TBLOCK_SIZE1034);

            /* Free the block with saddr1 */
            H5MF_xfree(f, H5FD_MEM_DRAW, saddr1, (hsize_t)TBLOCK_SIZE30);

            if(!fs_persist) {
                /* Verify that the freed block with saddr1 is found from the small raw data manager */
                if(H5MF__find_sect(f, H5FD_MEM_DRAW, (hsize_t)TBLOCK_SIZE30, f->shared->fs_man[H5F_MEM_PAGE_DRAW], &found_addr) < 0)
                    TEST_ERROR
                if(found_addr != saddr1)
                    TEST_ERROR
            } /* end if */

            /* Allocate 2 large data blocks: gaddr1, gaddr2 */
            gaddr1 = H5MF_alloc(f, H5FD_MEM_DRAW, (hsize_t)TBLOCK_SIZE5000);
            H5MF_alloc(f, H5FD_MEM_DRAW, (hsize_t)TBLOCK_SIZE8000);

            /* Free the block with gaddr1 */
            H5MF_xfree(f, H5FD_MEM_DRAW, gaddr1, (hsize_t)TBLOCK_SIZE5000);

            if(!fs_persist) {

                H5MF__alloc_to_fs_type(f, H5FD_MEM_DRAW, TBLOCK_SIZE5000, (H5F_mem_page_t *)&fs_type);

                /* Verify that the freed block with gaddr1 is found from the large data manager */
                if(H5MF__find_sect(f, H5FD_MEM_DRAW, (hsize_t)TBLOCK_SIZE8192, f->shared->fs_man[fs_type], &found_addr) < 0)
                    TEST_ERROR
                if(found_addr != gaddr1)
                    TEST_ERROR
            } /* end if */

            /* Close file */
            if(H5Fclose(fid) < 0)
                TEST_ERROR

            /* Close the property list */
            if(H5Pclose(fcpl) < 0)
                TEST_ERROR

            if(fs_persist) {
                /* Re-open the file */
                if((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl_new)) < 0)
                    TEST_ERROR

                /* Get a pointer to the internal file object */
                if(NULL == (f = (H5F_t *)H5I_object(fid)))
                    TEST_ERROR

                /* Verify that the large generic manager is there */
                H5MF__alloc_to_fs_type(f, H5FD_MEM_DRAW, TBLOCK_SIZE5000, (H5F_mem_page_t *)&fs_type);
                if(!H5F_addr_defined(f->shared->fs_addr[fs_type]))
                    TEST_ERROR

                /* Verify that the small metadata manager is there */
                H5MF__alloc_to_fs_type(f, H5FD_MEM_OHDR, f->shared->fs_page_size - 1, (H5F_mem_page_t *)&fs_type);
                if(!H5F_addr_defined(f->shared->fs_addr[fs_type]))
                    TEST_ERROR

                  /* Since we are about to open a self referential free space
                   * manager prior to the first file space allocation / deallocation
                   * call H5MF_tidy_self_referential_fsm_hack() first so as to avoid
                   * assertion failures on the first file space alloc / dealloc.
                   */
                if(f->shared->first_alloc_dealloc){
                    if(SUCCEED!=H5MF_tidy_self_referential_fsm_hack(f))
                        FAIL_STACK_ERROR
                }

                /* Set up to use the small meta data manager */
                if(!(f->shared->fs_man[fs_type]))
                    if(H5MF__open_fstype(f, fs_type) < 0)
                        TEST_ERROR

                /* Verify that the freed block with addr2 is found from the small metadata manager */
                if(H5MF__find_sect(f, H5FD_MEM_OHDR, (hsize_t)(f->shared->fs_page_size-(addr3+TBLOCK_SIZE50)), f->shared->fs_man[fs_type], &found_addr) < 0)
                    TEST_ERROR

                if(found_addr != (addr3+TBLOCK_SIZE50))
                    TEST_ERROR

                /* Verify that the small raw data manager is there */
                if(!H5F_addr_defined(f->shared->fs_addr[H5F_MEM_PAGE_DRAW]))
                    TEST_ERROR

                /* Set up to use the small raw data manager */
                if(!(f->shared->fs_man[H5F_MEM_PAGE_DRAW]))
                    if(H5MF__open_fstype(f, H5F_MEM_PAGE_DRAW) < 0)
                        TEST_ERROR

                /* Verify that the freed block with saddr1 is found from the small raw data manager */
                if(H5MF__find_sect(f, H5FD_MEM_DRAW, (hsize_t)TBLOCK_SIZE30, f->shared->fs_man[H5F_MEM_PAGE_DRAW], &found_addr) < 0)
                    TEST_ERROR
                if(found_addr != saddr1)
                    TEST_ERROR

                H5MF__alloc_to_fs_type(f, H5FD_MEM_DRAW, TBLOCK_SIZE5000, (H5F_mem_page_t *)&fs_type);

                if(!(f->shared->fs_man[fs_type]))
                    /* Set up to use the large data manager */
                    if(H5MF__open_fstype(f, fs_type) < 0)
                        TEST_ERROR

                /* Verify that the freed block with gaddr1 is found from the large data manager */
                if(H5MF__find_sect(f, H5FD_MEM_DRAW, (hsize_t)TBLOCK_SIZE8192, f->shared->fs_man[fs_type], &found_addr) < 0)
                    TEST_ERROR
                if(found_addr != gaddr1)
                    TEST_ERROR

                /* Close file */
                if(H5Fclose(fid) < 0)
                    TEST_ERROR
            } /* end if fs_persist */
        } /* end for */

        if(H5Pclose(fapl_new) < 0)
            TEST_ERROR

        PASSED()

    } else {
        SKIPPED();
        HDputs("    Current VFD doesn't support persisting free-space or paged aggregation strategy");
    }

    return(0);

error:
    H5E_BEGIN_TRY {
        H5Fclose(fid);
        H5Pclose(fcpl);
        H5Pclose(fapl_new);
    } H5E_END_TRY;
    return(1);

} /* test_page_alloc_xfree() */

/*-------------------------------------------------------------------------
 * Function:    test_page_try_shrink
 *
 * Purpose:     To verify that shrinking via H5MF_try_shrink() work properly
 *              when paged aggregation is enabled.
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Vailin Choi; Jan 2013
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_page_try_shrink(const char *env_h5_drvr, hid_t fapl)
{

    hid_t fid = -1;                 /* File ID */
    hid_t fcpl = -1;                /* File creation property list */
    H5F_t *f = NULL;                /* Internal file object pointer */
    haddr_t addr1;                  /* Address for small metadata block */
    haddr_t saddr1;                 /* Address for small raw data block */
    haddr_t gaddr1;                 /* Address for large data block */
    hbool_t contig_addr_vfd;        /* Whether VFD used has a contigous address space */
    htri_t status;                  /* status from shrinking */
    h5_stat_size_t file_size;       /* File size */
    char filename[FILENAME_LEN];    /* Filename to use */

    TESTING("Paged aggregation for file space: H5MF_try_shrink()");

    /* Current VFD that does not support continuous address space */
    contig_addr_vfd = (hbool_t)(HDstrcmp(env_h5_drvr, "split") && HDstrcmp(env_h5_drvr, "multi") );

    if(contig_addr_vfd) {

        /* Set the filename to use for this test (dependent on fapl) */
        h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

        /* File creation property list */
        if((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
            FAIL_STACK_ERROR

        /* Set the strategy to paged aggregation */
        if(H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_PAGE, FALSE, (hsize_t)1) < 0)
            FAIL_STACK_ERROR

        /* Create the file to work on */
        if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(fid)))
            FAIL_STACK_ERROR

        /* Allocate a small metadata block with addr1 */
        addr1 = H5MF_alloc(f, H5FD_MEM_OHDR, (hsize_t)TBLOCK_SIZE50);

        /* Try to shrink the block with addr1 */
        if((status = H5MF_try_shrink(f, H5FD_MEM_OHDR, addr1, (hsize_t)TBLOCK_SIZE50)) < 0)
            FAIL_STACK_ERROR

        /* Couldn't shrink due to the section (remaining space in the page) is in the small metadata free-space manager */
        if(status == TRUE) TEST_ERROR

        /* Allocate a small raw data block with saddr1 */
        saddr1 = H5MF_alloc(f, H5FD_MEM_DRAW, (hsize_t)TBLOCK_SIZE50);

        /* Try to shrink the block with saddr1 */
        if((status = H5MF_try_shrink(f, H5FD_MEM_DRAW, saddr1, (hsize_t)TBLOCK_SIZE50)) < 0)
            FAIL_STACK_ERROR

        /* Couldn't shrink due to the section (remaining space in the page) is in the small raw data free-space manager */
        if(status == TRUE) TEST_ERROR

        /* Allocate a large data block with gaddr1 */
        gaddr1 = H5MF_alloc(f, H5FD_MEM_DRAW, (hsize_t)TBLOCK_SIZE5000);

        /* Try to shrink the block with gaddr1 */
        if((status = H5MF_try_shrink(f, H5FD_MEM_DRAW, gaddr1, (hsize_t)TBLOCK_SIZE5000)) < 0)
            FAIL_STACK_ERROR

        /* Couldn't shrink due to the section (remaining space in the page) is in the large-sized free-space manager */
        if(status == TRUE) TEST_ERROR

        /* Free the block with saddr1--merge to become 1 page, then return to the large manager */
        H5MF_xfree(f, H5FD_MEM_DRAW, saddr1, (hsize_t)TBLOCK_SIZE50);

        /* Merge all 3 sections and shrunk */
        H5MF_xfree(f, H5FD_MEM_OHDR, gaddr1, (hsize_t)TBLOCK_SIZE5000);

        if(H5Fclose(fid) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Should be on page boundary */
        if(file_size % TBLOCK_SIZE4096)
            TEST_ERROR

        /* Close the property list */
        if(H5Pclose(fcpl) < 0)
            FAIL_STACK_ERROR

        PASSED()

    } else {
        SKIPPED();
        HDputs("    Current VFD doesn't support paged aggregation");
    }

    return(0);

error:
    H5E_BEGIN_TRY {
        H5Fclose(fid);
        H5Pclose(fcpl);
    } H5E_END_TRY;
    return(1);

} /* test_page_try_shrink() */

/*-------------------------------------------------------------------------
 * Function:    test_page_small_try_extend
 *
 * Purpose:     To verify that extending a small block via H5MF_try_extend() works
 *              properly when paged aggregation is enabled.
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Vailin Choi; Jan 2013
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_page_small_try_extend(const char *env_h5_drvr, hid_t fapl)
{

    hid_t fid = -1;                 /* File ID */
    hid_t fcpl = -1;                /* File creation property list */
    H5F_t *f = NULL;                /* Internal file object pointer */
    haddr_t addr1, addr2, addr3;    /* Addresses for small metadata blocks */
    haddr_t saddr1;                 /* Address for small raw data block */
    hbool_t contig_addr_vfd;        /* Whether VFD used has a contigous address space */
    htri_t was_extended;            /* Whether the block can be extended or not */
    char filename[FILENAME_LEN];    /* Filename to use */

    TESTING("Paged aggregation for file space: H5MF_try_extend() a small block");

    /* Current VFD that does not support continuous address space */
    contig_addr_vfd = (hbool_t)(HDstrcmp(env_h5_drvr, "split") && HDstrcmp(env_h5_drvr, "multi") && HDstrcmp(env_h5_drvr, "family"));

    if(contig_addr_vfd) {

        /* Set the filename to use for this test (dependent on fapl) */
        h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

        /* File creation property list */
        if((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
            FAIL_STACK_ERROR

        /* Set the strategy to paged aggregation */
        if(H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_PAGE, FALSE, (hsize_t)1) < 0)
            FAIL_STACK_ERROR

        /* Create the file to work on */
        if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(fid)))
            FAIL_STACK_ERROR

        /* Allocate a small metadata block with addr1 */
        addr1 = H5MF_alloc(f, H5FD_MEM_OHDR, (hsize_t)TBLOCK_SIZE98);

        /* Try extending the block with addr1 at EOF not crossing page boundary */
        was_extended = H5MF_try_extend(f, H5FD_MEM_OHDR, (haddr_t)addr1, (hsize_t)TBLOCK_SIZE98, (hsize_t)3100);
        /* Should succeed */
        if(was_extended != TRUE) TEST_ERROR

        /* Allocate 2 small metadata blocks with addr2 and addr3--will be on another metadata page */
        addr2 = H5MF_alloc(f, H5FD_MEM_OHDR, (hsize_t)TBLOCK_SIZE100);
        addr3 = H5MF_alloc(f, H5FD_MEM_OHDR, (hsize_t)TBLOCK_SIZE150);

        /* The block with addr2 should be page aligned */
        /* The block with addr3 resides right next to the block with addr2 */
        if(addr2 % TBLOCK_SIZE4096)
            TEST_ERROR
        if(addr3 != (addr2 + TBLOCK_SIZE100))
            TEST_ERROR

        /* Free the block with addr2 */
        H5MF_xfree(f, H5FD_MEM_OHDR, addr2, (hsize_t)TBLOCK_SIZE100);

        /* Try extending the block with addr1 that will cross to the next page where the freed block with addr2 resides */
        was_extended = H5MF_try_extend(f, H5FD_MEM_OHDR, (haddr_t)addr1, (hsize_t)TBLOCK_SIZE3198, (hsize_t)TBLOCK_SIZE100);
        /* Shouldn't succeed--should not cross page boundary */
        if(was_extended == TRUE) TEST_ERROR

        /* Try extending the block with addr1 into the free-space section that is big enough to fulfill the request */
        was_extended = H5MF_try_extend(f, H5FD_MEM_OHDR, (haddr_t)addr1, (hsize_t)TBLOCK_SIZE3198, (hsize_t)TBLOCK_SIZE50);
        /* Should succeed */
        if(was_extended != TRUE) TEST_ERROR

        /* Free the block with addr1 */
        H5MF_xfree(f, H5FD_MEM_OHDR, addr1, (hsize_t)TBLOCK_SIZE3248);

        /* Allocate a new metadata block with addr1 */
        /* There is a page end threshold of size H5F_FILE_SPACE_PGEND_META_THRES at the end of the block */
        /* The block is right next to the threshold */
        addr1 = H5MF_alloc(f, H5FD_MEM_OHDR, (hsize_t)TBLOCK_SIZE3286);

        /* Try extending the block into the threshold with size > H5F_FILE_SPACE_PGEND_META_THRES */
        was_extended = H5MF_try_extend(f, H5FD_MEM_OHDR, (haddr_t)addr1, (hsize_t)TBLOCK_SIZE3286, (hsize_t)TBLOCK_SIZE11);
        /* Shouldn't succeed */
        if(was_extended == TRUE) TEST_ERROR

        /* Try extending the block into the threshold with size < H5F_FILE_SPACE_PGEND_META_THRES */
        was_extended = H5MF_try_extend(f, H5FD_MEM_OHDR, (haddr_t)addr1, (hsize_t)TBLOCK_SIZE3286, (hsize_t)TBLOCK_SIZE2);
        /* Should succeed */
        if(was_extended != TRUE) TEST_ERROR

        /* Free the block with addr3--will merge with the remaining sections to become a page and then free the page */
        H5MF_xfree(f, H5FD_MEM_OHDR, addr3, (hsize_t)TBLOCK_SIZE150);

        /* Allocate a small raw data block with saddr1 */
        saddr1 = H5MF_alloc(f, H5FD_MEM_DRAW, (hsize_t)TBLOCK_SIZE4086);

        /* Try extending the block crossing the page boundary */
        was_extended = H5MF_try_extend(f, H5FD_MEM_DRAW, (haddr_t)saddr1, (hsize_t)TBLOCK_SIZE4086, (hsize_t)TBLOCK_SIZE11);
        /* Shouldn't succeed */
        if(was_extended == TRUE) TEST_ERROR

        /* Try extending the block not crossing page boundary */
        was_extended = H5MF_try_extend(f, H5FD_MEM_DRAW, (haddr_t)saddr1, (hsize_t)TBLOCK_SIZE4086, (hsize_t)TBLOCK_SIZE10);
        /* Should succeed */
        if(was_extended != TRUE) TEST_ERROR

        /* The extended block is now "large" in size */
        /* Try extending the block */
        was_extended = H5MF_try_extend(f, H5FD_MEM_DRAW, (haddr_t)saddr1, (hsize_t)TBLOCK_SIZE4096, (hsize_t)TBLOCK_SIZE10);
        /* Should succeed */
        if(was_extended != TRUE) TEST_ERROR

        /* Try extending the large-sized block */
        was_extended = H5MF_try_extend(f, H5FD_MEM_DRAW, (haddr_t)saddr1, (hsize_t)TBLOCK_SIZE4106, (hsize_t)TBLOCK_SIZE5000);
        /* Should not succeed because the mis-aligned fragment in the page is in the large-sized free-space manager */
        if(was_extended == TRUE) TEST_ERROR

        /* Close the file */
        if(H5Fclose(fid) < 0)
            FAIL_STACK_ERROR

        /* Close the property list */
        if(H5Pclose(fcpl) < 0)
            FAIL_STACK_ERROR

        PASSED()

    } else {
        SKIPPED();
        HDputs("    Current VFD doesn't support paged aggregation");
    }

    return(0);

error:
    H5E_BEGIN_TRY {
        H5Fclose(fid);
        H5Pclose(fcpl);
    } H5E_END_TRY;
    return(1);

} /* test_page_small_try_extend() */

/*-------------------------------------------------------------------------
 * Function:    test_page_large_try_extend
 *
 * Purpose:     To verify that extending a large block via H5MF_try_extend()
 *              is done properly when paged aggregation is enabled.
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Vailin Choi; Jan 2013
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_page_large_try_extend(const char *env_h5_drvr, hid_t fapl)
{

    hid_t fid = -1;                         /* File ID */
    hid_t fcpl = -1;                        /* File creation property list */
    H5F_t *f = NULL;                        /* Internal file object pointer */
    haddr_t gaddr1, gaddr2, gaddr3, gaddr4; /* Addresses for large data blocks */
    hbool_t contig_addr_vfd;                /* Whether VFD used has a contigous address space */
    htri_t was_extended;                    /* Whether the block can be extended or not */
    char filename[FILENAME_LEN];            /* Filename to use */

    TESTING("Paged aggregation for file space: H5MF_try_extend() a large block");

    /* Current VFD that does not support continuous address space */
    contig_addr_vfd = (hbool_t)(HDstrcmp(env_h5_drvr, "split") && HDstrcmp(env_h5_drvr, "multi"));

    if(contig_addr_vfd) {

        /* Set the filename to use for this test (dependent on fapl) */
        h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

        /* File creation property list */
        if((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
            FAIL_STACK_ERROR

        /* Set the strategy to paged aggregation */
        if(H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_PAGE, FALSE, (hsize_t)1) < 0)
            FAIL_STACK_ERROR

        /* Create the file to work on */
        if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(fid)))
            FAIL_STACK_ERROR

        /* Allocate a large data block with gaddr1 */
        gaddr1 = H5MF_alloc(f, H5FD_MEM_DRAW, (hsize_t)6000);

        /* Should be page aligned */
        if(gaddr1 % TBLOCK_SIZE4096)
            TEST_ERROR

        /* Extending the block with gaddr1 at EOF to become 2 pages */
        was_extended = H5MF_try_extend(f, H5FD_MEM_DRAW, (haddr_t)gaddr1, (hsize_t)TBLOCK_SIZE6000, (hsize_t)TBLOCK_SIZE2192);
        /* Should succeed */
        if(was_extended != TRUE) TEST_ERROR

        /* Allocate a large data block with gaddr2 */
        gaddr2 = H5MF_alloc(f, H5FD_MEM_DRAW, (hsize_t)TBLOCK_SIZE8000);
        /* Should be page aligned */
        if(gaddr2 % TBLOCK_SIZE4096)
            TEST_ERROR

        /* Try extending the block with gaddr1 */
        was_extended = H5MF_try_extend(f, H5FD_MEM_DRAW, (haddr_t)gaddr1, (hsize_t)TBLOCK_SIZE8192, (hsize_t)TBLOCK_SIZE50);
        /* Should not succeed */
        if(was_extended == TRUE) TEST_ERROR

        /* Allocate a large data block with gaddr3 */
        gaddr3 = H5MF_alloc(f, H5FD_MEM_DRAW, (hsize_t)TBLOCK_SIZE8000);
        /* Should be page aligned */
        if(gaddr3 % TBLOCK_SIZE4096)
            TEST_ERROR

        /* Try extending the block with gaddr2--there is a free-space section big enough to fulfill the request */
        was_extended = H5MF_try_extend(f, H5FD_MEM_DRAW, (haddr_t)gaddr2, (hsize_t)TBLOCK_SIZE8000, (hsize_t)TBLOCK_SIZE100);
        /* Should succeed */
        if(was_extended == FALSE) TEST_ERROR

        /* Try extending the block with gaddr2--there is no free-space section big enough to fulfill the request */
        was_extended = H5MF_try_extend(f, H5FD_MEM_DRAW, (haddr_t)gaddr2, (hsize_t)TBLOCK_SIZE8100, (hsize_t)TBLOCK_SIZE100);
        /* Should not succeed */
        if(was_extended == TRUE) TEST_ERROR

        /* Try extending the block with gaddr2--there is a free-space section big enough to fulfill the request */
        was_extended = H5MF_try_extend(f, H5FD_MEM_DRAW, (haddr_t)gaddr2, (hsize_t)TBLOCK_SIZE8100, (hsize_t)TBLOCK_SIZE90);
        /* Should succeed */
        if(was_extended == FALSE) TEST_ERROR

        /* Try extending the block with gaddr2 */
        /* There is no free-space section big enough to fulfill the request (request is < H5F_FILE_SPACE_PGEND_META_THRES) */
        was_extended = H5MF_try_extend(f, H5FD_MEM_DRAW, (haddr_t)gaddr2, (hsize_t)TBLOCK_SIZE8190, (hsize_t)TBLOCK_SIZE5);
        /* Should not succeed */
        if(was_extended == TRUE) TEST_ERROR

        /* Allocate a large data block with gaddr4 */
        gaddr4 = H5MF_alloc(f, H5FD_MEM_DRAW, (hsize_t)TBLOCK_SIZE5000);
        /* Should be page aligned */
        if(gaddr4 % TBLOCK_SIZE4096)
            TEST_ERROR

        /* Free the block with gaddr3--will merge with remaining free space to become 2 pages + section (size 2) in previous page */
        H5MF_xfree(f, H5FD_MEM_DRAW, gaddr3, (hsize_t)TBLOCK_SIZE8000);

        /* Try extending the block with gaddr2 crossing page boundary--there is free-space section big enough to fulfill the request */
        was_extended = H5MF_try_extend(f, H5FD_MEM_DRAW, (haddr_t)gaddr2, (hsize_t)TBLOCK_SIZE8190, (hsize_t)TBLOCK_SIZE5);
        /* Should succeed */
        if(was_extended == FALSE) TEST_ERROR

        /* Close file */
        if(H5Fclose(fid) < 0)
            FAIL_STACK_ERROR

        /* Close the property list */
        if(H5Pclose(fcpl) < 0)
            FAIL_STACK_ERROR

        PASSED()

    } else {
        SKIPPED();
        HDputs("    Current VFD doesn't support paged aggregation strategy");
    }

    return(0);

error:
    H5E_BEGIN_TRY {
        H5Fclose(fid);
        H5Pclose(fcpl);
    } H5E_END_TRY;
    return(1);

} /* test_page_large_try_extend() */

/*-------------------------------------------------------------------------
 * Function:    test_page_large
 *
 * Purpose:     To verify that allocations and de-allocations for large data
 *              are done properly when paged aggregation is enabled.
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Vailin Choi; Jan 2013
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_page_large(const char *env_h5_drvr, hid_t fapl)
{

    hid_t fid = -1;                         /* File ID */
    hid_t fcpl = -1;                        /* File creation property list ID */
    H5F_t *f = NULL;                        /* Internal file object pointer */
    haddr_t gaddr1, gaddr2, gaddr3, gaddr4; /* Addresses for blocks */
    hbool_t contig_addr_vfd;                /* Whether VFD used has a contigous address space */
    H5FS_stat_t fs_stat;                    /* Information for free-space manager */
    h5_stat_size_t file_size;               /* File size */
    char filename[FILENAME_LEN];            /* Filename to use */

    TESTING("Paged aggregation for file space: large allocations and de-allocations");

    /* Current VFD that does not support continuous address space */
    contig_addr_vfd = (hbool_t)(HDstrcmp(env_h5_drvr, "split") && HDstrcmp(env_h5_drvr, "multi"));

    if(contig_addr_vfd) {

        /* Set the filename to use for this test (dependent on fapl) */
        h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

        /* File creation property list */
        if((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
            FAIL_STACK_ERROR

        /* Set the strategy to paged aggregation */
        if(H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_PAGE, FALSE, (hsize_t)1) < 0)
            FAIL_STACK_ERROR

        /* Create the file to work on */
        if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(fid)))
            FAIL_STACK_ERROR

        /* Allocate a large data block with gaddr1 */
        /* 1 page + 1904 bytes; 2192 bytes in free-space manager */
        gaddr1 = H5MF_alloc(f, H5FD_MEM_DRAW, (hsize_t)TBLOCK_SIZE6000);

        /* Should be page aligned */
        if(gaddr1 % TBLOCK_SIZE4096)
            TEST_ERROR

        /* Allocate a large data block with gaddr2--should be on another page */
        /* Allocate 1 page + 3904 bytes; 192 bytes in free-space manager */
        gaddr2 = H5MF_alloc(f, H5FD_MEM_DRAW, (hsize_t)TBLOCK_SIZE8000);
        /* Should be page aligned */
        if(gaddr2 % TBLOCK_SIZE4096)
            TEST_ERROR

        /* Allocate a large data block with gaddr3--should be on another page */
        /* Allocate 2 pages + 3808 bytes; 288 bytes in free-space manager */
        gaddr3 = H5MF_alloc(f, H5FD_MEM_DRAW, (hsize_t)TBLOCK_SIZE12000);
        if(!H5F_addr_defined(gaddr3))
            TEST_ERROR

        /* Free the block with gaddr2 */
        /* Merged sections: 2192 + 8000 + 192 = 10384 */
        H5MF_xfree(f, H5FD_MEM_DRAW, gaddr2, (hsize_t)TBLOCK_SIZE8000);

        /* Get free-space info */
        if(H5FS_stat_info(f, f->shared->fs_man[H5F_MEM_PAGE_GENERIC], &fs_stat) < 0)
            FAIL_STACK_ERROR

        /* Verify that the manager contains 2 free-space sections: 10384 and 288 */
        if(fs_stat.tot_sect_count != 2)
            TEST_ERROR
        if(fs_stat.tot_space != (10384+288))
            TEST_ERROR

        /* Allocate a large data block with gaddr4--there is a free-space section able to fulfill the request */
        /* Free-space sections: 2192 + 3192 + 288 = 5672 bytes */
        gaddr4 = H5MF_alloc(f, H5FD_MEM_DRAW, (hsize_t)TBLOCK_SIZE5000);

        /* Should be page aligned */
        if(gaddr4 % TBLOCK_SIZE4096)
            TEST_ERROR
        if(gaddr4 != gaddr2)
            TEST_ERROR

        /* Get free-space info */
        if(H5FS_stat_info(f, f->shared->fs_man[H5F_MEM_PAGE_GENERIC], &fs_stat) < 0)
            FAIL_STACK_ERROR
        /* Verify that that there are 3 free-space sections */
        if(fs_stat.tot_sect_count != 3)
            TEST_ERROR
        if(fs_stat.tot_space != (2192+3192+288))
            TEST_ERROR

        /* Free the two blocks with gaddr1 and gaddr4 */
        H5MF_xfree(f, H5FD_MEM_DRAW, gaddr1, (hsize_t)TBLOCK_SIZE6000);
        H5MF_xfree(f, H5FD_MEM_DRAW, gaddr4, (hsize_t)TBLOCK_SIZE5000);

        /* Get free-space info */
        if(H5FS_stat_info(f, f->shared->fs_man[H5F_MEM_PAGE_GENERIC], &fs_stat) < 0)
            FAIL_STACK_ERROR
        /* Verify that that there are 2 free-space sections: 16384 (4 pages) + 288 */
        if(fs_stat.tot_sect_count != 2)
            TEST_ERROR
        if(fs_stat.tot_space != (16384+288))
            TEST_ERROR

        /* Close file */
        if(H5Fclose(fid) < 0)
            FAIL_STACK_ERROR

        /* Close the property list */
        if(H5Pclose(fcpl) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Verify that file size end on a page boundary */
        if(file_size % TBLOCK_SIZE4096)
            TEST_ERROR

        PASSED()

    } else {
        SKIPPED();
        HDputs("    Current VFD doesn't support paged aggregation strategy");
    }

    return(0);

error:
    H5E_BEGIN_TRY {
        H5Fclose(fid);
    } H5E_END_TRY;
    return(1);

} /* test_page_large() */

/*-------------------------------------------------------------------------
 * Function:    test_page_small
 *
 * Purpose:     To verify allocations and de-allocations for small meta/raw data
 *              are done properly when paged aggregation is enabled.
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Vailin Choi; Jan 2013
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_page_small(const char *env_h5_drvr, hid_t fapl)
{
    hid_t fid = -1;                     /* File ID */
    hid_t fcpl = -1;                    /* File creation property list */
    H5F_t *f = NULL;                    /* Internal file object pointer */
    haddr_t addr2, addr3, addr4, addr5; /* Addresses for blocks */
    haddr_t addr9, addr10, addr11;      /* Address for small metadata blocks */
    haddr_t saddr1, saddr2;             /* Addresses for small raw data blocks */
    H5FS_stat_t fs_stat;                /* Information for free-space manager */
    char filename[FILENAME_LEN];        /* Filename to use */
    hbool_t multi= FALSE, split = FALSE, family = FALSE;

    TESTING("Paged aggregation for file space: small allocations and de-allocations");

    if(!HDstrcmp(env_h5_drvr, "split"))
        split = TRUE;
    else if(!HDstrcmp(env_h5_drvr, "multi"))
        multi = TRUE;
    else if(!HDstrcmp(env_h5_drvr, "family"))
        family = TRUE;

    if(!multi && !split) {

        /* Set the filename to use for this test (dependent on fapl) */
        h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

        /* File creation property list */
        if((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
            FAIL_STACK_ERROR

        /* Set the strategy to paged aggregation */
        if(H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_PAGE, FALSE, (hsize_t)1) < 0)
            FAIL_STACK_ERROR

        /* Create the file to work on */
        if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(fid)))
            FAIL_STACK_ERROR

        /* Allocate 2 small metadata blocks: addr1, addr2 */
        H5MF_alloc(f, H5FD_MEM_OHDR, (hsize_t)TBLOCK_SIZE30);
        addr2 = H5MF_alloc(f, H5FD_MEM_OHDR, (hsize_t)TBLOCK_SIZE50);

        /* Allocate a small raw data block with saddr1 */
        saddr1 = H5MF_alloc(f, H5FD_MEM_DRAW, (hsize_t)TBLOCK_SIZE30);
        /* Should be on the second page and page aligned */
        if(saddr1 % TBLOCK_SIZE4096)
            TEST_ERROR

        /* Allocate a small raw data block with saddr2 */
        saddr2 = H5MF_alloc(f, H5FD_MEM_DRAW, (hsize_t)TBLOCK_SIZE50);
        /* Should not be page aligned */
        if(!(saddr2 % TBLOCK_SIZE4096))
            TEST_ERROR
        /* Should be next to the block with saddr1 */
        if(saddr2 != (saddr1 + TBLOCK_SIZE30))
            TEST_ERROR

        /* Allocate a small metadata block with addr3--there is no free-space section big enough to fulfill the request */
        addr3 = H5MF_alloc(f, H5FD_MEM_OHDR, (hsize_t)TBLOCK_SIZE4020);
        /* Should be on the third page and page aligned */
        if(addr3 % TBLOCK_SIZE4096)
            TEST_ERROR

        /* Allocate a small metadata block with addr4--there is a free-space section big enough to fulfill the request */
        addr4 = H5MF_alloc(f, H5FD_MEM_OHDR, (hsize_t)TBLOCK_SIZE80);
        /* Should not be page aligned */
        if(!(addr4 % TBLOCK_SIZE4096))
            TEST_ERROR
        /* Should be next to the block with addr2 */
        if(addr4 != (addr2 + TBLOCK_SIZE50))
            TEST_ERROR

        /* Allocate a small metadata block with addr5--there is a free-space section big enough to fulfill the request */
        addr5 = H5MF_alloc(f, H5FD_MEM_OHDR, (hsize_t)TBLOCK_SIZE40);
        /* Should not be page aligned */
        if(!(addr5 % TBLOCK_SIZE4096))
            TEST_ERROR

        /* Should be next to the block with addr3 */
        if(addr5 != (addr3 + TBLOCK_SIZE4020))
            TEST_ERROR

        /* Allocate a small metadata block with addr6--taking up the remaining space in the first page */
        if(family)
            H5MF_alloc(f, H5FD_MEM_OHDR, (hsize_t)TBLOCK_SIZE3080);
        else
            H5MF_alloc(f, H5FD_MEM_OHDR, (hsize_t)TBLOCK_SIZE3088);

        /* Allocate a small metadata block with addr7--taking up the remaining space in the third page */
        H5MF_alloc(f, H5FD_MEM_OHDR, (hsize_t)TBLOCK_SIZE36);

        /* Allocate 2 small metadata blocks: addr8, addr9--there is no free-space to fulfill the request */
        H5MF_alloc(f, H5FD_MEM_OHDR, (hsize_t)TBLOCK_SIZE50);
        addr9 = H5MF_alloc(f, H5FD_MEM_OHDR, (hsize_t)TBLOCK_SIZE80);

        /* Free the block with saddr1 and saddr2--merge with remaining section to become a page which will be returned to the large manager */
        H5MF_xfree(f, H5FD_MEM_DRAW, saddr1, (hsize_t)TBLOCK_SIZE30);
        H5MF_xfree(f, H5FD_MEM_DRAW, saddr2, (hsize_t)TBLOCK_SIZE50);

        /* Verify that the large manager does contain a section with file space page size (default is 4096) */
        if(!f->shared->fs_man[H5F_MEM_PAGE_GENERIC])
            TEST_ERROR
        if(H5FS_stat_info(f, f->shared->fs_man[H5F_MEM_PAGE_GENERIC], &fs_stat) < 0)
            FAIL_STACK_ERROR
        if(fs_stat.tot_space != TBLOCK_SIZE4096)
            TEST_ERROR

        /* Allocate a small metadata block with addr10--there is a free-space section big enough to fulfill the request */
        addr10 = H5MF_alloc(f, H5FD_MEM_OHDR, (hsize_t)TBLOCK_SIZE3900);
        /* The block should be next to the block with addr9 */
        if(addr10 != (addr9 + TBLOCK_SIZE80))
            TEST_ERROR

        /* Allocate a small metadata block with addr11 */
        /* The current free-space section is unable to fulfill the request; obtain a page from the large manager */
        addr11 = H5MF_alloc(f, H5FD_MEM_OHDR, (hsize_t)TBLOCK_SIZE80);
        /* The address of the block should be the same the freed block with saddr1 */
        if(addr11 != saddr1)
            TEST_ERROR

        /* Close file */
        if(H5Fclose(fid) < 0)
            FAIL_STACK_ERROR

        /* Close the property list */
        if(H5Pclose(fcpl) < 0)
            FAIL_STACK_ERROR

        PASSED()

    } else {
        SKIPPED();
        HDputs("    Current VFD doesn't support paged aggregation strategy");
    }

    return(0);

error:
    H5E_BEGIN_TRY {
        H5Fclose(fid);
        H5Pclose(fcpl);
    } H5E_END_TRY;
    return(1);

} /* test_page_small() */

/*-------------------------------------------------------------------------
 * Function:    test_page_alignment
 *
 * Purpose:     To verify the proper alignment is used when H5Pset_alignment()
 *              is set and paged aggregation is enabled.
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Vailin Choi; Jan 2013
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_page_alignment(const char *env_h5_drvr, hid_t fapl)
{

    hid_t fid = -1;                 /* File ID */
    hid_t fcpl = -1;                /* File creation property list ID */
    hid_t fcpl2 = -1;               /* File creation property list ID */
    hid_t fapl_new = -1;            /* File access property list ID */
    H5F_t *f = NULL;                /* Internal file object pointer */
    haddr_t addr1, addr2;           /* Addresses for small metadata blocks */
    haddr_t saddr1, saddr2;         /* Addresses for small raw data blocks */
    haddr_t gaddr1, gaddr2;         /* Addresses for blocks */
    char filename[FILENAME_LEN];    /* Filename to use */
    hbool_t split = FALSE, multi = FALSE;

    TESTING("Paged aggregation and H5Pset_alignment: verify proper alignment is used");

    /* Check for split or multi driver */
    if(!HDstrcmp(env_h5_drvr, "split"))
        split = TRUE;
    else if(!HDstrcmp(env_h5_drvr, "multi"))
        multi = TRUE;

    if(!multi && !split) {

        /* Set the filename to use for this test (dependent on fapl) */
        h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

        /*
         * Case 1: Verify that the alignment in use is the default file space
         *         page size when paged aggregation is enabled.
         */

        if((fapl_new = H5Pcopy(fapl)) < 0) TEST_ERROR

        /* The alignment to use will be the library's default file space page size */
        if(H5Pset_libver_bounds(fapl_new, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
            FAIL_STACK_ERROR

        /* Set alignment value to 16 */
        if(H5Pset_alignment(fapl_new, (hsize_t)0, (hsize_t)TEST_ALIGN16) < 0)
            TEST_ERROR

        if(split || multi) {
            hid_t memb_fapl;
            H5FD_mem_t memb_map[H5FD_MEM_NTYPES];
            hid_t memb_fapl_arr[H5FD_MEM_NTYPES];
            char *memb_name[H5FD_MEM_NTYPES];
            haddr_t memb_addr[H5FD_MEM_NTYPES];
            hbool_t relax;
            H5FD_mem_t  mt;

            /* Create fapl */
            if((memb_fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
                TEST_ERROR

            /* Set alignment */
            if(H5Pset_alignment(memb_fapl, 0, (hsize_t)TEST_ALIGN16) < 0)
                TEST_ERROR

            HDmemset(memb_name, 0, sizeof memb_name);

            if(split) {
                /* Set split driver with new FAPLs */
                if(H5Pset_fapl_split(fapl_new, "-m.h5", memb_fapl, "-r.h5", memb_fapl) < 0)
                    TEST_ERROR

                /* Get current multi settings */
                if(H5Pget_fapl_multi(fapl_new, memb_map, memb_fapl_arr, memb_name, memb_addr, &relax) < 0)
                    TEST_ERROR

                /* Set memb_addr aligned */
                memb_addr[H5FD_MEM_SUPER] = ((memb_addr[H5FD_MEM_SUPER] + TBLOCK_SIZE4096 - 1) / TBLOCK_SIZE4096) * TBLOCK_SIZE4096;
                memb_addr[H5FD_MEM_DRAW] = ((memb_addr[H5FD_MEM_DRAW] + TBLOCK_SIZE4096 - 1) / TBLOCK_SIZE4096) * TBLOCK_SIZE4096;

                /* Set split driver with new FAPLs */
                if(H5Pset_fapl_multi(fapl_new, memb_map, memb_fapl_arr, (const char * const *)memb_name, memb_addr, relax) < 0)
                    TEST_ERROR

            } else {
                /* Get current multi settings */
                if(H5Pget_fapl_multi(fapl_new, memb_map, NULL, memb_name, memb_addr, &relax) < 0)
                    TEST_ERROR

                /* Populate memb_fapl_arr */
                /* Set memb_addr aligned */
                for(mt = H5FD_MEM_DEFAULT; mt < H5FD_MEM_NTYPES; H5_INC_ENUM(H5FD_mem_t, mt)) {
                    memb_fapl_arr[mt] = memb_fapl;
                    memb_addr[mt] = ((memb_addr[mt] + TBLOCK_SIZE4096 - 1) / TBLOCK_SIZE4096) * TBLOCK_SIZE4096;
                }

                /* Set multi driver with new FAPLs */
                if(H5Pset_fapl_multi(fapl_new, memb_map, memb_fapl_arr, (const char * const *)memb_name, memb_addr, relax) < 0)
                    TEST_ERROR

            } /* end else */

            /* Free memb_name */
            for(mt = H5FD_MEM_DEFAULT; mt < H5FD_MEM_NTYPES; H5_INC_ENUM(H5FD_mem_t, mt))
                free(memb_name[mt]);

            /* Close memb_fapl */
            if(H5Pclose(memb_fapl) < 0)
                TEST_ERROR
        } /* end if */

        /* File creation property list */
        if((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
            TEST_ERROR

        /* Set the strategy to paged aggregation and persisting free space */
        /* The alignment to use will be the library's default file space page size */
        if(H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_PAGE, TRUE, (hsize_t)1) < 0)
            TEST_ERROR

        /* Create the file to work on */
        if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl_new)) < 0)
            TEST_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(fid)))
            TEST_ERROR

        /* Allocate 2 small raw data blocks */
        saddr1 = H5MF_alloc(f, H5FD_MEM_DRAW, (hsize_t)TBLOCK_SIZE30);
        saddr2 = H5MF_alloc(f, H5FD_MEM_DRAW, (hsize_t)TBLOCK_SIZE50);

        /* Should be on the second page and page aligned on 4096 (default file space page size) */
        if(saddr1 % TBLOCK_SIZE4096)
            TEST_ERROR

        /* Should be next to the block with saddr1 */
        if(saddr2 != (saddr1 + TBLOCK_SIZE30))
            TEST_ERROR

        /* Allocate 2 large raw data blocks */
        gaddr1 = H5MF_alloc(f, H5FD_MEM_DRAW, (hsize_t)TBLOCK_SIZE5000);
        gaddr2 = H5MF_alloc(f, H5FD_MEM_DRAW, (hsize_t)TBLOCK_SIZE8000);

        /* Should be on the 3rd page and page aligned */
        if(gaddr1 % TBLOCK_SIZE4096)
            TEST_ERROR

        /* Should be on the 4th page and page aligned */
        if(gaddr2 % TBLOCK_SIZE4096)
            TEST_ERROR

        /* Close the file creation property list */
        if(H5Pclose(fcpl) < 0)
            TEST_ERROR

        /* Close file */
        if(H5Fclose(fid) < 0)
            TEST_ERROR

        /*
         * Case 2: Verify that the alignment in use is the alignment set
         *         via H5Pset_alignment when paged aggregation not enabled.
         */
        /* fapl_new has latest format and H5Pset_alignment set */
        /* Disable small data block mechanism */
        if(H5Pset_small_data_block_size(fapl_new, (hsize_t)0) < 0)
            TEST_ERROR
        /* Disable metadata block mechanism */
        if(H5Pset_meta_block_size(fapl_new, (hsize_t)0) < 0)
            TEST_ERROR

        /* Create the file to work on */
        if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_new)) < 0)
            TEST_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(fid)))
            TEST_ERROR

        /* Allocate 2 small metadata blocks */
        addr1 = H5MF_alloc(f, H5FD_MEM_SUPER, (hsize_t)TBLOCK_SIZE30);
        addr2 = H5MF_alloc(f, H5FD_MEM_SUPER, (hsize_t)TBLOCK_SIZE50);

        /* Should be aligned on 16 */
        if(addr1 % TEST_ALIGN16 || addr2 % TEST_ALIGN16)
            TEST_ERROR

        /* addr2 should be right next to the block with addr1 */
        if((addr1 + TBLOCK_SIZE30) % TEST_ALIGN16)
            if(addr2 != (((addr1 + TBLOCK_SIZE30) / TEST_ALIGN16) + 1) * TEST_ALIGN16)
                TEST_ERROR

        /* Allocate 2 small raw data blocks */
        saddr1 = H5MF_alloc(f, H5FD_MEM_DRAW, (hsize_t)TBLOCK_SIZE80);
        saddr2 = H5MF_alloc(f, H5FD_MEM_DRAW, (hsize_t)TBLOCK_SIZE100);

        /* Should be aligned on 16 */
        if(saddr1 % TEST_ALIGN16 || saddr2 % TEST_ALIGN16)
            TEST_ERROR

        if(!multi && !split) {
            /* saddr1 should be right next to the block with addr2 */
            if((addr2 + TBLOCK_SIZE50) % TEST_ALIGN16)
                if(saddr1 != (((addr2 + TBLOCK_SIZE50) / TEST_ALIGN16) + 1) * TEST_ALIGN16)
                    TEST_ERROR
        }

        /* saddr2 should be right next to the block with saddr1 */
        if((saddr1 + TBLOCK_SIZE80) % TEST_ALIGN16)
            if(saddr2 != (((saddr1 + TBLOCK_SIZE80) / TEST_ALIGN16) + 1) * TEST_ALIGN16)
                TEST_ERROR

        /* Close file */
        if(H5Fclose(fid) < 0)
            TEST_ERROR

        /*
         * Case 3: Verify that the alignment in use is the alignment set
         *         via H5Pset_alignment when paged aggregation not enabled.
         */
        /* File creation property list */
        if((fcpl2 = H5Pcreate(H5P_FILE_CREATE)) < 0)
            TEST_ERROR

        /* Set file space page size */
        if(H5Pset_file_space_page_size(fcpl2, (hsize_t)TBLOCK_SIZE8192) < 0)
            TEST_ERROR

        /* Set strategy to H5F_FSPACE_STRATEGY_AGGR but meta/raw data block is 0 as set in fapl_new */
        if(H5Pset_file_space_strategy(fcpl2, H5F_FSPACE_STRATEGY_AGGR, FALSE, (hsize_t)1) < 0)
            TEST_ERROR

        /* fapl_new has latest format, H5Pset_alignment set, and disable meta/raw block */
        if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl2, fapl_new)) < 0)
            TEST_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(fid)))
            TEST_ERROR

        /* Allocate 2 small raw data blocks */
        saddr1 = H5MF_alloc(f, H5FD_MEM_DRAW, (hsize_t)TBLOCK_SIZE30);
        saddr2 = H5MF_alloc(f, H5FD_MEM_DRAW, (hsize_t)TBLOCK_SIZE50);

        /* Should be aligned on 16 */
        if(saddr1 % TEST_ALIGN16)
            TEST_ERROR
        if(saddr2 % TEST_ALIGN16)
            TEST_ERROR

        /* saddr2 should be right next to the block with saddr1 */
        if((saddr1 + TBLOCK_SIZE30) % TEST_ALIGN16)
            if(saddr2 != (((saddr1 + TBLOCK_SIZE30) / TEST_ALIGN16) + 1) * TEST_ALIGN16)
                TEST_ERROR

        /* Allocate a large raw data block */
        gaddr1 = H5MF_alloc(f, H5FD_MEM_DRAW, (hsize_t)TBLOCK_SIZE5000);

        /* Should be aligned on 16 */
        if(gaddr1 % TEST_ALIGN16)
            TEST_ERROR

        /* gaddr1 is right next to the block with saddr2 */
        if((saddr2 + TBLOCK_SIZE50) % TEST_ALIGN16)
            if(gaddr1 != (((saddr2 + TBLOCK_SIZE50) / TEST_ALIGN16) + 1) * TEST_ALIGN16)
                TEST_ERROR

        /* There is no free-space manager involved for H5F_FSPACE_STRATEGY_AGGR strategy */
        if(f->shared->fs_man[H5FD_MEM_DRAW] || f->shared->fs_man[H5FD_MEM_SUPER])
            TEST_ERROR

        /* Closing */
        if(H5Fclose(fid) < 0)
            TEST_ERROR
        if(H5Pclose(fcpl2) < 0)
            TEST_ERROR
        if(H5Pclose(fapl_new) < 0)
            TEST_ERROR

        PASSED()

    } else {
        SKIPPED();
        HDputs("    Current VFD doesn't support persisting free-space or paged aggregation strategy");
    }

    return(0);

error:
    H5E_BEGIN_TRY {
        H5Fclose(fid);
        H5Pclose(fcpl);
        H5Pclose(fapl_new);
    } H5E_END_TRY;
    return(1);

} /* test_page_alignment() */

int
main(void)
{
    hid_t       fapl = -1;       /* File access property list for data files */
    hid_t       new_fapl = -1;       /* File access property list for alignment & aggr setting */
    unsigned    nerrors = 0;       /* Cumulative error count */
    test_type_t    curr_test;       /* Current test being worked on */
    const char  *env_h5_drvr;      /* File Driver value from environment */
    hbool_t     api_ctx_pushed = FALSE;             /* Whether API context pushed */

    /* Get the VFD to use */
    env_h5_drvr = HDgetenv("HDF5_DRIVER");
    if(env_h5_drvr == NULL)
        env_h5_drvr = "nomatch";

    h5_reset();

    fapl = h5_fileaccess();

    /* Push API context */
    if(H5CX_push() < 0) FAIL_STACK_ERROR
    api_ctx_pushed = TRUE;

    /* Make a copy of the FAPL before adjusting the alignment */
    if((new_fapl = H5Pcopy(fapl)) < 0) TEST_ERROR
    /* For old library format--interaction with file allocation */
    nerrors += test_mf_eoa(env_h5_drvr, fapl);
    nerrors += test_mf_eoa_shrink(env_h5_drvr, fapl);
    nerrors += test_mf_eoa_extend(env_h5_drvr, fapl);

    /* For old library format */
    nerrors += test_dichotomy(new_fapl);

    /* For old library format--interaction with free-space manager */
    nerrors += test_mf_fs_start(fapl);
    nerrors += test_mf_fs_alloc_free(fapl);
    nerrors += test_mf_fs_extend(fapl);
    nerrors += test_mf_fs_absorb(env_h5_drvr, fapl);

    /* For old library format--interaction with meta/sdata aggregator */
    nerrors += test_mf_aggr_alloc1(env_h5_drvr, fapl);
    nerrors += test_mf_aggr_alloc2(env_h5_drvr, fapl);
    nerrors += test_mf_aggr_alloc3(env_h5_drvr, fapl);
    nerrors += test_mf_aggr_alloc4(env_h5_drvr, fapl);
    nerrors += test_mf_aggr_alloc5(env_h5_drvr, fapl);
    nerrors += test_mf_aggr_alloc6(env_h5_drvr, fapl);
    nerrors += test_mf_aggr_alloc7(env_h5_drvr, fapl);
    nerrors += test_mf_aggr_extend(env_h5_drvr, fapl);
    nerrors += test_mf_aggr_absorb(env_h5_drvr, fapl);

    /* For old library format--tests for alignment */
    for(curr_test = TEST_NORMAL; curr_test < TEST_NTESTS; H5_INC_ENUM(test_type_t, curr_test)) {

        switch(curr_test) {
            case TEST_NORMAL: /* set alignment = 1024 */
                if(H5Pset_alignment(new_fapl, (hsize_t)0, (hsize_t)TEST_ALIGN1024) < 0)
                    TEST_ERROR
                break;

            case TEST_AGGR_SMALL: /* set alignment = 4096 */
                if(H5Pset_alignment(new_fapl, (hsize_t)0, (hsize_t)TEST_ALIGN4096) < 0)
                    TEST_ERROR
                break;

            case TEST_NTESTS:
            default:
                TEST_ERROR;
                break;
        } /* end switch */

        nerrors += test_mf_align_eoa(env_h5_drvr, fapl, new_fapl);
        nerrors += test_mf_align_fs(env_h5_drvr, fapl, new_fapl);
        nerrors += test_mf_align_alloc1(env_h5_drvr, fapl, new_fapl);
        nerrors += test_mf_align_alloc2(env_h5_drvr, fapl, new_fapl);
        nerrors += test_mf_align_alloc3(env_h5_drvr, fapl, new_fapl);
        nerrors += test_mf_align_alloc4(env_h5_drvr, fapl, new_fapl);
        nerrors += test_mf_align_alloc5(env_h5_drvr, fapl, new_fapl);
        nerrors += test_mf_align_alloc6(env_h5_drvr, fapl, new_fapl);
    } /* end for */

    /* For old and new format--interaction with temporary file space allocation */
    nerrors += test_mf_tmp(env_h5_drvr, fapl, FALSE);
    nerrors += test_mf_tmp(env_h5_drvr, fapl, TRUE);

    /* For old and new format--free-space merge/shrunk away */

    /* Temporary: modify to skip testing for multi/split driver:
         fail file create when persisting free-space or using paged aggregation strategy */
    nerrors += test_mf_fs_gone(env_h5_drvr, fapl, FALSE);
    nerrors += test_mf_fs_gone(env_h5_drvr, fapl, TRUE);

    /* Temporary: modify to skip testing multi/split driver:
         fail file create when persisting free-space or using paged aggregation strategy */
    nerrors += test_mf_strat_thres_gone(env_h5_drvr, fapl, FALSE);
    nerrors += test_mf_strat_thres_gone(env_h5_drvr, fapl, TRUE);

    /* For old and new format--persisting free-space */

    /* Temporary: Modify to skip testing for multi/split driver:
         fail file create when persisting free-space or using paged aggregation strategy */
    nerrors += test_mf_fs_persist(env_h5_drvr, fapl, FALSE);
    nerrors += test_mf_fs_persist(env_h5_drvr, fapl, TRUE);

    /* Temporary: modify to skip testing for multi/split driver:
         fail file create when persisting free-space or using paged aggregation strategy */
    nerrors += test_mf_strat_thres_persist(env_h5_drvr, fapl, FALSE);
    nerrors += test_mf_strat_thres_persist(env_h5_drvr, fapl, TRUE);

    /* Temporary skipped for multi/split drivers:
         fail file create when persisting free-space or using paged aggregation strategy */
#ifdef PB_OUT
    /* Tests specific for multi and split files--persisting free-space */
    nerrors += test_mf_fs_persist_split();
    nerrors += test_mf_fs_persist_multi();
#endif

    /*
     * Tests specific for file space paging
     */
    /* Temporary: The following 7 tests are modified to skip testing for multi/split driver:
         fail file create when persisting free-space or using paged aggregation strategy */
    nerrors += test_page_small(env_h5_drvr, fapl);
    nerrors += test_page_large(env_h5_drvr, fapl);
    nerrors += test_page_large_try_extend(env_h5_drvr, fapl);
    nerrors += test_page_small_try_extend(env_h5_drvr, fapl);
    nerrors += test_page_try_shrink(env_h5_drvr, fapl);
    nerrors += test_page_alloc_xfree(env_h5_drvr, fapl);        /* can handle multi/split */
    nerrors += test_page_alignment(env_h5_drvr, fapl);          /* can handle multi/split */

    /* tests for specific bugs */
    nerrors += test_mf_bug1(env_h5_drvr, fapl);

    if(H5Pclose(new_fapl) < 0)
        FAIL_STACK_ERROR
    h5_cleanup(FILENAME, fapl);

    /* Pop API context */
    if(api_ctx_pushed && H5CX_pop() < 0) FAIL_STACK_ERROR
    api_ctx_pushed = FALSE;

    if(nerrors)
        goto error;
    HDputs("All free-space manager tests for file memory passed.");

    return(0);

error:
    HDputs("*** TESTS FAILED ***");
    H5E_BEGIN_TRY {
        H5Pclose(fapl);
        H5Pclose(new_fapl);
    } H5E_END_TRY;

    if(api_ctx_pushed) H5CX_pop();

    return(1);
} /* main() */

