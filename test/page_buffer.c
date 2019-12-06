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

/***********************************************************
*
* Test program:	 cache_page_buffer
*
* Tests the Page Buffer Feature.
*
*************************************************************/

#include <err.h>
#include <libgen.h>

#include "h5test.h"

#include "H5CXprivate.h"        /* API Contexts                         */
#include "H5Iprivate.h"
#include "H5PBprivate.h"
#include "H5VLprivate.h"        /* Virtual Object Layer                     */

/*
 * This file needs to access private information from the H5F package.
 */
#define H5MF_FRIEND		/*suppress error about including H5MFpkg	  */
#include "H5MFpkg.h"

#define H5F_FRIEND		/*suppress error about including H5Fpkg	  */
#define H5F_TESTING
#include "H5Fpkg.h"


#define FILENAME_LEN            1024
#define NUM_DSETS               5
#define NX                      100
#define NY                      50

/* helper routines */
#ifndef H5_HAVE_PARALLEL
static unsigned create_file(char *filename, hid_t fcpl, hid_t fapl);
static unsigned open_file(char *filename, hid_t fapl, hsize_t page_size, size_t page_buffer_size);
#endif /* H5_HAVE_PARALLEL */

/* test routines */
#ifdef H5_HAVE_PARALLEL
static unsigned verify_page_buffering_disabled(hid_t orig_fapl, 
    const char *env_h5_drvr);
#else
static unsigned test_args(hid_t fapl, const char *env_h5_drvr);
static unsigned test_raw_data_handling(hid_t orig_fapl, const char *env_h5_drvr,
    bool);
static unsigned test_lru_processing(hid_t orig_fapl, const char *env_h5_drvr);
static unsigned test_min_threshold(hid_t orig_fapl, const char *env_h5_drvr);
static unsigned test_stats_collection(hid_t orig_fapl, const char *env_h5_drvr);
#endif /* H5_HAVE_PARALLEL */

#define FILENAME "filepaged"
static const char *namebases[] = {FILENAME, NULL};
static const char *namebase = FILENAME;

static hid_t
paging_fcpl_create(const hsize_t pgsz)
{ 
    hid_t fcpl = H5I_INVALID_HID;

    if ((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
        goto error;
    if (H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_PAGE, 0, 1) < 0)
        goto error;
    if (H5Pset_file_space_page_size(fcpl, pgsz) < 0)
        goto error;

    return fcpl;
error:
    if (fcpl != H5I_INVALID_HID)
        H5Pclose(fcpl);
    return H5I_INVALID_HID;
}

static struct timespec
print_elapsed_time(const struct timespec H5_ATTR_UNUSED *lastp,
    const char H5_ATTR_UNUSED *fn, int H5_ATTR_UNUSED ln)
{
#if 0
    uint64_t elapsed_ns;
    struct timespec diff, now, last;
#endif
    struct timespec now;

    if (clock_gettime(CLOCK_MONOTONIC, &now) == -1)
        err(EXIT_FAILURE, "%s: clock_gettime", __func__);

#if 0
    last = (lastp == NULL) ? now : *lastp;

    timespecsub(&now, &last, &diff);

    elapsed_ns = (uint64_t)diff.tv_sec * (uint64_t)1000000000 + diff.tv_nsec;

    printf("%5" PRIu64 ".%03" PRIu64 " %s.%d\n",
        elapsed_ns / 1000000000, (elapsed_ns / 1000000) % 1000, fn, ln);
#endif

    return now;
}

static int
swmr_fapl_augment(hid_t fapl, const char *filename, uint32_t max_lag)
{
    H5F_vfd_swmr_config_t config = {
      .version = H5F__CURR_VFD_SWMR_CONFIG_VERSION
    , .tick_len = 4
    , .max_lag = max_lag
    , .writer = true
    , .md_pages_reserved = 128
    };
    const char *dname;
    char *tname;

    if ((tname = strdup(filename)) == NULL) {
        HDfprintf(stderr, "temporary string allocation failed\n");
        return -1;
    }
    dname = dirname(tname);
    snprintf(config.md_file_path, sizeof(config.md_file_path),
        "%s/my_md_file", dname);
    free(tname);

    /* Enable VFD SWMR configuration */
    if(H5Pset_vfd_swmr_config(fapl, &config) < 0) {
        HDfprintf(stderr, "H5Pset_vrd_swmr_config failed\n");
        return -1;
    }
    return 0;
}

static bool
pgbuf_read_each_equals(H5F_t *f, H5FD_mem_t ty, haddr_t addr, size_t nelts,
    int *data, int val)
{
    size_t i;

    /* Read all elements using the VFD. */
    if (H5F_block_read(f, ty, addr, sizeof(int) * nelts, data) < 0)
        FAIL_STACK_ERROR;

    for (i = 0; i < nelts; i++) {
        if (data[i] != val) {
            printf("%s: read %d at data[%zu], expected %d\n", __func__,
                data[i], i, val);
            return false;
        }
    }
    return true;
error:
    return false;
}

static bool
vfd_read_each_equals(H5F_t *f, H5FD_mem_t ty, haddr_t addr, size_t nelts,
    int *data, int val)
{
    size_t i;

    /* Read all elements using the VFD. */
    if (H5FD_read(f->shared->lf, ty, addr, sizeof(int) * nelts, data) < 0)
        FAIL_STACK_ERROR;

    for (i = 0; i < nelts; i++) {
        if (data[i] != val) {
#if 0
            printf("%s: read %d at data[%d], expected %d\n", __func__,
                data[i], i, val);
#endif
            return false;
        }
    }
    return true;
error:
    return false;
}


/*-------------------------------------------------------------------------
 * Function:    create_file()
 *
 * Purpose:     The purpose of this function appears to be a smoke check
 *              intended to exercise the page buffer.
 *
 *              Specifically, the function creates a file, and then goes 
 *              through a loop in which it creates four data sets, write 
 *              data to one of them, verifies the data written, and then 
 *              deletes the three that it didn't write to.
 *
 *              Any data mis-matches or failures reported by the HDF5
 *              library result in test failure.
 *
 * Return:      0 if test is sucessful
 *              1 if test fails
 *
 * Programmer:  unknown
 *              ?? / ?? / ??
 *
 *-------------------------------------------------------------------------
 */
static unsigned
create_file(char *filename, hid_t fcpl, hid_t fapl)
{
    hid_t       file_id = -1;
    hid_t       dset_id = -1;
    hid_t       grp_id = -1;
    hid_t       filespace = -1;
    hsize_t     dimsf[2] = {NX, NY};       /* dataset dimensions */
    int         *data = NULL;                     /* pointer to data buffer to write */
    hid_t       dcpl = -1;
    int         i;
    int         num_elements;
    int         j;
    char        dset_name[32];

    if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl)) < 0)
        FAIL_STACK_ERROR;

    if((grp_id = H5Gcreate2(file_id, "GROUP", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    num_elements  = NX * NY;
    if((data = (int *)HDcalloc((size_t)num_elements, sizeof(int))) == NULL)
        TEST_ERROR
    for (i=0; i < (int)num_elements; i++)
        data[i] = i;

    if((filespace = H5Screate_simple(2, dimsf, NULL)) < 0)
        FAIL_STACK_ERROR;

    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
       FAIL_STACK_ERROR;
    if(H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_EARLY) < 0)
        FAIL_STACK_ERROR;

    for(i=0 ; i<NUM_DSETS; i++) {

        HDsprintf(dset_name, "D1dset%d", i);
        if((dset_id = H5Dcreate2(grp_id, dset_name, H5T_NATIVE_INT, filespace,
                                 H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR;
        if(H5Dclose(dset_id) < 0)
            FAIL_STACK_ERROR;

        HDsprintf(dset_name, "D2dset%d", i);
        if((dset_id = H5Dcreate2(grp_id, dset_name, H5T_NATIVE_INT, filespace,
                                 H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR;
        if(H5Dclose(dset_id) < 0)
            FAIL_STACK_ERROR;

        HDsprintf(dset_name, "D3dset%d", i);
        if((dset_id = H5Dcreate2(grp_id, dset_name, H5T_NATIVE_INT, filespace,
                                 H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR;
        if(H5Dclose(dset_id) < 0)
            FAIL_STACK_ERROR;

        HDsprintf(dset_name, "dset%d", i);
        if((dset_id = H5Dcreate2(grp_id, dset_name, H5T_NATIVE_INT, filespace,
                                 H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR;

        if(H5Dwrite(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data) < 0)
            FAIL_STACK_ERROR;
        if(H5Dclose(dset_id) < 0)
            FAIL_STACK_ERROR;

        HDmemset(data, 0, (size_t)num_elements * sizeof(int));
        if((dset_id = H5Dopen2(grp_id, dset_name, H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR;        
        if(H5Dread(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data) < 0)
            FAIL_STACK_ERROR;
        if(H5Dclose(dset_id) < 0)
            FAIL_STACK_ERROR;

        for (j=0; j < num_elements; j++) {
            if(data[j] != j) {
                HDfprintf(stderr, "Read different values than written\n");
                FAIL_STACK_ERROR;
            }
        }

        HDsprintf(dset_name, "D1dset%d", i);
        if(H5Ldelete(grp_id, dset_name, H5P_DEFAULT) < 0)
            FAIL_STACK_ERROR;
        HDsprintf(dset_name, "D2dset%d", i);
        if(H5Ldelete(grp_id, dset_name, H5P_DEFAULT) < 0)
            FAIL_STACK_ERROR;
        HDsprintf(dset_name, "D3dset%d", i);
        if(H5Ldelete(grp_id, dset_name, H5P_DEFAULT) < 0)
            FAIL_STACK_ERROR;
    }

    if(H5Gclose(grp_id) < 0)
        FAIL_STACK_ERROR;
    if(H5Fclose(file_id) < 0)
        FAIL_STACK_ERROR;
    if(H5Pclose(dcpl) < 0)
        FAIL_STACK_ERROR;
    if(H5Sclose(filespace) < 0)
        FAIL_STACK_ERROR;

    HDfree(data);
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Sclose(filespace);
        H5Gclose(grp_id);
        H5Fclose(file_id);
        if(data)
            HDfree(data);
    } H5E_END_TRY;
    return(1);
}


/*-------------------------------------------------------------------------
 * Function:    open_file()
 *
 * Purpose:     The purpose of this function appears to be a smoke check
 *              intended to exercise the page buffer.
 *
 *              Specifically, the function opens a file (created by 
 *              create_file()?), and verify the contents of its datasets.
 *
 *              Any data mis-matches or failures reported by the HDF5
 *              library result in test failure.
 *
 * Return:      0 if test is sucessful
 *              1 if test fails
 *
 * Programmer:  unknown
 *              ?? / ?? / ??
 *
 *-------------------------------------------------------------------------
 */
static unsigned
open_file(char *filename, hid_t fapl, hsize_t page_size, 
    size_t page_buffer_size)
{
    hid_t       file_id = -1;
    hid_t       dset_id = -1;
    hid_t       grp_id = -1;
    int         *data = NULL;                    /* pointer to data buffer to write */
    int         i;
    int         j;
    int         num_elements;
    char        dset_name[32];
    H5F_t       *f = NULL;

    if((file_id = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Get a pointer to the internal file object */
    if(NULL == (f = (H5F_t *)H5VL_object(file_id)))
        FAIL_STACK_ERROR;

    if(f->shared->pb_ptr == NULL)
        FAIL_STACK_ERROR;
    if(f->shared->pb_ptr->page_size != page_size)
        FAIL_STACK_ERROR;
    if(f->shared->pb_ptr->max_size != page_buffer_size)
        FAIL_STACK_ERROR;

    if((grp_id = H5Gopen2(file_id, "GROUP", H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    num_elements  = NX * NY;
    if((data = (int *)HDcalloc((size_t)num_elements, sizeof(int))) == NULL)
        TEST_ERROR

    for(i=0 ; i<NUM_DSETS; i++) {

        HDsprintf(dset_name, "dset%d", i);
        if((dset_id = H5Dopen2(grp_id, dset_name, H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR;

        if(H5Dread(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data) < 0)
            FAIL_STACK_ERROR;

        if(H5Dclose(dset_id) < 0)
            FAIL_STACK_ERROR;

        for (j=0; j < num_elements; j++) {
            if(data[j] != j) {
                HDfprintf(stderr, "Read different values than written\n");
                FAIL_STACK_ERROR;
            }
        }
    }

    if(H5Gclose(grp_id) < 0)
        FAIL_STACK_ERROR;
    if(H5Fclose(file_id) < 0)
        FAIL_STACK_ERROR;
    HDfree(data);

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Gclose(grp_id);
        H5Fclose(file_id);
        if(data)
            HDfree(data);
    } H5E_END_TRY;
    return 1;
}

/*
 *
 *  set_multi_split():
 *      Internal routine to set up page-aligned address space for multi/split driver
 *      when testing paged aggregation.
 *
 */
static unsigned
set_multi_split(const char *env_h5_drvr, hid_t fapl, hsize_t pagesize)
{
    hbool_t split = FALSE;
    hbool_t multi = FALSE;
    H5FD_mem_t memb_map[H5FD_MEM_NTYPES];
    hid_t memb_fapl_arr[H5FD_MEM_NTYPES];
    char *memb_name[H5FD_MEM_NTYPES];
    haddr_t memb_addr[H5FD_MEM_NTYPES];
    hbool_t relax;
    H5FD_mem_t  mt;

    /* Check for split or multi driver */
    if(!HDstrcmp(env_h5_drvr, "split"))
        split = TRUE;
    else if(!HDstrcmp(env_h5_drvr, "multi"))
        multi = TRUE;

    if(split || multi) {

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
            for(mt = H5FD_MEM_DEFAULT; mt < H5FD_MEM_NTYPES; mt++)
                memb_addr[mt] = ((memb_addr[mt] + pagesize - 1) / pagesize) * pagesize;
        }

        /* Set multi driver with new FAPLs */
        if(H5Pset_fapl_multi(fapl, memb_map, memb_fapl_arr, (const char * const *)memb_name, memb_addr, relax) < 0)
            TEST_ERROR

        /* Free memb_name */
        for(mt = H5FD_MEM_DEFAULT; mt < H5FD_MEM_NTYPES; mt++)
            free(memb_name[mt]);

    }

    return 0;

error:
    return 1;

}


/*-------------------------------------------------------------------------
 * Function:    test_args()
 *
 * Purpose:     This test appears to be a quick smoke check directed at:
 *
 *              1) verifying that API errors are caught.
 *
 *              2) verifying that the page buffer behaves more or less 
 *                 as advertized.
 *
 *              Any data mis-matches or unexpected failures or successes
 *              reported by the HDF5 library result in test failure.
 *
 * Return:      0 if test is sucessful
 *              1 if test fails
 *
 * Programmer:  unknown
 *              ?? / ?? / ??
 *
 * Changes:     Minor updates to adapt to new implementation of page 
 *              buffer.
 *                                             JRM -- 10//26/18
 *
 *-------------------------------------------------------------------------
 */

#ifndef H5_HAVE_PARALLEL
static unsigned
test_args(hid_t orig_fapl, const char *env_h5_drvr)
{
    char filename[FILENAME_LEN]; /* Filename to use */
    hid_t file_id = -1;          /* File ID */
    hid_t fcpl = -1;
    hid_t fapl = -1;
    herr_t ret;

    TESTING("Settings for Page Buffering");

    h5_fixname(namebase, orig_fapl, filename, sizeof(filename));

    if((fapl = H5Pcopy(orig_fapl)) < 0) TEST_ERROR

    if((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
        TEST_ERROR;


    /* Test setting a page buffer without Paged Aggregation enabled - 
     * should fail 
     */
    if(H5Pset_page_buffer_size(fapl, 512, 0, 0) < 0)
        TEST_ERROR;

    H5E_BEGIN_TRY {
        file_id = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl);
    } H5E_END_TRY;

    if(file_id >= 0)
        TEST_ERROR;


    /* Test setting a page buffer with a size smaller than a single 
     * page size - should fail 
     */
    if(H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_PAGE, 0, 1) < 0)
        TEST_ERROR;

    if(H5Pset_file_space_page_size(fcpl, 512) < 0)
        TEST_ERROR;

    if(H5Pset_page_buffer_size(fapl, 511, 0, 0) < 0)
        TEST_ERROR;

    H5E_BEGIN_TRY {
        file_id = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl);
    } H5E_END_TRY;

    if(file_id >= 0)
        TEST_ERROR;


    /* Test setting a page buffer with sum of min meta and raw 
     * data percentage > 100 - should fail 
     */
    H5E_BEGIN_TRY {
        ret = H5Pset_page_buffer_size(fapl, 512, 50, 51);
    } H5E_END_TRY;

    if(ret >= 0)
        TEST_ERROR;

    if(set_multi_split(env_h5_drvr, fapl, 512)  != 0)
        TEST_ERROR;

    /* Test setting a page buffer with a size equal to a single page size */
    if(H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_PAGE, 0, 1) < 0)
        TEST_ERROR;

    if(H5Pset_file_space_page_size(fcpl, 512) < 0)
        TEST_ERROR;

    if(H5Pset_page_buffer_size(fapl, 512, 0, 0) < 0)
        TEST_ERROR;

    if(create_file(filename, fcpl, fapl) != 0)
        TEST_ERROR;

    if(open_file(filename, fapl, 512, 512) != 0)
        TEST_ERROR;


    /* Test setting a page buffer with a size slightly larger than a 
     * single page size 
     */
    if(H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_PAGE, 0, 1) < 0)
        TEST_ERROR;

    if(H5Pset_file_space_page_size(fcpl, 512) < 0)
        TEST_ERROR;

    if(H5Pset_page_buffer_size(fapl, 513, 0, 0) < 0)
        TEST_ERROR;

    if(create_file(filename, fcpl, fapl) != 0)
        TEST_ERROR;

    if(open_file(filename, fapl, 512, 512) != 0)
        TEST_ERROR;

    if(set_multi_split(env_h5_drvr, fapl, 4194304)  != 0)
        TEST_ERROR;


    /* Test setting a large page buffer size and page size */
    if(H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_PAGE, 0, 1) < 0)
        TEST_ERROR;

    if(H5Pset_file_space_page_size(fcpl, 4194304) < 0)
        TEST_ERROR;

    if(H5Pset_page_buffer_size(fapl, 16777216, 0, 0) < 0)
        TEST_ERROR;

    if(create_file(filename, fcpl, fapl) != 0)
        TEST_ERROR;

    if(open_file(filename, fapl, 4194304, 16777216) != 0)
        TEST_ERROR;

    if(set_multi_split(env_h5_drvr, fapl, 1)  != 0)
        TEST_ERROR;


    /* Test setting a 512 byte page buffer size and page size */
    if(H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_PAGE, 0, 1) < 0)
        TEST_ERROR;

    if(H5Pset_file_space_page_size(fcpl, 512) < 0)
        TEST_ERROR;
    if(H5Pset_page_buffer_size(fapl, 512, 0, 0) < 0)
        TEST_ERROR;
    if(create_file(filename, fcpl, fapl) != 0)
        TEST_ERROR;
    if(open_file(filename, fapl, 512, 512) != 0)
        TEST_ERROR;


    if(H5Pclose(fcpl) < 0)
        FAIL_STACK_ERROR;
    if(H5Pclose(fapl) < 0)
        FAIL_STACK_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(fapl);
        H5Pclose(fcpl);
    } H5E_END_TRY;
    return 1;
} /* test_args */
#endif


/*
 * Function:    test_mpmde_delay_basic()
 *
 * Purpose:     Check that a multi-page metadata entry
 *              (MPMDE) is not written immediately to the HDF5 file in
 *              VFD SWMR mode, but it is buffered in the shadow file
 *              until max_lag + 1 ticks have elapsed.  Furthermore,
 *              check that it appears *immediately* after max_lag + 1
 *              ticks, since the LRU list does not hold onto MPMDEs.
 *
 * Return:      0 if test is sucessful
 *              1 if test fails
 *
 * Programmer:  David Young
 *              16 Sep 2019
 */
static unsigned
test_mpmde_delay_basic(hid_t orig_fapl, const char *env_h5_drvr)
{
    char filename[FILENAME_LEN]; /* Filename to use */
    hid_t file_id = -1;          /* File ID */
    hid_t fcpl = -1;
    hid_t fapl = -1;
    size_t i, num_elements = 2000;
    int *data = NULL, *odata = NULL;
    H5F_t *f;
    const uint32_t max_lag = 5;
    hsize_t pgsz = sizeof(int) * 200;
    haddr_t addr;

    TESTING("Multipage Metadata Delay Handling");

    h5_fixname(namebase, orig_fapl, filename, sizeof(filename));

    if ((fapl = H5Pcopy(orig_fapl)) < 0)
        TEST_ERROR

    if (set_multi_split(env_h5_drvr, fapl, pgsz) != 0)
        TEST_ERROR;

    if ((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_PAGE, 0, 1) < 0)
        TEST_ERROR;
    if (H5Pset_file_space_page_size(fcpl, pgsz) < 0)
        TEST_ERROR;
    if (H5Pset_page_buffer_size(fapl, 10 * pgsz, 0, 0) < 0)
        TEST_ERROR;

    if (swmr_fapl_augment(fapl, filename, max_lag) < 0)
        TEST_ERROR;

    if ((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Get a pointer to the internal file object */
    if (NULL == (f = (H5F_t *)H5VL_object(file_id)))
        FAIL_STACK_ERROR;

    addr = H5MF_alloc(f, H5FD_MEM_BTREE, sizeof(int) * num_elements);
    /* allocate space for 2000 elements */
    if (HADDR_UNDEF == addr)
        FAIL_STACK_ERROR;

    if ((odata = (int *)HDcalloc(num_elements, sizeof(int))) == NULL)
        TEST_ERROR;

    if ((data = (int *)HDcalloc(num_elements, sizeof(int))) == NULL)
        TEST_ERROR;

    /* initialize all the elements to have a value of -1 */
    for(i = 0; i < num_elements; i++)
        odata[i] = -1;

    if (H5F_block_write(f, H5FD_MEM_BTREE, addr, sizeof(int) * num_elements,
           odata) < 0)
        FAIL_STACK_ERROR;

    /* H5Fvfd_swmr_end_tick() processes delayed writes before it increases
     * the tick number, so it takes `max_lag + 1` times through this loop
     * for a multi-page metadata write to make it to the HDF5 file.
     */
    for (i = 0; i < max_lag + 1; i++) {
        /* All elements read using the VFD should be 0. */
        if (!vfd_read_each_equals(f, H5FD_MEM_BTREE, addr, num_elements,
                data, 0))
            TEST_ERROR;
        H5Fvfd_swmr_end_tick(file_id);
    }

    /* It is not necessary to flush the page buffer because delayed
     * multi-page metadata buffers are flushed *immediately*
     * when their delay elapses.
     *
     * (If we were waiting for a single-page metadata buffer to
     * appear at the VFD layer, then it may reside in the LRU queue
     * for a while.)
     */
#if 0
    if (H5PB_flush(f) < 0)
        FAIL_STACK_ERROR;
#endif

    /* All elements read using the VFD should be -1. */
    if (!vfd_read_each_equals(f, H5FD_MEM_BTREE, addr, num_elements, data, -1))
        TEST_ERROR;

    if (H5Fclose(file_id) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(fcpl) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(fapl) < 0)
        FAIL_STACK_ERROR;
    HDfree(data);
    HDfree(odata);

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        if (fapl != H5I_INVALID_HID)
            H5Pclose(fapl);
        if (fcpl != H5I_INVALID_HID)
            H5Pclose(fcpl);
        if (file_id != H5I_INVALID_HID)
            H5Fclose(file_id);
        if (data != NULL)
            HDfree(data);
        if (odata != NULL)
            HDfree(odata);
    } H5E_END_TRY;
    return 1;
}


/*
 * Function:    test_spmde_lru_evict_basic()
 *
 * Purpose:     Check that once a single-page metadata entry
 *              (SPMDE) is eligible to be written to the HDF5 file
 *              (because it has resided unchanged in the metadata file
 *              for max_lag + 1 ticks), filling the page buffer to
 *              capacity causes the entry to be flushed.
 *
 *              Further check that the page was evicted by writing
 *              changes to the VFD layer ("under" the page buffer)
 *              and trying to read the changes back through the page
 *              buffer: stale page-buffer content should not shadow
 *              the changes.
 *
 *              XXX
 *              XXX reduce duplication with test_spmde_delay_basic!
 *              XXX
 *
 * Return:      0 if test is sucessful
 *              1 if test fails
 *
 * Programmer:  David Young
 *              16 Sep 2019
 */
static unsigned
test_spmde_lru_evict_basic(hid_t orig_fapl, const char *env_h5_drvr)
{
    char filename[FILENAME_LEN]; /* Filename to use */
    hid_t file_id = -1;          /* File ID */
    hid_t fcpl = -1;
    hid_t fapl = -1;
    size_t i, num_elements = 20;
    int *data = NULL, *odata = NULL;
    H5F_t *f;
    const uint32_t max_lag = 5;
    const hsize_t pgsz = sizeof(int) * 200;
    const hsize_t pgbufsz = 10 * pgsz;
    hsize_t ofs;
    bool flushed;
    struct timespec last;
    haddr_t addr;
    haddr_t pressure;

    TESTING("Single Page Metadata Flush & Eviction Handling");

    h5_fixname(namebase, orig_fapl, filename, sizeof(filename));

    last = print_elapsed_time(NULL, __func__, __LINE__);
    if ((fapl = H5Pcopy(orig_fapl)) < 0)
        TEST_ERROR

    if (set_multi_split(env_h5_drvr, fapl, pgsz) != 0)
        TEST_ERROR;

    if ((fcpl = paging_fcpl_create(pgsz)) < 0)
        FAIL_STACK_ERROR;

    if (H5Pset_page_buffer_size(fapl, pgbufsz, 0, 0) < 0)
        FAIL_STACK_ERROR;

    if (swmr_fapl_augment(fapl, filename, max_lag) < 0)
        FAIL_STACK_ERROR;

    if ((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Get a pointer to the internal file object */
    if (NULL == (f = (H5F_t *)H5VL_object(file_id)))
        FAIL_STACK_ERROR;

    last = print_elapsed_time(&last, __func__, __LINE__);

    /* Allocate a region whose pages we can write to force eviction of
     * least-recently used pages.
     */
    pressure = H5MF_alloc(f, H5FD_MEM_BTREE, pgbufsz);
    if (HADDR_UNDEF == pressure)
        FAIL_STACK_ERROR;

    /* Allocate a whole page for our 20 elements so that they do
     * not share a "hot" page with something else.
     */
    addr = H5MF_alloc(f, H5FD_MEM_BTREE, pgsz);
    if (HADDR_UNDEF == addr)
        FAIL_STACK_ERROR;

    if ((odata = (int *)HDcalloc(num_elements, sizeof(int))) == NULL)
        TEST_ERROR;

    if ((data = (int *)HDcalloc(num_elements, sizeof(int))) == NULL)
        TEST_ERROR;

    /* initialize all the elements to have a value of -1 */
    for(i = 0; i < num_elements; i++)
        odata[i] = -1;

    if (H5F_block_write(f, H5FD_MEM_BTREE, addr, sizeof(int) * num_elements,
           odata) < 0)
        FAIL_STACK_ERROR;

    last = print_elapsed_time(&last, __func__, __LINE__);

    /* H5Fvfd_swmr_end_tick() processes delayed writes before it increases
     * the tick number, so only after `max_lag + 1` times through this loop
     * is a metadata write eligible to be written to the HDF5 file.
     */
    for (i = 0; i < max_lag + 1; i++) {
        /* All elements read using the VFD should be 0. */
        if (!vfd_read_each_equals(f, H5FD_MEM_BTREE, addr, num_elements,
                data, 0))
            TEST_ERROR;
        H5Fvfd_swmr_end_tick(file_id);
    }

    last = print_elapsed_time(&last, __func__, __LINE__);

    flushed = false;
    /* We are waiting for a single-page metadata buffer to
     * appear at the VFD layer, but it may reside in the LRU queue.
     * Dirty new blocks to apply pressure on the page buffer so that
     * it empties the LRU queue.  Writing to N distinct pages,
     * for N the number of pages in the page buffer, ought to do
     * the trick.
     */
    for (ofs = 0; ofs < pgbufsz - pgsz * 2; ofs += pgsz) {
        int tmp = -1;
        if (H5F_block_write(f, H5FD_MEM_BTREE, pressure + ofs,
                sizeof(tmp), &tmp) < 0)
            FAIL_STACK_ERROR;
        if (!flushed &&
            vfd_read_each_equals(f, H5FD_MEM_BTREE, addr, num_elements,
                data, -1)) {
            flushed = true;
#if 0
            printf("Writing page %" PRIuHSIZE " flushed target page.\n",
                ofs / pgsz);
#endif
        }
    }

    last = print_elapsed_time(&last, __func__, __LINE__);

    if (!vfd_read_each_equals(f, H5FD_MEM_BTREE, addr, num_elements, data, -1))
        TEST_ERROR;

    /* initialize all the elements to have a value of -2 */
    for(i = 0; i < num_elements; i++)
        odata[i] = -2;
    /* Write -2 to our target page using the VFD. */
    if (H5FD_write(f->shared->lf, H5FD_MEM_BTREE, addr,
            sizeof(int) * num_elements, odata) < 0)
        FAIL_STACK_ERROR;

    last = print_elapsed_time(&last, __func__, __LINE__);

    /* All elements read through the page buffer should be -2.  That is,
     * no page-buffer entry should shadow the page.
     */
    if (!pgbuf_read_each_equals(f, H5FD_MEM_BTREE, addr, num_elements, data,
            -2))
        TEST_ERROR;

    last = print_elapsed_time(&last, __func__, __LINE__);

    /* Force ticks to occur so that H5Fclose() doesn't pause waiting
     * for them to elapse.
     */
    for (i = 0; i < max_lag + 1; i++)
        H5Fvfd_swmr_end_tick(file_id);

    if (H5Fclose(file_id) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(fcpl) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(fapl) < 0)
        FAIL_STACK_ERROR;
    HDfree(data);
    HDfree(odata);

    last = print_elapsed_time(&last, __func__, __LINE__);

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        if (fapl != H5I_INVALID_HID)
            H5Pclose(fapl);
        if (fcpl != H5I_INVALID_HID)
            H5Pclose(fcpl);
        if (file_id != H5I_INVALID_HID)
            H5Fclose(file_id);
        if (data != NULL)
            HDfree(data);
        if (odata != NULL)
            HDfree(odata);
    } H5E_END_TRY;
    return 1;
}


/*
 * Function:    test_spmde_delay_basic()
 *
 * Purpose:     Check that a single-page metadata entry
 *              (SPMDE) is not written immediately to the HDF5 file in
 *              VFD SWMR mode, but it is buffered in the shadow file
 *              until max_lag + 1 ticks have elapsed.
 *
 *              The LRU list will hold onto SPMDEs, so it's necessary to
 *              flush the page buffer to make sure the buffer is flushed
 *              to the VFD layer.
 *
 *              XXX
 *              XXX reduce duplication with test_mpmde_delay_basic!
 *              XXX
 *
 * Return:      0 if test is sucessful
 *              1 if test fails
 *
 * Programmer:  David Young
 *              16 Sep 2019
 */
static unsigned
test_spmde_delay_basic(hid_t orig_fapl, const char *env_h5_drvr)
{
    char filename[FILENAME_LEN]; /* Filename to use */
    hid_t file_id = -1;          /* File ID */
    hid_t fcpl = -1;
    hid_t fapl = -1;
    size_t i, num_elements = 20;
    int *data = NULL, *odata = NULL;
    H5F_t *f;
    const uint32_t max_lag = 5;
    hsize_t pgsz = sizeof(int) * 200;
    haddr_t addr;

    TESTING("Single Page Metadata Delay Handling");

    h5_fixname(namebase, orig_fapl, filename, sizeof(filename));

    if ((fapl = H5Pcopy(orig_fapl)) < 0)
        TEST_ERROR

    if (set_multi_split(env_h5_drvr, fapl, pgsz) != 0)
        TEST_ERROR;

    if ((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_PAGE, 0, 1) < 0)
        TEST_ERROR;
    if (H5Pset_file_space_page_size(fcpl, pgsz) < 0)
        TEST_ERROR;
    if (H5Pset_page_buffer_size(fapl, 10 * pgsz, 0, 0) < 0)
        TEST_ERROR;

    if (swmr_fapl_augment(fapl, filename, max_lag) < 0)
        TEST_ERROR;

    if ((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Get a pointer to the internal file object */
    if (NULL == (f = (H5F_t *)H5VL_object(file_id)))
        FAIL_STACK_ERROR;

    addr = H5MF_alloc(f, H5FD_MEM_BTREE,
        sizeof(int) * num_elements);
    /* allocate space for 2000 elements */
    if (HADDR_UNDEF == addr)
        FAIL_STACK_ERROR;

    if ((odata = (int *)HDcalloc(num_elements, sizeof(int))) == NULL)
        TEST_ERROR;

    if ((data = (int *)HDcalloc(num_elements, sizeof(int))) == NULL)
        TEST_ERROR;

    /* initialize all the elements to have a value of -1 */
    for(i = 0; i < num_elements; i++)
        odata[i] = -1;

    if (H5F_block_write(f, H5FD_MEM_BTREE, addr, sizeof(int) * num_elements,
           odata) < 0)
        FAIL_STACK_ERROR;

    /* H5Fvfd_swmr_end_tick() processes delayed writes before it increases
     * the tick number, so only after `max_lag + 1` times through this loop
     * is a metadata write eligible to be written to the HDF5 file.
     */
    for (i = 0; i < max_lag + 1; i++) {
        /* All elements read using the VFD should be 0. */
        if (!vfd_read_each_equals(f, H5FD_MEM_BTREE, addr, num_elements,
                data, 0))
            TEST_ERROR;
        H5Fvfd_swmr_end_tick(file_id);
    }

    /* We are waiting for a single-page metadata buffer to
     * appear at the VFD layer, but it may reside in the LRU queue
     * for a while if we do not flush the page buffer.
     */
    if (H5PB_flush(f->shared) < 0)
        FAIL_STACK_ERROR;

    /* All elements read using the VFD should be -1. */
    if (!vfd_read_each_equals(f, H5FD_MEM_BTREE, addr, num_elements, data, -1))
        TEST_ERROR;

    if (H5Fclose(file_id) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(fcpl) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(fapl) < 0)
        FAIL_STACK_ERROR;
    HDfree(data);
    HDfree(odata);

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        if (fapl != H5I_INVALID_HID)
            H5Pclose(fapl);
        if (fcpl != H5I_INVALID_HID)
            H5Pclose(fcpl);
        if (file_id != H5I_INVALID_HID)
            H5Fclose(file_id);
        if (data != NULL)
            HDfree(data);
        if (odata != NULL)
            HDfree(odata);
    } H5E_END_TRY;
    return 1;
}


/*
 * Function:    test_raw_data_handling()
 *
 * Purpose:  Check that raw data is written to the HDF5
 *           file when expected whether in VFD SWMR mode or not.
 *
 *           Any data mis-matches or failures reported by the HDF5
 *           library result in test failure.
 *
 * Return:      0 if test is sucessful
 *              1 if test fails
 *
 * Programmer:  David Young
 *              16 Sep 2019
 *
 * Changes:     Added base_page_cnt field as supporting code.  This allows
 *              the test to adjust to the number of page buffer pages
 *              accessed during file open / create.
 *
 *              The test for the value of base_page_cnt just after file 
 *              open exists detect changes in library behavior.  Assuming
 *              any such change is not indicative of other issues, these 
 *              tests can be modified to reflect the change.
 *
 *                                                    JRM -- 2/23/17
 *
 *              Minor changes to adapt to re-implementation of the 
 *              page buffer.
 *
 *                                                    JRM -- 10/26/18
 */

/* Changes due to file space page size has a minimum size of 512 */
#ifndef H5_HAVE_PARALLEL
static unsigned
test_raw_data_handling(hid_t orig_fapl, const char *env_h5_drvr,
    bool vfd_swmr_mode)
{
    char filename[FILENAME_LEN]; /* Filename to use */
    hid_t file_id = -1;          /* File ID */
    hid_t fcpl = -1;
    hid_t fapl = -1;
    int64_t base_page_cnt;
    int64_t page_count = 0;
    int i, num_elements = 2000;
    haddr_t addr = HADDR_UNDEF;
    int *data = NULL;
    H5F_t *f = NULL;
    const uint32_t max_lag = 5;

    TESTING("%sRaw Data Handling", vfd_swmr_mode ? "VFD SWMR " : "");

    h5_fixname(namebase, orig_fapl, filename, sizeof(filename));

    if ((fapl = H5Pcopy(orig_fapl)) < 0)
        TEST_ERROR

    if (set_multi_split(env_h5_drvr, fapl, sizeof(int) * 200) != 0)
        TEST_ERROR;

    if ((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_PAGE, 0, 1) < 0)
        TEST_ERROR;
    if (H5Pset_file_space_page_size(fcpl, sizeof(int) * 200) < 0)
        TEST_ERROR;
    if (H5Pset_page_buffer_size(fapl, sizeof(int) * 2000, 0, 0) < 0)
        TEST_ERROR;

    if (vfd_swmr_mode && swmr_fapl_augment(fapl, filename, max_lag) < 0)
        TEST_ERROR;
    if ((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Get a pointer to the internal file object */
    if(NULL == (f = (H5F_t *)H5VL_object(file_id)))
        FAIL_STACK_ERROR;

    /* opening the file inserts one or more pages into the page buffer.
     * Get the number of pages inserted, and verify that it is the 
     * expected value.
     */
    base_page_cnt = f->shared->pb_ptr->curr_pages;
    if (base_page_cnt != 2)
        TEST_ERROR;

    /* allocate space for 2000 elements */
    if (HADDR_UNDEF == (addr = H5MF_alloc(f, H5FD_MEM_DRAW, sizeof(int) * (size_t)num_elements)))
        FAIL_STACK_ERROR;

    if ((data = (int *)HDcalloc((size_t)num_elements, sizeof(int))) == NULL)
        TEST_ERROR;

    /* initialize all the elements to have a value of -1 */
    for(i=0 ; i<num_elements ; i++)
        data[i] = -1;
    if (H5F_block_write(f, H5FD_MEM_DRAW, addr, sizeof(int) * (size_t)num_elements, data) < 0)
        FAIL_STACK_ERROR;

    /* update the first 100 elements to have values 0-99 - this will be
       a page buffer update with 1 page resulting in the page
       buffer. */
    for(i=0 ; i<100 ; i++)
        data[i] = i;

    if (H5F_block_write(f, H5FD_MEM_DRAW, addr, sizeof(int) * 100, data) < 0)
        FAIL_STACK_ERROR;

    page_count ++;

    if (f->shared->pb_ptr->curr_pages != page_count + base_page_cnt)
        FAIL_STACK_ERROR;

    /* update elements 300 - 450, with values 300 -  - this will
       bring two more pages into the page buffer. */
    for(i=0 ; i<150 ; i++)
        data[i] = i+300;
    if (H5F_block_write(f, H5FD_MEM_DRAW, addr + (sizeof(int) * 300), sizeof(int) * 150, data) < 0)
        FAIL_STACK_ERROR;
    page_count += 2;
    if (f->shared->pb_ptr->curr_pages != page_count + base_page_cnt)
        FAIL_STACK_ERROR;

    /* update elements 100 - 300, this will go to disk but also update
       existing pages in the page buffer. */
    for(i=0 ; i<200 ; i++)
        data[i] = i+100;
    if (H5F_block_write(f, H5FD_MEM_DRAW, addr + (sizeof(int) * 100), sizeof(int) * 200, data) < 0)
        FAIL_STACK_ERROR;
    if (f->shared->pb_ptr->curr_pages != page_count + base_page_cnt)
        FAIL_STACK_ERROR;

    /* Update elements 225-300 - this will update an existing page in the PB */
    /* Changes: 450 - 600; 150 */
    for(i=0 ; i<150 ; i++)
        data[i] = i+450;
    if (H5F_block_write(f, H5FD_MEM_DRAW, addr + (sizeof(int) * 450), sizeof(int) * 150, data) < 0)
        FAIL_STACK_ERROR;
    if (f->shared->pb_ptr->curr_pages != page_count + base_page_cnt)
        FAIL_STACK_ERROR;

    /* Do a full page write to block 600-800 - should bypass the PB */
    for(i=0 ; i<200 ; i++)
        data[i] = i+600;
    if (H5F_block_write(f, H5FD_MEM_DRAW, addr + (sizeof(int) * 600), sizeof(int) * 200, data) < 0)
        FAIL_STACK_ERROR;
    if (f->shared->pb_ptr->curr_pages != page_count + base_page_cnt)
        FAIL_STACK_ERROR;

    /* read elements 800 - 1200, this should not affect the PB, and should read -1s */
    if (H5F_block_read(f, H5FD_MEM_DRAW, addr + (sizeof(int) * 800), sizeof(int) * 400, data) < 0)
        FAIL_STACK_ERROR;
    for (i=0; i < 400; i++) {
        if (data[i] != -1) {
            HDfprintf(stderr, "Read different values than written\n");
            HDfprintf(stderr, "data[%d] = %d, %d expected.\n", i, data[i], -1);
            FAIL_STACK_ERROR;
        }
    }
    if (f->shared->pb_ptr->curr_pages != page_count + base_page_cnt)
        FAIL_STACK_ERROR;

    /* read elements 1200 - 1201, this should read -1 and bring in an 
     * entire page of addr 1200 
     */
    if (H5F_block_read(f, H5FD_MEM_DRAW, addr + (sizeof(int) * 1200), sizeof(int) * 1, data) < 0)
        FAIL_STACK_ERROR;
    for (i=0; i < 1; i++) {
        if (data[i] != -1) {
            HDfprintf(stderr, "Read different values than written\n");
            HDfprintf(stderr, "data[%d] = %d, %d expected.\n", i, data[i], -1);
            TEST_ERROR;
        }
    }
    page_count ++;
    if (f->shared->pb_ptr->curr_pages != page_count + base_page_cnt)
        TEST_ERROR;

    /* read elements 175 - 225, this should use the PB existing pages */
    /* Changes: 350 - 450 */
    /* read elements 175 - 225, this should use the PB existing pages */
    if (H5F_block_read(f, H5FD_MEM_DRAW, addr + (sizeof(int) * 350), sizeof(int) * 100, data) < 0)
        FAIL_STACK_ERROR;
    for (i=0; i < 100; i++) {
        if (data[i] != i + 350) {
            HDfprintf(stderr, "Read different values than written\n");
            HDfprintf(stderr, "data[%d] = %d, %d expected.\n", i, data[i], 
                      i + 350);
            TEST_ERROR;
        }
    }
    if (f->shared->pb_ptr->curr_pages != page_count + base_page_cnt)
        TEST_ERROR;

    /* read elements 0 - 800 using the VFD.. this should result in -1s
       except for the writes that went through the PB (100-300 & 600-800) */
    if (H5FD_read(f->shared->lf, H5FD_MEM_DRAW, addr, sizeof(int) * 800, data) < 0)
        FAIL_STACK_ERROR;
    i = 0;
    while (i < 800) {
        if((i>=100 && i<300) || i >= 600) {
            if (data[i] != i) {
                HDfprintf(stderr, "Read different values than written\n");
                HDfprintf(stderr, "data[%d] = %d, %d expected.\n", 
                          i, data[i], i);
                TEST_ERROR;
            }
        }
        else {
            if (data[i] != -1) {
                HDfprintf(stderr, "Read different values than written\n");
                HDfprintf(stderr, "data[%d] = %d, %d expected.\n", 
                          i, data[i], -1);
                TEST_ERROR;
            }
        }
        i++;
    }

    /* read elements 0 - 800 using the PB.. this should result in all
     * what we have written so far and should get the updates from the PB 
     */
    if (H5F_block_read(f, H5FD_MEM_DRAW, addr, sizeof(int) * 800, data) < 0)
        FAIL_STACK_ERROR;
    if (f->shared->pb_ptr->curr_pages != page_count + base_page_cnt)
        TEST_ERROR;
    for (i=0; i < 800; i++) {
        if (data[i] != i) {
            HDfprintf(stderr, "Read different values than written\n");
                HDfprintf(stderr, "data[%d] = %d, %d expected.\n", 
                          i, data[i], i);
            TEST_ERROR;
        }
    }

    /* update elements 400 - 1400 to value 0, this will go to disk but
     * also evict existing pages from the PB (page 400 & 1200 that are
     * existing). 
     */
    for(i=0 ; i<1000 ; i++)
        data[i] = 0;
    if (H5F_block_write(f, H5FD_MEM_DRAW, addr + (sizeof(int) * 400), sizeof(int) * 1000, data) < 0)
        FAIL_STACK_ERROR;
    page_count -= 2;
    if (f->shared->pb_ptr->curr_pages != page_count + base_page_cnt)
        TEST_ERROR;

    /* read elements 0 - 1000.. this should go to disk then update the
     * buffer result 200-400 with existing pages
     */
    if (H5F_block_read(f, H5FD_MEM_DRAW, addr, sizeof(int) * 1000, data) < 0)
        FAIL_STACK_ERROR;
    i=0;
    while (i < 1000) {
        if(i<400) {
            if (data[i] != i) {
                HDfprintf(stderr, "Read different values than written\n");
                HDfprintf(stderr, "data[%d] = %d, %d expected.\n", 
                          i, data[i], i);
                TEST_ERROR;
            }
        }
        else {
            if (data[i] != 0) {
                HDfprintf(stderr, "Read different values than written\n");
                HDfprintf(stderr, "data[%d] = %d, %d expected.\n", 
                          i, data[i], 0);
                TEST_ERROR;
            }
        }
        i++;
    }
    if (f->shared->pb_ptr->curr_pages != page_count + base_page_cnt)
        TEST_ERROR;

    if (H5Fclose(file_id) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(fcpl) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(fapl) < 0)
        FAIL_STACK_ERROR;
    HDfree(data);

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        if (fapl != H5I_INVALID_HID)
            H5Pclose(fapl);
        if (fcpl != H5I_INVALID_HID)
            H5Pclose(fcpl);
        if (file_id != H5I_INVALID_HID)
            H5Fclose(file_id);
        if (data != NULL)
            HDfree(data);
    } H5E_END_TRY;
    return 1;
} /* test_raw_data_handling */
#endif


/*-------------------------------------------------------------------------
 * Function:    test_lru_processing()
 *
 * Purpose:     Basic set of tests verifying expected page buffer LRU
 *              management.
 *
 *              Any data mis-matches or failures reported by the HDF5
 *              library result in test failure.
 *
 * Return:      0 if test is sucessful
 *              1 if test fails
 *
 * Programmer:  unknown
 *              ?? / ?? / ??
 *              
 * Changes:     Reworked for new implementation of page buffer.  Major 
 *              change was adaption to the new implementation's greater 
 *              respect for max_pages.
 *
 *                                                    JRM -- 10/26/18
 *
 *
 *-------------------------------------------------------------------------
 */

#ifndef H5_HAVE_PARALLEL
static unsigned
test_lru_processing(hid_t orig_fapl, const char *env_h5_drvr)
{
    char filename[FILENAME_LEN]; /* Filename to use */
    hbool_t page_exists;
    hid_t file_id = -1;          /* File ID */
    hid_t fcpl = -1;
    hid_t fapl = -1;
    int i;
    int num_elements = 2000;
    haddr_t addr = HADDR_UNDEF;
    haddr_t search_addr = HADDR_UNDEF;
    int *data = NULL;
    H5F_t *f = NULL;

    TESTING("LRU Processing");

    h5_fixname(namebase, orig_fapl, filename, sizeof(filename));

    if((fapl = H5Pcopy(orig_fapl)) < 0)
        FAIL_STACK_ERROR

    if(set_multi_split(env_h5_drvr, fapl, sizeof(int)*200)  != 0)
        TEST_ERROR;

    if((data = (int *)HDcalloc((size_t)num_elements, sizeof(int))) == NULL)
        TEST_ERROR;

    if((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
        FAIL_STACK_ERROR;

    if(H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_PAGE, 0, 1) < 0)
        FAIL_STACK_ERROR;

    if(H5Pset_file_space_page_size(fcpl, sizeof(int)*200) < 0)
        FAIL_STACK_ERROR;

    /* keep 2 pages at max in the page buffer */
    if(H5Pset_page_buffer_size(fapl, sizeof(int)*400, 20, 0) < 0)
        FAIL_STACK_ERROR;

    if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Get a pointer to the internal file object */
    if(NULL == (f = (H5F_t *)H5VL_object(file_id)))
        FAIL_STACK_ERROR;

    /* allocate space for 2000 elements */
    if(HADDR_UNDEF == (addr = H5MF_alloc(f, H5FD_MEM_DRAW, 
                                         sizeof(int)*(size_t)num_elements)))
        FAIL_STACK_ERROR;

    /* initialize all the elements to have a value of -1 */
    for(i=0 ; i<num_elements ; i++)
        data[i] = -1;

    if(H5F_block_write(f, H5FD_MEM_DRAW, addr, 
                       sizeof(int)*(size_t)num_elements, data) < 0)
        FAIL_STACK_ERROR;

    /* there should be no raw data pages in the page buffer -- verify this */
    if (f->shared->pb_ptr->curr_rd_pages != 0)
        FAIL_STACK_ERROR;

    /* update the first 100 elements to have values 0-99 - this will be
     * a page buffer update that loads page addr + 0 into the page buffer.
     */
    for(i=0 ; i<100 ; i++)
        data[i] = i;

    if(H5F_block_write(f, H5FD_MEM_DRAW, addr, sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    /* verify addr + 0 is the only raw data page in the page buffer */
    search_addr = addr;
    if((H5PB_page_exists(f->shared, search_addr, &page_exists) < 0) || (!page_exists))
        FAIL_STACK_ERROR;
    if (f->shared->pb_ptr->curr_rd_pages != 1)
        FAIL_STACK_ERROR;


    /* update elements 300 - 450, with values 300 - 449 - this will
     * bring two pages (addr + 200 & addr + 400) into the page buffer and 
     * evict addr + 0. 
     */
    for(i=0 ; i<150 ; i++)
        data[i] = i+300;

    if(H5F_block_write(f, H5FD_MEM_DRAW, addr+(sizeof(int)*300), 
                       sizeof(int)*150, data) < 0)
        FAIL_STACK_ERROR;

    /* verify that addr + 200 and addr + 400 are the only raw data pages in
     * the page buffer.
     */
    search_addr = addr + sizeof(int)*200;
    if((H5PB_page_exists(f->shared, search_addr, &page_exists) < 0) || (!page_exists))
        FAIL_STACK_ERROR;
    search_addr = addr + sizeof(int)*400;
    if((H5PB_page_exists(f->shared, search_addr, &page_exists) < 0) || (!page_exists))
        FAIL_STACK_ERROR;
    if (f->shared->pb_ptr->curr_rd_pages != 2)
        FAIL_STACK_ERROR;

    /* at this point, the page buffer entries created at file open should
     * have been evicted.
     */
    if(f->shared->pb_ptr->curr_md_pages != 0)
        FAIL_STACK_ERROR;


    /* update elements 300-301, this will update page addr + 200 in 
     * page buffer and move it to the top of the LRU. 
     */
    for(i=0 ; i<1 ; i++)
        data[i] = i+300;
    if(H5F_block_write(f, H5FD_MEM_DRAW, addr+(sizeof(int)*300), 
                       sizeof(int)*2, data) < 0)
        FAIL_STACK_ERROR;

    /* verify that addr + 200 and addr + 400 are the only raw data pages in
     * the page buffer.
     */
    search_addr = addr + sizeof(int)*200;
    if((H5PB_page_exists(f->shared, search_addr, &page_exists) < 0) || (!page_exists))
        FAIL_STACK_ERROR;
    search_addr = addr + sizeof(int)*400;
    if((H5PB_page_exists(f->shared, search_addr, &page_exists) < 0) || (!page_exists))
        FAIL_STACK_ERROR;
    if (f->shared->pb_ptr->curr_rd_pages != 2)
        FAIL_STACK_ERROR;
    if(f->shared->pb_ptr->curr_pages != 2)
        FAIL_STACK_ERROR;

    /* read elements 1200 - 1201, this should read -1, bring in page 
     * addr + 1200, and evict page addr + 400
     */
    if(H5F_block_read(f, H5FD_MEM_DRAW, addr+(sizeof(int)*1200), 
                      sizeof(int)*1, data) < 0)
        FAIL_STACK_ERROR;
    for (i=0; i < 1; i++) {
        if(data[i] != -1) {
            HDfprintf(stderr, "Read different values than written\n");
            HDfprintf(stderr, "data[%d] = %d, %d expected.\n", i, data[i], -1);
            TEST_ERROR;
        }
    }

    /* verify that addr + 200 and addr + 1200 are the only raw data pages in
     * the page buffer.
     */
    search_addr = addr + sizeof(int)*200;
    if((H5PB_page_exists(f->shared, search_addr, &page_exists) < 0) || (!page_exists))
        FAIL_STACK_ERROR;
    search_addr = addr + sizeof(int)*1200;
    if((H5PB_page_exists(f->shared, search_addr, &page_exists) < 0) || (!page_exists))
        FAIL_STACK_ERROR;
    if (f->shared->pb_ptr->curr_rd_pages != 2)
        FAIL_STACK_ERROR;
    if(f->shared->pb_ptr->curr_pages != 2)
        FAIL_STACK_ERROR;

    /* read elements 350 - 450, this should load page addr + 400 and move
     * it to the top of the LRU, and evict page addr + 1200.
     */ 
    if(H5F_block_read(f, H5FD_MEM_DRAW, addr+(sizeof(int)*350), 
                      sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;
    for (i=0; i < 100; i++) {
        if(data[i] != i+350) {
            HDfprintf(stderr, "Read different values than written\n");
            HDfprintf(stderr, "data[%d] = %d, %d expected.\n", i, data[i], 
                      i + 350);
            TEST_ERROR;
        }
    }

    /* verify that addr + 200 and addr + 400 are the only raw data pages in
     * the page buffer.
     */
    search_addr = addr + sizeof(int)*200;
    if((H5PB_page_exists(f->shared, search_addr, &page_exists) < 0) || (!page_exists))
        FAIL_STACK_ERROR;
    search_addr = addr + sizeof(int)*400;
    if((H5PB_page_exists(f->shared, search_addr, &page_exists) < 0) || (!page_exists))
        FAIL_STACK_ERROR;
    if (f->shared->pb_ptr->curr_rd_pages != 2)
        FAIL_STACK_ERROR;
    if(f->shared->pb_ptr->curr_pages != 2)
        FAIL_STACK_ERROR;


    /* update elements 400 - 1400 to value 0, this will overwrite and 
     * evict page addr + 400.
     */
    for(i=0 ; i<1000 ; i++)
        data[i] = 0;
    if(H5F_block_write(f, H5FD_MEM_DRAW, addr+(sizeof(int)*400), 
                       sizeof(int)*1000, data) < 0)
        FAIL_STACK_ERROR;

    /* verify that addr + 200 is the only raw data page in the page buffer.
     */
    search_addr = addr + sizeof(int)*200;
    if((H5PB_page_exists(f->shared, search_addr, &page_exists) < 0) || (!page_exists))
        FAIL_STACK_ERROR;
    if (f->shared->pb_ptr->curr_rd_pages != 1)
        FAIL_STACK_ERROR;
    if(f->shared->pb_ptr->curr_pages != 1)
        FAIL_STACK_ERROR;

    if(H5Fclose(file_id) < 0)
        FAIL_STACK_ERROR;
    if(H5Pclose(fcpl) < 0)
        FAIL_STACK_ERROR;
    if(H5Pclose(fapl) < 0)
        FAIL_STACK_ERROR;
    HDfree(data);

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        if (fapl != H5I_INVALID_HID)
            H5Pclose(fapl);
        if (fcpl != H5I_INVALID_HID)
            H5Pclose(fcpl);
        if (file_id != H5I_INVALID_HID)
            H5Fclose(file_id);
        if (data != NULL)
            HDfree(data);
    } H5E_END_TRY;
    return 1;
} /* test_lru_processing */
#endif


/*-------------------------------------------------------------------------
 * Function:    test_min_threshold()
 *
 * Purpose:     Tests verifying observation of minimum and maximum 
 *              raw and metadata page counts in the page buffer.
 *
 *              Any data mis-matches or failures reported by the HDF5
 *              library result in test failure.
 *
 * Return:      0 if test is sucessful
 *              1 if test fails
 *
 * Programmer:  unknown
 *              ?? / ?? / ??
 *              
 * Changes:     Added the base_raw_cnt and base_meta_cnt fields and
 *              supporting code.  This allows the test to adjust to the 
 *              number of page buffer pages accessed during file open / 
 *              create.
 *
 *              The tests for the values of base_raw_cnt and base_meta_cnt
 *              just after file open exist detect changes in library 
 *              behavior.  Assuming any such change is not indicative of 
 *              other issues, these tests can be modified to reflect the 
 *              change.
 *
 *                                                    JRM -- 2/23/17
 *
 *              Reworked test for new implementatin of the page buffer.
 *              The major change was adapting the test for the new 
 *              page buffers refusal to buffer any raw data when
 *              min_md_pages == max_pages, or any metadata pages wwhen 
 *              min_rd_pages == max_pages.
 *
 *                                                    JRM -- 10/27/18
 *
 *-------------------------------------------------------------------------
 */

#ifndef H5_HAVE_PARALLEL
static unsigned
test_min_threshold(hid_t orig_fapl, const char *env_h5_drvr)
{
    char filename[FILENAME_LEN]; /* Filename to use */
    hid_t file_id = -1;          /* File ID */
    hid_t fcpl = -1;
    hid_t fapl = -1;
    int64_t base_raw_cnt = 0;
    int64_t base_meta_cnt = 0;
    int i;
    int num_elements = 1000;
    H5PB_t *pb_ptr;
    haddr_t meta_addr = HADDR_UNDEF;
    haddr_t raw_addr = HADDR_UNDEF;
    int *data = NULL;
    H5F_t *f = NULL;

    TESTING("Minimum Metadata threshold Processing");
    HDprintf("\n");
    h5_fixname(namebase, orig_fapl, filename, sizeof(filename));

    if((fapl = H5Pcopy(orig_fapl)) < 0)
        TEST_ERROR

    if(set_multi_split(env_h5_drvr, fapl, sizeof(int)*200)  != 0)
        TEST_ERROR;

    if((data = (int *)HDcalloc((size_t)num_elements, sizeof(int))) == NULL)
        TEST_ERROR;

    if((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
        FAIL_STACK_ERROR;

    if(H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_PAGE, 0, 1) < 0)
        FAIL_STACK_ERROR;

    if(H5Pset_file_space_page_size(fcpl, sizeof(int)*200) < 0)
        FAIL_STACK_ERROR;

    HDprintf("\tMinimum metadata threshold = 100%%\n");

    /* keep 5 pages at max in the page buffer and 5 meta page minimum */
    if(H5Pset_page_buffer_size(fapl, sizeof(int)*1000, 100, 0) < 0)
        FAIL_STACK_ERROR;

    /* create the file */
    if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Get a pointer to the internal file object */
    if(NULL == (f = (H5F_t *)H5VL_object(file_id)))
        FAIL_STACK_ERROR;

    /* opening the file inserts one or more pages into the page buffer.
     * Get the raw and meta counts now, so we can adjust tests accordingly.
     */
    HDassert(f);
    HDassert(f->shared);
    HDassert(f->shared->pb_ptr);

    base_raw_cnt = f->shared->pb_ptr->curr_rd_pages;
    base_meta_cnt = f->shared->pb_ptr->curr_md_pages;

    if(base_raw_cnt != 0)
        TEST_ERROR;

    if(base_meta_cnt != 2)
        TEST_ERROR;

    pb_ptr = f->shared->pb_ptr;

    if(pb_ptr->min_md_pages != 5)
        TEST_ERROR;

    if(pb_ptr->min_rd_pages != 0)
        TEST_ERROR;

    if(HADDR_UNDEF == (meta_addr = H5MF_alloc(f, H5FD_MEM_SUPER, 
                                            sizeof(int)*(size_t)num_elements)))
        FAIL_STACK_ERROR;

    if(HADDR_UNDEF == (raw_addr = H5MF_alloc(f, H5FD_MEM_DRAW, 
                                            sizeof(int)*(size_t)num_elements)))
        FAIL_STACK_ERROR;

    /* write all raw data.  Since min_md_pages == max_pages, none of it 
     * should end up in the page buffer. 
     */
    for(i=0 ; i<100 ; i++)
        data[i] = i;

    if(H5F_block_write(f, H5FD_MEM_DRAW, raw_addr, sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_write(f, H5FD_MEM_DRAW, raw_addr+(sizeof(int)*200), 
                       sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_write(f, H5FD_MEM_DRAW, raw_addr+(sizeof(int)*400), 
                       sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_write(f, H5FD_MEM_DRAW, raw_addr+(sizeof(int)*600), 
                       sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_write(f, H5FD_MEM_DRAW, raw_addr+(sizeof(int)*800), 
                       sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(f->shared->pb_ptr->curr_pages != base_meta_cnt)
        FAIL_STACK_ERROR;

    if(pb_ptr->curr_rd_pages != 0)
        TEST_ERROR;

    /* write all meta data, this would end up in page buffer */
    if(H5F_block_write(f, H5FD_MEM_SUPER, meta_addr, 
                       sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_write(f, H5FD_MEM_SUPER, meta_addr+(sizeof(int)*200), 
                       sizeof(int)*50, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_write(f, H5FD_MEM_SUPER, meta_addr+(sizeof(int)*400), 
                       sizeof(int)*50, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_read(f, H5FD_MEM_SUPER, meta_addr+(sizeof(int)*600), 
                      sizeof(int)*50, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_read(f, H5FD_MEM_SUPER, meta_addr+(sizeof(int)*800), 
                      sizeof(int)*50, data) < 0)
        FAIL_STACK_ERROR;

    if(f->shared->pb_ptr->curr_pages != 5)
        FAIL_STACK_ERROR;

    if(pb_ptr->curr_md_pages != 5)
        TEST_ERROR;

    if(pb_ptr->curr_rd_pages != 0)
        TEST_ERROR;

    /* write and read more raw data and make sure that they don't end up in
     * page buffer 
     */
    if(H5F_block_write(f, H5FD_MEM_DRAW, raw_addr+(sizeof(int)*200), 
                       sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_write(f, H5FD_MEM_DRAW, raw_addr+(sizeof(int)*350), 
                       sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_read(f, H5FD_MEM_DRAW, raw_addr+(sizeof(int)*500), 
                      sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_read(f, H5FD_MEM_DRAW, raw_addr+(sizeof(int)*750), 
                      sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_read(f, H5FD_MEM_DRAW, raw_addr+(sizeof(int)*900), 
                      sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(f->shared->pb_ptr->curr_pages != 5)
        FAIL_STACK_ERROR;

    if(pb_ptr->curr_md_pages != 5)
        TEST_ERROR;

    if(pb_ptr->curr_rd_pages != 0)
        TEST_ERROR;

    if(H5Fclose(file_id) < 0)
        FAIL_STACK_ERROR;


    HDprintf("\tMinimum raw data threshold = 100%%\n");

    /* keep 5 pages at max in the page buffer and 5 raw page minimum */
    if(H5Pset_page_buffer_size(fapl, sizeof(int)*1000, 0, 100) < 0)
        TEST_ERROR;

    /* create the file */
    if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Get a pointer to the internal file object */
    if(NULL == (f = (H5F_t *)H5VL_object(file_id)))
        FAIL_STACK_ERROR;

    /* opening the file inserts one or more pages into the page buffer.
     * Get the raw and meta counts now, so we can adjust tests accordingly.
     */
    HDassert(f);
    HDassert(f->shared);
    HDassert(f->shared->pb_ptr);

    base_raw_cnt = f->shared->pb_ptr->curr_rd_pages;
    base_meta_cnt = f->shared->pb_ptr->curr_md_pages;

    if(base_raw_cnt != 0)
        TEST_ERROR;

    if(base_meta_cnt != 0)
        TEST_ERROR;

    pb_ptr = f->shared->pb_ptr;

    if(pb_ptr->min_md_pages != 0)
        TEST_ERROR;
    if(pb_ptr->min_rd_pages != 5)
        FAIL_STACK_ERROR;

    if(HADDR_UNDEF == (meta_addr = H5MF_alloc(f, H5FD_MEM_SUPER, 
                                             sizeof(int)*(size_t)num_elements)))
        FAIL_STACK_ERROR;

    if(HADDR_UNDEF == (raw_addr = H5MF_alloc(f, H5FD_MEM_DRAW, 
                                             sizeof(int)*(size_t)num_elements)))
        TEST_ERROR;

    /* write all meta data, none of this should end up in the page buffer since
     * min_rd_pages == max_pages
     * is no raw data yet
     */
    for(i=0 ; i<100 ; i++)
        data[i] = i;

    if(H5F_block_write(f, H5FD_MEM_SUPER, meta_addr, sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_write(f, H5FD_MEM_SUPER, meta_addr+(sizeof(int)*200), 
                       sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_write(f, H5FD_MEM_SUPER, meta_addr+(sizeof(int)*400), 
                       sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_write(f, H5FD_MEM_SUPER, meta_addr+(sizeof(int)*600), 
                       sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_write(f, H5FD_MEM_SUPER, meta_addr+(sizeof(int)*800), 
                       sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(f->shared->pb_ptr->curr_pages != 0)
        FAIL_STACK_ERROR;
    if(pb_ptr->curr_md_pages != 0)
        TEST_ERROR;

    /* write/read all raw data, this would end up in page buffer */
    if(H5F_block_write(f, H5FD_MEM_DRAW, raw_addr, sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_write(f, H5FD_MEM_DRAW, raw_addr+(sizeof(int)*200), 
                       sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_write(f, H5FD_MEM_DRAW, raw_addr+(sizeof(int)*400), 
                       sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_read(f, H5FD_MEM_DRAW, raw_addr+(sizeof(int)*600), 
                      sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_read(f, H5FD_MEM_DRAW, raw_addr+(sizeof(int)*800), 
                      sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(f->shared->pb_ptr->curr_pages != 5)
        FAIL_STACK_ERROR;

    if(pb_ptr->curr_rd_pages != 5)
        TEST_ERROR;

    if(pb_ptr->curr_md_pages != 0)
        TEST_ERROR;

    /* write and read more meta data and make sure that they don't end up in
     * page buffer 
     */
    if(H5F_block_write(f, H5FD_MEM_SUPER, meta_addr+(sizeof(int)*100), 
                       sizeof(int)*50, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_write(f, H5FD_MEM_SUPER, meta_addr+(sizeof(int)*350), 
                       sizeof(int)*50, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_read(f, H5FD_MEM_SUPER, meta_addr+(sizeof(int)*500), 
                      sizeof(int)*50, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_read(f, H5FD_MEM_SUPER, meta_addr+(sizeof(int)*750), 
                      sizeof(int)*50, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_read(f, H5FD_MEM_SUPER, meta_addr+(sizeof(int)*900), 
                      sizeof(int)*50, data) < 0)
        FAIL_STACK_ERROR;

    if(f->shared->pb_ptr->curr_pages != 5)
        FAIL_STACK_ERROR;

    if(pb_ptr->curr_rd_pages != 5)
        TEST_ERROR;

    if(pb_ptr->curr_md_pages != 0)
        TEST_ERROR;

    if(H5Fclose(file_id) < 0)
        FAIL_STACK_ERROR;


    HDprintf("\tMinimum metadata threshold = 40%%, ");
    HDprintf("Minimum rawdata threshold = 40%%\n");

    /* keep 5 pages at max in the page buffer 2 meta pages, 2 raw pages  
     * minimum 
     */
    if(H5Pset_page_buffer_size(fapl, sizeof(int)*1000, 40, 40) < 0)
        TEST_ERROR;

    /* create the file */
    if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Get a pointer to the internal file object */
    if(NULL == (f = (H5F_t *)H5VL_object(file_id)))
        FAIL_STACK_ERROR;

    /* opening the file inserts one or more pages into the page buffer.
     *
     * However, with the current 1 metadata page inserted into the 
     * the page buffer, it is not necessary to track the base raw and 
     * metadata entry counts.
     */

    base_raw_cnt = f->shared->pb_ptr->curr_rd_pages;
    base_meta_cnt = f->shared->pb_ptr->curr_md_pages;

    if(base_raw_cnt != 0)
        TEST_ERROR;

    if(base_meta_cnt != 2)
        TEST_ERROR;

    pb_ptr = f->shared->pb_ptr;

    if(pb_ptr->min_md_pages != 2)
        TEST_ERROR;

    if(pb_ptr->min_rd_pages != 2)
        TEST_ERROR;

    if(HADDR_UNDEF == (meta_addr = H5MF_alloc(f, H5FD_MEM_SUPER, 
                                            sizeof(int)*(size_t)num_elements)))
        FAIL_STACK_ERROR;

    if(HADDR_UNDEF == (raw_addr = H5MF_alloc(f, H5FD_MEM_DRAW, 
                                            sizeof(int)*(size_t)num_elements)))
        FAIL_STACK_ERROR;

    /* initialize all the elements to have a value of -1 */
    for(i=0 ; i<num_elements ; i++)
        data[i] = -1;

    if(H5F_block_write(f, H5FD_MEM_DRAW, raw_addr, 
                       sizeof(int)*(size_t)num_elements, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_write(f, H5FD_MEM_SUPER, meta_addr, 
                       sizeof(int)*(size_t)num_elements, data) < 0)
        FAIL_STACK_ERROR;

    /* fill the page buffer with raw data */
    for(i=0 ; i<100 ; i++)
        data[i] = i;

    if(H5F_block_write(f, H5FD_MEM_DRAW, raw_addr, sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_write(f, H5FD_MEM_DRAW, raw_addr+(sizeof(int)*200), 
                       sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_write(f, H5FD_MEM_DRAW, raw_addr+(sizeof(int)*400), 
                       sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_write(f, H5FD_MEM_DRAW, raw_addr+(sizeof(int)*600), 
                       sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_write(f, H5FD_MEM_DRAW, raw_addr+(sizeof(int)*800), 
                       sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(f->shared->pb_ptr->curr_pages != 5)
        TEST_ERROR;

    if(f->shared->pb_ptr->curr_rd_pages != 5 - base_meta_cnt)
        TEST_ERROR;

    /* add 3 meta entries evicting 1 raw entry */
    if(H5F_block_write(f, H5FD_MEM_SUPER, meta_addr, sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_write(f, H5FD_MEM_SUPER, meta_addr+(sizeof(int)*200), 
                       sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_write(f, H5FD_MEM_SUPER, meta_addr+(sizeof(int)*400), 
                       sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(f->shared->pb_ptr->curr_pages != 5)
        FAIL_STACK_ERROR;

    if(f->shared->pb_ptr->curr_md_pages != 3)
        TEST_ERROR;

    if(f->shared->pb_ptr->curr_rd_pages != 2)
        TEST_ERROR;

    /* adding more meta entires should replace meta entries since raw data 
     * is at its minimum 
     */
    if(H5F_block_write(f, H5FD_MEM_SUPER, meta_addr+(sizeof(int)*600), 
                       sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_write(f, H5FD_MEM_SUPER, meta_addr+(sizeof(int)*800), 
                       sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(f->shared->pb_ptr->curr_md_pages != 3)
        TEST_ERROR;

    if(f->shared->pb_ptr->curr_rd_pages != 2)
        TEST_ERROR;

    /* bring existing raw entires up the LRU */
    if(H5F_block_read(f, H5FD_MEM_DRAW, raw_addr+(sizeof(int)*750), 
                      sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    /* adding 2 raw entries (even with 1 call) should only evict 1 meta 
     * entry and another raw entry 
     */
    if(H5F_block_read(f, H5FD_MEM_DRAW, raw_addr+(sizeof(int)*350), 
                      sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(f->shared->pb_ptr->curr_md_pages != 2)
        TEST_ERROR;

    if(f->shared->pb_ptr->curr_rd_pages != 3)
        TEST_ERROR;

    /* read a metadata entry to force the flush of the metadata entries 
     * in the page buffer, and then read some raw data so that the metadata 
     * pages are at the bottom of the LRU.
     *
     * When we are done, should still have 2 metadata pages and 3 raw data 
     * pages in the page buffer
     */

    if(H5F_block_read(f, H5FD_MEM_SUPER, meta_addr+(sizeof(int)*442), 
                      sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_read(f, H5FD_MEM_DRAW, raw_addr+(sizeof(int)*150), 
                      sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_read(f, H5FD_MEM_DRAW, raw_addr+(sizeof(int)*550), 
                      sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(f->shared->pb_ptr->curr_md_pages != 2)
        TEST_ERROR;

    if(f->shared->pb_ptr->curr_rd_pages != 3)
        TEST_ERROR;

    /* adding 2 meta entries should replace 2 entires at the bottom 
     * of the LRU 
     */
    if(H5F_block_read(f, H5FD_MEM_SUPER, meta_addr+(sizeof(int)*98), 
                      sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_read(f, H5FD_MEM_SUPER, meta_addr+(sizeof(int)*242), 
                      sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(f->shared->pb_ptr->curr_md_pages != 2)
        TEST_ERROR;

    if(f->shared->pb_ptr->curr_rd_pages != 3)
        TEST_ERROR;

    if(H5Fclose(file_id) < 0)
        FAIL_STACK_ERROR;

    HDprintf("\tMinimum metadata threshold = 20%%\n");

    /* keep 5 pages at max in the page buffer and 1 meta page minimum */
    if(H5Pset_page_buffer_size(fapl, sizeof(int)*1000, 39, 0) < 0)
        TEST_ERROR;

    /* create the file */
    if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Get a pointer to the internal file object */
    if(NULL == (f = (H5F_t *)H5VL_object(file_id)))
        FAIL_STACK_ERROR;

    pb_ptr = f->shared->pb_ptr;

    if(pb_ptr->min_md_pages != 1)
        TEST_ERROR;
    if(pb_ptr->min_rd_pages != 0)
        TEST_ERROR;

    if(HADDR_UNDEF == (meta_addr = H5MF_alloc(f, H5FD_MEM_SUPER, 
                                             sizeof(int)*(size_t)num_elements)))
        FAIL_STACK_ERROR;

    if(HADDR_UNDEF == (raw_addr = H5MF_alloc(f, H5FD_MEM_DRAW, 
                                             sizeof(int)*(size_t)num_elements)))
        FAIL_STACK_ERROR;

    /* initialize all the elements to have a value of -1 */
    for(i=0 ; i<num_elements ; i++)
        data[i] = -1;

    if(H5F_block_write(f, H5FD_MEM_DRAW, raw_addr, 
                       sizeof(int)*(size_t)num_elements, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_write(f, H5FD_MEM_SUPER, meta_addr, 
                       sizeof(int)*(size_t)num_elements, data) < 0)
        FAIL_STACK_ERROR;

    /* fill the page buffer with raw data */
    for(i=0 ; i<100 ; i++)
        data[i] = i;

    if(H5F_block_write(f, H5FD_MEM_DRAW, raw_addr, sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_write(f, H5FD_MEM_DRAW, raw_addr+(sizeof(int)*200), 
                       sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_write(f, H5FD_MEM_DRAW, raw_addr+(sizeof(int)*400), 
                       sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_write(f, H5FD_MEM_DRAW, raw_addr+(sizeof(int)*600), 
                       sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_write(f, H5FD_MEM_DRAW, raw_addr+(sizeof(int)*800), 
                       sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(f->shared->pb_ptr->curr_pages != 5)
        FAIL_STACK_ERROR;

    if(f->shared->pb_ptr->curr_md_pages != 1)
        TEST_ERROR;

    if(f->shared->pb_ptr->curr_rd_pages != 4)
        TEST_ERROR;

    /* add 2 meta entries evicting 2 raw entries */
    if(H5F_block_write(f, H5FD_MEM_SUPER, meta_addr, sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_write(f, H5FD_MEM_SUPER, meta_addr+(sizeof(int)*200), 
                       sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(f->shared->pb_ptr->curr_pages != 5)
        FAIL_STACK_ERROR;

    if(f->shared->pb_ptr->curr_md_pages != 3)
        TEST_ERROR;

    if(f->shared->pb_ptr->curr_rd_pages != 2)
        TEST_ERROR;

    /* bring the rest of the raw entries up the LRU */
    if(H5F_block_write(f, H5FD_MEM_DRAW, raw_addr+(sizeof(int)*500), 
                       sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_write(f, H5FD_MEM_DRAW, raw_addr+(sizeof(int)*700), 
                       sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_write(f, H5FD_MEM_DRAW, raw_addr+(sizeof(int)*900), 
                       sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    /* write one more raw entry which replace one meta entry */
    if(H5F_block_write(f, H5FD_MEM_DRAW, raw_addr+(sizeof(int)*100), 
                       sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(f->shared->pb_ptr->curr_pages != 5)
        FAIL_STACK_ERROR;

    if(f->shared->pb_ptr->curr_md_pages != 1)
        TEST_ERROR;

    if(f->shared->pb_ptr->curr_rd_pages != 4)
        TEST_ERROR;

    /* write one more raw entry which should replace another raw entry 
     * keeping min threshold of meta entries 
     */
    if(H5F_block_write(f, H5FD_MEM_DRAW, raw_addr+(sizeof(int)*300), 
                       sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(f->shared->pb_ptr->curr_pages != 5)
        FAIL_STACK_ERROR;

    if(f->shared->pb_ptr->curr_md_pages != 1)
        TEST_ERROR;

    if(f->shared->pb_ptr->curr_rd_pages != 4)
        TEST_ERROR;

    /* write a metadata entry that should replace the metadata entry 
     * at the bottom of the LRU 
     */
    if(H5F_block_write(f, H5FD_MEM_SUPER, meta_addr+(sizeof(int)*500), 
                       sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(f->shared->pb_ptr->curr_pages != 5)
        FAIL_STACK_ERROR;

    if(f->shared->pb_ptr->curr_md_pages != 1)
        TEST_ERROR;

    if(f->shared->pb_ptr->curr_rd_pages != 4)
        TEST_ERROR;

    if(H5Fclose(file_id) < 0)
        FAIL_STACK_ERROR;

    if(H5Pclose(fcpl) < 0)
        FAIL_STACK_ERROR;

    if(H5Pclose(fapl) < 0)
        FAIL_STACK_ERROR;

    HDfree(data);

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY {
        if (fapl != H5I_INVALID_HID)
            H5Pclose(fapl);
        if (fcpl != H5I_INVALID_HID)
            H5Pclose(fcpl);
        if (file_id != H5I_INVALID_HID)
            H5Fclose(file_id);
        if (data != NULL)
            HDfree(data);
    } H5E_END_TRY;
    return 1;

} /* test_min_threshold */
#endif


/*-------------------------------------------------------------------------
 * Function:    test_stats_collection()
 *
 * Purpose:     Tests verifying correct collection of statistics
 *              by the page buffer.
 *
 *              Any data mis-matches or failures reported by the HDF5
 *              library result in test failure.
 *
 * Return:      0 if test is sucessful
 *              1 if test fails
 *
 * Programmer:  unknown
 *              ?? / ?? / ??
 *              
 * Changes:     Added the base_raw_cnt and base_meta_cnt fields and
 *              supporting code.  This allows the test to adjust to the 
 *              number of page buffer pages accessed during file open / 
 *              create.
 *
 *              The tests for the values of base_raw_cnt and base_meta_cnt
 *              just after file open exist detect changes in library 
 *              behavior.  Assuming any such change is not indicative of 
 *              other issues, these tests can be modified to reflect the 
 *              change.
 *
 *                                                    JRM -- 2/23/17
 *
 *              Reworked test for the new page buffer implementation.
 *
 *                                                    JRM -- 10/28/18
 *
 *-------------------------------------------------------------------------
 */
#ifndef H5_HAVE_PARALLEL
static unsigned
test_stats_collection(hid_t orig_fapl, const char *env_h5_drvr)
{
    char filename[FILENAME_LEN]; /* Filename to use */
    hid_t file_id = -1;          /* File ID */
    hid_t fcpl = -1;
    hid_t fapl = -1;
    int i;
    int num_elements = 1000;
    int64_t base_raw_cnt = 0;
    int64_t base_meta_cnt = 0;
    haddr_t meta_addr = HADDR_UNDEF;
    haddr_t raw_addr = HADDR_UNDEF;
    int *data = NULL;
    H5F_t *f = NULL;

    TESTING("Statistics Collection");

    h5_fixname(namebase, orig_fapl, filename, sizeof(filename));

    if((fapl = H5Pcopy(orig_fapl)) < 0)
        TEST_ERROR

    if(set_multi_split(env_h5_drvr, fapl, sizeof(int)*200)  != 0)
        TEST_ERROR;

    if((data = (int *)HDcalloc((size_t)num_elements, sizeof(int))) == NULL)
        TEST_ERROR

    if((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
        TEST_ERROR;

    if(H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_PAGE, 0, 1) < 0)
        TEST_ERROR;

    if(H5Pset_file_space_page_size(fcpl, sizeof(int)*200) < 0)
        TEST_ERROR;

    /* keep 5 pages at max in the page buffer */
    if(H5Pset_page_buffer_size(fapl, sizeof(int)*1000, 20, 0) < 0)
        TEST_ERROR;

    if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Get a pointer to the internal file object */
    if(NULL == (f = (H5F_t *)H5VL_object(file_id)))
        FAIL_STACK_ERROR;

    /* opening the file inserts one or more pages into the page buffer.
     * Get the raw and meta counts now, so we can adjust the expected 
     * statistics accordingly.
     */
    HDassert(f);
    HDassert(f->shared);
    HDassert(f->shared->pb_ptr);

    base_raw_cnt = f->shared->pb_ptr->curr_rd_pages;
    base_meta_cnt = f->shared->pb_ptr->curr_md_pages;

    if(base_raw_cnt != 0)
        TEST_ERROR;

    if(base_meta_cnt != 2)
        TEST_ERROR;

    /* reset statistics before we begin the tests */
    if(H5Freset_page_buffering_stats(file_id) < 0)
        FAIL_STACK_ERROR;

    if(HADDR_UNDEF == (meta_addr = H5MF_alloc(f, H5FD_MEM_SUPER, 
                                           sizeof(int)*(size_t)num_elements)))
        FAIL_STACK_ERROR;

    if(HADDR_UNDEF == (raw_addr = H5MF_alloc(f, H5FD_MEM_DRAW, 
                                           sizeof(int)*(size_t)num_elements)))
        FAIL_STACK_ERROR;


    /* initialize all the elements to have a value of -1 */
    for(i=0 ; i<num_elements ; i++)
        data[i] = -1;

    if(H5F_block_write(f, H5FD_MEM_DRAW, raw_addr, 
                       sizeof(int)*(size_t)num_elements, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_write(f, H5FD_MEM_SUPER, meta_addr, 
                       sizeof(int)*(size_t)num_elements, data) < 0)
        FAIL_STACK_ERROR;

    for(i=0 ; i<200 ; i++)
        data[i] = i;

    if(H5F_block_write(f, H5FD_MEM_DRAW, raw_addr, sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_write(f, H5FD_MEM_DRAW, raw_addr+(sizeof(int)*200), 
                       sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_write(f, H5FD_MEM_DRAW, raw_addr+(sizeof(int)*400), 
                       sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_write(f, H5FD_MEM_SUPER, meta_addr, sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_write(f, H5FD_MEM_SUPER, meta_addr+(sizeof(int)*200), 
                       sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_write(f, H5FD_MEM_DRAW, raw_addr+(sizeof(int)*600), 
                       sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_write(f, H5FD_MEM_DRAW, raw_addr+(sizeof(int)*800), 
                       sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_write(f, H5FD_MEM_SUPER, meta_addr+(sizeof(int)*600), 
                       sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_write(f, H5FD_MEM_DRAW, raw_addr+(sizeof(int)*500), 
                       sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_write(f, H5FD_MEM_DRAW, raw_addr+(sizeof(int)*700), 
                       sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_write(f, H5FD_MEM_DRAW, raw_addr+(sizeof(int)*900), 
                       sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_write(f, H5FD_MEM_SUPER, meta_addr+(sizeof(int)*400), 
                       sizeof(int)*200, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_write(f, H5FD_MEM_DRAW, raw_addr+(sizeof(int)*100), 
                       sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_write(f, H5FD_MEM_DRAW, raw_addr+(sizeof(int)*300), 
                       sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_write(f, H5FD_MEM_SUPER, meta_addr+(sizeof(int)*800), 
                       sizeof(int)*182, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_read(f, H5FD_MEM_DRAW, raw_addr, sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_read(f, H5FD_MEM_DRAW, raw_addr+(sizeof(int)*200), 
                      sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_read(f, H5FD_MEM_DRAW, raw_addr+(sizeof(int)*400), 
                      sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_read(f, H5FD_MEM_SUPER, meta_addr, sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_read(f, H5FD_MEM_SUPER, meta_addr+(sizeof(int)*200), 
                      sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_read(f, H5FD_MEM_DRAW, raw_addr+(sizeof(int)*600), 
                      sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_read(f, H5FD_MEM_DRAW, raw_addr+(sizeof(int)*800), 
                      sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_read(f, H5FD_MEM_SUPER, meta_addr+(sizeof(int)*400), 
                      sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_read(f, H5FD_MEM_SUPER, meta_addr+(sizeof(int)*600), 
                      sizeof(int)*200, data) < 0)
        FAIL_STACK_ERROR;

    if(H5F_block_read(f, H5FD_MEM_SUPER, meta_addr+(sizeof(int)*800), 
                      sizeof(int)*100, data) < 0)
        FAIL_STACK_ERROR;

    if ( ( f->shared->pb_ptr->accesses[0] != 9 ) ||
         ( f->shared->pb_ptr->accesses[1] != 16 ) ||
         ( f->shared->pb_ptr->accesses[2] != 0 ) ) {

        HDfprintf(stderr, "accesses[] = {%d, %d, %d}. {9, 16, 0} expected\n",
                 f->shared->pb_ptr->accesses[0],
                 f->shared->pb_ptr->accesses[1],
                 f->shared->pb_ptr->accesses[2]);
        TEST_ERROR;
    }

    if ( ( f->shared->pb_ptr->bypasses[0] != 2 ) ||
         ( f->shared->pb_ptr->bypasses[1] != 1 ) ||
         ( f->shared->pb_ptr->bypasses[2] != 1 ) ) {

        HDfprintf(stderr, "bypasses[] = {%d, %d, %d}. {2, 1, 1} expected\n",
                 f->shared->pb_ptr->bypasses[0],
                 f->shared->pb_ptr->bypasses[1],
                 f->shared->pb_ptr->bypasses[2]);
        TEST_ERROR;
    }

    if ( ( f->shared->pb_ptr->hits[0] != 0 ) ||
         ( f->shared->pb_ptr->hits[1] != 4 ) ||
         ( f->shared->pb_ptr->hits[2] != 0 ) ) {

        HDfprintf(stderr, "hits[] = {%d, %d, %d}. {0, 4, 0} expected\n",
                 f->shared->pb_ptr->hits[0],
                 f->shared->pb_ptr->hits[1],
                 f->shared->pb_ptr->hits[2]);
        TEST_ERROR;
    }

    if ( ( f->shared->pb_ptr->misses[0] != 9 ) ||
         ( f->shared->pb_ptr->misses[1] != 16 ) ||
         ( f->shared->pb_ptr->misses[2] != 0 ) ) {

        HDfprintf(stderr, "misses[] = {%d, %d, %d}. {9, 16, 0} expected\n",
                 f->shared->pb_ptr->misses[0],
                 f->shared->pb_ptr->misses[1],
                 f->shared->pb_ptr->misses[2]);
        TEST_ERROR;
    }

    if ( ( f->shared->pb_ptr->evictions[0] != 7) ||
         ( f->shared->pb_ptr->evictions[1] != 9) ||
         ( f->shared->pb_ptr->evictions[2] != 0 ) ) {

        HDfprintf(stderr, "evictions[] = {%d, %d, %d}. {%d, %d, 0} expected\n",
                 f->shared->pb_ptr->evictions[0],
                 f->shared->pb_ptr->evictions[1],
                 f->shared->pb_ptr->evictions[2], 7, 9);
        TEST_ERROR;
    }

    {
        unsigned accesses[3];
        unsigned hits[3];
        unsigned misses[3];
        unsigned evictions[3];
        unsigned bypasses[3];

        if(H5Fget_page_buffering_stats(file_id, accesses, hits, misses, 
                                       evictions, bypasses) < 0)
            FAIL_STACK_ERROR;

        if ( ( accesses[0] != 9 ) ||
             ( accesses[1] != 16 ) ||
             ( accesses[2] != 0 ) ) {

            HDfprintf(stderr, 
                      "accesses[] = {%d, %d, %d}. {9, 16, 0} expected\n",
                      accesses[0], accesses[1], accesses[2]);
            TEST_ERROR;
        }

        if ( ( bypasses[0] != 2 ) ||
             ( bypasses[1] != 1 ) ||
             ( bypasses[2] != 1 ) ) {

            HDfprintf(stderr, "bypasses[] = {%d, %d, %d}. {2, 1, 1} expected\n",
                     bypasses[0], bypasses[1], bypasses[2]);
            TEST_ERROR;
        }

        if ( ( hits[0] != 0 ) ||
             ( hits[1] != 4 ) ||
             ( hits[2] != 0 ) ) {

            HDfprintf(stderr, "hits[] = {%d, %d, %d}. {0, 4, 0} expected\n",
                      hits[0], hits[1], hits[2]);
            TEST_ERROR;
        }

        if ( ( misses[0] != 9 ) ||
             ( misses[1] != 16 ) ||
             ( misses[2] != 0 ) ) {

            HDfprintf(stderr, "misses[] = {%d, %d, %d}. {9, 16, 0} expected\n",
                      misses[0], misses[1], misses[2]);
            TEST_ERROR;
        }

        if ( ( evictions[0] != 7 ) ||
             ( evictions[1] != 9 ) ||
             ( evictions[2] != 0 ) ) {

            HDfprintf(stderr, 
                      "evictions[] = {%d, %d, %d}. {%d, %d, 0} expected\n",
                      evictions[0], evictions[1], evictions[2], 7, 9);
            TEST_ERROR;
        }

        if(H5Freset_page_buffering_stats(file_id) < 0)
            FAIL_STACK_ERROR;

        if(H5Fget_page_buffering_stats(file_id, accesses, hits, misses, 
           evictions, bypasses) < 0)
            FAIL_STACK_ERROR;

        if(accesses[0] != 0)
            TEST_ERROR;
        if(accesses[1] != 0)
            TEST_ERROR;
        if(bypasses[0] != 0)
            TEST_ERROR;
        if(bypasses[1] != 0)
            TEST_ERROR;
        if(hits[0] != 0)
            TEST_ERROR;
        if(hits[1] != 0)
            TEST_ERROR;
        if(misses[0] != 0)
            TEST_ERROR;
        if(misses[1] != 0)
            TEST_ERROR;
        if(evictions[0] != 0)
            TEST_ERROR;
        if(evictions[1] != 0)
            TEST_ERROR;
    }

    if(H5Fclose(file_id) < 0)
        FAIL_STACK_ERROR;
    if(H5Pclose(fcpl) < 0)
        FAIL_STACK_ERROR;
    if(H5Pclose(fapl) < 0)
        FAIL_STACK_ERROR;
    HDfree(data);


    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        if (fapl != H5I_INVALID_HID)
            H5Pclose(fapl);
        if (fcpl != H5I_INVALID_HID)
            H5Pclose(fcpl);
        if (file_id != H5I_INVALID_HID)
            H5Fclose(file_id);
        if (data != NULL)
            HDfree(data);
    } H5E_END_TRY;

    return 1;
} /* test_stats_collection */
#endif


/*-------------------------------------------------------------------------
 * Function:    verify_page_buffering_disabled()
 *
 * Purpose:     This function should only be called in parallel 
 *              builds.
 *
 *              At present, page buffering should be disabled in parallel
 *              builds.  Verify this.
 *
 * Return:      0 if test is sucessful
 *              1 if test fails
 *
 * Programmer:  John Mainzer
 *              03/21/17
 *              
 * Changes:     None.
 *
 *-------------------------------------------------------------------------
 */

#ifdef H5_HAVE_PARALLEL
static unsigned
verify_page_buffering_disabled(hid_t orig_fapl, const char *env_h5_drvr)
{
    char filename[FILENAME_LEN]; /* Filename to use */
    hid_t file_id = -1;          /* File ID */
    hid_t fcpl = -1;
    hid_t fapl = -1;

    TESTING("Page Buffering Disabled");
    h5_fixname(namebase, orig_fapl, filename, sizeof(filename));


    /* first, try to create a file with page buffering enabled */

    if((fapl = H5Pcopy(orig_fapl)) < 0)
        TEST_ERROR

    if(set_multi_split(env_h5_drvr, fapl, 4096)  != 0)
        TEST_ERROR;

    if((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
        FAIL_STACK_ERROR;

    if(H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_PAGE, 0, 1) < 0)
        FAIL_STACK_ERROR;

    if(H5Pset_file_space_page_size(fcpl, 4096) < 0)
        FAIL_STACK_ERROR;

    if(H5Pset_page_buffer_size(fapl, 4096*8, 0, 0) < 0)
        FAIL_STACK_ERROR;

    /* try to create  the file -- should fail */
    H5E_BEGIN_TRY {
        file_id = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl);
    } H5E_END_TRY;

    if(file_id >= 0)
        TEST_ERROR;

    /* now, create a file, close it, and then try to open it with page 
     * buffering enabled.
     */
    if((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
        FAIL_STACK_ERROR;

    if(H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_PAGE, 0, 1) < 0)
        FAIL_STACK_ERROR;

    if(H5Pset_file_space_page_size(fcpl, 4096) < 0)
        FAIL_STACK_ERROR;

    /* create the file */
    if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* close the file */
    if(H5Fclose(file_id) < 0)
        FAIL_STACK_ERROR;

    /* try to open the file using the fapl prepared above which enables 
     * page buffering.  Should fail.
     */
    H5E_BEGIN_TRY {
        file_id = H5Fopen(filename, H5F_ACC_RDWR, fapl);
    } H5E_END_TRY;

    if(file_id >= 0)
        TEST_ERROR;

    if(H5Pclose(fcpl) < 0)
        FAIL_STACK_ERROR;

    if(H5Pclose(fapl) < 0)
        FAIL_STACK_ERROR;

    PASSED();

    return 0;

error:

    H5E_BEGIN_TRY {
        if (fapl != H5I_INVALID_HID)
            H5Pclose(fapl);
        if (fcpl != H5I_INVALID_HID)
            H5Pclose(fcpl);
        if (file_id != H5I_INVALID_HID)
            H5Fclose(file_id);
    } H5E_END_TRY;

    return 1;

}
#endif /* H5_HAVE_PARALLEL */


/*-------------------------------------------------------------------------
 * Function:    main()
 *
 * Purpose:     Main function for the page buffer tests.
 *
 * Return:      0 if test is sucessful
 *              1 if test fails
 *
 * Programmer:  unknown
 *              ?? / ?? / ??
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t       fapl = -1;              /* File access property list for data files */
    unsigned    nerrors = 0;            /* Cumulative error count */
    const char  *env_h5_drvr = NULL;    /* File Driver value from environment */
    hbool_t     api_ctx_pushed = FALSE;             /* Whether API context pushed */

    h5_reset();

    /* Get the VFD to use */
    env_h5_drvr = HDgetenv("HDF5_DRIVER");
    if(env_h5_drvr == NULL)
        env_h5_drvr = "nomatch";

    /* Temporary skip testing with multi/split drivers:
     * Page buffering depends on paged aggregation which is
     * currently disabled for multi/split drivers.
     */
    if((0 == HDstrcmp(env_h5_drvr, "multi")) || 
       (0 == HDstrcmp(env_h5_drvr, "split"))) {

        SKIPPED()
        HDputs("Skip page buffering test because paged aggregation is disabled for multi/split drivers");
        HDputs("Furthermore, VFD SWMR is not (yet) expected to work with multi/split drivers");
        HDexit(EXIT_SUCCESS);
    }

    if((fapl = h5_fileaccess()) < 0) {
        nerrors++;
        PUTS_ERROR("Can't get VFD-dependent fapl")
    }

    /* Push API context */
    if(H5CX_push() < 0) FAIL_STACK_ERROR
    api_ctx_pushed = TRUE;

#ifdef H5_HAVE_PARALLEL 

    HDputs("Page Buffering is disabled for parallel.");
    nerrors += verify_page_buffering_disabled(fapl, env_h5_drvr);

#else /* H5_HAVE_PARALLEL */

    nerrors += test_args(fapl, env_h5_drvr);
    nerrors += test_raw_data_handling(fapl, env_h5_drvr, false);
    nerrors += test_raw_data_handling(fapl, env_h5_drvr, true);
    nerrors += test_spmde_delay_basic(fapl, env_h5_drvr);
    nerrors += test_mpmde_delay_basic(fapl, env_h5_drvr);
    nerrors += test_spmde_lru_evict_basic(fapl, env_h5_drvr);
    nerrors += test_lru_processing(fapl, env_h5_drvr);
    nerrors += test_min_threshold(fapl, env_h5_drvr);
    nerrors += test_stats_collection(fapl, env_h5_drvr);

#endif /* H5_HAVE_PARALLEL */

    h5_clean_files(namebases, fapl);

    if(nerrors)
        goto error;

    /* Pop API context */
    if(api_ctx_pushed && H5CX_pop() < 0) FAIL_STACK_ERROR
    api_ctx_pushed = FALSE;

    HDputs("All Page Buffering tests passed.");

    HDexit(EXIT_SUCCESS);

error:
    HDprintf("***** %d Page Buffering TEST%s FAILED! *****\n",
        nerrors, nerrors > 1 ? "S" : "");

    H5E_BEGIN_TRY {
        H5Pclose(fapl);
    } H5E_END_TRY;

    if(api_ctx_pushed) H5CX_pop();

    HDexit(EXIT_FAILURE);
}
