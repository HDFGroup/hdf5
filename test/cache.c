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

/* Programmer:  John Mainzer
 *              6/9/04
 *
 *		This file contains tests for the cache implemented in
 *		H5C.c
 */
#include "h5test.h"
#include "H5Iprivate.h"
#include "H5ACprivate.h"
#include "cache_common.h"

/* private function declarations: */

static void smoke_check_1(void);
static void smoke_check_2(void);
static void smoke_check_3(void);
static void smoke_check_4(void);
static void smoke_check_5(void);
static void smoke_check_6(void);
static void smoke_check_7(void);
static void smoke_check_8(void);
static void write_permitted_check(void);
static void check_flush_cache(void);
static void check_flush_cache__empty_cache(H5C_t * cache_ptr);
static void check_flush_cache__multi_entry(H5C_t * cache_ptr);
static void check_flush_cache__multi_entry_test(H5C_t * cache_ptr,
                                          int test_num,
                                          unsigned int flush_flags,
                                          int spec_size,
                                          struct flush_cache_test_spec spec[]);
static void check_flush_cache__pe_multi_entry_test(H5C_t * cache_ptr,
                                        int test_num,
                                        unsigned int flush_flags,
                                        int spec_size,
                                        struct pe_flush_cache_test_spec spec[]);
static void check_flush_cache__single_entry(H5C_t * cache_ptr);
static void check_flush_cache__single_entry_test(H5C_t * cache_ptr,
                                                 int test_num,
                                                 int entry_type,
                                                 int entry_idx,
                                                 hbool_t insert_flag,
                                                 hbool_t dirty_flag,
                                                 unsigned int flags,
                                                 unsigned int flush_flags,
                                                 hbool_t expected_loaded,
                                                 hbool_t expected_cleared,
                                                 hbool_t expected_flushed,
                                                 hbool_t expected_destroyed);
static void check_flush_cache__pinned_single_entry_test(H5C_t * cache_ptr,
                                                 int test_num,
                                                 int entry_type,
                                                 int entry_idx,
                                                 hbool_t dirty_flag,
					         hbool_t mark_dirty,
                                                 hbool_t pop_mark_dirty_prot,
                                                 hbool_t pop_mark_dirty_pinned,
                                                 hbool_t unprotect_unpin,
                                                 unsigned int flags,
                                                 unsigned int flush_flags,
                                                 hbool_t expected_cleared,
                                                 hbool_t expected_flushed,
                                                 hbool_t expected_destroyed);
static void check_flush_protected_err(void);
static void check_get_entry_status(void);
static void check_expunge_entry(void);
static void check_rename_entry(void);
static void check_rename_entry__run_test(H5C_t * cache_ptr, int test_num,
                                      struct rename_entry_test_spec * spec_ptr);
static void check_pin_protected_entry(void);
static void check_resize_entry(void);
static void check_destroy_pinned_err(void);
static void check_destroy_protected_err(void);
static void check_duplicate_insert_err(void);
static void check_rename_err(void);
static void check_double_pin_err(void);
static void check_double_unpin_err(void);
static void check_pin_entry_errs(void);
static void check_double_protect_err(void);
static void check_double_unprotect_err(void);
static void check_mark_entry_dirty_errs(void);
static void check_expunge_entry_errs(void);
static void check_resize_entry_errs(void);
static void check_auto_cache_resize(void);
static void check_auto_cache_resize_disable(void);
static void check_auto_cache_resize_epoch_markers(void);
static void check_auto_cache_resize_input_errs(void);
static void check_auto_cache_resize_aux_fcns(void);


/**************************************************************************/
/**************************************************************************/
/********************************* tests: *********************************/
/**************************************************************************/
/**************************************************************************/

/*-------------------------------------------------------------------------
 * Function:	smoke_check_1()
 *
 * Purpose:	A basic functional test, inserts, destroys, and renames in
 *              the mix, along with repeated protects and unprotects.
 *		All entries are marked as clean.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/16/04
 *
 * Modifications:
 *
 *		JRM -- 1/18/05
 *		Added code to skip this test if the skip_long_tests global
 *		is true.
 *
 *-------------------------------------------------------------------------
 */

static void
smoke_check_1(void)
{
    const char * fcn_name = "smoke_check_1";
    hbool_t show_progress = FALSE;
    hbool_t dirty_inserts = FALSE;
    int dirty_unprotects = FALSE;
    int dirty_destroys = FALSE;
    hbool_t display_stats = FALSE;
    int32_t lag = 10;
    int mile_stone = 1;
    H5C_t * cache_ptr = NULL;

    TESTING("smoke check #1 -- all clean, ins, dest, ren, 4/2 MB cache");

    if ( skip_long_tests ) {

        SKIPPED();

        HDfprintf(stdout, "	Long tests disabled.\n");

        return;
    }

    pass = TRUE;

    if ( show_progress ) /* 1 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    reset_entries();

    if ( show_progress ) /* 2 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    cache_ptr = setup_cache((size_t)(4 * 1024 * 1024),
                            (size_t)(2 * 1024 * 1024));

    if ( show_progress ) /* 3 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    row_major_scan_forward(/* cache_ptr              */ cache_ptr,
                           /* lag                    */ lag,
                           /* verbose                */ FALSE,
                           /* reset_stats            */ TRUE,
                           /* display_stats          */ display_stats,
                           /* display_detailed_stats */ TRUE,
                           /* do_inserts             */ TRUE,
                           /* dirty_inserts          */ dirty_inserts,
                           /* do_renames             */ TRUE,
                           /* rename_to_main_addr    */ FALSE,
                           /* do_destroys            */ TRUE,
                           /* dirty_destroys         */ dirty_destroys,
                           /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 4 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    row_major_scan_backward(/* cache_ptr              */ cache_ptr,
                            /* lag                    */ lag,
                            /* verbose                */ FALSE,
                            /* reset_stats            */ TRUE,
                            /* display_stats          */ display_stats,
                            /* display_detailed_stats */ TRUE,
                            /* do_inserts             */ FALSE,
                            /* dirty_inserts          */ dirty_inserts,
                            /* do_renames             */ TRUE,
                            /* rename_to_main_addr    */ TRUE,
                            /* do_destroys            */ FALSE,
                            /* dirty_destroys         */ dirty_destroys,
                            /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 5 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    row_major_scan_forward(/* cache_ptr              */ cache_ptr,
                           /* lag                    */ lag,
                           /* verbose                */ FALSE,
                           /* reset_stats            */ TRUE,
                           /* display_stats          */ display_stats,
                           /* display_detailed_stats */ TRUE,
                           /* do_inserts             */ TRUE,
                           /* dirty_inserts          */ dirty_inserts,
                           /* do_renames             */ TRUE,
                           /* rename_to_main_addr    */ FALSE,
                           /* do_destroys            */ FALSE,
                           /* dirty_destroys         */ dirty_destroys,
                           /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 6 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    /* flush and destroy all entries in the cache: */

    flush_cache(/* cache_ptr           */ cache_ptr,
                /* destroy_entries     */ TRUE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 7 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    col_major_scan_forward(/* cache_ptr              */ cache_ptr,
                           /* lag                    */ lag,
                           /* verbose                */ FALSE,
                           /* reset_stats            */ TRUE,
                           /* display_stats          */ display_stats,
                           /* display_detailed_stats */ TRUE,
                           /* do_inserts             */ TRUE,
                           /* dirty_inserts          */ dirty_inserts,
                           /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 8 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    /* flush all entries in the cache: */

    flush_cache(/* cache_ptr           */ cache_ptr,
                /* destroy_entries     */ FALSE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 9 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    col_major_scan_backward(/* cache_ptr              */ cache_ptr,
                            /* lag                    */ lag,
                            /* verbose                */ FALSE,
                            /* reset_stats            */ TRUE,
                            /* display_stats          */ display_stats,
                            /* display_detailed_stats */ TRUE,
                            /* do_inserts             */ TRUE,
                            /* dirty_inserts          */ dirty_inserts,
                            /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 10 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    takedown_cache(cache_ptr, display_stats, TRUE);

    if ( show_progress ) /* 11 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    verify_clean();
    verify_unprotected();

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass ) {

        HDfprintf(stdout, "%s(): failure_mssg = \"%s\".\n",
                  fcn_name, failure_mssg);
    }

    return;

} /* smoke_check_1() */


/*-------------------------------------------------------------------------
 * Function:	smoke_check_2()
 *
 * Purpose:	A basic functional test, with inserts, destroys, and
 *		renames in the mix, along with some repeated protects
 *		and unprotects.  About half the entries are marked as
 *		dirty.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/24/04
 *
 * Modifications:
 *
 *		JRM -- 1/18/05
 *		Added code to skip this test if the skip_long_tests global
 *		is true.
 *
 *-------------------------------------------------------------------------
 */

static void
smoke_check_2(void)
{
    const char * fcn_name = "smoke_check_2";
    hbool_t show_progress = FALSE;
    hbool_t dirty_inserts = TRUE;
    int dirty_unprotects = TRUE;
    int dirty_destroys = TRUE;
    hbool_t display_stats = FALSE;
    int32_t lag = 10;
    int mile_stone = 1;
    H5C_t * cache_ptr = NULL;

    TESTING("smoke check #2 -- ~1/2 dirty, ins, dest, ren, 4/2 MB cache");

    if ( skip_long_tests ) {

        SKIPPED();

        HDfprintf(stdout, "	Long tests disabled.\n");

        return;
    }

    pass = TRUE;

    if ( show_progress ) /* 1 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    reset_entries();

    if ( show_progress ) /* 2 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    cache_ptr = setup_cache((size_t)(4 * 1024 * 1024),
                            (size_t)(2 * 1024 * 1024));

    if ( show_progress ) /* 3 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    row_major_scan_forward(/* cache_ptr              */ cache_ptr,
                           /* lag                    */ lag,
                           /* verbose                */ FALSE,
                           /* reset_stats            */ TRUE,
                           /* display_stats          */ display_stats,
                           /* display_detailed_stats */ TRUE,
                           /* do_inserts             */ TRUE,
                           /* dirty_inserts          */ dirty_inserts,
                           /* do_renames             */ TRUE,
                           /* rename_to_main_addr    */ FALSE,
                           /* do_destroys            */ TRUE,
                           /* dirty_destroys         */ dirty_destroys,
                           /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 4 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    row_major_scan_backward(/* cache_ptr              */ cache_ptr,
                            /* lag                    */ lag,
                            /* verbose                */ FALSE,
                            /* reset_stats            */ TRUE,
                            /* display_stats          */ display_stats,
                            /* display_detailed_stats */ TRUE,
                            /* do_inserts             */ FALSE,
                            /* dirty_inserts          */ dirty_inserts,
                            /* do_renames             */ TRUE,
                            /* rename_to_main_addr    */ TRUE,
                            /* do_destroys            */ FALSE,
                            /* dirty_destroys         */ dirty_destroys,
                            /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 5 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    row_major_scan_forward(/* cache_ptr              */ cache_ptr,
                           /* lag                    */ lag,
                           /* verbose                */ FALSE,
                           /* reset_stats            */ TRUE,
                           /* display_stats          */ display_stats,
                           /* display_detailed_stats */ TRUE,
                           /* do_inserts             */ TRUE,
                           /* dirty_inserts          */ dirty_inserts,
                           /* do_renames             */ TRUE,
                           /* rename_to_main_addr    */ FALSE,
                           /* do_destroys            */ FALSE,
                           /* dirty_destroys         */ dirty_destroys,
                           /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 6 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    /* flush and destroy all entries in the cache: */

    flush_cache(/* cache_ptr           */ cache_ptr,
                /* destroy_entries     */ TRUE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 7 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    col_major_scan_forward(/* cache_ptr              */ cache_ptr,
                           /* lag                    */ lag,
                           /* verbose                */ FALSE,
                           /* reset_stats            */ TRUE,
                           /* display_stats          */ display_stats,
                           /* display_detailed_stats */ TRUE,
                           /* do_inserts             */ TRUE,
                           /* dirty_inserts          */ dirty_inserts,
                           /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 8 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    /* flush all entries in the cache: */

    flush_cache(/* cache_ptr           */ cache_ptr,
                /* destroy_entries     */ FALSE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 9 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    col_major_scan_backward(/* cache_ptr              */ cache_ptr,
                            /* lag                    */ lag,
                            /* verbose                */ FALSE,
                            /* reset_stats            */ TRUE,
                            /* display_stats          */ display_stats,
                            /* display_detailed_stats */ TRUE,
                            /* do_inserts             */ TRUE,
                            /* dirty_inserts          */ dirty_inserts,
                            /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 10 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    takedown_cache(cache_ptr, display_stats, TRUE);

    if ( show_progress ) /* 11 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    verify_clean();
    verify_unprotected();

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass ) {

        HDfprintf(stdout, "%s(): failure_mssg = \"%s\".\n",
                  fcn_name, failure_mssg);
    }

    return;

} /* smoke_check_2() */


/*-------------------------------------------------------------------------
 * Function:	smoke_check_3()
 *
 * Purpose:	A basic functional test on a tiny cache, with inserts,
 *		destroys, and renames in the mix, along with repeated
 *		protects and unprotects.  All entries are marked as clean.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/16/04
 *
 * Modifications:
 *
 *		JRM -- 1/18/05
 *		Added code to skip this test if the skip_long_tests global
 *		is true.
 *
 *-------------------------------------------------------------------------
 */

static void
smoke_check_3(void)
{
    const char * fcn_name = "smoke_check_3";
    hbool_t show_progress = FALSE;
    hbool_t dirty_inserts = FALSE;
    int dirty_unprotects = FALSE;
    int dirty_destroys = FALSE;
    hbool_t display_stats = FALSE;
    int32_t lag = 10;
    int mile_stone = 1;
    H5C_t * cache_ptr = NULL;

    TESTING("smoke check #3 -- all clean, ins, dest, ren, 2/1 KB cache");

    if ( skip_long_tests ) {

        SKIPPED();

        HDfprintf(stdout, "	Long tests disabled.\n");

        return;
    }

    pass = TRUE;

    if ( show_progress ) /* 1 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    reset_entries();

    if ( show_progress ) /* 2 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    cache_ptr = setup_cache((size_t)(2 * 1024),
                            (size_t)(1 * 1024));

    if ( show_progress ) /* 3 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    row_major_scan_forward(/* cache_ptr              */ cache_ptr,
                           /* lag                    */ lag,
                           /* verbose                */ FALSE,
                           /* reset_stats            */ TRUE,
                           /* display_stats          */ display_stats,
                           /* display_detailed_stats */ TRUE,
                           /* do_inserts             */ TRUE,
                           /* dirty_inserts          */ dirty_inserts,
                           /* do_renames             */ TRUE,
                           /* rename_to_main_addr    */ FALSE,
                           /* do_destroys            */ TRUE,
                           /* dirty_destroys         */ dirty_destroys,
                           /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 4 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    row_major_scan_backward(/* cache_ptr              */ cache_ptr,
                            /* lag                    */ lag,
                            /* verbose                */ FALSE,
                            /* reset_stats            */ TRUE,
                            /* display_stats          */ display_stats,
                            /* display_detailed_stats */ TRUE,
                            /* do_inserts             */ FALSE,
                            /* dirty_inserts          */ dirty_inserts,
                            /* do_renames             */ TRUE,
                            /* rename_to_main_addr    */ TRUE,
                            /* do_destroys            */ FALSE,
                            /* dirty_destroys         */ dirty_destroys,
                            /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 5 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    row_major_scan_forward(/* cache_ptr              */ cache_ptr,
                           /* lag                    */ lag,
                           /* verbose                */ FALSE,
                           /* reset_stats            */ TRUE,
                           /* display_stats          */ display_stats,
                           /* display_detailed_stats */ TRUE,
                           /* do_inserts             */ TRUE,
                           /* dirty_inserts          */ dirty_inserts,
                           /* do_renames             */ TRUE,
                           /* rename_to_main_addr    */ FALSE,
                           /* do_destroys            */ FALSE,
                           /* dirty_destroys         */ dirty_destroys,
                           /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 6 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    /* flush and destroy all entries in the cache: */

    flush_cache(/* cache_ptr           */ cache_ptr,
                /* destroy_entries     */ TRUE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 7 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    col_major_scan_forward(/* cache_ptr              */ cache_ptr,
                           /* lag                    */ lag,
                           /* verbose                */ FALSE,
                           /* reset_stats            */ TRUE,
                           /* display_stats          */ display_stats,
                           /* display_detailed_stats */ TRUE,
                           /* do_inserts             */ TRUE,
                           /* dirty_inserts          */ dirty_inserts,
                           /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 8 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    /* flush all entries in the cache: */

    flush_cache(/* cache_ptr           */ cache_ptr,
                /* destroy_entries     */ FALSE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 9 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    col_major_scan_backward(/* cache_ptr              */ cache_ptr,
                            /* lag                    */ lag,
                            /* verbose                */ FALSE,
                            /* reset_stats            */ TRUE,
                            /* display_stats          */ display_stats,
                            /* display_detailed_stats */ TRUE,
                            /* do_inserts             */ TRUE,
                            /* dirty_inserts          */ dirty_inserts,
                            /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 10 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    takedown_cache(cache_ptr, display_stats, TRUE);

    if ( show_progress ) /* 11 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    verify_clean();
    verify_unprotected();

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass ) {

        HDfprintf(stdout, "%s(): failure_mssg = \"%s\".\n",
                  fcn_name, failure_mssg);
    }

    return;

} /* smoke_check_3() */


/*-------------------------------------------------------------------------
 * Function:	smoke_check_4()
 *
 * Purpose:	A basic functional test on a tiny cache, with inserts,
 *	 	destroys, and renames in the mix, along with repeated
 *		protects and unprotects.  About half the entries are
 *		marked as dirty.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/24/04
 *
 * Modifications:
 *
 *		JRM -- 1/18/05
 *		Added code to skip this test if the skip_long_tests global
 *		is true.
 *
 *-------------------------------------------------------------------------
 */

static void
smoke_check_4(void)
{
    const char * fcn_name = "smoke_check_4";
    hbool_t show_progress = FALSE;
    hbool_t dirty_inserts = TRUE;
    int dirty_unprotects = TRUE;
    int dirty_destroys = TRUE;
    hbool_t display_stats = FALSE;
    int32_t lag = 10;
    int mile_stone = 1;
    H5C_t * cache_ptr = NULL;

    TESTING("smoke check #4 -- ~1/2 dirty, ins, dest, ren, 2/1 KB cache");

    if ( skip_long_tests ) {

        SKIPPED();

        HDfprintf(stdout, "	Long tests disabled.\n");

        return;
    }

    pass = TRUE;

    if ( show_progress ) /* 1 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    reset_entries();

    if ( show_progress ) /* 2 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    cache_ptr = setup_cache((size_t)(2 * 1024),
                            (size_t)(1 * 1024));

    if ( show_progress ) /* 3 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    row_major_scan_forward(/* cache_ptr              */ cache_ptr,
                           /* lag                    */ lag,
                           /* verbose                */ FALSE,
                           /* reset_stats            */ TRUE,
                           /* display_stats          */ display_stats,
                           /* display_detailed_stats */ TRUE,
                           /* do_inserts             */ TRUE,
                           /* dirty_inserts          */ dirty_inserts,
                           /* do_renames             */ TRUE,
                           /* rename_to_main_addr    */ FALSE,
                           /* do_destroys            */ TRUE,
                           /* dirty_destroys         */ dirty_destroys,
                           /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 4 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    row_major_scan_backward(/* cache_ptr              */ cache_ptr,
                            /* lag                    */ lag,
                            /* verbose                */ FALSE,
                            /* reset_stats            */ TRUE,
                            /* display_stats          */ display_stats,
                            /* display_detailed_stats */ TRUE,
                            /* do_inserts             */ FALSE,
                            /* dirty_inserts          */ dirty_inserts,
                            /* do_renames             */ TRUE,
                            /* rename_to_main_addr    */ TRUE,
                            /* do_destroys            */ FALSE,
                            /* dirty_destroys         */ dirty_destroys,
                            /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 5 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    row_major_scan_forward(/* cache_ptr              */ cache_ptr,
                           /* lag                    */ lag,
                           /* verbose                */ FALSE,
                           /* reset_stats            */ TRUE,
                           /* display_stats          */ display_stats,
                           /* display_detailed_stats */ TRUE,
                           /* do_inserts             */ TRUE,
                           /* dirty_inserts          */ dirty_inserts,
                           /* do_renames             */ TRUE,
                           /* rename_to_main_addr    */ FALSE,
                           /* do_destroys            */ FALSE,
                           /* dirty_destroys         */ dirty_destroys,
                           /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 6 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    /* flush and destroy all entries in the cache: */

    flush_cache(/* cache_ptr           */ cache_ptr,
                /* destroy_entries     */ TRUE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 7 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    col_major_scan_forward(/* cache_ptr              */ cache_ptr,
                           /* lag                    */ lag,
                           /* verbose                */ FALSE,
                           /* reset_stats            */ TRUE,
                           /* display_stats          */ display_stats,
                           /* display_detailed_stats */ TRUE,
                           /* do_inserts             */ TRUE,
                           /* dirty_inserts          */ dirty_inserts,
                           /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 8 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    /* flush all entries in the cache: */

    flush_cache(/* cache_ptr           */ cache_ptr,
                /* destroy_entries     */ FALSE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 9 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    col_major_scan_backward(/* cache_ptr              */ cache_ptr,
                            /* lag                    */ lag,
                            /* verbose                */ FALSE,
                            /* reset_stats            */ TRUE,
                            /* display_stats          */ display_stats,
                            /* display_detailed_stats */ TRUE,
                            /* do_inserts             */ TRUE,
                            /* dirty_inserts          */ dirty_inserts,
                            /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 10 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    takedown_cache(cache_ptr, display_stats, TRUE);

    if ( show_progress ) /* 11 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    verify_clean();
    verify_unprotected();

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass ) {

        HDfprintf(stdout, "%s(): failure_mssg = \"%s\".\n",
                  fcn_name, failure_mssg);
    }

    return;

} /* smoke_check_4() */


/*-------------------------------------------------------------------------
 * Function:	smoke_check_5()
 *
 * Purpose:	A basic functional test on a cache with automatic cache
 *		resizing enabled, with inserts in the mix, along with
 *		repeated protects and unprotects.  All entries are marked
 *		as clean.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              10/14/04
 *
 * Modifications:
 *
 *		JRM -- 1/18/05
 *		Added code to skip this test if the skip_long_tests global
 *		is true.
 *
 *-------------------------------------------------------------------------
 */

static void
smoke_check_5(void)
{
    const char * fcn_name = "smoke_check_5";
    herr_t result;
    hbool_t show_progress = FALSE;
    hbool_t dirty_inserts = FALSE;
    int dirty_unprotects = FALSE;
    hbool_t display_stats = FALSE;
    int32_t max_index = 1024;
    int mile_stone = 1;
    H5C_t * cache_ptr = NULL;
    H5C_auto_size_ctl_t auto_size_ctl =
    {
        /* int32_t     version                = */ H5C__CURR_AUTO_SIZE_CTL_VER,
#if 1
        /* H5C_auto_resize_report_fcn rpt_fcn = */ NULL,
#else
        /* H5C_auto_resize_report_fcn rpt_fcn = */ H5C_def_auto_resize_rpt_fcn,
#endif
        /* hbool_t     set_initial_size       = */ TRUE,
        /* size_t      initial_size           = */ (2 * 1024 * 1024),

        /* double      min_clean_fraction     = */ 0.1,

        /* size_t      max_size               = */ (32 * 1024 * 1025),
        /* size_t      min_size               = */ (512 * 1024),

        /* int64_t     epoch_length           = */ 50000,


        /* enum H5C_cache_incr_mode incr_mode = */ H5C_incr__threshold,

        /* double     lower_hr_threshold      = */ 0.75,

        /* double      increment              = */ 2.0,

        /* hbool_t     apply_max_increment    = */ TRUE,
        /* size_t      max_increment          = */ (4 * 1024 * 1024),


        /* enum H5C_cache_decr_mode decr_mode = */ H5C_decr__threshold,

        /* double      upper_hr_threshold     = */ 0.995,

        /* double      decrement              = */ 0.9,

        /* hbool_t     apply_max_decrement    = */ TRUE,
        /* size_t      max_decrement          = */ (1 * 1024 * 1024),

        /* int32_t     epochs_before_eviction = */ 3,

        /* hbool_t     apply_empty_reserve    = */ TRUE,
        /* double      empty_reserve          = */ 0.5
    };

    TESTING("smoke check #5 -- all clean, ins, prot, unprot, AR cache 1");

    if ( skip_long_tests ) {

        SKIPPED();

        HDfprintf(stdout, "	Long tests disabled.\n");

        return;
    }

    if ( run_full_test ) {

        max_index = (10 * 1024) - 1;
    }

    pass = TRUE;

    if ( show_progress ) /* 1 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    reset_entries();

    if ( show_progress ) /* 2 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    cache_ptr = setup_cache((size_t)(2 * 1024),
                            (size_t)(1 * 1024));

    if ( pass ) {

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 1.\n";
        }
    }

    if ( show_progress ) /* 3 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    hl_row_major_scan_forward(/* cache_ptr              */ cache_ptr,
                              /* max_index              */ max_index,
                              /* verbose                */ FALSE,
                              /* reset_stats            */ TRUE,
                              /* display_stats          */ display_stats,
                              /* display_detailed_stats */ FALSE,
                              /* do_inserts             */ FALSE,
                              /* dirty_inserts          */ dirty_inserts);

    if ( show_progress ) /* 4 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    hl_row_major_scan_backward(/* cache_ptr              */ cache_ptr,
                               /* max_index              */ max_index,
                               /* verbose                */ FALSE,
                               /* reset_stats            */ TRUE,
                               /* display_stats          */ display_stats,
                               /* display_detailed_stats */ FALSE,
                               /* do_inserts             */ FALSE,
                               /* dirty_inserts          */ dirty_inserts);

    if ( show_progress ) /* 5 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    hl_row_major_scan_forward(/* cache_ptr              */ cache_ptr,
                              /* max_index              */ max_index,
                              /* verbose                */ FALSE,
                              /* reset_stats            */ TRUE,
                              /* display_stats          */ display_stats,
                              /* display_detailed_stats */ FALSE,
                              /* do_inserts             */ TRUE,
                              /* dirty_inserts          */ dirty_inserts);

    if ( show_progress ) /* 6 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    /* flush and destroy all entries in the cache: */

    flush_cache(/* cache_ptr           */ cache_ptr,
                /* destroy_entries     */ TRUE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 7 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    hl_col_major_scan_forward(/* cache_ptr              */ cache_ptr,
                              /* max_index              */ max_index,
                              /* verbose                */ FALSE,
                              /* reset_stats            */ TRUE,
                              /* display_stats          */ display_stats,
                              /* display_detailed_stats */ FALSE,
                              /* do_inserts             */ TRUE,
                              /* dirty_inserts          */ dirty_inserts,
                              /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 8 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    /* flush all entries in the cache: */

    flush_cache(/* cache_ptr           */ cache_ptr,
                /* destroy_entries     */ FALSE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 9 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    hl_col_major_scan_backward(/* cache_ptr              */ cache_ptr,
                               /* max_index              */ max_index,
                               /* verbose                */ FALSE,
                               /* reset_stats            */ TRUE,
                               /* display_stats          */ display_stats,
                               /* display_detailed_stats */ FALSE,
                               /* do_inserts             */ TRUE,
                               /* dirty_inserts          */ dirty_inserts,
                               /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 10 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    takedown_cache(cache_ptr, display_stats, TRUE);

    if ( show_progress ) /* 11 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    verify_clean();
    verify_unprotected();

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass ) {

        HDfprintf(stdout, "%s(): failure_mssg = \"%s\".\n",
                  fcn_name, failure_mssg);
    }

    return;

} /* smoke_check_5() */


/*-------------------------------------------------------------------------
 * Function:	smoke_check_6()
 *
 * Purpose:	A basic functional test on a cache with automatic cache
 *		resizing enabled, with inserts in the mix, along with
 *              repeated protects and unprotects.  About one half of all
 *		entries are marked as dirty.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              10/25/04
 *
 * Modifications:
 *
 *		JRM -- 1/18/05
 *		Added code to skip this test if the skip_long_tests global
 *		is true.
 *
 *-------------------------------------------------------------------------
 */

static void
smoke_check_6(void)
{
    const char * fcn_name = "smoke_check_6";
    herr_t result;
    hbool_t show_progress = FALSE;
    hbool_t dirty_inserts = TRUE;
    int dirty_unprotects = FALSE;
    hbool_t display_stats = FALSE;
    int mile_stone = 1;
    int32_t max_index = 1024;
    H5C_t * cache_ptr = NULL;
    H5C_auto_size_ctl_t auto_size_ctl =
    {
        /* int32_t     version                = */ H5C__CURR_AUTO_SIZE_CTL_VER,
#if 1
        /* H5C_auto_resize_report_fcn rpt_fcn = */ NULL,
#else
        /* H5C_auto_resize_report_fcn rpt_fcn = */ H5C_def_auto_resize_rpt_fcn,
#endif
        /* hbool_t     set_initial_size       = */ TRUE,
        /* size_t      initial_size           = */ (2 * 1024 * 1024),

        /* double      min_clean_fraction     = */ 0.1,

        /* size_t      max_size               = */ (32 * 1024 * 1025),
        /* size_t      min_size               = */ (512 * 1024),

        /* int64_t     epoch_length           = */ 50000,


        /* enum H5C_cache_incr_mode incr_mode = */ H5C_incr__threshold,

        /* double     lower_hr_threshold      = */ 0.75,

        /* double      increment              = */ 2.0,

        /* hbool_t     apply_max_increment    = */ TRUE,
        /* size_t      max_increment          = */ (4 * 1024 * 1024),


        /* enum H5C_cache_decr_mode decr_mode = */ H5C_decr__threshold,

        /* double      upper_hr_threshold     = */ 0.995,

        /* double      decrement              = */ 0.9,

        /* hbool_t     apply_max_decrement    = */ TRUE,
        /* size_t      max_decrement          = */ (1 * 1024 * 1024),

        /* int32_t     epochs_before_eviction = */ 3,

        /* hbool_t     apply_empty_reserve    = */ TRUE,
        /* double      empty_reserve          = */ 0.05
    };

    TESTING("smoke check #6 -- ~1/2 dirty, ins, prot, unprot, AR cache 1");

    pass = TRUE;

    if ( skip_long_tests ) {

        SKIPPED();

        HDfprintf(stdout, "	Long tests disabled.\n");

        return;
    }

    if ( run_full_test ) {

        max_index = (10 * 1024) - 1;
    }

    if ( show_progress ) /* 1 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    reset_entries();

    if ( show_progress ) /* 2 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    cache_ptr = setup_cache((size_t)(2 * 1024),
                            (size_t)(1 * 1024));

    if ( pass ) {

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 1.\n";
        }
    }

    if ( show_progress ) /* 3 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    hl_row_major_scan_forward(/* cache_ptr              */ cache_ptr,
                              /* max_index              */ max_index,
                              /* verbose                */ FALSE,
                              /* reset_stats            */ TRUE,
                              /* display_stats          */ display_stats,
                              /* display_detailed_stats */ FALSE,
                              /* do_inserts             */ FALSE,
                              /* dirty_inserts          */ dirty_inserts);

    if ( show_progress ) /* 4 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    hl_row_major_scan_backward(/* cache_ptr              */ cache_ptr,
                               /* max_index              */ max_index,
                               /* verbose                */ FALSE,
                               /* reset_stats            */ TRUE,
                               /* display_stats          */ display_stats,
                               /* display_detailed_stats */ FALSE,
                               /* do_inserts             */ FALSE,
                               /* dirty_inserts          */ dirty_inserts);

    if ( show_progress ) /* 5 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    hl_row_major_scan_forward(/* cache_ptr              */ cache_ptr,
                              /* max_index              */ max_index,
                              /* verbose                */ FALSE,
                              /* reset_stats            */ TRUE,
                              /* display_stats          */ display_stats,
                              /* display_detailed_stats */ FALSE,
                              /* do_inserts             */ TRUE,
                              /* dirty_inserts          */ dirty_inserts);

    if ( show_progress ) /* 6 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    /* flush and destroy all entries in the cache: */

    flush_cache(/* cache_ptr           */ cache_ptr,
                /* destroy_entries     */ TRUE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 7 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    hl_col_major_scan_forward(/* cache_ptr              */ cache_ptr,
                              /* max_index              */ max_index,
                              /* verbose                */ FALSE,
                              /* reset_stats            */ TRUE,
                              /* display_stats          */ display_stats,
                              /* display_detailed_stats */ FALSE,
                              /* do_inserts             */ TRUE,
                              /* dirty_inserts          */ dirty_inserts,
                              /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 8 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    /* flush all entries in the cache: */

    flush_cache(/* cache_ptr           */ cache_ptr,
                /* destroy_entries     */ FALSE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 9 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    hl_col_major_scan_backward(/* cache_ptr              */ cache_ptr,
                               /* max_index              */ max_index,
                               /* verbose                */ FALSE,
                               /* reset_stats            */ TRUE,
                               /* display_stats          */ display_stats,
                               /* display_detailed_stats */ FALSE,
                               /* do_inserts             */ TRUE,
                               /* dirty_inserts          */ dirty_inserts,
                               /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 10 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    takedown_cache(cache_ptr, display_stats, TRUE);

    if ( show_progress ) /* 11 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    verify_clean();
    verify_unprotected();

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass ) {

        HDfprintf(stdout, "%s(): failure_mssg = \"%s\".\n",
                  fcn_name, failure_mssg);
    }

    return;

} /* smoke_check_6() */


/*-------------------------------------------------------------------------
 * Function:	smoke_check_7()
 *
 * Purpose:	A basic functional test on a cache with automatic cache
 *		resizing enabled, with inserts in the mix, along with
 *		repeated protects and unprotects.  All entries are marked
 *		as clean.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              12/2/04
 *
 * Modifications:
 *
 *		JRM -- 1/18/05
 *		Added code to skip this test if the skip_long_tests global
 *		is true.
 *
 *-------------------------------------------------------------------------
 */

static void
smoke_check_7(void)
{
    const char * fcn_name = "smoke_check_7";
    herr_t result;
    hbool_t show_progress = FALSE;
    hbool_t dirty_inserts = FALSE;
    int dirty_unprotects = FALSE;
    hbool_t display_stats = FALSE;
    int mile_stone = 1;
    int32_t max_index = 1024;
    H5C_t * cache_ptr = NULL;
    H5C_auto_size_ctl_t auto_size_ctl =
    {
        /* int32_t     version                = */ H5C__CURR_AUTO_SIZE_CTL_VER,
#if 1
        /* H5C_auto_resize_report_fcn rpt_fcn = */ NULL,
#else
        /* H5C_auto_resize_report_fcn rpt_fcn = */ H5C_def_auto_resize_rpt_fcn,
#endif
        /* hbool_t     set_initial_size       = */ TRUE,
        /* size_t      initial_size           = */ (2 * 1024 * 1024),

        /* double      min_clean_fraction     = */ 0.1,

        /* size_t      max_size               = */ (32 * 1024 * 1025),
        /* size_t      min_size               = */ (512 * 1024),

        /* int64_t     epoch_length           = */ 100000,


        /* enum H5C_cache_incr_mode incr_mode = */ H5C_incr__threshold,

        /* double     lower_hr_threshold      = */ 0.75,

        /* double      increment              = */ 2.0,

        /* hbool_t     apply_max_increment    = */ TRUE,
        /* size_t      max_increment          = */ (8 * 1024 * 1024),


        /* enum H5C_cache_decr_mode decr_mode = */
                                             H5C_decr__age_out_with_threshold,

        /* double      upper_hr_threshold     = */ 0.995,

        /* double      decrement              = */ 0.9,

        /* hbool_t     apply_max_decrement    = */ TRUE,
        /* size_t      max_decrement          = */ (1 * 1024 * 1024),

        /* int32_t     epochs_before_eviction = */ 3,

        /* hbool_t     apply_empty_reserve    = */ TRUE,
        /* double      empty_reserve          = */ 0.1
    };

    TESTING("smoke check #7 -- all clean, ins, prot, unprot, AR cache 2");

    if ( skip_long_tests ) {

        SKIPPED();

        HDfprintf(stdout, "	Long tests disabled.\n");

        return;
    }

    if ( run_full_test ) {

        max_index = (10 * 1024) - 1;
    }

    pass = TRUE;

    if ( show_progress ) /* 1 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    reset_entries();

    if ( show_progress ) /* 2 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    cache_ptr = setup_cache((size_t)(2 * 1024),
                            (size_t)(1 * 1024));

    if ( pass ) {

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 1.\n";
        }
    }

    if ( show_progress ) /* 3 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    hl_row_major_scan_forward(/* cache_ptr              */ cache_ptr,
                              /* max_index              */ max_index,
                              /* verbose                */ FALSE,
                              /* reset_stats            */ TRUE,
                              /* display_stats          */ display_stats,
                              /* display_detailed_stats */ FALSE,
                              /* do_inserts             */ FALSE,
                              /* dirty_inserts          */ dirty_inserts);

    if ( show_progress ) /* 4 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    hl_row_major_scan_backward(/* cache_ptr              */ cache_ptr,
                               /* max_index              */ max_index,
                               /* verbose                */ FALSE,
                               /* reset_stats            */ TRUE,
                               /* display_stats          */ display_stats,
                               /* display_detailed_stats */ FALSE,
                               /* do_inserts             */ FALSE,
                               /* dirty_inserts          */ dirty_inserts);

    if ( show_progress ) /* 5 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    hl_row_major_scan_forward(/* cache_ptr              */ cache_ptr,
                              /* max_index              */ max_index,
                              /* verbose                */ FALSE,
                              /* reset_stats            */ TRUE,
                              /* display_stats          */ display_stats,
                              /* display_detailed_stats */ FALSE,
                              /* do_inserts             */ TRUE,
                              /* dirty_inserts          */ dirty_inserts);

    if ( show_progress ) /* 6 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    /* flush and destroy all entries in the cache: */

    flush_cache(/* cache_ptr           */ cache_ptr,
                /* destroy_entries     */ TRUE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 7 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    hl_col_major_scan_forward(/* cache_ptr              */ cache_ptr,
                              /* max_index              */ max_index,
                              /* verbose                */ FALSE,
                              /* reset_stats            */ TRUE,
                              /* display_stats          */ display_stats,
                              /* display_detailed_stats */ FALSE,
                              /* do_inserts             */ TRUE,
                              /* dirty_inserts          */ dirty_inserts,
                              /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 8 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    /* flush all entries in the cache: */

    flush_cache(/* cache_ptr           */ cache_ptr,
                /* destroy_entries     */ FALSE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 9 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    hl_col_major_scan_backward(/* cache_ptr              */ cache_ptr,
                               /* max_index              */ max_index,
                               /* verbose                */ FALSE,
                               /* reset_stats            */ TRUE,
                               /* display_stats          */ display_stats,
                               /* display_detailed_stats */ FALSE,
                               /* do_inserts             */ TRUE,
                               /* dirty_inserts          */ dirty_inserts,
                               /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 10 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    takedown_cache(cache_ptr, display_stats, TRUE);

    if ( show_progress ) /* 11 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    verify_clean();
    verify_unprotected();

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass ) {

        HDfprintf(stdout, "%s(): failure_mssg = \"%s\".\n",
                  fcn_name, failure_mssg);
    }

    return;

} /* smoke_check_7() */


/*-------------------------------------------------------------------------
 * Function:	smoke_check_8()
 *
 * Purpose:	A basic functional test on a cache with automatic cache
 *		resizing enabled, with inserts in the mix, along with
 *              repeated protects and unprotects.  About one half of all
 *		entries are marked as dirty.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              10/25/04
 *
 * Modifications:
 *
 *		JRM -- 1/18/05
 *		Added code to skip this test if the skip_long_tests global
 *		is true.
 *
 *-------------------------------------------------------------------------
 */

static void
smoke_check_8(void)
{
    const char * fcn_name = "smoke_check_8";
    herr_t result;
    hbool_t show_progress = FALSE;
    hbool_t dirty_inserts = TRUE;
    int dirty_unprotects = FALSE;
    hbool_t display_stats = FALSE;
    int mile_stone = 1;
    int32_t max_index = 1024;
    H5C_t * cache_ptr = NULL;
    H5C_auto_size_ctl_t auto_size_ctl =
    {
        /* int32_t     version                = */ H5C__CURR_AUTO_SIZE_CTL_VER,
#if 1
        /* H5C_auto_resize_report_fcn rpt_fcn = */ NULL,
#else
        /* H5C_auto_resize_report_fcn rpt_fcn = */ H5C_def_auto_resize_rpt_fcn,
#endif
        /* hbool_t     set_initial_size       = */ TRUE,
        /* size_t      initial_size           = */ (2 * 1024 * 1024),

        /* double      min_clean_fraction     = */ 0.1,

        /* size_t      max_size               = */ (32 * 1024 * 1025),
        /* size_t      min_size               = */ (512 * 1024),

        /* int64_t     epoch_length           = */ 100000,


        /* enum H5C_cache_incr_mode incr_mode = */ H5C_incr__threshold,

        /* double     lower_hr_threshold      = */ 0.75,

        /* double      increment              = */ 2.0,

        /* hbool_t     apply_max_increment    = */ TRUE,
        /* size_t      max_increment          = */ (4 * 1024 * 1024),


        /* enum H5C_cache_decr_mode decr_mode = */
                                             H5C_decr__age_out_with_threshold,

        /* double      upper_hr_threshold     = */ 0.995,

        /* double      decrement              = */ 0.9,

        /* hbool_t     apply_max_decrement    = */ TRUE,
        /* size_t      max_decrement          = */ (1 * 1024 * 1024),

        /* int32_t     epochs_before_eviction = */ 3,

        /* hbool_t     apply_empty_reserve    = */ TRUE,
        /* double      empty_reserve          = */ 0.1
    };

    TESTING("smoke check #8 -- ~1/2 dirty, ins, prot, unprot, AR cache 2");

    if ( skip_long_tests ) {

        SKIPPED();

        HDfprintf(stdout, "	Long tests disabled.\n");

        return;
    }

    if ( run_full_test ) {

        max_index = (10 * 1024) - 1;
    }

    pass = TRUE;

    if ( show_progress ) /* 1 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    reset_entries();

    if ( show_progress ) /* 2 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    cache_ptr = setup_cache((size_t)(2 * 1024),
                            (size_t)(1 * 1024));

    if ( pass ) {

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 1.\n";
        }
    }

    if ( show_progress ) /* 3 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    hl_row_major_scan_forward(/* cache_ptr              */ cache_ptr,
                              /* max_index              */ max_index,
                              /* verbose                */ FALSE,
                              /* reset_stats            */ TRUE,
                              /* display_stats          */ display_stats,
                              /* display_detailed_stats */ FALSE,
                              /* do_inserts             */ FALSE,
                              /* dirty_inserts          */ dirty_inserts);

    if ( show_progress ) /* 4 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    hl_row_major_scan_backward(/* cache_ptr              */ cache_ptr,
                               /* max_index              */ max_index,
                               /* verbose                */ FALSE,
                               /* reset_stats            */ TRUE,
                               /* display_stats          */ display_stats,
                               /* display_detailed_stats */ FALSE,
                               /* do_inserts             */ FALSE,
                               /* dirty_inserts          */ dirty_inserts);

    if ( show_progress ) /* 5 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    hl_row_major_scan_forward(/* cache_ptr              */ cache_ptr,
                              /* max_index              */ max_index,
                              /* verbose                */ FALSE,
                              /* reset_stats            */ TRUE,
                              /* display_stats          */ display_stats,
                              /* display_detailed_stats */ FALSE,
                              /* do_inserts             */ TRUE,
                              /* dirty_inserts          */ dirty_inserts);

    if ( show_progress ) /* 6 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    /* flush and destroy all entries in the cache: */

    flush_cache(/* cache_ptr           */ cache_ptr,
                /* destroy_entries     */ TRUE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 7 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    hl_col_major_scan_forward(/* cache_ptr              */ cache_ptr,
                              /* max_index              */ max_index,
                              /* verbose                */ FALSE,
                              /* reset_stats            */ TRUE,
                              /* display_stats          */ display_stats,
                              /* display_detailed_stats */ FALSE,
                              /* do_inserts             */ TRUE,
                              /* dirty_inserts          */ dirty_inserts,
                              /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 8 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    /* flush all entries in the cache: */

    flush_cache(/* cache_ptr           */ cache_ptr,
                /* destroy_entries     */ FALSE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 9 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    hl_col_major_scan_backward(/* cache_ptr              */ cache_ptr,
                               /* max_index              */ max_index,
                               /* verbose                */ FALSE,
                               /* reset_stats            */ TRUE,
                               /* display_stats          */ display_stats,
                               /* display_detailed_stats */ FALSE,
                               /* do_inserts             */ TRUE,
                               /* dirty_inserts          */ dirty_inserts,
                               /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 10 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    takedown_cache(cache_ptr, display_stats, TRUE);

    if ( show_progress ) /* 11 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    verify_clean();
    verify_unprotected();

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass ) {

        HDfprintf(stdout, "%s(): failure_mssg = \"%s\".\n",
                  fcn_name, failure_mssg);
    }

    return;

} /* smoke_check_8() */


/*-------------------------------------------------------------------------
 * Function:	write_permitted_check()
 *
 * Purpose:	A basic test of the write permitted function.  In essence,
 *		we load the cache up with dirty entryies, set
 *		write_permitted to FALSE, and then protect a bunch of
 *		entries.  If there are any writes while write_permitted is
 *		FALSE, the test will fail.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/24/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
write_permitted_check(void)
{

#if H5C_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS

    const char * fcn_name = "write_permitted_check";
    hbool_t show_progress = FALSE;
    hbool_t display_stats = FALSE;
    int32_t lag = 10;
    int mile_stone = 1;
    H5C_t * cache_ptr = NULL;

#endif /* H5C_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS */

    TESTING("write permitted check -- 1/0 MB cache");

#if H5C_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS

    pass = TRUE;

    if ( show_progress ) /* 1 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    reset_entries();

    if ( show_progress ) /* 2 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    cache_ptr = setup_cache((size_t)(1 * 1024 * 1024),
                            (size_t)(0));

    if ( show_progress ) /* 3 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    row_major_scan_forward(/* cache_ptr              */ cache_ptr,
                           /* lag                    */ lag,
                           /* verbose                */ FALSE,
                           /* reset_stats            */ TRUE,
                           /* display_stats          */ display_stats,
                           /* display_detailed_stats */ TRUE,
                           /* do_inserts             */ TRUE,
                           /* dirty_inserts          */ TRUE,
                           /* do_renames             */ TRUE,
                           /* rename_to_main_addr    */ FALSE,
                           /* do_destroys            */ TRUE,
                           /* dirty_destroys         */ TRUE,
                           /* dirty_unprotects       */ TRUE);

    if ( show_progress ) /* 4 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    write_permitted = FALSE;

    row_major_scan_backward(/* cache_ptr              */ cache_ptr,
                            /* lag                    */ lag,
                            /* verbose                */ FALSE,
                            /* reset_stats            */ TRUE,
                            /* display_stats          */ display_stats,
                            /* display_detailed_stats */ TRUE,
                            /* do_inserts             */ FALSE,
                            /* dirty_inserts          */ FALSE,
                            /* do_renames             */ TRUE,
                            /* rename_to_main_addr    */ TRUE,
                            /* do_destroys            */ FALSE,
                            /* dirty_destroys         */ FALSE,
                            /* dirty_unprotects       */ NO_CHANGE);

    if ( show_progress ) /* 5 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    write_permitted = TRUE;

    row_major_scan_forward(/* cache_ptr              */ cache_ptr,
                           /* lag                    */ lag,
                           /* verbose                */ FALSE,
                           /* reset_stats            */ TRUE,
                           /* display_stats          */ display_stats,
                           /* display_detailed_stats */ TRUE,
                           /* do_inserts             */ TRUE,
                           /* dirty_inserts          */ TRUE,
                           /* do_renames             */ TRUE,
                           /* rename_to_main_addr    */ FALSE,
                           /* do_destroys            */ FALSE,
                           /* dirty_destroys         */ TRUE,
                           /* dirty_unprotects       */ TRUE);

    if ( show_progress ) /* 6 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    /* flush and destroy all entries in the cache: */

    flush_cache(/* cache_ptr           */ cache_ptr,
                /* destroy_entries     */ TRUE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 7 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    col_major_scan_forward(/* cache_ptr              */ cache_ptr,
                           /* lag                    */ lag,
                           /* verbose                */ FALSE,
                           /* reset_stats            */ TRUE,
                           /* display_stats          */ display_stats,
                           /* display_detailed_stats */ TRUE,
                           /* do_inserts             */ TRUE,
                           /* dirty_inserts          */ TRUE,
                           /* dirty_unprotects       */ TRUE);

    if ( show_progress ) /* 8 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    write_permitted = FALSE;

    col_major_scan_backward(/* cache_ptr              */ cache_ptr,
                            /* lag                    */ lag,
                            /* verbose                */ FALSE,
                            /* reset_stats            */ TRUE,
                            /* display_stats          */ display_stats,
                            /* display_detailed_stats */ TRUE,
                            /* do_inserts             */ FALSE,
                            /* dirty_inserts          */ FALSE,
                            /* dirty_unprotects       */ NO_CHANGE);

    write_permitted = TRUE;

    if ( show_progress ) /* 9 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    takedown_cache(cache_ptr, display_stats, TRUE);

    if ( show_progress ) /* 10 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    verify_clean();
    verify_unprotected();

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass ) {

        HDfprintf(stdout, "%s(): failure_mssg = \"%s\".\n",
                  fcn_name, failure_mssg);
    }

#else /* H5C_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS */

    SKIPPED();

    HDfprintf(stdout, "	Clean and dirty LRU lists disabled.\n");

#endif /* H5C_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS */

    return;

} /* write_permitted_check() */


/*-------------------------------------------------------------------------
 * Function:	check_flush_cache()
 *
 * Purpose:	Verify that flush_cache behaves as expected.  In particular,
 *		test the behaviour with different flags.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              1/10/05
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
check_flush_cache(void)
{
    const char * fcn_name = "check_flush_cache";
    H5C_t *      cache_ptr = NULL;

    TESTING("H5C_flush_cache() functionality");

    pass = TRUE;

    /* allocate a cache, and flush it under various circumstances.
     * To the extent possible, verify that the desired actions took
     * place.
     */

    if ( pass ) {

        reset_entries();

        cache_ptr = setup_cache((size_t)(2 * 1024 * 1024),
                                (size_t)(1 * 1024 * 1024));
    }

    /* first test behaviour on an empty cache.  Can't do much sanity
     * checking in this case, so simply check the return values.
     */

    if ( pass ) {

        check_flush_cache__empty_cache(cache_ptr);
    }

    /* now do a series of similar tests with a cache with a single entry.
     * Start with a clean entry, with no flags set.
     */

    if ( pass ) {

        check_flush_cache__single_entry(cache_ptr);
    }

    if ( pass ) {

        check_flush_cache__multi_entry(cache_ptr);
    }

    if ( pass ) {

        takedown_cache(cache_ptr, FALSE, FALSE);
    }

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass ) {

        HDfprintf(stdout, "%s(): failure_mssg = \"%s\".\n",
                  fcn_name, failure_mssg);
    }

    return;

} /* check_flush_cache() */


/*-------------------------------------------------------------------------
 * Function:	check_flush_cache__empty_cache()
 *
 * Purpose:	Verify that flush_cache behaves as expected with an empty
 *              cache.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              1/12/05
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
check_flush_cache__empty_cache(H5C_t * cache_ptr)
{
    /* const char * fcn_name = "check_flush_cache__empty_cache"; */
    herr_t	 result;

    if ( cache_ptr == NULL ) {

        pass = FALSE;
        failure_mssg = "cache_ptr NULL on entry to empty cache case.";
    }
    else if ( ( cache_ptr->index_len != 0 ) ||
              ( cache_ptr->index_size != 0 ) ) {

        pass = FALSE;
        failure_mssg = "cache not empty at beginning of empty cache case.";
    }


    /* Test behaviour on an empty cache.  Can't do much sanity
     * checking in this case, so simply check the return values.
     */

    if ( pass ) {

        result = H5C_flush_cache(NULL, -1, -1, cache_ptr, H5C__NO_FLAGS_SET);

        if ( result < 0 ) {

            pass = FALSE;
            failure_mssg = "flush with flags = 0x00 failed on empty cache.\n";
        }
    }

    if ( pass ) {

        result = H5C_flush_cache(NULL, -1, -1, cache_ptr,
                                 H5C__FLUSH_INVALIDATE_FLAG);

        if ( result < 0 ) {

            pass = FALSE;
            failure_mssg = "flush with flags = 0x04 failed on empty cache.\n";
        }
    }

    if ( pass ) {

        result = H5C_flush_cache(NULL, -1, -1, cache_ptr,
                                 H5C__FLUSH_CLEAR_ONLY_FLAG);

        if ( result < 0 ) {

            pass = FALSE;
            failure_mssg = "flush with flags = 0x08 failed on empty cache.\n";
        }
    }


    if ( pass ) {

        result = H5C_flush_cache(NULL, -1, -1, cache_ptr,
                                 H5C__FLUSH_MARKED_ENTRIES_FLAG);

        if ( result < 0 ) {

            pass = FALSE;
            failure_mssg = "flush with flags = 0x10 failed on empty cache.\n";
        }
    }

} /* check_flush_cache__empty_cache() */


/*-------------------------------------------------------------------------
 * Function:	check_flush_cache__multi_entry()
 *
 * Purpose:	Verify that flush_cache behaves as expected when the cache
 *		contains multiple elements.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              1/14/05
 *
 * Modifications:
 *
 * 		JRM -- 4/5/06
 * 		Added pinned entry tests.
 *
 *-------------------------------------------------------------------------
 */

static void
check_flush_cache__multi_entry(H5C_t * cache_ptr)
{
    /* const char * fcn_name = "check_flush_cache__multi_entry"; */

    if ( cache_ptr == NULL ) {

        pass = FALSE;
        failure_mssg = "cache_ptr NULL on entry to multi entry case.";
    }
    else if ( ( cache_ptr->index_len != 0 ) ||
              ( cache_ptr->index_size != 0 ) ) {

        pass = FALSE;
        failure_mssg = "cache not empty at beginning of multi entry case.";
    }

    if ( pass )
    {
        int test_num                         = 1;
        unsigned int flush_flags             = H5C__NO_FLAGS_SET;
        int spec_size                        = 8;
        struct flush_cache_test_spec spec[8] =
        {
          {
            /* entry_num          = */ 0,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 100,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 1,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 75,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 2,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 25,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 3,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 50,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 4,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 10,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 5,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 20,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 6,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 30,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 7,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 40,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ FALSE
          }
        };

        check_flush_cache__multi_entry_test(cache_ptr, test_num,
                                            flush_flags, spec_size, spec);
    }


    if ( pass )
    {
        int test_num                         = 2;
        unsigned int flush_flags             = H5C__FLUSH_INVALIDATE_FLAG;
        int spec_size                        = 8;
        struct flush_cache_test_spec spec[8] =
        {
          {
            /* entry_num          = */ 0,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 100,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 1,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 75,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 2,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 25,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 3,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 50,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 4,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 10,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 5,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 20,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 6,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 30,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 7,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 40,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ TRUE
          }
        };

        check_flush_cache__multi_entry_test(cache_ptr, test_num,
                                            flush_flags, spec_size, spec);
    }


    if ( pass )
    {
        int test_num                         = 3;
        unsigned int flush_flags             = H5C__FLUSH_CLEAR_ONLY_FLAG;
        int spec_size                        = 8;
        struct flush_cache_test_spec spec[8] =
        {
          {
            /* entry_num          = */ 0,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 100,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 1,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 75,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 2,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 25,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 3,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 50,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 4,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 10,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 5,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 20,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 6,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 30,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 7,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 40,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ FALSE
          }
        };

        check_flush_cache__multi_entry_test(cache_ptr, test_num,
                                            flush_flags, spec_size, spec);
    }


    if ( pass )
    {
        int test_num                         = 4;
        unsigned int flush_flags             = H5C__FLUSH_MARKED_ENTRIES_FLAG;
        int spec_size                        = 8;
        struct flush_cache_test_spec spec[8] =
        {
          {
            /* entry_num          = */ 0,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 100,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 1,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 75,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 2,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 25,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 3,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 50,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 4,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 10,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 5,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 20,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 6,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 30,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 7,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 40,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ FALSE
          }
        };

        check_flush_cache__multi_entry_test(cache_ptr, test_num,
                                            flush_flags, spec_size, spec);
    }


    if ( pass )
    {
        int test_num                         = 5;
        unsigned int flush_flags             = H5C__FLUSH_INVALIDATE_FLAG |
                                               H5C__FLUSH_CLEAR_ONLY_FLAG;
        int spec_size                        = 8;
        struct flush_cache_test_spec spec[8] =
        {
          {
            /* entry_num          = */ 0,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 100,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 1,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 75,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 2,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 25,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 3,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 50,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 4,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 10,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 5,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 20,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 6,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 30,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 7,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 40,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ TRUE
          }
        };

        check_flush_cache__multi_entry_test(cache_ptr, test_num,
                                            flush_flags, spec_size, spec);
    }


    if ( pass )
    {
        int test_num                         = 6;
        unsigned int flush_flags             = H5C__FLUSH_INVALIDATE_FLAG |
                                               H5C__FLUSH_MARKED_ENTRIES_FLAG;
        int spec_size                        = 8;
        struct flush_cache_test_spec spec[8] =
        {
          {
            /* entry_num          = */ 0,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 100,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 1,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 75,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 2,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 25,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 3,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 50,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 4,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 10,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 5,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 20,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 6,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 30,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 7,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 40,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ TRUE
          }
        };

        check_flush_cache__multi_entry_test(cache_ptr, test_num,
                                            flush_flags, spec_size, spec);
    }


    if ( pass )
    {
        int test_num                         = 7;
        unsigned int flush_flags             = H5C__FLUSH_CLEAR_ONLY_FLAG |
                                               H5C__FLUSH_MARKED_ENTRIES_FLAG;
        int spec_size                        = 8;
        struct flush_cache_test_spec spec[8] =
        {
          {
            /* entry_num          = */ 0,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 100,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 1,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 75,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 2,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 25,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 3,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 50,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 4,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 10,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 5,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 20,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 6,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 30,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 7,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 40,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ FALSE
          }
        };

        check_flush_cache__multi_entry_test(cache_ptr, test_num,
                                            flush_flags, spec_size, spec);
    }


    if ( pass )
    {
        int test_num                         = 8;
        unsigned int flush_flags             = H5C__FLUSH_INVALIDATE_FLAG |
                                               H5C__FLUSH_CLEAR_ONLY_FLAG |
                                               H5C__FLUSH_MARKED_ENTRIES_FLAG;
        int spec_size                        = 8;
        struct flush_cache_test_spec spec[8] =
        {
          {
            /* entry_num          = */ 0,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 100,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 1,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 75,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 2,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 25,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 3,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 50,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 4,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 10,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 5,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 20,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 6,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 30,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 7,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 40,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ TRUE
          }
        };

        check_flush_cache__multi_entry_test(cache_ptr, test_num,
                                            flush_flags, spec_size, spec);
    }


    /* verify that all other flags are ignored */
    if ( pass )
    {
        int test_num                         = 9;
        unsigned int flush_flags             = (unsigned)
                                               ~(H5C__FLUSH_INVALIDATE_FLAG |
                                                H5C__FLUSH_CLEAR_ONLY_FLAG |
                                                H5C__FLUSH_MARKED_ENTRIES_FLAG);
        int spec_size                        = 8;
        struct flush_cache_test_spec spec[8] =
        {
          {
            /* entry_num          = */ 0,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 100,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 1,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 75,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 2,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 25,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 3,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 50,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__NO_FLAGS_SET,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 4,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 10,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 5,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 20,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 6,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 30,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 7,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 40,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ FALSE
          }
        };

        check_flush_cache__multi_entry_test(cache_ptr, test_num,
                                            flush_flags, spec_size, spec);
    }

    /* Now do pinned entry tests:
     *
     * For the most part, this test is directed at testing the ability
     * of the flush routine to unravel collections of pinned entries.
     */

    if ( pass )
    {
        int test_num                            = 1;
        unsigned int flush_flags                = H5C__NO_FLAGS_SET;
        int spec_size                           = 8;
        struct pe_flush_cache_test_spec spec[8] =
        {
          {
            /* entry_num          = */ 0,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 100,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__NO_FLAGS_SET,
	    /* num_pins           = */ 0,
	    /* pin_type[MAX_PINS] = */ {-1, -1, -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]  = */ {-1, -1, -1, -1, -1, -1, -1, -1},
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 1,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 75,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__NO_FLAGS_SET,
	    /* num_pins           = */ 1,
	    /* pin_type[MAX_PINS] = */ {PICO_ENTRY_TYPE,
		                        -1, -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]  = */ {100, -1, -1, -1, -1, -1, -1, -1},
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 2,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 25,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__NO_FLAGS_SET,
	    /* num_pins           = */ 2,
	    /* pin_type[MAX_PINS] = */ {PICO_ENTRY_TYPE,
		                        PICO_ENTRY_TYPE,
		                        -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]  = */ {100, 75, -1, -1, -1, -1, -1, -1},
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 3,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 50,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__NO_FLAGS_SET,
	    /* num_pins           = */ 3,
	    /* pin_type[MAX_PINS] = */ {PICO_ENTRY_TYPE,
		                        PICO_ENTRY_TYPE,
					PICO_ENTRY_TYPE,
		                        -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]  = */ {100, 75, 25, -1, -1, -1, -1, -1},
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 4,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 10,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
	    /* num_pins           = */ 4,
	    /* pin_type[MAX_PINS] = */ {PICO_ENTRY_TYPE,
                                        PICO_ENTRY_TYPE,
                                        PICO_ENTRY_TYPE,
					PICO_ENTRY_TYPE,
					-1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]  = */ {100, 75, 25, 50, -1, -1, -1, -1},
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 5,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 20,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
	    /* num_pins           = */ 5,
	    /* pin_type[MAX_PINS] = */ {PICO_ENTRY_TYPE,
                                        PICO_ENTRY_TYPE,
                                        PICO_ENTRY_TYPE,
					PICO_ENTRY_TYPE,
					MONSTER_ENTRY_TYPE,
					-1, -1, -1},
	    /* pin_idx[MAX_PINS]  = */ {100, 75, 25, 50, 10, -1, -1, -1},
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 6,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 30,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
	    /* num_pins           = */ 6,
	    /* pin_type[MAX_PINS] = */ {PICO_ENTRY_TYPE,
                                        PICO_ENTRY_TYPE,
                                        PICO_ENTRY_TYPE,
					PICO_ENTRY_TYPE,
					MONSTER_ENTRY_TYPE,
					MONSTER_ENTRY_TYPE,
					-1, -1},
	    /* pin_idx[MAX_PINS]  = */ {100, 75, 25, 50, 10, 20, -1, -1},
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ FALSE
          },
          {
            /* entry_num          = */ 7,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 40,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
	    /* num_pins           = */ 7,
	    /* pin_type[MAX_PINS] = */ {PICO_ENTRY_TYPE,
                                        PICO_ENTRY_TYPE,
                                        PICO_ENTRY_TYPE,
					PICO_ENTRY_TYPE,
					MONSTER_ENTRY_TYPE,
					MONSTER_ENTRY_TYPE,
					MONSTER_ENTRY_TYPE,
					-1},
	    /* pin_idx[MAX_PINS]  = */ {100, 75, 25, 50, 10, 20, 30, -1},
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ FALSE
          }
        };

        check_flush_cache__pe_multi_entry_test(cache_ptr, test_num,
                                               flush_flags, spec_size, spec);
    }


    if ( pass )
    {
        int test_num                            = 2;
        unsigned int flush_flags                = H5C__FLUSH_INVALIDATE_FLAG;
        int spec_size                           = 8;
        struct pe_flush_cache_test_spec spec[8] =
        {
          {
            /* entry_num          = */ 0,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 100,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__NO_FLAGS_SET,
	    /* num_pins           = */ 0,
	    /* pin_type[MAX_PINS] = */ {-1, -1, -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]  = */ {-1, -1, -1, -1, -1, -1, -1, -1},
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 1,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 75,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__NO_FLAGS_SET,
	    /* num_pins           = */ 1,
	    /* pin_type[MAX_PINS] = */ {PICO_ENTRY_TYPE,
		                        -1, -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]  = */ {100, -1, -1, -1, -1, -1, -1, -1},
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 2,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 25,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__NO_FLAGS_SET,
	    /* num_pins           = */ 2,
	    /* pin_type[MAX_PINS] = */ {PICO_ENTRY_TYPE,
		                        PICO_ENTRY_TYPE,
		                        -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]  = */ {100, 75, -1, -1, -1, -1, -1, -1},
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 3,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 50,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__NO_FLAGS_SET,
	    /* num_pins           = */ 3,
	    /* pin_type[MAX_PINS] = */ {PICO_ENTRY_TYPE,
		                        PICO_ENTRY_TYPE,
					PICO_ENTRY_TYPE,
		                        -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]  = */ {100, 75, 25, -1, -1, -1, -1, -1},
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 4,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 10,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
	    /* num_pins           = */ 0,
	    /* pin_type[MAX_PINS] = */ {-1, -1, -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]  = */ {-1, -1, -1, -1, -1, -1, -1, -1},
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 5,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 20,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
	    /* num_pins           = */ 1,
	    /* pin_type[MAX_PINS] = */ {MONSTER_ENTRY_TYPE,
					-1, -1, -1, -1 -1, -1, -1},
	    /* pin_idx[MAX_PINS]  = */ {10, -1, -1, -1 -1, -1, -1, -1},
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 6,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 30,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
	    /* num_pins           = */ 2,
	    /* pin_type[MAX_PINS] = */ {MONSTER_ENTRY_TYPE,
					MONSTER_ENTRY_TYPE,
					-1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]  = */ {10, 20, -1, -1, -1, -1, -1, -1},
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 7,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 40,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
	    /* num_pins           = */ 3,
	    /* pin_type[MAX_PINS] = */ {MONSTER_ENTRY_TYPE,
					MONSTER_ENTRY_TYPE,
					MONSTER_ENTRY_TYPE,
					-1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]  = */ {10, 20, 30, -1, -1, -1, -1, -1},
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ TRUE
          }
        };

        check_flush_cache__pe_multi_entry_test(cache_ptr, test_num,
                                               flush_flags, spec_size, spec);
    }

    if ( pass )
    {
        int test_num                            = 3;
        unsigned int flush_flags                = H5C__FLUSH_INVALIDATE_FLAG |
                                                  H5C__FLUSH_CLEAR_ONLY_FLAG;
        int spec_size                           = 8;
        struct pe_flush_cache_test_spec spec[8] =
        {
          {
            /* entry_num          = */ 0,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 100,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__NO_FLAGS_SET,
	    /* num_pins           = */ 0,
	    /* pin_type[MAX_PINS] = */ {-1, -1, -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]  = */ {-1, -1, -1, -1, -1, -1, -1, -1},
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 1,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 75,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__NO_FLAGS_SET,
	    /* num_pins           = */ 1,
	    /* pin_type[MAX_PINS] = */ {PICO_ENTRY_TYPE,
		                        -1, -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]  = */ {100, -1, -1, -1, -1, -1, -1, -1},
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 2,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 25,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__NO_FLAGS_SET,
	    /* num_pins           = */ 1,
	    /* pin_type[MAX_PINS] = */ {PICO_ENTRY_TYPE,
		                        -1, -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]  = */ {100, -1, -1, -1, -1, -1, -1, -1},
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 3,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 50,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__NO_FLAGS_SET,
	    /* num_pins           = */ 1,
	    /* pin_type[MAX_PINS] = */ {PICO_ENTRY_TYPE,
		                        -1, -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]  = */ {100, -1, -1, -1, -1, -1, -1, -1},
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 4,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 10,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
	    /* num_pins           = */ 0,
	    /* pin_type[MAX_PINS] = */ {-1, -1, -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]  = */ {-1, -1, -1, -1, -1, -1, -1, -1},
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 5,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 20,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
	    /* num_pins           = */ 0,
	    /* pin_type[MAX_PINS] = */ {-1, -1, -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]  = */ {-1, -1, -1, -1, -1, -1, -1, -1},
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 6,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 30,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
	    /* num_pins           = */ 0,
	    /* pin_type[MAX_PINS] = */ {-1, -1, -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]  = */ {-1, -1, -1, -1, -1, -1, -1, -1},
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 7,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 40,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
	    /* num_pins           = */ 0,
	    /* pin_type[MAX_PINS] = */ {-1, -1, -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]  = */ {-1, -1, -1, -1, -1, -1, -1, -1},
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ TRUE
          }
        };

        check_flush_cache__pe_multi_entry_test(cache_ptr, test_num,
                                               flush_flags, spec_size, spec);
    }


    if ( pass )
    {
        int test_num                            = 4;
        unsigned int flush_flags                = H5C__FLUSH_INVALIDATE_FLAG |
                                                 H5C__FLUSH_MARKED_ENTRIES_FLAG;
        int spec_size                           = 8;
        struct pe_flush_cache_test_spec spec[8] =
        {
          {
            /* entry_num          = */ 0,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 100,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__NO_FLAGS_SET,
	    /* num_pins           = */ 0,
	    /* pin_type[MAX_PINS] = */ {-1, -1, -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]  = */ {-1, -1, -1, -1, -1, -1, -1, -1},
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 1,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 75,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__NO_FLAGS_SET,
	    /* num_pins           = */ 1,
	    /* pin_type[MAX_PINS] = */ {PICO_ENTRY_TYPE,
		                        -1, -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]  = */ {100, -1, -1, -1, -1, -1, -1, -1},
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 2,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 25,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__NO_FLAGS_SET,
	    /* num_pins           = */ 1,
	    /* pin_type[MAX_PINS] = */ {PICO_ENTRY_TYPE,
		                        -1, -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]  = */ {100, -1, -1, -1, -1, -1, -1, -1},
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 3,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 50,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__NO_FLAGS_SET,
	    /* num_pins           = */ 1,
	    /* pin_type[MAX_PINS] = */ {PICO_ENTRY_TYPE,
		                        -1, -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]  = */ {100, -1, -1, -1, -1, -1, -1, -1},
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 4,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 10,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
	    /* num_pins           = */ 0,
	    /* pin_type[MAX_PINS] = */ {-1, -1, -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]  = */ {-1, -1, -1, -1, -1, -1, -1, -1},
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 5,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 20,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
	    /* num_pins           = */ 4,
	    /* pin_type[MAX_PINS] = */ {PICO_ENTRY_TYPE,
                                        PICO_ENTRY_TYPE,
                                        PICO_ENTRY_TYPE,
					PICO_ENTRY_TYPE,
					-1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]  = */ {100, 75, 25, 50, -1, -1, -1, -1},
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 6,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 30,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
	    /* num_pins           = */ 4,
	    /* pin_type[MAX_PINS] = */ {PICO_ENTRY_TYPE,
                                        PICO_ENTRY_TYPE,
                                        PICO_ENTRY_TYPE,
					PICO_ENTRY_TYPE,
					-1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]  = */ {100, 75, 25, 50, -1, -1, -1, -1},
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 7,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 40,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
	    /* num_pins           = */ 0,
	    /* pin_type[MAX_PINS] = */ {-1, -1, -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]  = */ {-1, -1, -1, -1, -1, -1, -1, -1},
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ FALSE,
            /* expected_flushed   = */ TRUE,
            /* expected_destroyed = */ TRUE
          }
        };

        check_flush_cache__pe_multi_entry_test(cache_ptr, test_num,
                                               flush_flags, spec_size, spec);
    }


    if ( pass )
    {
        int test_num                            = 5;
        unsigned int flush_flags                = H5C__FLUSH_INVALIDATE_FLAG |
                                                  H5C__FLUSH_CLEAR_ONLY_FLAG |
                                                 H5C__FLUSH_MARKED_ENTRIES_FLAG;
        int spec_size                           = 8;
        struct pe_flush_cache_test_spec spec[8] =
        {
          {
            /* entry_num          = */ 0,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 100,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__NO_FLAGS_SET,
	    /* num_pins           = */ 0,
	    /* pin_type[MAX_PINS] = */ {-1, -1, -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]  = */ {-1, -1, -1, -1, -1, -1, -1, -1},
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 1,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 75,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__NO_FLAGS_SET,
	    /* num_pins           = */ 1,
	    /* pin_type[MAX_PINS] = */ {PICO_ENTRY_TYPE,
		                        -1, -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]  = */ {100, -1, -1, -1, -1, -1, -1, -1},
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 2,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 25,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__NO_FLAGS_SET,
	    /* num_pins           = */ 1,
	    /* pin_type[MAX_PINS] = */ {PICO_ENTRY_TYPE,
		                        -1, -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]  = */ {100, -1, -1, -1, -1, -1, -1, -1},
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 3,
            /* entry_type         = */ PICO_ENTRY_TYPE,
            /* entry_index        = */ 50,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__NO_FLAGS_SET,
	    /* num_pins           = */ 1,
	    /* pin_type[MAX_PINS] = */ {PICO_ENTRY_TYPE,
		                        -1, -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]  = */ {100, -1, -1, -1, -1, -1, -1, -1},
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 4,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 10,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
	    /* num_pins           = */ 1,
	    /* pin_type[MAX_PINS] = */ {PICO_ENTRY_TYPE,
		                        -1, -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]  = */ {100, -1, -1, -1, -1, -1, -1, -1},
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 5,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 20,
            /* insert_flag        = */ FALSE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
	    /* num_pins           = */ 1,
	    /* pin_type[MAX_PINS] = */ {PICO_ENTRY_TYPE,
		                        -1, -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]  = */ {100, -1, -1, -1, -1, -1, -1, -1},
            /* expected_loaded    = */ TRUE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 6,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 30,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ FALSE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
	    /* num_pins           = */ 1,
	    /* pin_type[MAX_PINS] = */ {PICO_ENTRY_TYPE,
		                        -1, -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]  = */ {100, -1, -1, -1, -1, -1, -1, -1},
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ TRUE
          },
          {
            /* entry_num          = */ 7,
            /* entry_type         = */ MONSTER_ENTRY_TYPE,
            /* entry_index        = */ 40,
            /* insert_flag        = */ TRUE,
            /* dirty_flag         = */ TRUE,
            /* flags              = */ H5C__SET_FLUSH_MARKER_FLAG,
	    /* num_pins           = */ 1,
	    /* pin_type[MAX_PINS] = */ {PICO_ENTRY_TYPE,
		                        -1, -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]  = */ {100, -1, -1, -1, -1, -1, -1, -1},
            /* expected_loaded    = */ FALSE,
            /* expected_cleared   = */ TRUE,
            /* expected_flushed   = */ FALSE,
            /* expected_destroyed = */ TRUE
          }
        };

        check_flush_cache__pe_multi_entry_test(cache_ptr, test_num,
                                               flush_flags, spec_size, spec);
    }

    return;

} /* check_flush_cache__multi_entry() */


/*-------------------------------------------------------------------------
 * Function:	check_flush_cache__multi_entry_test()
 *
 * Purpose:	Run a multi entry flush cache test.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              1/13/05
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
check_flush_cache__multi_entry_test(H5C_t * cache_ptr,
                                    int test_num,
                                    unsigned int flush_flags,
                                    int spec_size,
                                    struct flush_cache_test_spec spec[])
{
    /* const char *   fcn_name = "check_flush_cache__multi_entry_test"; */
    static char    msg[128];
    herr_t	   result;
    int            i;
    size_t	   total_entry_size = 0;
    test_entry_t * base_addr;
    test_entry_t * entry_ptr;

    if ( cache_ptr == NULL ) {

        pass = FALSE;
        HDsnprintf(msg, (size_t)128,
                   "cache_ptr NULL on entry to single entry test #%d.",
                   test_num);
        failure_mssg = msg;
    }
    else if ( ( cache_ptr->index_len != 0 ) ||
              ( cache_ptr->index_size != 0 ) ) {

        pass = FALSE;

        HDsnprintf(msg, (size_t)128,
                   "cache not empty at beginning of multi entry test #%d.",
                   test_num);
        failure_mssg = msg;
    }
    else if ( ( spec_size < 1 ) || ( spec == NULL ) ) {

        pass = FALSE;
        HDsnprintf(msg, (size_t)128,
                   "missing/bad test spec on entry to multi entry test #%d.",
                   test_num);
        failure_mssg = msg;
    }

    i = 0;
    while ( ( pass ) && ( i < spec_size ) )
    {
        if ( ( spec[i].entry_num != i ) ||
             ( spec[i].entry_type < 0 ) ||
             ( spec[i].entry_type >= NUMBER_OF_ENTRY_TYPES ) ||
             ( spec[i].entry_index < 0 ) ||
             ( spec[i].entry_index > max_indices[spec[i].entry_type] ) ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "bad data in spec[%d] on entry to multi entry test #%d.",
                       i, test_num);
            failure_mssg = msg;
        }
        i++;
    }

    i = 0;
    while ( ( pass ) && ( i < spec_size ) )
    {
        if ( spec[i].insert_flag ) {

            insert_entry(cache_ptr, spec[i].entry_type, spec[i].entry_index,
                         spec[i].dirty_flag, spec[i].flags);

        } else {

            protect_entry(cache_ptr, spec[i].entry_type, spec[i].entry_index);

            unprotect_entry(cache_ptr, spec[i].entry_type, spec[i].entry_index,
                            (int)(spec[i].dirty_flag), spec[i].flags);
        }

        total_entry_size += entry_sizes[spec[i].entry_type];

        i++;
    }

    if ( pass ) {

        result = H5C_flush_cache(NULL, -1, -1, cache_ptr, flush_flags);

        if ( result < 0 ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "flush with flags 0x%x failed in multi entry test #%d.",
                       flush_flags, test_num);
            failure_mssg = msg;
        }
    }

    i = 0;
    while ( ( pass ) && ( i < spec_size ) )
    {
        base_addr = entries[spec[i].entry_type];
        entry_ptr = &(base_addr[spec[i].entry_index]);

        if ( ( entry_ptr->loaded != spec[i].expected_loaded ) ||
             ( entry_ptr->cleared != spec[i].expected_cleared ) ||
             ( entry_ptr->flushed != spec[i].expected_flushed ) ||
             ( entry_ptr->destroyed != spec[i].expected_destroyed ) ) {

#if 0 /* This is useful debugging code.  Lets keep it around. */

            HDfprintf(stdout,
              "loaded = %d(%d), clrd = %d(%d), flshd = %d(%d), dest = %d(%d)\n",
              (int)(entry_ptr->loaded),
              (int)(spec[i].expected_loaded),
              (int)(entry_ptr->cleared),
              (int)(spec[i].expected_cleared),
              (int)(entry_ptr->flushed),
              (int)(spec[i].expected_flushed),
              (int)(entry_ptr->destroyed),
              (int)(spec[i].expected_destroyed));

#endif

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
                "Bad status on entry %d after flush in multi entry test #%d.",
                i, test_num);
            failure_mssg = msg;
        }
        i++;
    }

    if ( pass ) {

        if ( ( ( (flush_flags & H5C__FLUSH_INVALIDATE_FLAG) == 0 )
               &&
               ( ( cache_ptr->index_len != spec_size )
                 ||
                 ( cache_ptr->index_size != total_entry_size )
               )
             )
             ||
             ( ( (flush_flags & H5C__FLUSH_INVALIDATE_FLAG) != 0 )
               &&
               ( ( cache_ptr->index_len != 0 )
                 ||
                 ( cache_ptr->index_size != 0 )
               )
             )
           ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
              "Unexpected cache len/size after flush in multi entry test #%d.",
              test_num);
            failure_mssg = msg;
        }
    }

    /* clean up the cache to prep for the next test */
    if ( pass ) {

        result = H5C_flush_cache(NULL, -1, -1, cache_ptr,
                                 H5C__FLUSH_INVALIDATE_FLAG);

        if ( result < 0 ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "Flush failed on cleanup in multi entry test #%d.",
                       test_num);
            failure_mssg = msg;
        }
        else if ( ( cache_ptr->index_len != 0 ) ||
                  ( cache_ptr->index_size != 0 ) ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
            "Unexpected cache len/size after cleanup in multi entry test #%d.",
            test_num);
            failure_mssg = msg;

        }
    }

    i = 0;
    while ( ( pass ) && ( i < spec_size ) )
    {
        base_addr = entries[spec[i].entry_type];
        entry_ptr = &(base_addr[spec[i].entry_index]);

        entry_ptr->loaded    = FALSE;
        entry_ptr->cleared   = FALSE;
        entry_ptr->flushed   = FALSE;
        entry_ptr->destroyed = FALSE;

        i++;
    }

    return;

} /* check_flush_cache__multi_entry_test() */


/*-------------------------------------------------------------------------
 * Function:	check_flush_cache__pe_multi_entry_test()
 *
 * Purpose:	Run a multi entry flush cache test.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              4/5/06
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
check_flush_cache__pe_multi_entry_test(H5C_t * cache_ptr,
                                       int test_num,
                                       unsigned int flush_flags,
                                       int spec_size,
                                       struct pe_flush_cache_test_spec spec[])
{
    /* const char *   fcn_name = "check_flush_cache__pe_multi_entry_test"; */
    static char    msg[128];
    herr_t	   result;
    int            i;
    int            j;
    size_t	   total_entry_size = 0;
    test_entry_t * base_addr;
    test_entry_t * entry_ptr;

    if ( cache_ptr == NULL ) {

        pass = FALSE;
        HDsnprintf(msg, (size_t)128,
                   "cache_ptr NULL on entry to pe multi entry test #%d.",
                   test_num);
        failure_mssg = msg;
    }
    else if ( ( cache_ptr->index_len != 0 ) ||
              ( cache_ptr->index_size != 0 ) ) {

        pass = FALSE;

        HDsnprintf(msg, (size_t)128,
                   "cache not empty at beginning of pe multi entry test #%d.",
                   test_num);
        failure_mssg = msg;
    }
    else if ( ( spec_size < 1 ) || ( spec == NULL ) ) {

        pass = FALSE;
        HDsnprintf(msg, (size_t)128,
                   "missing/bad test spec on entry to pe multi entry test #%d.",
                   test_num);
        failure_mssg = msg;
    }

    i = 0;
    while ( ( pass ) && ( i < spec_size ) )
    {
        if ( ( spec[i].entry_num != i ) ||
             ( spec[i].entry_type < 0 ) ||
             ( spec[i].entry_type >= NUMBER_OF_ENTRY_TYPES ) ||
             ( spec[i].entry_index < 0 ) ||
             ( spec[i].entry_index > max_indices[spec[i].entry_type] ) ||
	     ( spec[i].num_pins < 0 ) ||
	     ( spec[i].num_pins > MAX_PINS ) ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
                    "bad data in spec[%d] on entry to pe multi entry test #%d.",
                    i, test_num);
            failure_mssg = msg;
        }
        i++;
    }

    i = 0;
    while ( ( pass ) && ( i < spec_size ) )
    {
        if ( spec[i].insert_flag ) {

            insert_entry(cache_ptr, spec[i].entry_type, spec[i].entry_index,
                         spec[i].dirty_flag, spec[i].flags);

        } else {

            protect_entry(cache_ptr, spec[i].entry_type, spec[i].entry_index);

            unprotect_entry(cache_ptr, spec[i].entry_type, spec[i].entry_index,
                            (int)(spec[i].dirty_flag), spec[i].flags);
        }

        total_entry_size += entry_sizes[spec[i].entry_type];

	for ( j = 0; j < spec[i].num_pins; j++ )
	{
            create_pinned_entry_dependency(cache_ptr,
		                           spec[i].entry_type,
					   spec[i].entry_index,
					   spec[i].pin_type[j],
					   spec[i].pin_idx[j]);
	}

        i++;
    }

    if ( pass ) {

        result = H5C_flush_cache(NULL, -1, -1, cache_ptr, flush_flags);

        if ( result < 0 ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
                     "flush with flags 0x%x failed in pe multi entry test #%d.",
                     flush_flags, test_num);
            failure_mssg = msg;
        }
    }

    i = 0;
    while ( ( pass ) && ( i < spec_size ) )
    {
        base_addr = entries[spec[i].entry_type];
        entry_ptr = &(base_addr[spec[i].entry_index]);

        if ( ( entry_ptr->loaded != spec[i].expected_loaded ) ||
             ( entry_ptr->cleared != spec[i].expected_cleared ) ||
             ( entry_ptr->flushed != spec[i].expected_flushed ) ||
             ( entry_ptr->destroyed != spec[i].expected_destroyed ) ) {

#if 0 /* This is useful debugging code.  Lets keep it around. */

            HDfprintf(stdout,
              "loaded = %d(%d), clrd = %d(%d), flshd = %d(%d), dest = %d(%d)\n",
              (int)(entry_ptr->loaded),
              (int)(spec[i].expected_loaded),
              (int)(entry_ptr->cleared),
              (int)(spec[i].expected_cleared),
              (int)(entry_ptr->flushed),
              (int)(spec[i].expected_flushed),
              (int)(entry_ptr->destroyed),
              (int)(spec[i].expected_destroyed));

#endif

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
               "Bad status on entry %d after flush in pe multi entry test #%d.",
               i, test_num);
            failure_mssg = msg;
        }
        i++;
    }

    if ( pass ) {

        if ( ( ( (flush_flags & H5C__FLUSH_INVALIDATE_FLAG) == 0 )
               &&
               ( ( cache_ptr->index_len != spec_size )
                 ||
                 ( cache_ptr->index_size != total_entry_size )
               )
             )
             ||
             ( ( (flush_flags & H5C__FLUSH_INVALIDATE_FLAG) != 0 )
               &&
               ( ( cache_ptr->index_len != 0 )
                 ||
                 ( cache_ptr->index_size != 0 )
               )
             )
           ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
            "Unexpected cache len/size after flush in pe multi entry test #%d.",
            test_num);
            failure_mssg = msg;
        }
    }

    /* clean up the cache to prep for the next test */
    if ( pass ) {

        result = H5C_flush_cache(NULL, -1, -1, cache_ptr,
                                 H5C__FLUSH_INVALIDATE_FLAG);

        if ( result < 0 ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "Flush failed on cleanup in pe multi entry test #%d.",
                       test_num);
            failure_mssg = msg;
        }
        else if ( ( cache_ptr->index_len != 0 ) ||
                  ( cache_ptr->index_size != 0 ) ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
          "Unexpected cache len/size after cleanup in pe multi entry test #%d.",
            test_num);
            failure_mssg = msg;

        }
    }

    i = 0;
    while ( ( pass ) && ( i < spec_size ) )
    {
        base_addr = entries[spec[i].entry_type];
        entry_ptr = &(base_addr[spec[i].entry_index]);

        entry_ptr->loaded    = FALSE;
        entry_ptr->cleared   = FALSE;
        entry_ptr->flushed   = FALSE;
        entry_ptr->destroyed = FALSE;

        i++;
    }

    return;

} /* check_flush_cache__pe_multi_entry_test() */


/*-------------------------------------------------------------------------
 * Function:	check_flush_cache__single_entry()
 *
 * Purpose:	Verify that flush_cache behaves as expected when the cache
 *		contains only one element.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              1/12/05
 *
 * Modifications:
 *
 * 		JRM -- 3/29/06
 * 		Added tests for pinned entries.
 *
 * 		JRM -- 5/17/06
 * 		Complete reqrite of pinned entry tests to accomodate
 * 		the new H5C_mark_pinned_or_protected_entry_dirty()
 * 		call.
 *
 *-------------------------------------------------------------------------
 */

static void
check_flush_cache__single_entry(H5C_t * cache_ptr)
{
    /* const char * fcn_name = "check_flush_cache__single_entry"; */

    if ( cache_ptr == NULL ) {

        pass = FALSE;
        failure_mssg = "cache_ptr NULL on entry to single entry case.";
    }
    else if ( ( cache_ptr->index_len != 0 ) ||
              ( cache_ptr->index_size != 0 ) ) {

        pass = FALSE;
        failure_mssg = "cache not empty at beginning of single entry case.";
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 1,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__NO_FLAGS_SET,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 2,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__NO_FLAGS_SET,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ TRUE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 3,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 4,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ TRUE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 5,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__FLUSH_INVALIDATE_FLAG,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ TRUE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 6,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__FLUSH_INVALIDATE_FLAG,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ TRUE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 7,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__FLUSH_MARKED_ENTRIES_FLAG,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 8,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__FLUSH_MARKED_ENTRIES_FLAG,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 9,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__FLUSH_INVALIDATE_FLAG |
                                       H5C__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ TRUE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 10,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__FLUSH_INVALIDATE_FLAG |
                                       H5C__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ TRUE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 11,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__FLUSH_MARKED_ENTRIES_FLAG |
                                       H5C__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 12,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__FLUSH_MARKED_ENTRIES_FLAG |
                                       H5C__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 13,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__FLUSH_MARKED_ENTRIES_FLAG |
                                       H5C__FLUSH_INVALIDATE_FLAG,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ TRUE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 14,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__FLUSH_MARKED_ENTRIES_FLAG |
                                       H5C__FLUSH_INVALIDATE_FLAG,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ TRUE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 15,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__FLUSH_INVALIDATE_FLAG |
                                       H5C__FLUSH_CLEAR_ONLY_FLAG |
                                       H5C__FLUSH_MARKED_ENTRIES_FLAG,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ TRUE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 16,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__FLUSH_INVALIDATE_FLAG |
                                       H5C__FLUSH_CLEAR_ONLY_FLAG |
                                       H5C__FLUSH_MARKED_ENTRIES_FLAG,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ TRUE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 17,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__NO_FLAGS_SET,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 18,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__NO_FLAGS_SET,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ TRUE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 19,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 20,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ TRUE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 21,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__FLUSH_INVALIDATE_FLAG,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ TRUE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 22,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__FLUSH_INVALIDATE_FLAG,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ TRUE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 23,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__FLUSH_MARKED_ENTRIES_FLAG,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 24,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__FLUSH_MARKED_ENTRIES_FLAG,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ TRUE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 25,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__FLUSH_INVALIDATE_FLAG |
                                       H5C__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ TRUE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 26,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__FLUSH_INVALIDATE_FLAG |
                                       H5C__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ TRUE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 27,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__FLUSH_MARKED_ENTRIES_FLAG |
                                       H5C__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 28,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__FLUSH_MARKED_ENTRIES_FLAG |
                                       H5C__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ TRUE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 29,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__FLUSH_MARKED_ENTRIES_FLAG |
                                       H5C__FLUSH_INVALIDATE_FLAG,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ TRUE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 30,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__FLUSH_MARKED_ENTRIES_FLAG |
                                       H5C__FLUSH_INVALIDATE_FLAG,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ TRUE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 31,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__FLUSH_INVALIDATE_FLAG |
                                       H5C__FLUSH_CLEAR_ONLY_FLAG |
                                       H5C__FLUSH_MARKED_ENTRIES_FLAG,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ TRUE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 32,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ FALSE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__FLUSH_INVALIDATE_FLAG |
                                       H5C__FLUSH_CLEAR_ONLY_FLAG |
                                       H5C__FLUSH_MARKED_ENTRIES_FLAG,
            /* expected_loaded      */ TRUE,
            /* expected_cleared     */ TRUE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 33,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__NO_FLAGS_SET,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ TRUE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 34,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__NO_FLAGS_SET,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ TRUE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 35,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ TRUE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 36,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ TRUE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 37,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__FLUSH_INVALIDATE_FLAG,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ TRUE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 38,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__FLUSH_INVALIDATE_FLAG,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ TRUE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 39,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__FLUSH_MARKED_ENTRIES_FLAG,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 40,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__FLUSH_MARKED_ENTRIES_FLAG,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 41,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__FLUSH_INVALIDATE_FLAG |
                                       H5C__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ TRUE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 42,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__FLUSH_INVALIDATE_FLAG |
                                       H5C__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ TRUE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 43,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__FLUSH_MARKED_ENTRIES_FLAG |
                                       H5C__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 44,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__FLUSH_MARKED_ENTRIES_FLAG |
                                       H5C__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 45,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__FLUSH_MARKED_ENTRIES_FLAG |
                                       H5C__FLUSH_INVALIDATE_FLAG,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ TRUE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 46,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__FLUSH_MARKED_ENTRIES_FLAG |
                                       H5C__FLUSH_INVALIDATE_FLAG,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ TRUE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 47,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__FLUSH_INVALIDATE_FLAG |
                                       H5C__FLUSH_CLEAR_ONLY_FLAG |
                                       H5C__FLUSH_MARKED_ENTRIES_FLAG,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ TRUE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 48,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__NO_FLAGS_SET,
            /* flush_flags          */ H5C__FLUSH_INVALIDATE_FLAG |
                                       H5C__FLUSH_CLEAR_ONLY_FLAG |
                                       H5C__FLUSH_MARKED_ENTRIES_FLAG,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ TRUE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 49,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__NO_FLAGS_SET,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ TRUE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 50,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__NO_FLAGS_SET,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ TRUE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 51,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ TRUE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 52,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ TRUE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 53,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__FLUSH_INVALIDATE_FLAG,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ TRUE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 54,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__FLUSH_INVALIDATE_FLAG,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ TRUE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 55,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__FLUSH_MARKED_ENTRIES_FLAG,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ TRUE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 56,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__FLUSH_MARKED_ENTRIES_FLAG,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ TRUE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 57,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__FLUSH_INVALIDATE_FLAG |
                                       H5C__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ TRUE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 58,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__FLUSH_INVALIDATE_FLAG |
                                       H5C__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ TRUE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 59,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__FLUSH_MARKED_ENTRIES_FLAG |
                                       H5C__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ TRUE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 60,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__FLUSH_MARKED_ENTRIES_FLAG |
                                       H5C__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ TRUE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ FALSE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 61,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__FLUSH_MARKED_ENTRIES_FLAG |
                                       H5C__FLUSH_INVALIDATE_FLAG,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ TRUE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 62,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__FLUSH_MARKED_ENTRIES_FLAG |
                                       H5C__FLUSH_INVALIDATE_FLAG,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ FALSE,
            /* expected_flushed     */ TRUE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 63,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ FALSE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__FLUSH_INVALIDATE_FLAG |
                                       H5C__FLUSH_CLEAR_ONLY_FLAG |
                                       H5C__FLUSH_MARKED_ENTRIES_FLAG,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ TRUE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ TRUE
        );
    }

    if ( pass ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr            */ cache_ptr,
            /* test_num             */ 64,
            /* entry_type           */ PICO_ENTRY_TYPE,
            /* entry_idx            */ 0,
            /* insert_flag          */ TRUE,
            /* dirty_flag           */ TRUE,
            /* flags                */ H5C__SET_FLUSH_MARKER_FLAG,
            /* flush_flags          */ H5C__FLUSH_INVALIDATE_FLAG |
                                       H5C__FLUSH_CLEAR_ONLY_FLAG |
                                       H5C__FLUSH_MARKED_ENTRIES_FLAG,
            /* expected_loaded      */ FALSE,
            /* expected_cleared     */ TRUE,
            /* expected_flushed     */ FALSE,
            /* expected_destroyed   */ TRUE
        );
    }

    /* Now run single entry tests for pinned entries.  Test all combinations
     * of:
     *
     * 	1) Unpin by unprotect vs. unpin by call to H5C_unpin_entry().
     *
     * 	2) Marked dirty by unprotect or not.
     *
     * 	3) Marked dirty by call to H5C_mark_pinned_entry_dirty() or not.
     *
     *  4) Marked dirty by call to H5C_mark_pinned_or_protected_entry_dirty()
     *     while protected or not.
     *
     *  5) Marked dirty by call to H5C_mark_pinned_or_protected_entry_dirty()
     *     while pinned or not.
     *
     * 	6) Entry marked for flush or not.
     *
     * 	7) Call flush with H5C__FLUSH_MARKED_ENTRIES_FLAG or not.
     *
     * 	8) Call flush with H5C__FLUSH_CLEAR_ONLY_FLAG or not.
     *
     * This yields a total of 256 tests.
     *
     * The tests and their expected results are given in the spec table
     * below.  The values assigned to the expected_cleared, expected_flushed,
     * and expected_destroyed fields are somewhat arcane, so the following
     * overview may be useful.
     *
     * In addition to simply checking to see if the test case runs,
     * we also check to see if the desired operations take place on the
     * cache entry.  Thus expected_cleared is set to TRUE if we expect
     * the entry to be flushed, expected_flushed is set to TRUE if we
     * we expect the entry to be flushed, and expected_destroyed is set
     * to TRUE if we expect the entry to be destroyed.
     *
     * In this test, we are working with pinned entries which can't be
     * evicted, so expected_destroyed is always FALSE.  We could pull it
     * from the table, but it is a hold over from the code this test
     * was adapted from, and it doesn't do any particular harm.
     *
     * In general, we expect an entry to be flushed if it is dirty, and
     * flush in invoked WITHOUT the H5C__FLUSH_CLEAR_ONLY_FLAG.  However,
     * there are exceptions:  If flush is invoked with the
     * H5C__FLUSH_MARKED_ENTRIES_FLAG, only marked entries will be flushed.
     *
     * Further, unprotecting an entry with the H5C__SET_FLUSH_MARKER_FLAG
     * will NOT mark the entry unless the entry has either been marked
     * dirty either before or durting the unprotect call.  This results in
     * some counterintuitive entries in the table.  It make be useful to
     * look in the test code to see the exact order of operations.
     *
     * Similarly, we expect an entry to be cleared if it is dirty, and
     * flush is invoked WITH the H5C__FLUSH_CLEAR_ONLY_FLAG.  Again, there
     * are exceptions -- If flush is also invoked with the
     * H5C__FLUSH_MARKED_ENTRIES_FLAG, only the marked entries will be
     * cleared.
     *
     * The above comments about applying unprotect with the
     * H5C__SET_FLUSH_MARKER_FLAG apply here as well.
     */

    if ( pass ) {

	int i;
	struct pinned_single_entry_test_spec
	{
            int 		test_num;
            int			entry_type;
            int			entry_idx;
            hbool_t		dirty_flag;
            hbool_t		mark_dirty;
            hbool_t		pop_mark_dirty_prot;
            hbool_t		pop_mark_dirty_pinned;
            hbool_t		unprotect_unpin;
            unsigned int	flags;
            unsigned int	flush_flags;
            hbool_t		expected_cleared;
            hbool_t		expected_flushed;
            hbool_t		expected_destroyed;
	} spec[256] =
	/*                                           pop    pop
	 *                         ent               mark   mark
	 *  test  entry            -ry dirty  mark   dirty  dirty  unprot                             flush                                                        expect  expect expect
         *  num   type             idx  flag  dirty  prot   pinned unpin  flags                       flags                                                        clear   flush  destroy
	 */
	{ {   1,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE, FALSE, FALSE, H5C__NO_FLAGS_SET,          H5C__NO_FLAGS_SET,                                           FALSE, FALSE, FALSE },
	  {   2,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE, FALSE,  TRUE, H5C__NO_FLAGS_SET,          H5C__NO_FLAGS_SET,                                           FALSE, FALSE, FALSE },
	  {   3,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE,  TRUE, FALSE, H5C__NO_FLAGS_SET,          H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {   4,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE,  TRUE,  TRUE, H5C__NO_FLAGS_SET,          H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {   5,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE, FALSE, FALSE, H5C__NO_FLAGS_SET,          H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {   6,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE, FALSE,  TRUE, H5C__NO_FLAGS_SET,          H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {   7,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE,  TRUE, FALSE, H5C__NO_FLAGS_SET,          H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {   8,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE,  TRUE,  TRUE, H5C__NO_FLAGS_SET,          H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {   9,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE, FALSE, FALSE, H5C__NO_FLAGS_SET,          H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  10,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE, FALSE,  TRUE, H5C__NO_FLAGS_SET,          H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  11,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE,  TRUE, FALSE, H5C__NO_FLAGS_SET,          H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  12,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE,  TRUE,  TRUE, H5C__NO_FLAGS_SET,          H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  13,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE, FALSE, FALSE, H5C__NO_FLAGS_SET,          H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  14,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE, FALSE,  TRUE, H5C__NO_FLAGS_SET,          H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  15,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE,  TRUE, FALSE, H5C__NO_FLAGS_SET,          H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  16,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE,  TRUE,  TRUE, H5C__NO_FLAGS_SET,          H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  17,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE, FALSE, FALSE, H5C__NO_FLAGS_SET,          H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  18,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE, FALSE,  TRUE, H5C__NO_FLAGS_SET,          H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  19,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE,  TRUE, FALSE, H5C__NO_FLAGS_SET,          H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  20,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE,  TRUE,  TRUE, H5C__NO_FLAGS_SET,          H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  21,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE, FALSE, FALSE, H5C__NO_FLAGS_SET,          H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  22,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE, FALSE,  TRUE, H5C__NO_FLAGS_SET,          H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  23,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE,  TRUE, FALSE, H5C__NO_FLAGS_SET,          H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  24,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE,  TRUE,  TRUE, H5C__NO_FLAGS_SET,          H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  25,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE, FALSE, FALSE, H5C__NO_FLAGS_SET,          H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  26,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE, FALSE,  TRUE, H5C__NO_FLAGS_SET,          H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  27,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE,  TRUE, FALSE, H5C__NO_FLAGS_SET,          H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  28,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE,  TRUE,  TRUE, H5C__NO_FLAGS_SET,          H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  29,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE, FALSE, FALSE, H5C__NO_FLAGS_SET,          H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  30,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE, FALSE,  TRUE, H5C__NO_FLAGS_SET,          H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  31,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE,  TRUE, FALSE, H5C__NO_FLAGS_SET,          H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  32,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE, H5C__NO_FLAGS_SET,          H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  33,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE, FALSE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__NO_FLAGS_SET,                                           FALSE, FALSE, FALSE },
	  {  34,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE, FALSE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__NO_FLAGS_SET,                                           FALSE, FALSE, FALSE },
	  {  35,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE,  TRUE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  36,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE,  TRUE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  37,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE, FALSE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  38,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE, FALSE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  39,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE,  TRUE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  40,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE,  TRUE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  41,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE, FALSE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  42,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE, FALSE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  43,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE,  TRUE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  44,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE,  TRUE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  45,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE, FALSE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  46,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE, FALSE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  47,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE,  TRUE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  48,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE,  TRUE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  49,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE, FALSE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  50,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE, FALSE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  51,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE,  TRUE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  52,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE,  TRUE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  53,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE, FALSE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  54,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE, FALSE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  55,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE,  TRUE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  56,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE,  TRUE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  57,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE, FALSE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  58,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE, FALSE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  59,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE,  TRUE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  60,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE,  TRUE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  61,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE, FALSE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  62,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE, FALSE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  63,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE,  TRUE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  64,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  65,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE, FALSE, FALSE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  66,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE, FALSE,  TRUE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  67,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE,  TRUE, FALSE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  68,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE,  TRUE,  TRUE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  69,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE, FALSE, FALSE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  70,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE, FALSE,  TRUE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  71,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE,  TRUE, FALSE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  72,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE,  TRUE,  TRUE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  73,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE, FALSE, FALSE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  74,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE, FALSE,  TRUE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  75,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE,  TRUE, FALSE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  76,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE,  TRUE,  TRUE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  77,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE, FALSE, FALSE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  78,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE, FALSE,  TRUE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  79,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE,  TRUE, FALSE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  80,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE,  TRUE,  TRUE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  81,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE, FALSE, FALSE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  82,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE, FALSE,  TRUE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  83,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE,  TRUE, FALSE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  84,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE,  TRUE,  TRUE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  85,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE, FALSE, FALSE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  86,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE, FALSE,  TRUE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  87,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE,  TRUE, FALSE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  88,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE,  TRUE,  TRUE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  89,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE, FALSE, FALSE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  90,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE, FALSE,  TRUE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  91,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE,  TRUE, FALSE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  92,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE,  TRUE,  TRUE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  93,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE, FALSE, FALSE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  94,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE, FALSE,  TRUE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  95,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE,  TRUE, FALSE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  96,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  97,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE, FALSE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  98,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE, FALSE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  99,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE,  TRUE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  { 100,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE,  TRUE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  { 101,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE, FALSE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE,  TRUE, FALSE },
	  { 102,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE, FALSE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE,  TRUE, FALSE },
	  { 103,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE,  TRUE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE,  TRUE, FALSE },
	  { 104,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE,  TRUE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE,  TRUE, FALSE },
	  { 105,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE, FALSE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  { 106,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE, FALSE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  { 107,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE,  TRUE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  { 108,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE,  TRUE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  { 109,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE, FALSE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE,  TRUE, FALSE },
	  { 110,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE, FALSE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE,  TRUE, FALSE },
	  { 111,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE,  TRUE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE,  TRUE, FALSE },
	  { 112,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE,  TRUE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE,  TRUE, FALSE },
	  { 113,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE, FALSE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE,  TRUE, FALSE },
	  { 114,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE, FALSE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE,  TRUE, FALSE },
	  { 115,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE,  TRUE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE,  TRUE, FALSE },
	  { 116,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE,  TRUE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE,  TRUE, FALSE },
	  { 117,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE, FALSE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE,  TRUE, FALSE },
	  { 118,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE, FALSE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE,  TRUE, FALSE },
	  { 119,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE,  TRUE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE,  TRUE, FALSE },
	  { 120,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE,  TRUE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE,  TRUE, FALSE },
	  { 121,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE, FALSE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE,  TRUE, FALSE },
	  { 122,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE, FALSE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE,  TRUE, FALSE },
	  { 123,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE,  TRUE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE,  TRUE, FALSE },
	  { 124,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE,  TRUE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE,  TRUE, FALSE },
	  { 125,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE, FALSE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE,  TRUE, FALSE },
	  { 126,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE, FALSE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE,  TRUE, FALSE },
	  { 127,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE,  TRUE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE,  TRUE, FALSE },
	  { 128,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE,  TRUE, FALSE },
	  { 129,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE, FALSE, FALSE, H5C__NO_FLAGS_SET,          H5C__FLUSH_CLEAR_ONLY_FLAG,                                  FALSE, FALSE, FALSE },
	  { 130,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE, FALSE,  TRUE, H5C__NO_FLAGS_SET,          H5C__FLUSH_CLEAR_ONLY_FLAG,                                  FALSE, FALSE, FALSE },
	  { 131,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE,  TRUE, FALSE, H5C__NO_FLAGS_SET,          H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 132,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE,  TRUE,  TRUE, H5C__NO_FLAGS_SET,          H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 133,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE, FALSE, FALSE, H5C__NO_FLAGS_SET,          H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 134,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE, FALSE,  TRUE, H5C__NO_FLAGS_SET,          H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 135,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE,  TRUE, FALSE, H5C__NO_FLAGS_SET,          H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 136,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE,  TRUE,  TRUE, H5C__NO_FLAGS_SET,          H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 137,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE, FALSE, FALSE, H5C__NO_FLAGS_SET,          H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 138,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE, FALSE,  TRUE, H5C__NO_FLAGS_SET,          H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 139,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE,  TRUE, FALSE, H5C__NO_FLAGS_SET,          H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 140,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE,  TRUE,  TRUE, H5C__NO_FLAGS_SET,          H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 141,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE, FALSE, FALSE, H5C__NO_FLAGS_SET,          H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 142,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE, FALSE,  TRUE, H5C__NO_FLAGS_SET,          H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 143,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE,  TRUE, FALSE, H5C__NO_FLAGS_SET,          H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 144,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE,  TRUE,  TRUE, H5C__NO_FLAGS_SET,          H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 145,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE, FALSE, FALSE, H5C__NO_FLAGS_SET,          H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 146,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE, FALSE,  TRUE, H5C__NO_FLAGS_SET,          H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 147,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE,  TRUE, FALSE, H5C__NO_FLAGS_SET,          H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 148,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE,  TRUE,  TRUE, H5C__NO_FLAGS_SET,          H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 149,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE, FALSE, FALSE, H5C__NO_FLAGS_SET,          H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 150,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE, FALSE,  TRUE, H5C__NO_FLAGS_SET,          H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 151,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE,  TRUE, FALSE, H5C__NO_FLAGS_SET,          H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 152,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE,  TRUE,  TRUE, H5C__NO_FLAGS_SET,          H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 153,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE, FALSE, FALSE, H5C__NO_FLAGS_SET,          H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 154,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE, FALSE,  TRUE, H5C__NO_FLAGS_SET,          H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 155,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE,  TRUE, FALSE, H5C__NO_FLAGS_SET,          H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 156,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE,  TRUE,  TRUE, H5C__NO_FLAGS_SET,          H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 157,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE, FALSE, FALSE, H5C__NO_FLAGS_SET,          H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 158,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE, FALSE,  TRUE, H5C__NO_FLAGS_SET,          H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 159,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE,  TRUE, FALSE, H5C__NO_FLAGS_SET,          H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 160,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE, H5C__NO_FLAGS_SET,          H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 161,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE, FALSE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_CLEAR_ONLY_FLAG,                                  FALSE, FALSE, FALSE },
	  { 162,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE, FALSE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_CLEAR_ONLY_FLAG,                                  FALSE, FALSE, FALSE },
	  { 163,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE,  TRUE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 164,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE,  TRUE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 165,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE, FALSE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 166,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE, FALSE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 167,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE,  TRUE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 168,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE,  TRUE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 169,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE, FALSE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 170,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE, FALSE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 171,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE,  TRUE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 172,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE,  TRUE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 173,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE, FALSE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 174,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE, FALSE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 175,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE,  TRUE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 176,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE,  TRUE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 177,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE, FALSE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 178,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE, FALSE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 179,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE,  TRUE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 180,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE,  TRUE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 181,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE, FALSE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 182,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE, FALSE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 183,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE,  TRUE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 184,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE,  TRUE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 185,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE, FALSE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 186,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE, FALSE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 187,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE,  TRUE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 188,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE,  TRUE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 189,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE, FALSE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 190,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE, FALSE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 191,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE,  TRUE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 192,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 193,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE, FALSE, FALSE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 194,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE, FALSE,  TRUE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 195,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE,  TRUE, FALSE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 196,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE,  TRUE,  TRUE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 197,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE, FALSE, FALSE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 198,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE, FALSE,  TRUE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 199,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE,  TRUE, FALSE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 200,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE,  TRUE,  TRUE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 201,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE, FALSE, FALSE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 202,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE, FALSE,  TRUE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 203,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE,  TRUE, FALSE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 204,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE,  TRUE,  TRUE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 205,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE, FALSE, FALSE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 206,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE, FALSE,  TRUE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 207,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE,  TRUE, FALSE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 208,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE,  TRUE,  TRUE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 209,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE, FALSE, FALSE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 210,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE, FALSE,  TRUE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 211,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE,  TRUE, FALSE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 212,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE,  TRUE,  TRUE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 213,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE, FALSE, FALSE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 214,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE, FALSE,  TRUE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 215,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE,  TRUE, FALSE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 216,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE,  TRUE,  TRUE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 217,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE, FALSE, FALSE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 218,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE, FALSE,  TRUE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 219,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE,  TRUE, FALSE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 220,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE,  TRUE,  TRUE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 221,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE, FALSE, FALSE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 222,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE, FALSE,  TRUE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 223,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE,  TRUE, FALSE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 224,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE, H5C__NO_FLAGS_SET,          H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 225,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE, FALSE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 226,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE, FALSE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 227,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE,  TRUE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 228,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE,  TRUE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 229,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE, FALSE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG,  TRUE, FALSE, FALSE },
	  { 230,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE, FALSE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG,  TRUE, FALSE, FALSE },
	  { 231,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE,  TRUE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG,  TRUE, FALSE, FALSE },
	  { 232,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE,  TRUE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG,  TRUE, FALSE, FALSE },
	  { 233,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE, FALSE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 234,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE, FALSE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 235,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE,  TRUE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 236,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE,  TRUE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 237,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE, FALSE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG,  TRUE, FALSE, FALSE },
	  { 238,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE, FALSE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG,  TRUE, FALSE, FALSE },
	  { 239,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE,  TRUE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG,  TRUE, FALSE, FALSE },
	  { 240,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE,  TRUE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG,  TRUE, FALSE, FALSE },
	  { 241,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE, FALSE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG,  TRUE, FALSE, FALSE },
	  { 242,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE, FALSE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG,  TRUE, FALSE, FALSE },
	  { 243,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE,  TRUE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG,  TRUE, FALSE, FALSE },
	  { 244,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE,  TRUE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG,  TRUE, FALSE, FALSE },
	  { 245,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE, FALSE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG,  TRUE, FALSE, FALSE },
	  { 246,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE, FALSE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG,  TRUE, FALSE, FALSE },
	  { 247,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE,  TRUE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG,  TRUE, FALSE, FALSE },
	  { 248,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE,  TRUE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG,  TRUE, FALSE, FALSE },
	  { 249,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE, FALSE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG,  TRUE, FALSE, FALSE },
	  { 250,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE, FALSE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG,  TRUE, FALSE, FALSE },
	  { 251,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE,  TRUE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG,  TRUE, FALSE, FALSE },
	  { 252,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE,  TRUE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG,  TRUE, FALSE, FALSE },
	  { 253,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE, FALSE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG,  TRUE, FALSE, FALSE },
	  { 254,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE, FALSE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG,  TRUE, FALSE, FALSE },
	  { 255,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE,  TRUE, FALSE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG,  TRUE, FALSE, FALSE },
	  { 256,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE, H5C__SET_FLUSH_MARKER_FLAG, H5C__FLUSH_MARKED_ENTRIES_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG,  TRUE, FALSE, FALSE } };

	i = 0;
	while ( ( pass ) && ( i < 256 ) )
	{
	    check_flush_cache__pinned_single_entry_test
	    (
                /* cache_ptr             */ cache_ptr,
                /* test_num              */ spec[i].test_num,
                /* entry_type            */ spec[i].entry_type,
                /* entry_idx             */ spec[i].entry_idx,
                /* dirty_flag            */ spec[i].dirty_flag,
	        /* mark_dirty            */ spec[i].mark_dirty,
                /* pop_mark_dirty_prot   */ spec[i].pop_mark_dirty_prot,
                /* pop_mark_dirty_pinned */ spec[i].pop_mark_dirty_pinned,
	        /* unprotect_unpin       */ spec[i].unprotect_unpin,
                /* flags                 */ spec[i].flags,
                /* flush_flags           */ spec[i].flush_flags,
                /* expected_cleared      */ spec[i].expected_cleared,
                /* expected_flushed      */ spec[i].expected_flushed,
                /* expected_destroyed    */ spec[i].expected_destroyed
            );
	    i++;
	}
    }

    return;

} /* check_flush_cache__single_entry() */


/*-------------------------------------------------------------------------
 * Function:	check_flush_cache__single_entry_test()
 *
 * Purpose:	Run a single entry flush cache test.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              1/12/05
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
check_flush_cache__single_entry_test(H5C_t * cache_ptr,
                                     int test_num,
                                     int entry_type,
                                     int entry_idx,
                                     hbool_t insert_flag,
                                     hbool_t dirty_flag,
                                     unsigned int flags,
                                     unsigned int flush_flags,
                                     hbool_t expected_loaded,
                                     hbool_t expected_cleared,
                                     hbool_t expected_flushed,
                                     hbool_t expected_destroyed)
{
    /* const char *   fcn_name = "check_flush_cache__single_entry_test"; */
    static char    msg[128];
    herr_t	   result;
    test_entry_t * base_addr;
    test_entry_t * entry_ptr;

    if ( cache_ptr == NULL ) {

        pass = FALSE;
        HDsnprintf(msg, (size_t)128,
                   "cache_ptr NULL on entry to single entry test #%d.",
                   test_num);
        failure_mssg = msg;
    }
    else if ( ( cache_ptr->index_len != 0 ) ||
              ( cache_ptr->index_size != 0 ) ) {

        pass = FALSE;
        HDsnprintf(msg, (size_t)128,
                   "cache not empty at beginning of single entry test #%d.",
                   test_num);
        failure_mssg = msg;
    }
    else if ( ( entry_type < 0 ) || ( entry_type >= NUMBER_OF_ENTRY_TYPES ) ||
              ( entry_idx < 0 ) || ( entry_idx > max_indices[entry_type] ) ) {

        pass = FALSE;
        HDsnprintf(msg, (size_t)128,
                   "Bad parameters on entry to single entry test #%d.",
                   test_num);
        failure_mssg = msg;
    }

    if ( pass ) {

        base_addr = entries[entry_type];
        entry_ptr = &(base_addr[entry_idx]);

        if ( insert_flag ) {

            insert_entry(cache_ptr, entry_type, entry_idx, dirty_flag, flags);

        } else {

            protect_entry(cache_ptr, entry_type, entry_idx);

            unprotect_entry(cache_ptr, entry_type, entry_idx,
                            (int)dirty_flag, flags);
        }
    }

    if ( pass ) {

        result = H5C_flush_cache(NULL, -1, -1, cache_ptr, flush_flags);

        if ( result < 0 ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "flush with flags 0x%x failed in single entry test #%d.",
                       flush_flags, test_num);
            failure_mssg = msg;
        }
        else if ( ( entry_ptr->loaded != expected_loaded ) ||
                  ( entry_ptr->cleared != expected_cleared ) ||
                  ( entry_ptr->flushed != expected_flushed ) ||
                  ( entry_ptr->destroyed != expected_destroyed ) ) {

            HDfprintf(stdout,
              "loaded = %d(%d), clrd = %d(%d), flshd = %d(%d), dest = %d(%d)\n",
              (int)(entry_ptr->loaded),
              (int)expected_loaded,
              (int)(entry_ptr->cleared),
              (int)expected_cleared,
              (int)(entry_ptr->flushed),
              (int)expected_flushed,
              (int)(entry_ptr->destroyed),
              (int)expected_destroyed);

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
                "Unexpected entry status after flush in single entry test #%d.",
                test_num);
            failure_mssg = msg;
        }
        else if ( ( ( (flush_flags & H5C__FLUSH_INVALIDATE_FLAG) == 0 )
                    &&
                    ( ( cache_ptr->index_len != 1 )
                      ||
                      ( cache_ptr->index_size != entry_sizes[entry_type] )
                    )
                  )
                  ||
                  ( ( (flush_flags & H5C__FLUSH_INVALIDATE_FLAG) != 0 )
                    &&
                    ( ( cache_ptr->index_len != 0 )
                      ||
                      ( cache_ptr->index_size != 0 )
                    )
                  )
                ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
              "Unexpected cache len/size after flush in single entry test #%d.",
              test_num);
            failure_mssg = msg;
        }
    }


    /* clean up the cache to prep for the next test */
    if ( pass ) {

        result = H5C_flush_cache(NULL, -1, -1, cache_ptr,
                                 H5C__FLUSH_INVALIDATE_FLAG);

        if ( result < 0 ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "Flush failed on cleanup in single entry test #%d.",
                       test_num);
            failure_mssg = msg;
        }
        else if ( ( cache_ptr->index_len != 0 ) ||
                  ( cache_ptr->index_size != 0 ) ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
            "Unexpected cache len/size after cleanup in single entry test #%d.",
            test_num);
            failure_mssg = msg;

        } else {

            entry_ptr->loaded    = FALSE;
            entry_ptr->cleared   = FALSE;
            entry_ptr->flushed   = FALSE;
            entry_ptr->destroyed = FALSE;
        }
    }

    return;

} /* check_flush_cache__single_entry_test() */


/*-------------------------------------------------------------------------
 * Function:	check_flush_cache__pinned_single_entry_test()
 *
 * Purpose:	Run a pinned single entry flush cache test.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              3/28/06
 *
 * Modifications:
 *
 * 		JRM -- 5/17/06
 * 		Added the pop_mark_dirty_prot and pop_mark_dirty_pinned
 * 		flags and supporting code to allow us to test the
 * 		H5C_mark_pinned_or_protected_entry_dirty() call.  Use the
 * 		call to mark the entry dirty while the entry is protected
 * 		if pop_mark_dirty_prot is TRUE, and to mark the entry
 * 		dirty while it is pinned if pop_mark_dirty_pinned is TRUE.
 *
 *-------------------------------------------------------------------------
 */

static void
check_flush_cache__pinned_single_entry_test(H5C_t * cache_ptr,
                                            int test_num,
                                            int entry_type,
                                            int entry_idx,
                                            hbool_t dirty_flag,
					    hbool_t mark_dirty,
					    hbool_t pop_mark_dirty_prot,
					    hbool_t pop_mark_dirty_pinned,
					    hbool_t unprotect_unpin,
                                            unsigned int flags,
                                            unsigned int flush_flags,
                                            hbool_t expected_cleared,
                                            hbool_t expected_flushed,
                                            hbool_t expected_destroyed)
{
    /* const char *fcn_name = "check_flush_cache__pinned_single_entry_test"; */
    static char    msg[128];
    hbool_t        expected_loaded = TRUE;
    herr_t	   result;
    test_entry_t * base_addr;
    test_entry_t * entry_ptr;

    if ( cache_ptr == NULL ) {

        pass = FALSE;
        HDsnprintf(msg, (size_t)128,
                   "cache_ptr NULL on entry to pinned single entry test #%d.",
                   test_num);
        failure_mssg = msg;
    }
    else if ( ( cache_ptr->index_len != 0 ) ||
              ( cache_ptr->index_size != 0 ) ) {

        pass = FALSE;
        HDsnprintf(msg, (size_t)128,
               "cache not empty at beginning of pinned single entry test #%d.",
               test_num);
        failure_mssg = msg;
    }
    else if ( ( entry_type < 0 ) || ( entry_type >= NUMBER_OF_ENTRY_TYPES ) ||
              ( entry_idx < 0 ) || ( entry_idx > max_indices[entry_type] ) ) {

        pass = FALSE;
        HDsnprintf(msg, (size_t)128,
                   "Bad parameters on entry to pinned single entry test #%d.",
                   test_num);
        failure_mssg = msg;
    }

    if ( pass ) {

        base_addr = entries[entry_type];
        entry_ptr = &(base_addr[entry_idx]);

        protect_entry(cache_ptr, entry_type, entry_idx);

	if ( pop_mark_dirty_prot ) {

	    mark_pinned_or_protected_entry_dirty(cache_ptr,
			                         entry_type,
			                         entry_idx);
	}

        unprotect_entry(cache_ptr, entry_type, entry_idx,
                        (int)dirty_flag, (flags | H5C__PIN_ENTRY_FLAG));

	if ( mark_dirty ) {

            mark_pinned_entry_dirty(cache_ptr, entry_type, entry_idx,
			            FALSE, (size_t)0);
	}

	if ( pop_mark_dirty_pinned ) {

	    mark_pinned_or_protected_entry_dirty(cache_ptr,
			                         entry_type,
			                         entry_idx);
	}
    }

    if ( pass ) {

        result = H5C_flush_cache(NULL, -1, -1, cache_ptr, flush_flags);

        if ( result < 0 ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
               "flush with flags 0x%x failed in pinned single entry test #%d.",
               flush_flags, test_num);
            failure_mssg = msg;
        }
        else if ( ( entry_ptr->loaded != expected_loaded ) ||
                  ( entry_ptr->cleared != expected_cleared ) ||
                  ( entry_ptr->flushed != expected_flushed ) ||
                  ( entry_ptr->destroyed != expected_destroyed ) ) {

            HDfprintf(stdout,
              "loaded = %d(%d), clrd = %d(%d), flshd = %d(%d), dest = %d(%d)\n",
              (int)(entry_ptr->loaded),
              (int)expected_loaded,
              (int)(entry_ptr->cleared),
              (int)expected_cleared,
              (int)(entry_ptr->flushed),
              (int)expected_flushed,
              (int)(entry_ptr->destroyed),
              (int)expected_destroyed);

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
                "Unexpected entry status after flush in pinned single entry test #%d.",
                test_num);
            failure_mssg = msg;
        }
        else if ( ( ( (flush_flags & H5C__FLUSH_INVALIDATE_FLAG) == 0 )
                    &&
                    ( ( cache_ptr->index_len != 1 )
                      ||
                      ( cache_ptr->index_size != entry_sizes[entry_type] )
                    )
                  )
                  ||
                  ( ( (flush_flags & H5C__FLUSH_INVALIDATE_FLAG) != 0 )
                    &&
                    ( ( cache_ptr->index_len != 0 )
                      ||
                      ( cache_ptr->index_size != 0 )
                    )
                  )
                ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
              "Unexpected cache len/size after flush in pinned single entry test #%d.",
              test_num);
            failure_mssg = msg;
        }
    }


    /* clean up the cache to prep for the next test */
    if ( pass ) {

        if ( unprotect_unpin ) {

            protect_entry(cache_ptr, entry_type, entry_idx);

            unprotect_entry(cache_ptr, entry_type, entry_idx,
                            (int)dirty_flag, H5C__UNPIN_ENTRY_FLAG);

        } else {

            unpin_entry(cache_ptr, entry_type, entry_idx);

        }
    }

    if ( pass ) {

        result = H5C_flush_cache(NULL, -1, -1, cache_ptr,
                                 H5C__FLUSH_INVALIDATE_FLAG);

        if ( result < 0 ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
                    "Flush failed on cleanup in pinned single entry test #%d.",
                    test_num);
            failure_mssg = msg;
        }
        else if ( ( cache_ptr->index_len != 0 ) ||
                  ( cache_ptr->index_size != 0 ) ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
            "Unexpected cache len/size after cleanup in pinned single entry test #%d.",
            test_num);
            failure_mssg = msg;

        } else {

            entry_ptr->loaded    = FALSE;
            entry_ptr->cleared   = FALSE;
            entry_ptr->flushed   = FALSE;
            entry_ptr->destroyed = FALSE;
        }
    }

    return;

} /* check_flush_cache__pinned_single_entry_test() */


/*-------------------------------------------------------------------------
 * Function:	check_get_entry_status()
 *
 * Purpose:	Verify that H5C_get_entry_status() behaves as expected.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              4/28/06
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
check_get_entry_status(void)
{
    const char *  fcn_name = "check_get_entry_status";
    static char   msg[128];
    herr_t        result;
    hbool_t	  in_cache;
    hbool_t	  is_dirty;
    hbool_t	  is_protected;
    hbool_t	  is_pinned;
    size_t	  entry_size;
    H5C_t *       cache_ptr = NULL;
    test_entry_t * base_addr;
    test_entry_t * entry_ptr;

    TESTING("H5C_get_entry_status() functionality");

    pass = TRUE;

    if ( pass ) {

        reset_entries();

        cache_ptr = setup_cache((size_t)(2 * 1024 * 1024),
                                (size_t)(1 * 1024 * 1024));

        base_addr = entries[0];
        entry_ptr = &(base_addr[0]);
    }

    if ( pass ) {

        /* entry not in cache -- only in_cache should be touched by
         * the call.  Thus, only check that boolean.
         */

        result = H5C_get_entry_status(cache_ptr, entry_ptr->addr, &entry_size,
                &in_cache, &is_dirty, &is_protected, &is_pinned);

	if ( result < 0 ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "H5AC_get_entry_status() reports failure 1.");
            failure_mssg = msg;

	} else if ( in_cache ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected status 1.");
            failure_mssg = msg;
        }
    }

    protect_entry(cache_ptr, 0, 0);

    unprotect_entry(cache_ptr, 0, 0, FALSE, H5C__NO_FLAGS_SET);

    if ( pass ) {

        result = H5C_get_entry_status(cache_ptr, entry_ptr->addr, &entry_size,
                &in_cache, &is_dirty, &is_protected, &is_pinned);

	if ( result < 0 ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "H5AC_get_entry_status() reports failure 2.");
            failure_mssg = msg;

	} else if ( !in_cache || is_dirty || is_protected || is_pinned ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected status 2.");
            failure_mssg = msg;
        }
    }

    protect_entry(cache_ptr, 0, 0);

    if ( pass ) {

        result = H5C_get_entry_status(cache_ptr, entry_ptr->addr, &entry_size,
                &in_cache, &is_dirty, &is_protected, &is_pinned);

	if ( result < 0 ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "H5AC_get_entry_status() reports failure 3.");
            failure_mssg = msg;

	} else if ( !in_cache || is_dirty || !is_protected || is_pinned ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected status 3.");
            failure_mssg = msg;
        }
    }

    unprotect_entry(cache_ptr, 0, 0, FALSE, H5C__PIN_ENTRY_FLAG);

    if ( pass ) {

        result = H5C_get_entry_status(cache_ptr, entry_ptr->addr, &entry_size,
                &in_cache, &is_dirty, &is_protected, &is_pinned);

	if ( result < 0 ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "H5AC_get_entry_status() reports failure 4.");
            failure_mssg = msg;

	} else if ( !in_cache || is_dirty || is_protected || !is_pinned ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected status 4.");
            failure_mssg = msg;
        }
    }

    mark_pinned_entry_dirty(cache_ptr, 0, 0, FALSE, 0);

    if ( pass ) {

        result = H5C_get_entry_status(cache_ptr, entry_ptr->addr, &entry_size,
                &in_cache, &is_dirty, &is_protected, &is_pinned);

	if ( result < 0 ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "H5AC_get_entry_status() reports failure 5.");
            failure_mssg = msg;

	} else if ( !in_cache || !is_dirty || is_protected || !is_pinned ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected status 5.");
            failure_mssg = msg;
        }
    }

    unpin_entry(cache_ptr, 0, 0);

    if ( pass ) {

        result = H5C_get_entry_status(cache_ptr, entry_ptr->addr, &entry_size,
                &in_cache, &is_dirty, &is_protected, &is_pinned);

	if ( result < 0 ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "H5AC_get_entry_status() reports failure 6.");
            failure_mssg = msg;

	} else if ( !in_cache || !is_dirty || is_protected || is_pinned ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected status 6.");
            failure_mssg = msg;
        }
    }

    if ( pass ) {

        takedown_cache(cache_ptr, FALSE, FALSE);
    }

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass ) {

        HDfprintf(stdout, "%s(): failure_mssg = \"%s\".\n",
                  fcn_name, failure_mssg);
    }

    return;

} /* check_get_entry_status() */


/*-------------------------------------------------------------------------
 * Function:	check_expunge_entry()
 *
 * Purpose:	Verify that H5C_expunge_entry() behaves as expected.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              7/5/06
 *
 * Modifications:
 *
 * 		None.
 *
 *-------------------------------------------------------------------------
 */

static void
check_expunge_entry(void)
{
    const char *  fcn_name = "check_expunge_entry";
    static char   msg[128];
    herr_t        result;
    hbool_t	  in_cache;
    hbool_t	  is_dirty;
    hbool_t	  is_protected;
    hbool_t	  is_pinned;
    size_t	  entry_size;
    H5C_t *       cache_ptr = NULL;
    test_entry_t * base_addr;
    test_entry_t * entry_ptr;

    TESTING("H5C_expunge_entry() functionality");

    pass = TRUE;

    if ( pass ) {

        reset_entries();

        cache_ptr = setup_cache((size_t)(2 * 1024 * 1024),
                                (size_t)(1 * 1024 * 1024));

        base_addr = entries[0];
        entry_ptr = &(base_addr[0]);
    }

    if ( pass ) {

        /* entry not in cache -- only in_cache should be touched by
         * the status call.  Thus, only check that boolean.
         */

        result = H5C_get_entry_status(cache_ptr, entry_ptr->addr, &entry_size,
                                      &in_cache, &is_dirty, &is_protected, 
				      &is_pinned);

	if ( result < 0 ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "H5AC_get_entry_status() reports failure 1.");
            failure_mssg = msg;

	} else if ( in_cache ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected status 1.");
            failure_mssg = msg;

        } else if ( ( entry_ptr->loaded ) ||
                    ( entry_ptr->cleared ) ||
		    ( entry_ptr->flushed ) ||
		    ( entry_ptr->destroyed ) ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected entry history 1.");
            failure_mssg = msg;

	}
    }

    /* protect an entry to force the cache to load it, and then unprotect
     * it without marking it dirty.
     */

    protect_entry(cache_ptr, 0, 0);

    unprotect_entry(cache_ptr, 0, 0, FALSE, H5C__NO_FLAGS_SET);

    if ( pass ) {

        result = H5C_get_entry_status(cache_ptr, entry_ptr->addr, &entry_size,
                &in_cache, &is_dirty, &is_protected, &is_pinned);

	if ( result < 0 ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "H5AC_get_entry_status() reports failure 2.");
            failure_mssg = msg;

	} else if ( !in_cache || is_dirty || is_protected || is_pinned ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected status 2.");
            failure_mssg = msg;

        } else if ( ( ! entry_ptr->loaded ) ||
                    ( entry_ptr->cleared ) ||
		    ( entry_ptr->flushed ) ||
		    ( entry_ptr->destroyed ) ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected entry history 2.");
            failure_mssg = msg;

	}
    }

    /* Expunge the entry and then verify that it is no longer in the cache.
     * Also verify that the entry was loaded, cleared, and destroyed, but 
     * not flushed.
     */
    expunge_entry(cache_ptr, 0, 0);

    if ( pass ) {

        /* entry shouldn't be in cache -- only in_cache should be touched 
	 * by the status call.  Thus, only check that boolean.
         */

        result = H5C_get_entry_status(cache_ptr, entry_ptr->addr, &entry_size,
                &in_cache, &is_dirty, &is_protected, &is_pinned);

	if ( result < 0 ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "H5AC_get_entry_status() reports failure 3.");
            failure_mssg = msg;

	} else if ( in_cache ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected status 3.");
            failure_mssg = msg;

        } else if ( ( ! entry_ptr->loaded ) ||
                    ( ! entry_ptr->cleared ) ||
		    ( entry_ptr->flushed ) ||
		    ( ! entry_ptr->destroyed ) ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected entry history 3.");
            failure_mssg = msg;

	}
    }

    /* now repeat the process with a different entry.  On unprotect
     * mark the entry as dirty.  Verify that it is not flushed.
     */
    
    base_addr = entries[0];
    entry_ptr = &(base_addr[1]);

    if ( pass ) {

        /* entry not in cache -- only in_cache should be touched by
         * the status call.  Thus, only check that boolean.
         */

        result = H5C_get_entry_status(cache_ptr, entry_ptr->addr, &entry_size,
                                      &in_cache, &is_dirty, &is_protected, 
				      &is_pinned);

	if ( result < 0 ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "H5AC_get_entry_status() reports failure 4.");
            failure_mssg = msg;

	} else if ( in_cache ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected status 4.");
            failure_mssg = msg;

        } else if ( ( entry_ptr->loaded ) ||
                    ( entry_ptr->cleared ) ||
		    ( entry_ptr->flushed ) ||
		    ( entry_ptr->destroyed ) ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected entry history 4.");
            failure_mssg = msg;

	}
    }

    /* protect the entry to force the cache to load it, and then unprotect
     * it with the dirty flag set.
     */

    protect_entry(cache_ptr, 0, 1);

    unprotect_entry(cache_ptr, 0, 1, TRUE, H5C__NO_FLAGS_SET);

    if ( pass ) {

        result = H5C_get_entry_status(cache_ptr, entry_ptr->addr, &entry_size,
                                      &in_cache, &is_dirty, &is_protected, 
				      &is_pinned);

	if ( result < 0 ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "H5AC_get_entry_status() reports failure 5.");
            failure_mssg = msg;

	} else if ( !in_cache || !is_dirty || is_protected || is_pinned ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected status 5.");
            failure_mssg = msg;

        } else if ( ( ! entry_ptr->loaded ) ||
                    ( entry_ptr->cleared ) ||
		    ( entry_ptr->flushed ) ||
		    ( entry_ptr->destroyed ) ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected entry history 5.");
            failure_mssg = msg;

	}
    }

    /* Expunge the entry and then verify that it is no longer in the cache.
     * Also verify that the entry was loaded, cleared and destroyed, but not
     * flushed.
     */
    expunge_entry(cache_ptr, 0, 1);

    if ( pass ) {

        /* entry shouldn't be in cache -- only in_cache should be touched 
	 * by the status call.  Thus, only check that boolean.
         */

        result = H5C_get_entry_status(cache_ptr, entry_ptr->addr, &entry_size,
                                      &in_cache, &is_dirty, &is_protected, 
				      &is_pinned);

	if ( result < 0 ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "H5AC_get_entry_status() reports failure 6.");
            failure_mssg = msg;

	} else if ( in_cache ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected status 6.");
            failure_mssg = msg;

        } else if ( ( ! entry_ptr->loaded ) ||
                    ( ! entry_ptr->cleared ) ||
		    ( entry_ptr->flushed ) ||
		    ( ! entry_ptr->destroyed ) ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected entry history 6.");
            failure_mssg = msg;

	}
    }

    if ( pass ) {

        takedown_cache(cache_ptr, FALSE, FALSE);
    }

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass ) {

        HDfprintf(stdout, "%s(): failure_mssg = \"%s\".\n",
                  fcn_name, failure_mssg);
    }

    return;

} /* check_expunge_entry() */


/*-------------------------------------------------------------------------
 * Function:	check_rename_entry()
 *
 * Purpose:	Verify that H5C_rename_entry behaves as expected.  In
 * 		particular, verify that it works correctly with pinned
 * 		entries.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              4/26/06
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
check_rename_entry(void)
{
    const char * fcn_name = "check_rename_entry";
    int          i;
    H5C_t *      cache_ptr = NULL;
    struct rename_entry_test_spec test_specs[4] =
    {
      {
        /* int     entry_type  = */ PICO_ENTRY_TYPE,
        /* int     entry_index = */ 10,
	/* hbool_t is_dirty    = */ FALSE,
	/* hbool_t is_pinned   = */ FALSE
      },
      {
        /* int     entry_type  = */ PICO_ENTRY_TYPE,
        /* int     entry_index = */ 20,
	/* hbool_t is_dirty    = */ TRUE,
	/* hbool_t is_pinned   = */ FALSE
      },
      {
        /* int     entry_type  = */ PICO_ENTRY_TYPE,
        /* int     entry_index = */ 30,
	/* hbool_t is_dirty    = */ FALSE,
	/* hbool_t is_pinned   = */ TRUE
      },
      {
        /* int     entry_type  = */ PICO_ENTRY_TYPE,
        /* int     entry_index = */ 40,
	/* hbool_t is_dirty    = */ TRUE,
	/* hbool_t is_pinned   = */ TRUE
      }
    };

    TESTING("H5C_rename_entry() functionality");

    pass = TRUE;

    /* allocate a cache, load entries into it, and then rename
     * them.  To the extent possible, verify that the desired
     * actions took place.
     *
     * At present, we should do the following tests:
     *
     * 1) Rename a clean, unprotected, unpinned entry.
     *
     * 2) Rename a dirty, unprotected, unpinned entry.
     *
     * 3) Rename a clean, unprotected, pinned entry.
     *
     * 4) Rename a dirty, unprotected, pinned entry.
     *
     * In all cases, the entry should have moved to its
     * new location, and have been marked dirty if it wasn't
     * already.
     *
     * Unpinned entries should have been moved to the head
     * of the LRU list.
     *
     * Pinned entries should remain untouched on the pinned entry
     * list.
     */

    if ( pass ) {

        reset_entries();

        cache_ptr = setup_cache((size_t)(2 * 1024 * 1024),
                                (size_t)(1 * 1024 * 1024));
    }

    i = 0;
    while ( ( pass ) && ( i < 4 ) )
    {
        check_rename_entry__run_test(cache_ptr, i, &(test_specs[i]));
	i++;
    }

    if ( pass ) {

        takedown_cache(cache_ptr, FALSE, FALSE);
    }

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass ) {

        HDfprintf(stdout, "%s(): failure_mssg = \"%s\".\n",
                  fcn_name, failure_mssg);
    }

    return;

} /* check_rename_entry() */


/*-------------------------------------------------------------------------
 * Function:    check_rename_entry__run_test()
 *
 * Purpose:     Run a rename entry test.
 *
 *		Do nothing if pass is FALSE on entry.
 *
 * Return:      void
 *
 * Programmer:  John Mainzer
 *              4/27/06
 *
 * Modifications:
 *
 *		None.
 *
 *-------------------------------------------------------------------------
 */

static void
check_rename_entry__run_test(H5C_t * cache_ptr,
                             int test_num,
                             struct rename_entry_test_spec * spec_ptr)
{
    /* const char *   fcn_name = "check_rename_entry__run_test"; */
    static char    msg[128];
    unsigned int   flags = H5C__NO_FLAGS_SET;
    test_entry_t * base_addr;
    test_entry_t * entry_ptr = NULL;
    H5C_cache_entry_t * test_ptr = NULL;

    if ( cache_ptr == NULL ) {

        pass = FALSE;
        HDsnprintf(msg, (size_t)128,
                   "cache_ptr NULL on entry to rename test #%d.",
                   test_num);
        failure_mssg = msg;

    } else if ( spec_ptr == NULL ) {

        pass = FALSE;
        HDsnprintf(msg, (size_t)128,
                   "spec_ptr NULL on entry to rename test #%d.",
                   test_num);
        failure_mssg = msg;

    }

    if ( pass ) {

        base_addr = entries[spec_ptr->entry_type];
        entry_ptr = &(base_addr[spec_ptr->entry_index]);

        if ( ( entry_ptr->self != entry_ptr ) ||
             ( ( entry_ptr->cache_ptr != cache_ptr ) &&
               ( entry_ptr->cache_ptr != NULL ) ) ||
             ( ! ( entry_ptr->at_main_addr ) ) ||
             ( entry_ptr->addr != entry_ptr->main_addr ) ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "bad entry_ptr in rename test #%d.",
                       test_num);
            failure_mssg = msg;

        } else if ( spec_ptr->is_pinned ) {

            flags |= H5C__PIN_ENTRY_FLAG;
        }
    }

    protect_entry(cache_ptr, spec_ptr->entry_type, spec_ptr->entry_index);

    unprotect_entry(cache_ptr, spec_ptr->entry_type, spec_ptr->entry_index,
                    (int)(spec_ptr->is_dirty), flags);

    rename_entry(cache_ptr, spec_ptr->entry_type, spec_ptr->entry_index, FALSE);

    if ( pass ) {

        /* verify that the rename took place, and that the cache's internal
         * structures are as expected.  Note that some sanity checking is
         * done by rename_entry(), so we don't have to repeat it here.
         */

        if ( spec_ptr->is_pinned ) {

            if ( ! ( entry_ptr->header.is_pinned ) ) {

                pass = FALSE;
                HDsnprintf(msg, (size_t)128,
                           "Pinned entry not pinned after rename in test #%d.",
                           test_num);
                failure_mssg = msg;
            }

            if ( pass ) {

                test_ptr = cache_ptr->pel_head_ptr;

                while ( ( test_ptr != NULL ) &&
                        ( test_ptr != (H5C_cache_entry_t *)entry_ptr ) )
                {
                    test_ptr = test_ptr->next;
                }

                if ( test_ptr == NULL ) {

                    pass = FALSE;
                    HDsnprintf(msg, (size_t)128,
                           "Pinned entry not in pel after rename in test #%d.",
                           test_num);
                    failure_mssg = msg;
                }
            }

            unpin_entry(cache_ptr, spec_ptr->entry_type, spec_ptr->entry_index);

        } else {

            if ( entry_ptr->header.is_pinned ) {

                pass = FALSE;
                HDsnprintf(msg, (size_t)128,
                           "Unpinned entry pinned after rename in test #%d.",
                           test_num);
                failure_mssg = msg;
            }

            if ( ( entry_ptr->header.prev != NULL ) ||
                 ( cache_ptr->LRU_head_ptr != (H5C_cache_entry_t *)entry_ptr ) )
            {
                pass = FALSE;
                HDsnprintf(msg, (size_t)128,
                           "Entry not at head of LRU after rename in test #%d.",
                           test_num);
                failure_mssg = msg;
            }
        }
    }

    /* put the entry back where it started from */
    rename_entry(cache_ptr, spec_ptr->entry_type, spec_ptr->entry_index, TRUE);

    return;

} /* check_rename_entry__run_test() */


/*-------------------------------------------------------------------------
 * Function:	check_pin_protected_entry()
 *
 * Purpose:	Verify that H5C_pin_protected_entry behaves as expected.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              4/28/06
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
check_pin_protected_entry(void)
{
    const char *  fcn_name = "check_pin_protected_entry";
    static char   msg[128];
    herr_t        result;
    H5C_t *       cache_ptr = NULL;
    test_entry_t * base_addr;
    test_entry_t * entry_ptr;

    TESTING("H5C_pin_protected_entry() functionality");

    pass = TRUE;

    /* Create a cache, protect an entry, and then use H5C_pin_protected_entry()
     * to pin it.  Verify that the entry is in fact pined.  Unprotect the entry
     * to unpin it, and then destroy the cache.
     */

    if ( pass ) {

        reset_entries();

        cache_ptr = setup_cache((size_t)(2 * 1024 * 1024),
                                (size_t)(1 * 1024 * 1024));
    }

    protect_entry(cache_ptr, 0, 0);

    if ( pass ) {

        base_addr = entries[0];
        entry_ptr = &(base_addr[0]);

	result = H5C_pin_protected_entry(cache_ptr, (void *)entry_ptr);

	if ( result < 0 ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "H5C_pin_protected_entry() reports failure.");
            failure_mssg = msg;

	} else if ( ! ( entry_ptr->header.is_pinned ) ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "entry not pinned when it should be.");
            failure_mssg = msg;

	} else {

	    entry_ptr->is_pinned = TRUE;
	}
    }

    unprotect_entry(cache_ptr, 0, 0, FALSE, H5C__UNPIN_ENTRY_FLAG);

    if ( pass ) {

        takedown_cache(cache_ptr, FALSE, FALSE);
    }

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass ) {

        HDfprintf(stdout, "%s(): failure_mssg = \"%s\".\n",
                  fcn_name, failure_mssg);
    }

    return;

} /* check_pin_protected_entry() */


/*-------------------------------------------------------------------------
 * Function:	check_resize_entry()
 *
 * Purpose:	Verify that H5C_resize_entry() and H5C_unprotect() resize
 * 		entries as expected.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              7/7/06
 *
 * Modifications:
 *
 * 		None.
 *
 *-------------------------------------------------------------------------
 */

static void
check_resize_entry(void)
{
    const char *   fcn_name = "check_resize_entry";
    static char    msg[128];
    herr_t         result;
    hbool_t	   in_cache;
    hbool_t	   is_dirty;
    hbool_t	   is_protected;
    hbool_t	   is_pinned;
    size_t	   entry_size;
    size_t	   reported_entry_size;
    H5C_t *        cache_ptr = NULL;
    test_entry_t * base_addr;
    test_entry_t * entry_ptr;

    TESTING("entry resize functionality");

    /* Setup a cache and verify that it is empty.
     *
     * Then force the load of an entry by protecting it, and verify that 
     * the entry and cache have the expected sizes.
     *
     * Then unprotect the entry with the size changed flag and a reduced
     * size.  Verify that the entry and cache have the expected expected
     * sizes.
     *
     * Use a second protect/unprotect cycle to restore the entry to
     * its original size.  Verify that the entry and cache have the 
     * expected sizes.
     *
     * Protect and unprotect the entry again to pin it.  Use 
     * H5C_resize_entry to reduce its size.  Verify that the entry
     * and cache have the expected sizes.
     *
     * Use H5C_resize_entry again to restore the entry to its original
     * size.  Verify that the entry  and cache have the expected sizes.
     *
     * Use a protect / unprotect cycle to unpin and destroy the entry.
     * Verify that the entry  and cache have the expected sizes.
     *
     *
     * Obesrve that all the above tests have been done with only one
     * entry in the cache.  Repeat the tests with several entries in 
     * the cache.
     */

    pass = TRUE;

    /* tests with only one entry in the cache: */

    if ( pass ) {

        reset_entries();

        cache_ptr = setup_cache((size_t)(2 * 1024 * 1024),
                                (size_t)(1 * 1024 * 1024));

        base_addr = entries[LARGE_ENTRY_TYPE];
        entry_ptr = &(base_addr[0]);
	entry_size = LARGE_ENTRY_SIZE;
    }

    if ( pass ) {

	if ( ( cache_ptr->index_len != 0 ) ||
	     ( cache_ptr->index_size != 0 ) ||
	     ( cache_ptr->slist_len != 0 ) ||
	     ( cache_ptr->slist_size != 0 ) ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected cache status 1.");
            failure_mssg = msg;

	}
    }

    protect_entry(cache_ptr, LARGE_ENTRY_TYPE, 0);

    if ( pass ) {

	if ( ( cache_ptr->index_len != 1 ) ||
	     ( cache_ptr->index_size != LARGE_ENTRY_SIZE ) ||
	     ( cache_ptr->slist_len != 0 ) ||
	     ( cache_ptr->slist_size != 0 ) ) {


            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected cache status 2.");
            failure_mssg = msg;

	}
    }

    if ( pass ) {

        result = H5C_get_entry_status(cache_ptr, entry_ptr->addr, 
			              &reported_entry_size, &in_cache, 
				      &is_dirty, &is_protected, &is_pinned);

	if ( result < 0 ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "H5AC_get_entry_status() reports failure 1.");
            failure_mssg = msg;

	} else if ( !in_cache || is_dirty || !is_protected || is_pinned ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected status 1.");
            failure_mssg = msg;

        } else if ( ( ! entry_ptr->loaded ) ||
                    ( entry_ptr->cleared ) ||
		    ( entry_ptr->flushed ) ||
		    ( entry_ptr->destroyed ) ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected entry history 1.");
            failure_mssg = msg;

	}
    }

    if ( pass ) {

        result = H5C_unprotect(NULL, -1, -1, cache_ptr, 
			       &(types[LARGE_ENTRY_TYPE]), entry_ptr->addr,
			       (void *)entry_ptr, 
			       H5C__SIZE_CHANGED_FLAG | H5C__DIRTIED_FLAG,
			       (LARGE_ENTRY_SIZE / 2));

	if ( result < 0 ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "H5C_unprotect() reports failure 1.");
            failure_mssg = msg;

	} else {

	    /* tidy up so we play nice with the standard protect / unprotect
	     * calls.
	     */
	    entry_ptr->is_protected = FALSE;
	    entry_ptr->is_dirty = TRUE;
	    entry_ptr->size = LARGE_ENTRY_SIZE / 2;
	}
    }

    if ( pass ) {

	if ( ( cache_ptr->index_len != 1 ) ||
	     ( cache_ptr->index_size != (LARGE_ENTRY_SIZE / 2) ) ||
	     ( cache_ptr->slist_len != 1 ) ||
	     ( cache_ptr->slist_size != (LARGE_ENTRY_SIZE / 2) ) ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected cache status 3.");
            failure_mssg = msg;

	}
    }

    if ( pass ) {

        result = H5C_get_entry_status(cache_ptr, entry_ptr->addr, 
			              &reported_entry_size, &in_cache, 
				      &is_dirty, &is_protected, &is_pinned);

	if ( result < 0 ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "H5AC_get_entry_status() reports failure 2.");
            failure_mssg = msg;

	} else if ( !in_cache || !is_dirty || is_protected || is_pinned ||
		    ( reported_entry_size != (LARGE_ENTRY_SIZE / 2) ) ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected status 2.");
            failure_mssg = msg;

        } else if ( ( ! entry_ptr->loaded ) ||
                    ( entry_ptr->cleared ) ||
		    ( entry_ptr->flushed ) ||
		    ( entry_ptr->destroyed ) ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected entry history 2.");
            failure_mssg = msg;

	}
    }

    protect_entry(cache_ptr, LARGE_ENTRY_TYPE, 0);

    if ( pass ) {

        result = H5C_unprotect(NULL, -1, -1, cache_ptr, 
			       &(types[LARGE_ENTRY_TYPE]), entry_ptr->addr,
			       (void *)entry_ptr, 
			       (H5C__DIRTIED_FLAG | H5C__SIZE_CHANGED_FLAG),
			       LARGE_ENTRY_SIZE);

	if ( result < 0 ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "H5C_unprotect() reports failure 2.");
            failure_mssg = msg;

	} else {

	    /* tidy up so we play nice with the standard protect / unprotect
	     * calls.
	     */
	    entry_ptr->is_protected = FALSE;
	    entry_ptr->is_dirty = TRUE;
	    entry_ptr->size = LARGE_ENTRY_SIZE;
	}
    }

    if ( pass ) {

	if ( ( cache_ptr->index_len != 1 ) ||
	     ( cache_ptr->index_size != LARGE_ENTRY_SIZE ) ||
	     ( cache_ptr->slist_len != 1 ) ||
	     ( cache_ptr->slist_size != LARGE_ENTRY_SIZE ) ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected cache status 4.");
            failure_mssg = msg;

	}
    }

    if ( pass ) {

        result = H5C_get_entry_status(cache_ptr, entry_ptr->addr, 
			              &reported_entry_size, &in_cache, 
				      &is_dirty, &is_protected, &is_pinned);

	if ( result < 0 ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "H5AC_get_entry_status() reports failure 3.");
            failure_mssg = msg;

	} else if ( !in_cache || !is_dirty || is_protected || is_pinned ||
		    ( reported_entry_size != LARGE_ENTRY_SIZE ) ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected status 3.");
            failure_mssg = msg;

        } else if ( ( ! entry_ptr->loaded ) ||
                    ( entry_ptr->cleared ) ||
		    ( entry_ptr->flushed ) ||
		    ( entry_ptr->destroyed ) ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected entry history 3.");
            failure_mssg = msg;

	}
    }

    protect_entry(cache_ptr, LARGE_ENTRY_TYPE, 0);

    unprotect_entry(cache_ptr, LARGE_ENTRY_TYPE, 0, FALSE, H5C__PIN_ENTRY_FLAG);

    if ( pass ) {

        result = H5C_resize_pinned_entry(cache_ptr, (void *)entry_ptr, 
			                 (LARGE_ENTRY_SIZE / 4));

	if ( result < 0 ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, 
		       "H5C_resize_pinned_entry() reports failure 1.");
            failure_mssg = msg;

	} 
    }

    if ( pass ) {

	if ( ( cache_ptr->index_len != 1 ) ||
	     ( cache_ptr->index_size != (LARGE_ENTRY_SIZE / 4) ) ||
	     ( cache_ptr->slist_len != 1 ) ||
	     ( cache_ptr->slist_size != (LARGE_ENTRY_SIZE / 4) ) ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected cache status 5.");
            failure_mssg = msg;

	}
    }

    if ( pass ) {

        result = H5C_get_entry_status(cache_ptr, entry_ptr->addr, 
			              &reported_entry_size, &in_cache, 
				      &is_dirty, &is_protected, &is_pinned);

	if ( result < 0 ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "H5AC_get_entry_status() reports failure 4.");
            failure_mssg = msg;

	} else if ( !in_cache || !is_dirty || is_protected || ! is_pinned ||
		    ( reported_entry_size != (LARGE_ENTRY_SIZE / 4) ) ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected status 4.");
            failure_mssg = msg;

        } else if ( ( ! entry_ptr->loaded ) ||
                    ( entry_ptr->cleared ) ||
		    ( entry_ptr->flushed ) ||
		    ( entry_ptr->destroyed ) ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected entry history 4.");
            failure_mssg = msg;

	}
    }

    if ( pass ) {

        result = H5C_resize_pinned_entry(cache_ptr, (void *)entry_ptr, 
			                 LARGE_ENTRY_SIZE);

	if ( result < 0 ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, 
		       "H5C_resize_pinned_entry() reports failure 2.");
            failure_mssg = msg;

	} 
    }

    if ( pass ) {

	if ( ( cache_ptr->index_len != 1 ) ||
	     ( cache_ptr->index_size != LARGE_ENTRY_SIZE ) ||
	     ( cache_ptr->slist_len != 1 ) ||
	     ( cache_ptr->slist_size != LARGE_ENTRY_SIZE ) ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected cache status 6.");
            failure_mssg = msg;

	}
    }

    if ( pass ) {

        result = H5C_get_entry_status(cache_ptr, entry_ptr->addr, 
			              &reported_entry_size, &in_cache, 
				      &is_dirty, &is_protected, &is_pinned);

	if ( result < 0 ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "H5AC_get_entry_status() reports failure 5.");
            failure_mssg = msg;

	} else if ( !in_cache || !is_dirty || is_protected || ! is_pinned ||
		    ( reported_entry_size != LARGE_ENTRY_SIZE ) ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected status 5.");
            failure_mssg = msg;

        } else if ( ( ! entry_ptr->loaded ) ||
                    ( entry_ptr->cleared ) ||
		    ( entry_ptr->flushed ) ||
		    ( entry_ptr->destroyed ) ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected entry history 5.");
            failure_mssg = msg;

	}
    }

    protect_entry(cache_ptr, LARGE_ENTRY_TYPE, 0);

    unprotect_entry(cache_ptr, LARGE_ENTRY_TYPE, 0, FALSE, 
		    H5C__UNPIN_ENTRY_FLAG | H5C__DELETED_FLAG);

    if ( pass ) {

        result = H5C_get_entry_status(cache_ptr, entry_ptr->addr, &entry_size,
                                      &in_cache, &is_dirty, &is_protected, 
				      &is_pinned);

	if ( result < 0 ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "H5AC_get_entry_status() reports failure 6.");
            failure_mssg = msg;

	} else if ( in_cache ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected status 6.");
            failure_mssg = msg;

        } else if ( ( ! entry_ptr->loaded ) ||
                    ( ! entry_ptr->cleared ) ||
		    ( entry_ptr->flushed ) ||
		    ( ! entry_ptr->destroyed ) ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected entry history 6.");
            failure_mssg = msg;

	}
    }

    if ( pass ) {

	if ( ( cache_ptr->index_len != 0 ) ||
	     ( cache_ptr->index_size != 0 ) ||
	     ( cache_ptr->slist_len != 0 ) ||
	     ( cache_ptr->slist_size != 0 ) ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected cache status 7.");
            failure_mssg = msg;

	}
    }


    /* now repreat the above tests with several entries in the cache: */

    if ( pass ) {

	if ( ( cache_ptr->index_len != 0 ) ||
	     ( cache_ptr->index_size != 0 ) ||
	     ( cache_ptr->slist_len != 0 ) ||
	     ( cache_ptr->slist_size != 0 ) ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected cache status 8.");
            failure_mssg = msg;

	}
        base_addr = entries[LARGE_ENTRY_TYPE];
        entry_ptr = &(base_addr[3]);
	entry_size = LARGE_ENTRY_SIZE;
    }

    protect_entry(cache_ptr, LARGE_ENTRY_TYPE, 0);
    unprotect_entry(cache_ptr, LARGE_ENTRY_TYPE, 0, FALSE, H5C__NO_FLAGS_SET);

    protect_entry(cache_ptr, LARGE_ENTRY_TYPE, 1);
    unprotect_entry(cache_ptr, LARGE_ENTRY_TYPE, 1, TRUE, H5C__NO_FLAGS_SET);

    protect_entry(cache_ptr, LARGE_ENTRY_TYPE, 2);
    unprotect_entry(cache_ptr, LARGE_ENTRY_TYPE, 2, FALSE, H5C__NO_FLAGS_SET);

    if ( pass ) {

	if ( ( cache_ptr->index_len != 3 ) ||
	     ( cache_ptr->index_size != 3 * LARGE_ENTRY_SIZE ) ||
	     ( cache_ptr->slist_len != 1 ) ||
	     ( cache_ptr->slist_size != LARGE_ENTRY_SIZE ) ) {


            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected cache status 9.");
            failure_mssg = msg;

	}
    }

    protect_entry(cache_ptr, LARGE_ENTRY_TYPE, 3);

    if ( pass ) {

	if ( ( cache_ptr->index_len != 4 ) ||
	     ( cache_ptr->index_size != 4 * LARGE_ENTRY_SIZE ) ||
	     ( cache_ptr->slist_len != 1 ) ||
	     ( cache_ptr->slist_size != LARGE_ENTRY_SIZE ) ) {


            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected cache status 10.");
            failure_mssg = msg;

	}
    }

    if ( pass ) {

        result = H5C_get_entry_status(cache_ptr, entry_ptr->addr, 
			              &reported_entry_size, &in_cache, 
				      &is_dirty, &is_protected, &is_pinned);

	if ( result < 0 ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "H5AC_get_entry_status() reports failure 7.");
            failure_mssg = msg;

	} else if ( !in_cache || is_dirty || !is_protected || is_pinned ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected status 7.");
            failure_mssg = msg;

        } else if ( ( ! entry_ptr->loaded ) ||
                    ( entry_ptr->cleared ) ||
		    ( entry_ptr->flushed ) ||
		    ( entry_ptr->destroyed ) ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected entry history 7.");
            failure_mssg = msg;

	}
    }

    if ( pass ) {

        result = H5C_unprotect(NULL, -1, -1, cache_ptr, 
			       &(types[LARGE_ENTRY_TYPE]), entry_ptr->addr,
			       (void *)entry_ptr, 
			       H5C__SIZE_CHANGED_FLAG | H5C__DIRTIED_FLAG,
			       (LARGE_ENTRY_SIZE / 2));

	if ( result < 0 ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "H5C_unprotect() reports failure 3.");
            failure_mssg = msg;

	} else {

	    /* tidy up so we play nice with the standard protect / unprotect
	     * calls.
	     */
	    entry_ptr->is_protected = FALSE;
	    entry_ptr->is_dirty = TRUE;
	    entry_ptr->size = LARGE_ENTRY_SIZE / 2;
	}
    }

    if ( pass ) {

	if ( ( cache_ptr->index_len != 4 ) ||
	     ( cache_ptr->index_size != 
	       ((3 * LARGE_ENTRY_SIZE) + (LARGE_ENTRY_SIZE / 2)) ) ||
	     ( cache_ptr->slist_len != 2 ) ||
	     ( cache_ptr->slist_size != 
	       (LARGE_ENTRY_SIZE + (LARGE_ENTRY_SIZE / 2)) ) ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected cache status 11.");
            failure_mssg = msg;

	}
    }

    if ( pass ) {

        result = H5C_get_entry_status(cache_ptr, entry_ptr->addr, 
			              &reported_entry_size, &in_cache, 
				      &is_dirty, &is_protected, &is_pinned);

	if ( result < 0 ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "H5AC_get_entry_status() reports failure 8.");
            failure_mssg = msg;

	} else if ( !in_cache || !is_dirty || is_protected || is_pinned ||
		    ( reported_entry_size != (LARGE_ENTRY_SIZE / 2) ) ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected status 8.");
            failure_mssg = msg;

        } else if ( ( ! entry_ptr->loaded ) ||
                    ( entry_ptr->cleared ) ||
		    ( entry_ptr->flushed ) ||
		    ( entry_ptr->destroyed ) ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected entry history 8.");
            failure_mssg = msg;

	}
    }

    protect_entry(cache_ptr, LARGE_ENTRY_TYPE, 3);

    if ( pass ) {

        result = H5C_unprotect(NULL, -1, -1, cache_ptr, 
			       &(types[LARGE_ENTRY_TYPE]), entry_ptr->addr,
			       (void *)entry_ptr, 
			       (H5C__DIRTIED_FLAG | H5C__SIZE_CHANGED_FLAG),
			       LARGE_ENTRY_SIZE);

	if ( result < 0 ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "H5C_unprotect() reports failure 4.");
            failure_mssg = msg;

	} else {

	    /* tidy up so we play nice with the standard protect / unprotect
	     * calls.
	     */
	    entry_ptr->is_protected = FALSE;
	    entry_ptr->is_dirty = TRUE;
	    entry_ptr->size = LARGE_ENTRY_SIZE;
	}
    }

    if ( pass ) {

	if ( ( cache_ptr->index_len != 4 ) ||
	     ( cache_ptr->index_size != 4 * LARGE_ENTRY_SIZE ) ||
	     ( cache_ptr->slist_len != 2 ) ||
	     ( cache_ptr->slist_size != 2 * LARGE_ENTRY_SIZE ) ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected cache status 12.");
            failure_mssg = msg;

	}
    }

    if ( pass ) {

        result = H5C_get_entry_status(cache_ptr, entry_ptr->addr, 
			              &reported_entry_size, &in_cache, 
				      &is_dirty, &is_protected, &is_pinned);

	if ( result < 0 ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "H5AC_get_entry_status() reports failure 9.");
            failure_mssg = msg;

	} else if ( !in_cache || !is_dirty || is_protected || is_pinned ||
		    ( reported_entry_size != LARGE_ENTRY_SIZE ) ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected status 9.");
            failure_mssg = msg;

        } else if ( ( ! entry_ptr->loaded ) ||
                    ( entry_ptr->cleared ) ||
		    ( entry_ptr->flushed ) ||
		    ( entry_ptr->destroyed ) ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected entry history 9.");
            failure_mssg = msg;

	}
    }

    protect_entry(cache_ptr, LARGE_ENTRY_TYPE, 3);

    unprotect_entry(cache_ptr, LARGE_ENTRY_TYPE, 3, FALSE, H5C__PIN_ENTRY_FLAG);

    if ( pass ) {

        result = H5C_resize_pinned_entry(cache_ptr, (void *)entry_ptr, 
			                 (LARGE_ENTRY_SIZE / 4));

	if ( result < 0 ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, 
		       "H5C_resize_pinned_entry() reports failure 3.");
            failure_mssg = msg;

	} 
    }

    if ( pass ) {

	if ( ( cache_ptr->index_len != 4 ) ||
	     ( cache_ptr->index_size != 
	       ((3 * LARGE_ENTRY_SIZE) + (LARGE_ENTRY_SIZE / 4)) ) ||
	     ( cache_ptr->slist_len != 2 ) ||
	     ( cache_ptr->slist_size != 
	       (LARGE_ENTRY_SIZE + (LARGE_ENTRY_SIZE / 4)) ) ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected cache status 13.");
            failure_mssg = msg;

	}
    }

    if ( pass ) {

        result = H5C_get_entry_status(cache_ptr, entry_ptr->addr, 
			              &reported_entry_size, &in_cache, 
				      &is_dirty, &is_protected, &is_pinned);

	if ( result < 0 ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "H5AC_get_entry_status() reports failure 10.");
            failure_mssg = msg;

	} else if ( !in_cache || !is_dirty || is_protected || ! is_pinned ||
		    ( reported_entry_size != (LARGE_ENTRY_SIZE / 4) ) ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected status 10.");
            failure_mssg = msg;

        } else if ( ( ! entry_ptr->loaded ) ||
                    ( entry_ptr->cleared ) ||
		    ( entry_ptr->flushed ) ||
		    ( entry_ptr->destroyed ) ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected entry history 10.");
            failure_mssg = msg;

	}
    }

    if ( pass ) {

        result = H5C_resize_pinned_entry(cache_ptr, (void *)entry_ptr, 
			                 LARGE_ENTRY_SIZE);

	if ( result < 0 ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, 
		       "H5C_resize_pinned_entry() reports failure 4.");
            failure_mssg = msg;

	} 
    }

    if ( pass ) {

	if ( ( cache_ptr->index_len != 4 ) ||
	     ( cache_ptr->index_size != (4 * LARGE_ENTRY_SIZE) ) ||
	     ( cache_ptr->slist_len != 2 ) ||
	     ( cache_ptr->slist_size != (2 * LARGE_ENTRY_SIZE) ) ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected cache status 14.");
            failure_mssg = msg;

	}
    }

    if ( pass ) {

        result = H5C_get_entry_status(cache_ptr, entry_ptr->addr, 
			              &reported_entry_size, &in_cache, 
				      &is_dirty, &is_protected, &is_pinned);

	if ( result < 0 ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "H5AC_get_entry_status() reports failure 11.");
            failure_mssg = msg;

	} else if ( !in_cache || !is_dirty || is_protected || ! is_pinned ||
		    ( reported_entry_size != LARGE_ENTRY_SIZE ) ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected status 11.");
            failure_mssg = msg;

        } else if ( ( ! entry_ptr->loaded ) ||
                    ( entry_ptr->cleared ) ||
		    ( entry_ptr->flushed ) ||
		    ( entry_ptr->destroyed ) ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected entry history 11.");
            failure_mssg = msg;

	}
    }

    protect_entry(cache_ptr, LARGE_ENTRY_TYPE, 3);

    unprotect_entry(cache_ptr, LARGE_ENTRY_TYPE, 3, FALSE, 
		    H5C__UNPIN_ENTRY_FLAG | H5C__DELETED_FLAG);

    if ( pass ) {

        result = H5C_get_entry_status(cache_ptr, entry_ptr->addr, &entry_size,
                                      &in_cache, &is_dirty, &is_protected, 
				      &is_pinned);

	if ( result < 0 ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "H5AC_get_entry_status() reports failure 12.");
            failure_mssg = msg;

	} else if ( in_cache ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected status 12.");
            failure_mssg = msg;

        } else if ( ( ! entry_ptr->loaded ) ||
                    ( ! entry_ptr->cleared ) ||
		    ( entry_ptr->flushed ) ||
		    ( ! entry_ptr->destroyed ) ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected entry history 12.");
            failure_mssg = msg;

	}
    }

    if ( pass ) {

	if ( ( cache_ptr->index_len != 3 ) ||
	     ( cache_ptr->index_size != (3 * LARGE_ENTRY_SIZE) ) ||
	     ( cache_ptr->slist_len != 1 ) ||
	     ( cache_ptr->slist_size != LARGE_ENTRY_SIZE ) ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected cache status 15.");
            failure_mssg = msg;

	}
    }

    protect_entry(cache_ptr, LARGE_ENTRY_TYPE, 2);
    unprotect_entry(cache_ptr, LARGE_ENTRY_TYPE, 2, FALSE, H5C__DELETED_FLAG);

    protect_entry(cache_ptr, LARGE_ENTRY_TYPE, 1);
    unprotect_entry(cache_ptr, LARGE_ENTRY_TYPE, 1, FALSE, H5C__DELETED_FLAG);

    protect_entry(cache_ptr, LARGE_ENTRY_TYPE, 0);
    unprotect_entry(cache_ptr, LARGE_ENTRY_TYPE, 0, FALSE, H5C__DELETED_FLAG);


    if ( pass ) {

	if ( ( cache_ptr->index_len != 0 ) ||
	     ( cache_ptr->index_size != 0 ) ||
	     ( cache_ptr->slist_len != 0 ) ||
	     ( cache_ptr->slist_size != 0 ) ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected cache status 16.");
            failure_mssg = msg;

	}
    }

    if ( pass ) {

        takedown_cache(cache_ptr, FALSE, FALSE);
    }

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass ) {

        HDfprintf(stdout, "%s(): failure_mssg = \"%s\".\n",
                  fcn_name, failure_mssg);
    }

    return;

} /* check_resize_entry() */


/*-------------------------------------------------------------------------
 * Function:	check_flush_protected_err()
 *
 * Purpose:	Verify that an attempt to flush the cache when it contains
 *		a protected entry will generate an error.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/24/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
check_flush_protected_err(void)
{
    const char * fcn_name = "check_flush_protected_err";
    H5C_t * cache_ptr = NULL;

    TESTING("flush cache with protected entry error");

    pass = TRUE;

    /* allocate a cache, protect an entry, and try to flush.  This
     * should fail.  Unprotect the entry and flush again -- should
     * succeed.
     */

    if ( pass ) {

        reset_entries();

        cache_ptr = setup_cache((size_t)(2 * 1024),
                                (size_t)(1 * 1024));

        protect_entry(cache_ptr, 0, 0);

        if ( H5C_flush_cache(NULL, -1, -1, cache_ptr, H5C__NO_FLAGS_SET)
             >= 0 ) {

            pass = FALSE;
            failure_mssg = "flush succeeded on cache with protected entry.\n";

        } else {

            unprotect_entry(cache_ptr, 0, 0, TRUE, H5C__NO_FLAGS_SET);

            if ( H5C_flush_cache(NULL, -1, -1, cache_ptr, H5C__NO_FLAGS_SET)
                 < 0 ) {

                pass = FALSE;
                failure_mssg = "flush failed after unprotect.\n";

            } else {

                takedown_cache(cache_ptr, FALSE, FALSE);
            }
        }
    }

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass ) {

        HDfprintf(stdout, "%s(): failure_mssg = \"%s\".\n",
                  fcn_name, failure_mssg);
    }

    return;

} /* check_flush_protected_err() */


/*-------------------------------------------------------------------------
 * Function:	check_destroy_pinned_err()
 *
 * Purpose:	Verify that an attempt to destroy the cache when it contains
 *		a pinned entry that can't be unpined during the flush destroy
 *		will generate an error.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              4/7/06
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
check_destroy_pinned_err(void)
{
    const char * fcn_name = "check_destroy_pinned_err()";
    H5C_t * cache_ptr = NULL;

    TESTING("destroy cache with permanently pinned entry error");

    pass = TRUE;

    /* allocate a cache, pin an entry, and try to flush destroy.  This
     * should fail.  Unpin the entry and flush destroy again -- should
     * succeed.
     */

    if ( pass ) {

        reset_entries();

        cache_ptr = setup_cache((size_t)(2 * 1024),
                                (size_t)(1 * 1024));

        protect_entry(cache_ptr, 0, 0);
	unprotect_entry(cache_ptr, 0, 0, FALSE, H5C__PIN_ENTRY_FLAG);

        if ( H5C_dest(NULL, -1, -1, cache_ptr) >= 0 ) {

            pass = FALSE;
            failure_mssg = "destroy succeeded on cache with pinned entry.\n";

        } else {

	    unpin_entry(cache_ptr, 0, 0);

            if ( H5C_dest(NULL, -1, -1, cache_ptr) < 0 ) {

                pass = FALSE;
                failure_mssg = "destroy failed after unpin.\n";

            }
        }
    }

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass ) {

        HDfprintf(stdout, "%s(): failure_mssg = \"%s\".\n",
                  fcn_name, failure_mssg);
    }

    return;

} /* check_destroy_pinned_err() */


/*-------------------------------------------------------------------------
 * Function:	check_destroy_protected_err()
 *
 * Purpose:	Verify that an attempt to destroy the cache when it contains
 *		a protected entry will generate an error.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/24/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
check_destroy_protected_err(void)
{
    const char * fcn_name = "check_destroy_protected_err";
    H5C_t * cache_ptr = NULL;

    TESTING("destroy cache with protected entry error");

    pass = TRUE;

    /* allocate a cache, protect an entry, and try to flush.  This
     * should fail.  Unprotect the entry and flush again -- should
     * succeed.
     */

    if ( pass ) {

        reset_entries();

        cache_ptr = setup_cache((size_t)(2 * 1024),
                                (size_t)(1 * 1024));

        protect_entry(cache_ptr, 0, 0);

        if ( H5C_dest(NULL, -1, -1, cache_ptr) >= 0 ) {

            pass = FALSE;
            failure_mssg = "destroy succeeded on cache with protected entry.\n";

        } else {

            unprotect_entry(cache_ptr, 0, 0, TRUE, H5C__NO_FLAGS_SET);

            if ( H5C_dest(NULL, -1, -1, cache_ptr) < 0 ) {

                pass = FALSE;
                failure_mssg = "destroy failed after unprotect.\n";

            }
        }
    }

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass ) {

        HDfprintf(stdout, "%s(): failure_mssg = \"%s\".\n",
                  fcn_name, failure_mssg);
    }

    return;

} /* check_destroy_protected_err() */


/*-------------------------------------------------------------------------
 * Function:	check_duplicate_insert_err()
 *
 * Purpose:	Verify that an attempt to insert and entry that is
 *		alread in the cache will generate an error.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/24/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
check_duplicate_insert_err(void)
{
    const char * fcn_name = "check_duplicate_insert_err";
    herr_t result;
    H5C_t * cache_ptr = NULL;
    test_entry_t * base_addr;
    test_entry_t * entry_ptr;

    TESTING("duplicate entry insertion error");

    pass = TRUE;

    /* allocate a cache, protect an entry, and then try to insert
     * the entry again.  This should fail.  Unprotect the entry and
     * destroy the cache -- should succeed.
     */

    if ( pass ) {

        reset_entries();

        cache_ptr = setup_cache((size_t)(2 * 1024),
                                (size_t)(1 * 1024));

        protect_entry(cache_ptr, 0, 0);

        if ( pass ) {

            base_addr = entries[0];
            entry_ptr = &(base_addr[0]);

            result = H5C_insert_entry(NULL, -1, -1, cache_ptr,
                                      &(types[0]), entry_ptr->addr,
                                      (void *)entry_ptr, H5C__NO_FLAGS_SET);

            if ( result >= 0 ) {

                pass = FALSE;
                failure_mssg = "insert of duplicate entry succeeded.\n";

            } else {

                unprotect_entry(cache_ptr, 0, 0, TRUE, H5C__NO_FLAGS_SET);

                takedown_cache(cache_ptr, FALSE, FALSE);
            }
        }
    }

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass ) {

        HDfprintf(stdout, "%s(): failure_mssg = \"%s\".\n",
                  fcn_name, failure_mssg);
    }

    return;

} /* check_duplicate_insert_err() */


/*-------------------------------------------------------------------------
 * Function:	check_rename_err()
 *
 * Purpose:	Verify that an attempt to rename an entry to the address
 *		of an existing entry will generate an error.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/24/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
check_rename_err(void)
{
    const char * fcn_name = "check_rename_err()";
    herr_t result;
    H5C_t * cache_ptr = NULL;
    test_entry_t * entry_0_0_ptr;
    test_entry_t * entry_0_1_ptr;
    test_entry_t * entry_1_0_ptr;

    TESTING("rename to existing entry errors");

    pass = TRUE;

    /* allocate a cache, and insert several entries.  Try to rename
     * entries to other entries resident in the cache.  This should
     * fail.  Destroy the cache -- should succeed.
     */

    if ( pass ) {

        reset_entries();

        cache_ptr = setup_cache((size_t)(2 * 1024),
                                (size_t)(1 * 1024));

        insert_entry(cache_ptr, 0, 0, TRUE, H5C__NO_FLAGS_SET);
        insert_entry(cache_ptr, 0, 1, TRUE, H5C__NO_FLAGS_SET);
        insert_entry(cache_ptr, 1, 0, TRUE, H5C__NO_FLAGS_SET);

        entry_0_0_ptr = &((entries[0])[0]);
        entry_0_1_ptr = &((entries[0])[1]);
        entry_1_0_ptr = &((entries[1])[0]);
    }

    if ( pass ) {

        result = H5C_rename_entry(cache_ptr, &(types[0]),
                                  entry_0_0_ptr->addr, entry_0_1_ptr->addr);

        if ( result >= 0 ) {

            pass = FALSE;
            failure_mssg = "rename to addr of same type succeeded.\n";
        }
    }

    if ( pass ) {

        result = H5C_rename_entry(cache_ptr, &(types[0]),
                                  entry_0_0_ptr->addr, entry_1_0_ptr->addr);

        if ( result >= 0 ) {

            pass = FALSE;
            failure_mssg = "rename to addr of different type succeeded.\n";
        }
    }

    if ( pass ) {

        takedown_cache(cache_ptr, FALSE, FALSE);
    }

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass ) {

        HDfprintf(stdout, "%s: failure_mssg = \"%s\".\n",
                  fcn_name, failure_mssg);
    }

    return;

} /* check_rename_err() */


/*-------------------------------------------------------------------------
 * Function:	check_double_pin_err()
 *
 * Purpose:	Verify that an attempt to pin an entry that is already
 *		pinned will generate an error.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              4/24/06
 *
 * Modifications:
 *
 *		None.
 *
 *-------------------------------------------------------------------------
 */

static void
check_double_pin_err(void)
{
    const char * fcn_name = "check_double_pin_err()";
    herr_t result;
    H5C_t * cache_ptr = NULL;
    test_entry_t * entry_ptr;

    TESTING("pin a pinned entry error");

    pass = TRUE;

    /* allocate a cache, protect an entry, unprotect it with the pin flag,
     * protect it again, and then try to unprotect it again with the pin
     * flag.  This should fail.  Unpin the entry and destroy the cache
     * -- should succeed.
     */

    if ( pass ) {

        reset_entries();

        cache_ptr = setup_cache((size_t)(2 * 1024),
                                (size_t)(1 * 1024));

        protect_entry(cache_ptr, 0, 0);

        unprotect_entry(cache_ptr, 0, 0, FALSE, H5C__PIN_ENTRY_FLAG);

        protect_entry(cache_ptr, 0, 0);

        entry_ptr = &((entries[0])[0]);
    }

    if ( pass ) {

        result = H5C_unprotect(NULL, -1, -1, cache_ptr, &(types[0]),
                               entry_ptr->addr, (void *)entry_ptr,
                               H5C__PIN_ENTRY_FLAG, 0);

        if ( result > 0 ) {

            pass = FALSE;
            failure_mssg =
                "attempt to pin a pinned entry succeeded.\n";

        } else {

	    unprotect_entry(cache_ptr, 0, 0, FALSE, H5C__UNPIN_ENTRY_FLAG);
	}
    }

    if ( pass ) {

        takedown_cache(cache_ptr, FALSE, FALSE);
    }

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass ) {

        HDfprintf(stdout, "%s: failure_mssg = \"%s\".\n",
                  fcn_name, failure_mssg);
    }

    return;

} /* check_double_pin_err() */


/*-------------------------------------------------------------------------
 * Function:	check_double_unpin_err()
 *
 * Purpose:	Verify that an attempt to unpin an unpinned entry will
 * 		generate an error.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              4/24/06
 *
 * Modifications:
 *
 *		None.
 *
 *-------------------------------------------------------------------------
 */

static void
check_double_unpin_err(void)
{
    const char * fcn_name = "check_double_unpin_err()";
    herr_t result;
    H5C_t * cache_ptr = NULL;
    test_entry_t * entry_ptr;

    TESTING("unpin an unpinned entry error");

    pass = TRUE;

    /* allocate a cache, protect an entry, unprotect it with the unpin flag.
     * -- This should fail.
     *
     * Try again with H5C_unpin_entry -- this should also fail.
     *
     * Destroy the cache -- should succeed.
     */

    if ( pass ) {

        reset_entries();

        cache_ptr = setup_cache((size_t)(2 * 1024),
                                (size_t)(1 * 1024));

        protect_entry(cache_ptr, 0, 0);

        entry_ptr = &((entries[0])[0]);
    }

    if ( pass ) {

        result = H5C_unprotect(NULL, -1, -1, cache_ptr, &(types[0]),
                               entry_ptr->addr, (void *)entry_ptr,
                               H5C__UNPIN_ENTRY_FLAG, 0);

        if ( result > 0 ) {

            pass = FALSE;
            failure_mssg =
                "attempt to unpin an unpinned entry succeeded 1.\n";

        } else {

	    unprotect_entry(cache_ptr, 0, 0, FALSE, H5C__NO_FLAGS_SET);
	}
    }

    if ( pass ) {

	result =  H5C_unpin_entry(cache_ptr, (void *)entry_ptr);

        if ( result > 0 ) {

            pass = FALSE;
            failure_mssg =
                "attempt to unpin an unpinned entry succeeded 2.\n";

        }
    }

    if ( pass ) {

        takedown_cache(cache_ptr, FALSE, FALSE);
    }

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass ) {

        HDfprintf(stdout, "%s: failure_mssg = \"%s\".\n",
                  fcn_name, failure_mssg);
    }

    return;

} /* check_double_unpin_err() */


/*-------------------------------------------------------------------------
 * Function:	check_pin_entry_errs()
 *
 * Purpose:	Verify that invalid calls to H5C_pin_protected_entry()
 * 		generate errors as expected.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              4/24/06
 *
 * Modifications:
 *
 *		None.
 *
 *-------------------------------------------------------------------------
 */

static void
check_pin_entry_errs(void)
{
    const char * fcn_name = "check_pin_entry_errs()";
    herr_t result;
    H5C_t * cache_ptr = NULL;
    test_entry_t * entry_ptr;

    TESTING("pin entry related errors");

    pass = TRUE;

    /* Allocate a cache, protect an entry, unprotect it with no flags,
     * and then call H5C_pin_protected_entry() to pin it -- This should fail.
     *
     * Protect the entry again, unprotect it with a pin flag, protect it
     * again, and then call H5C_pin_protected_entry() to pin it -- This
     * should fail also.
     *
     * Unprotect the entry with the unpin flag.
     *
     * Destroy the cache -- should succeed.
     */

    if ( pass ) {

        reset_entries();

        cache_ptr = setup_cache((size_t)(2 * 1024),
                                (size_t)(1 * 1024));

        protect_entry(cache_ptr, 0, 0);

	unprotect_entry(cache_ptr, 0, 0, FALSE, H5C__NO_FLAGS_SET);

        entry_ptr = &((entries[0])[0]);
    }

    if ( pass ) {

        result = H5C_pin_protected_entry(cache_ptr, (void *)entry_ptr);

        if ( result > 0 ) {

            pass = FALSE;
            failure_mssg =
                "attempt to pin an unprotected entry succeeded.\n";

        } else {

            protect_entry(cache_ptr, 0, 0);

	    unprotect_entry(cache_ptr, 0, 0, FALSE, H5C__PIN_ENTRY_FLAG);

            protect_entry(cache_ptr, 0, 0);
	}
    }

    if ( pass ) {

        result = H5C_pin_protected_entry(cache_ptr, (void *)entry_ptr);

        if ( result > 0 ) {

            pass = FALSE;
            failure_mssg =
                "attempt to pin a pinned, protected entry succeeded.\n";

        } else {

	    unprotect_entry(cache_ptr, 0, 0, FALSE, H5C__UNPIN_ENTRY_FLAG);

	}
    }

    if ( pass ) {

        takedown_cache(cache_ptr, FALSE, FALSE);
    }

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass ) {

        HDfprintf(stdout, "%s: failure_mssg = \"%s\".\n",
                  fcn_name, failure_mssg);
    }

    return;

} /* check_pin_entry_errs() */


/*-------------------------------------------------------------------------
 * Function:	check_double_protect_err()
 *
 * Purpose:	Verify that an attempt to protect an entry that is already
 *		protected will generate an error.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/24/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
check_double_protect_err(void)
{
    const char * fcn_name = "check_double_protect_err()";
    H5C_t * cache_ptr = NULL;
    test_entry_t * entry_ptr;
    H5C_cache_entry_t * cache_entry_ptr;

    TESTING("protect a protected entry error");

    pass = TRUE;

    /* allocate a cache, protect an entry, and then try to protect
     * the entry again.  This should fail.  Unprotect the entry and
     * destroy the cache -- should succeed.
     */

    if ( pass ) {

        reset_entries();

        cache_ptr = setup_cache((size_t)(2 * 1024),
                                (size_t)(1 * 1024));

        protect_entry(cache_ptr, 0, 0);

        entry_ptr = &((entries[0])[0]);
    }

    if ( pass ) {

        cache_entry_ptr = H5C_protect(NULL, -1, -1, cache_ptr, &(types[0]),
                                      entry_ptr->addr, NULL, NULL);

        if ( cache_entry_ptr != NULL ) {

            pass = FALSE;
            failure_mssg = "attempt to protect a protected entry succeeded.\n";
        }
    }

    if ( pass ) {

        unprotect_entry(cache_ptr, 0, 0, FALSE, H5C__NO_FLAGS_SET);
    }

    if ( pass ) {

        takedown_cache(cache_ptr, FALSE, FALSE);
    }

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass ) {

        HDfprintf(stdout, "%s: failure_mssg = \"%s\".\n",
                  fcn_name, failure_mssg);
    }

    return;

} /* check_double_protect_err() */


/*-------------------------------------------------------------------------
 * Function:	check_double_unprotect_err()
 *
 * Purpose:	Verify that an attempt to unprotect an entry that is already
 *		unprotected will generate an error.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/24/04
 *
 * Modifications:
 *
 *		JRM -- 6/17/05
 *		Modified function to use the new dirtied parameter in
 *		H5C_unprotect().
 *
 *		JRM -- 9/8/05
 *		Updated function for the new size change parameter in
 *		H5C_unprotect().  We don't use them for now.
 *
 *-------------------------------------------------------------------------
 */

static void
check_double_unprotect_err(void)
{
    const char * fcn_name = "check_double_unprotect_err()";
    herr_t result;
    H5C_t * cache_ptr = NULL;
    test_entry_t * entry_ptr;

    TESTING("unprotect an unprotected entry error");

    pass = TRUE;

    /* allocate a cache, protect an entry, unprotect it, and then try to
     * unprotect the entry again.  This should fail.  Destroy the cache
     * -- should succeed.
     */

    if ( pass ) {

        reset_entries();

        cache_ptr = setup_cache((size_t)(2 * 1024),
                                (size_t)(1 * 1024));

        protect_entry(cache_ptr, 0, 0);

        unprotect_entry(cache_ptr, 0, 0, FALSE, H5C__NO_FLAGS_SET);

        entry_ptr = &((entries[0])[0]);
    }

    if ( pass ) {

        result = H5C_unprotect(NULL, -1, -1, cache_ptr, &(types[0]),
                               entry_ptr->addr, (void *)entry_ptr,
                               H5C__NO_FLAGS_SET, 0);

        if ( result > 0 ) {

            pass = FALSE;
            failure_mssg =
                "attempt to unprotect an unprotected entry succeeded 1.\n";
        }
    }

    if ( pass ) {

        takedown_cache(cache_ptr, FALSE, FALSE);
    }

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass ) {

        HDfprintf(stdout, "%s: failure_mssg = \"%s\".\n",
                  fcn_name, failure_mssg);
    }

    return;

} /* check_double_unprotect_err() */


/*-------------------------------------------------------------------------
 * Function:	check_mark_entry_dirty_errs()
 *
 * Purpose:	Verify that:
 *
 * 		1) a call to H5C_mark_pinned_entry_dirty with an upinned
 * 		   entry as the target will generate an error.
 *
 * 		2) a call to H5C_mark_pinned_entry_dirty with a protected
 * 		   entry as the target will generate an error.
 *
 *		3) a call to H5C_mark_pinned_or_protected_entry_dirty with
 *		   and unpinned and unprotected entry will generate an
 *		   error.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              5/17/06
 *
 * Modifications:
 *
 *		None.
 *
 *-------------------------------------------------------------------------
 */

static void
check_mark_entry_dirty_errs(void)
{
    const char * fcn_name = "check_mark_entry_dirty_errs()";
    herr_t result;
    H5C_t * cache_ptr = NULL;
    test_entry_t * entry_ptr;

    TESTING("mark entry dirty related errors");

    pass = TRUE;

    /* allocate a cache, protect an entry, and then attempt to mark it dirty
     * with the H5C_mark_pinned_entry_dirty() call -- This should fail.
     *
     * Then unprotect the entry without pinning it, and try to mark it dirty
     * again -- this should fail too.
     *
     * Try it again using H5C_mark_pinned_or_protected_entry_dirty -- this
     * should fail as well.
     *
     * Destroy the cache -- should succeed.
     */

    if ( pass ) {

        reset_entries();

        cache_ptr = setup_cache((size_t)(2 * 1024),
                                (size_t)(1 * 1024));

        protect_entry(cache_ptr, 0, 0);

	unprotect_entry(cache_ptr, 0, 0, FALSE, H5C__PIN_ENTRY_FLAG);

        protect_entry(cache_ptr, 0, 0);

        entry_ptr = &((entries[0])[0]);
    }

    if ( pass ) {

	result = H5C_mark_pinned_entry_dirty(cache_ptr, (void *)entry_ptr,
			                     FALSE, 0);

        if ( result > 0 ) {

            pass = FALSE;
            failure_mssg =
                    "attempt dirty a pinned and protected entry succeeded.\n";

        } else {

	    unprotect_entry(cache_ptr, 0, 0, FALSE, H5C__UNPIN_ENTRY_FLAG);
	}
    }

    if ( pass ) {

	result = H5C_mark_pinned_entry_dirty(cache_ptr, (void *)entry_ptr,
			                     FALSE, 0);


        if ( result > 0 ) {

            pass = FALSE;
            failure_mssg =
            "attempt to dirty a unpinned and unprotected entry succeeded 1.\n";
        }
    }

    if ( pass ) {

	result = H5C_mark_pinned_or_protected_entry_dirty(cache_ptr,
			                                  (void *)entry_ptr);


        if ( result > 0 ) {

            pass = FALSE;
            failure_mssg =
            "attempt to dirty a unpinned and unprotected entry succeeded 2.\n";
        }
    }

    if ( pass ) {

        takedown_cache(cache_ptr, FALSE, FALSE);
    }

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass ) {

        HDfprintf(stdout, "%s: failure_mssg = \"%s\".\n",
                  fcn_name, failure_mssg);
    }

    return;

} /* check_mark_entry_dirty_errs() */


/*-------------------------------------------------------------------------
 * Function:	check_expunge_entry_errs()
 *
 * Purpose:	Verify that invalid calls to H5C_expunge_entry()
 * 		generate errors as expected.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              7/6/06
 *
 * Modifications:
 *
 *		None.
 *
 *-------------------------------------------------------------------------
 */

static void
check_expunge_entry_errs(void)
{
    const char * fcn_name = "check_expunge_entry_errs()";
    herr_t result;
    H5C_t * cache_ptr = NULL;
    test_entry_t * entry_ptr;

    TESTING("expunge entry related errors");

    pass = TRUE;

    /* Allocate a cache, protect an entry, and then call H5C_expunge_entry()
     * to expunge it -- this should fail
     *
     * Unprotect the the entry with the pinned flag, and then call
     * H5C_expunge_entry() again.  This should fail too.
     *
     * Finally, unpin the entry and call H5C_expunge_entry() yet again.
     * This should succeed.
     *
     * Destroy the cache -- should succeed.
     */

    if ( pass ) {

        reset_entries();

        cache_ptr = setup_cache((size_t)(2 * 1024),
                                (size_t)(1 * 1024));

        entry_ptr = &((entries[0])[0]);

        protect_entry(cache_ptr, 0, 0);

    }

    if ( pass ) {

	result = H5C_expunge_entry(NULL, -1, -1, cache_ptr, 
                                   &(types[0]), entry_ptr->addr);

        if ( result > 0 ) {

            pass = FALSE;
            failure_mssg =
                "attempt to expunge a protected entry succeeded.\n";

        } else {

	    unprotect_entry(cache_ptr, 0, 0, FALSE, H5C__PIN_ENTRY_FLAG);

	}
    }

    if ( pass ) {

	result = H5C_expunge_entry(NULL, -1, -1, cache_ptr, 
                                   &(types[0]), entry_ptr->addr);

        if ( result > 0 ) {

            pass = FALSE;
            failure_mssg =
                "attempt to expunge a pinned entry succeeded.\n";

        } else {

	    unpin_entry(cache_ptr, 0, 0);

	}
    }

    if ( pass ) {

	result = H5C_expunge_entry(NULL, -1, -1, cache_ptr, 
                                   &(types[0]), entry_ptr->addr);

        if ( result < 0 ) {

            pass = FALSE;
            failure_mssg =
              "attempt to expunge an unpinned and unprotected entry failed.\n";

	}
    }


    if ( pass ) {

        takedown_cache(cache_ptr, FALSE, FALSE);
    }

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass ) {

        HDfprintf(stdout, "%s: failure_mssg = \"%s\".\n",
                  fcn_name, failure_mssg);
    }

    return;

} /* check_expunge_entry_errs() */


/*-------------------------------------------------------------------------
 * Function:	check_resize_entry_errs()
 *
 * Purpose:	Verify that invalid calls to H5C_resize_pinned_entry()
 * 		generates errors as expected.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              7/7/06
 *
 * Modifications:
 *
 *		None.
 *
 *-------------------------------------------------------------------------
 */

static void
check_resize_entry_errs(void)
{
    const char * fcn_name = "check_resize_entry_errs()";
    herr_t result;
    H5C_t * cache_ptr = NULL;
    test_entry_t * entry_ptr;

    TESTING("resize entry related errors");

    pass = TRUE;

    /* Allocate a cache, protect an entry, and then call 
     * H5C_resize_pinned_entry() to resize it -- this should fail.
     *
     * Unprotect the the entry with the pinned flag, and then call
     * H5C_resize_pinned_entry() again with new size of zero.  
     * This should fail too.
     *
     * Finally, unpin the entry and destroy the cache.
     * This should succeed.
     */

    if ( pass ) {

        reset_entries();

        cache_ptr = setup_cache((size_t)(2 * 1024),
                                (size_t)(1 * 1024));

        entry_ptr = &((entries[0])[0]);

        protect_entry(cache_ptr, 0, 0);

    }

    if ( pass ) {

	result = H5C_resize_pinned_entry(cache_ptr, (void *)entry_ptr, 1);

        if ( result > 0 ) {

            pass = FALSE;
            failure_mssg =
            "Call to H5C_resize_pinned_entry on a protected entry succeeded.\n";

        } else {

	    unprotect_entry(cache_ptr, 0, 0, FALSE, H5C__PIN_ENTRY_FLAG);

	}
    }

    if ( pass ) {

	result = H5C_resize_pinned_entry(cache_ptr, (void *)entry_ptr, 0);

        if ( result > 0 ) {

            pass = FALSE;
            failure_mssg =
                 "Call to H5C_resize_pinned_entry with 0 new size succeeded.\n";

        } else {

	    unpin_entry(cache_ptr, 0, 0);

	}
    }

    if ( pass ) {

        takedown_cache(cache_ptr, FALSE, FALSE);
    }

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass ) {

        HDfprintf(stdout, "%s: failure_mssg = \"%s\".\n",
                  fcn_name, failure_mssg);
    }

    return;

} /* check_resize_entry_errs() */


/*-------------------------------------------------------------------------
 * Function:	check_auto_cache_resize()
 *
 * Purpose:	Exercise the automatic cache resizing functionality.
 *		The objective is to operate the auto-resize code in
 *		all possible modes.  Unfortunately, there are quite
 *		a few of them.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              10/29/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

hbool_t rpt_fcn_called = FALSE;
enum H5C_resize_status rpt_status;

void test_rpt_fcn(UNUSED H5C_t * cache_ptr,
                  UNUSED int32_t version,
                  UNUSED double hit_rate,
                  UNUSED enum H5C_resize_status status,
                  UNUSED size_t old_max_cache_size,
                  UNUSED size_t new_max_cache_size,
                  UNUSED size_t old_min_clean_size,
                  UNUSED size_t new_min_clean_size);

void test_rpt_fcn(UNUSED H5C_t * cache_ptr,
                  UNUSED int32_t version,
                  UNUSED double hit_rate,
                  UNUSED enum H5C_resize_status status,
                  UNUSED size_t old_max_cache_size,
                  UNUSED size_t new_max_cache_size,
                  UNUSED size_t old_min_clean_size,
                  UNUSED size_t new_min_clean_size)
{
    rpt_fcn_called = TRUE;
    rpt_status = status;
}

static void
check_auto_cache_resize(void)
{
    const char * fcn_name = "check_auto_cache_resize()";
    hbool_t show_progress = FALSE;
    herr_t result;
    int32_t i;
    int32_t checkpoint = 0;
    H5C_t * cache_ptr = NULL;
    H5C_auto_size_ctl_t auto_size_ctl =
    {
        /* int32_t     version                = */ H5C__CURR_AUTO_SIZE_CTL_VER,
        /* H5C_auto_resize_report_fcn rpt_fcn = */ test_rpt_fcn,

        /* hbool_t     set_initial_size       = */ TRUE,
        /* size_t      initial_size           = */ (512 * 1024),

        /* double      min_clean_fraction     = */ 0.5,

        /* size_t      max_size               = */ (14 * 1024 * 1024),
        /* size_t      min_size               = */ (512 * 1024),

        /* int64_t     epoch_length           = */ 1000,


        /* enum H5C_cache_incr_mode incr_mode = */ H5C_incr__threshold,

        /* double     lower_hr_threshold      = */ 0.75,

        /* double      increment              = */ 2.0,

        /* hbool_t     apply_max_increment    = */ TRUE,
        /* size_t      max_increment          = */ (4 * 1024 * 1024),


        /* enum H5C_cache_decr_mode decr_mode = */ H5C_decr__threshold,

        /* double      upper_hr_threshold     = */ 0.995,

        /* double      decrement              = */ 0.1,

        /* hbool_t     apply_max_decrement    = */ TRUE,
        /* size_t      max_decrement          = */ (1 * 1024 * 1024),

        /* int32_t     epochs_before_eviction = */ 3,

        /* hbool_t     apply_empty_reserve    = */ TRUE,
        /* double      empty_reserve          = */ 0.05
    };

    TESTING("automatic cache resizing");

    pass = TRUE;

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* allocate a cache, enable automatic cache resizing, and then force
     * the cache through all its operational modes.  Verify that all
     * performs as expected.
     */

    if ( pass ) {

        reset_entries();

        cache_ptr = setup_cache((size_t)(2 * 1024),
                                (size_t)(1 * 1024));
    }

    if ( pass ) {

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 1.\n";
        }
    }

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (512 * 1024) ) ||
             ( cache_ptr->min_clean_size != (256 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after initialization.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate with cache not full -- should result in not
     * full status.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, PICO_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, PICO_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != not_full ) ||
             ( cache_ptr->max_cache_size != (512 * 1024) ) ||
             ( cache_ptr->min_clean_size != (256 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 1.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate with cache full -- should result in increase
     * of cache size from .5 to 1 meg.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != increase ) ||
             ( cache_ptr->max_cache_size != (1 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (512 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 2.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate with cache not full -- should result in not
     * full status.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, PICO_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, PICO_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != not_full ) ||
             ( cache_ptr->max_cache_size != (1 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (512 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 3.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate with cache full again -- should result in increase
     * of cache size from 1 to 2 meg.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != increase ) ||
             ( cache_ptr->max_cache_size != (2 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (1 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 4.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate with cache full again -- should result in increase
     * of cache size from 2 to 4 meg.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != increase ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 5.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate with cache full again -- should result in increase
     * of cache size from 4 to 8 meg.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != increase ) ||
             ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 6.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate with cache full again -- should result in increase
     * of cache size from 8 to 12 meg.  Note that max increase reduced the
     * size of the increase.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != increase ) ||
             ( cache_ptr->max_cache_size != (12 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (6 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 7.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate with cache full again -- should result in increase
     * of cache size from 12 to 14 meg.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != increase ) ||
             ( cache_ptr->max_cache_size != (14 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (7 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 8.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate with cache full and at maximum size -- should
     * in no change in size and a result of at_max_size.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != at_max_size ) ||
             ( cache_ptr->max_cache_size != (14 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (7 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 9.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate with cache full and at maximum size -- should
     * result in a decrease from 14 to 13 Meg -- note that max decrease
     * reduced the size of the reduction
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (13 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (6 * 1024 * 1024 + 512 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 10.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* the current cache configuration is unconvenient for testing cache
     * size reduction, so lets change it some something easier to work
     * with.
     */

    if ( pass ) {

        auto_size_ctl.version                = H5C__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 4 * 1000 * 1000 + 10;

        auto_size_ctl.min_clean_fraction     = 0.1;

        auto_size_ctl.max_size               = 8 * 1000 * 1000;
        auto_size_ctl.min_size               = 500 * 1000;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (4 * 1000 * 1000);


        auto_size_ctl.decr_mode              = H5C_decr__threshold;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = TRUE;
        auto_size_ctl.max_decrement          = (1 * 1000 * 1000);

        auto_size_ctl.epochs_before_eviction = 3;

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 2.\n";
        }
    }

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (4 * 1000 * 1000 + 10) ) ||
             ( cache_ptr->min_clean_size != (400 * 1000 + 1) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after set resize re-config 1.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate  -- should result in a decrease from ~4 to ~3
     * M -- note that max decrease reduces the size of the reduction
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (3 * 1000 * 1000 + 10) ) ||
             ( cache_ptr->min_clean_size != (300 * 1000 + 1) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 11.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate again  -- should result in a decrease from ~3
     * to ~2 M -- again note that max decrease reduces the size of the
     * reduction.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (2 * 1000 * 1000 + 10) ) ||
             ( cache_ptr->min_clean_size != (200 * 1000 + 1) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 12.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate again  -- should result in a decrease from ~2
     * to ~1 M -- again note that max decrease reduces the size of the
     * reduction, but only by five bites.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (1 * 1000 * 1000 + 10) ) ||
             ( cache_ptr->min_clean_size != (100 * 1000 + 1) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 13.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate again  -- should result in a decrease from ~1
     * to ~0.5 M -- max decrease is no longer a factor.  New size is five
     * bytes above the minimum.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (500 * 1000 + 5) ) ||
             ( cache_ptr->min_clean_size != (50 * 1000) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 14.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate again  -- should result in a decrease of five
     * bytes to the minimum cache size.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (500 * 1000) ) ||
             ( cache_ptr->min_clean_size != (50 * 1000) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 15.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate again -- Already at minimum size so no change in
     * cache size and result should be at_min_size.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != at_min_size ) ||
             ( cache_ptr->max_cache_size != (500 * 1000) ) ||
             ( cache_ptr->min_clean_size != (50 * 1000) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 16.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force in range hit rate  -- should be no change in cache size,
     * and result should be in_spec.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 900 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i + 1000);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i + 1000,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (500 * 1000) ) ||
             ( cache_ptr->min_clean_size != (50 * 1000) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 17.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate with cache full -- should
     * increase cache size from .5 to 1 M.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != increase ) ||
             ( cache_ptr->max_cache_size != (1 * 1000 * 1000) ) ||
             ( cache_ptr->min_clean_size != (100 * 1000) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 18.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate -- should result in a decrease to the
     * minimum cache size.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (500 * 1000) ) ||
             ( cache_ptr->min_clean_size != (50 * 1000) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 19.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /******************************************************************
     * now do some tests with the maximum increase and decrease sizes
     * disabled.
     ******************************************************************/

    if ( pass ) {

        auto_size_ctl.version                = H5C__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 16 * 1024 * 1024;
        auto_size_ctl.min_size               = 1 * 1024 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 4.0;

        auto_size_ctl.apply_max_increment    = FALSE;
        auto_size_ctl.max_increment          = (4 * 1024 * 1024);


        auto_size_ctl.decr_mode              = H5C_decr__threshold;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 0.25;

        auto_size_ctl.apply_max_decrement    = FALSE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 3;

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 3.\n";
        }
    }

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after set resize re-config 2.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate -- should result in a decrease to the
     * minimum cache size.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (1 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (512 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 20.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate with cache full -- should increase cache size
     * from 1 to 4 Meg.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != increase ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 21.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate again with cache full -- should increase cache
     * size from 4 to 16 Meg.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != increase ) ||
             ( cache_ptr->max_cache_size != (16 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != ( 8 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 22.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate -- should result in a decrease cache size from
     * 16 to 4 Meg.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 23.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /******************************************************************
     * We have tested the threshold increment and decrement modes.
     * must now test the ageout decrement mode.
     *
     * Reconfigure the cache for this testing.
     ******************************************************************/

    if ( pass ) {

        auto_size_ctl.version                = H5C__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 8 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 8 * 1024 * 1024;
        auto_size_ctl.min_size               = 512 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (4 * 1024 * 1024);


        auto_size_ctl.decr_mode              = H5C_decr__age_out;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = FALSE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 3;

        auto_size_ctl.apply_empty_reserve    = FALSE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 4.\n";
        }
    }

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after set resize re-config 3.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);


    /* fill the cache with 1024 byte entries -- nothing should happen
     * for three epochs while the markers are inserted into the cache
     *
     * Note that hit rate will be zero, so the cache will attempt to
     * increase its size. Since we are already at max size, it will
     * not be able to.
     */
    if ( pass ) { /* first epoch */

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != at_max_size ) ||
             ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 24.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) { /* second epoch */

        rpt_fcn_called = FALSE;
        i = 1000;
        while ( ( pass ) && ( i < 2000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != at_max_size ) ||
             ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 25.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) { /* third epoch */

        rpt_fcn_called = FALSE;
        i = 2000;
        while ( ( pass ) && ( i < 3000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != at_max_size ) ||
             ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 26.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* fourth epoch -- If the hit rate were above the lower threshold,
     * we would see cache size reduction now.  However, nothing will
     * happen until we get the hit rate above the lower threshold.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 3000;
        while ( ( pass ) && ( i < 4000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != at_max_size ) ||
             ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 27.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* fifth epoch -- force the hit rate to 100%.  We should see cache size
     * reduction now.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 3000;
        while ( ( pass ) && ( i < 4000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (2001 * 1024) ) ||
             ( cache_ptr->min_clean_size != (int)(2001 * 512) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 28.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* sixth epoch -- force the hit rate to 100% again.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 3000;
        while ( ( pass ) && ( i < 4000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (1001 * 1024) ) ||
             ( cache_ptr->min_clean_size != (int)(1001 * 512) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 29.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* seventh epoch -- force the hit rate to 100% again.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 3000;
        while ( ( pass ) && ( i < 4000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (int)(1000 * 512) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 30.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* eigth epoch -- force the hit rate to 100% again -- should be steady
     * state.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 3000;
        while ( ( pass ) && ( i < 4000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (int)(1000 * 512) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 31.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "*check point %d\n", checkpoint++);

    /* now just bang on one entry -- after three epochs, this should
     * get all entries other than the one evicted, and the cache size
     * should be decreased to the minimum.
     */
    if ( pass ) { /* ninth epoch */

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (int)(1000 * 512) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 32.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) { /* tenth epoch */

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (int)(1000 * 512) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 33.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) { /* eleventh epoch -- cache size reduction */

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (512 * 1024) ) ||
             ( cache_ptr->min_clean_size != (256 * 1024) ) ||
             ( cache_ptr->index_len != 2 ) ||
             ( cache_ptr->index_size !=
               MONSTER_ENTRY_SIZE + MEDIUM_ENTRY_SIZE ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 34.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) { /* twelth epoch -- at minimum size so no more ageouts */

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != at_min_size ) ||
             ( cache_ptr->max_cache_size != (512 * 1024) ) ||
             ( cache_ptr->min_clean_size != (256 * 1024) ) ||
             ( cache_ptr->index_len != 2 ) ||
             ( cache_ptr->index_size !=
               MONSTER_ENTRY_SIZE + MEDIUM_ENTRY_SIZE ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 35.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);


    /* repeat the above test, but with max_decrement enabled to see
     * if that features works as it should.  Note that this will change
     * the structure of the test a bit.
     */

    if ( pass ) {

        auto_size_ctl.version                = H5C__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 8 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 8 * 1024 * 1024;
        auto_size_ctl.min_size               = 512 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (4 * 1024 * 1024);


        auto_size_ctl.decr_mode              = H5C_decr__age_out;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = TRUE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 3;

        auto_size_ctl.apply_empty_reserve    = FALSE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 5.\n";
        }
    }

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after set resize re-config 4.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);


    /* fill the cache with 1024 byte entries -- nothing should happen
     * for three epochs while the markers are inserted into the cache
     *
     * Note that hit rate will be zero, so the cache will attempt to
     * increase its size. Since we are already at max size, it will
     * not be able to.
     */
    if ( pass ) { /* first epoch */

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != at_max_size ) ||
             ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 36.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) { /* second epoch */

        rpt_fcn_called = FALSE;
        i = 1000;
        while ( ( pass ) && ( i < 2000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != at_max_size ) ||
             ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 37.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) { /* third epoch */

        rpt_fcn_called = FALSE;
        i = 2000;
        while ( ( pass ) && ( i < 3000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != at_max_size ) ||
             ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 38.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* fourth epoch -- If the hit rate were above the lower threshold,
     * we would see cache size reduction now.  However, nothing will
     * happen until we get the hit rate above the lower threshold.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 3000;
        while ( ( pass ) && ( i < 4000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != at_max_size ) ||
             ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 39.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* fifth epoch -- force the hit rate to 100%.  We should see cache size
     * reduction now.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 3000;
        while ( ( pass ) && ( i < 4000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (7 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (7 * 512 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 40.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* sixth epoch -- force the hit rate to 100% again.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 2000;
        while ( ( pass ) && ( i < 3000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (6 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (6 * 512 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 41.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* seventh epoch -- keep hit rate at 100%, and keep 2K entries active.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 3000;
        while ( ( pass ) && ( i < 4000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (5 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (5 * 512 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 42.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* eigth epoch -- still 100% hit rate
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 2000;
        while ( ( pass ) && ( i < 3000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 512 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 43.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* ninth epoch --hit rate at 100%.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 3000;
        while ( ( pass ) && ( i < 4000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (3 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (3 * 512 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 44.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* tenth epoch -- still 100% hit rate
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 2000;
        while ( ( pass ) && ( i < 3000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (2 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 512 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 45.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* eleventh epoch -- hit rate at 100% -- starting to stableize
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 3000;
        while ( ( pass ) && ( i < 4000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (2000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (int)(2000 * 512) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 46.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* twelth epoch -- force the hit rate to 100% again -- should be steady
     * state.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 2000;
        while ( ( pass ) && ( i < 3000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (2000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (int)(2000 * 512) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 47.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* now just bang on one entry -- after three epochs, this should
     * get all entries other than the one evicted, and the cache size
     * should be decreased to the minimum.
     */
    if ( pass ) { /* thirteenth epoch */

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (2000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (int)(2000 * 512) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 48.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) { /* fourteenth epoch */

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size !=
               (1001 * 1024 + MONSTER_ENTRY_SIZE) ) ||
             ( cache_ptr->min_clean_size !=
               (1001 * 512 + MONSTER_ENTRY_SIZE / 2) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 49.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) { /* fifteenth epoch -- cache size reduction */

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (512 * 1024) ) ||
             ( cache_ptr->min_clean_size != (256 * 1024) ) ||
             ( cache_ptr->index_len != 2 ) ||
             ( cache_ptr->index_size !=
               MONSTER_ENTRY_SIZE + MEDIUM_ENTRY_SIZE ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 50.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) { /* sixteenth epoch -- at minimum size so no more ageouts */

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != at_min_size ) ||
             ( cache_ptr->max_cache_size != (512 * 1024) ) ||
             ( cache_ptr->min_clean_size != (256 * 1024) ) ||
             ( cache_ptr->index_len != 2 ) ||
             ( cache_ptr->index_size !=
               MONSTER_ENTRY_SIZE + MEDIUM_ENTRY_SIZE ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 51.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);


    /* repeat the test yet again, this time with empty reserve enabled.
     * Again, some structural changes in the test are necessary.
     */

    if ( pass ) {

        auto_size_ctl.version                = H5C__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 8 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 8 * 1024 * 1024;
        auto_size_ctl.min_size               = 512 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (4 * 1024 * 1024);


        auto_size_ctl.decr_mode              = H5C_decr__age_out;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = FALSE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 3;

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 0.5; /* for ease of testing */

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 6.\n";
        }
    }

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after set resize re-config 5.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);


    /* fill the cache with 1024 byte entries -- nothing should happen
     * for three epochs while the markers are inserted into the cache
     *
     * Note that hit rate will be zero, so the cache will attempt to
     * increase its size. Since we are already at max size, it will
     * not be able to.
     */
    if ( pass ) { /* first epoch */

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != at_max_size ) ||
             ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 52.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) { /* second epoch */

        rpt_fcn_called = FALSE;
        i = 1000;
        while ( ( pass ) && ( i < 2000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != at_max_size ) ||
             ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 53.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) { /* third epoch */

        rpt_fcn_called = FALSE;
        i = 2000;
        while ( ( pass ) && ( i < 3000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != at_max_size ) ||
             ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 54.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* fourth epoch -- If the hit rate were above the lower threshold,
     * we would see cache size reduction now.  However, nothing will
     * happen until we get the hit rate above the lower threshold.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 3000;
        while ( ( pass ) && ( i < 4000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != at_max_size ) ||
             ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 55.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* fifth epoch -- force the hit rate to 100%.  We should see cache size
     * reduction now.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 3000;
        while ( ( pass ) && ( i < 4000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (4002 * 1024) ) ||
             ( cache_ptr->min_clean_size != (int)(4002 * 512) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 56.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* sixth epoch -- force the hit rate to 100% again.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 3000;
        while ( ( pass ) && ( i < 4000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (2002 * 1024) ) ||
             ( cache_ptr->min_clean_size != (int)(2002 * 512) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 57.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* seventh epoch -- force the hit rate to 100% again.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 3000;
        while ( ( pass ) && ( i < 4000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (2000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (int)(2000 * 512) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 58.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* eigth epoch -- force the hit rate to 100% again -- should be steady
     * state.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 3000;
        while ( ( pass ) && ( i < 4000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (2000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (int)(2000 * 512) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 59.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* now just bang on one entry -- after three epochs, this should
     * get all entries other than the one evicted, and the cache size
     * should be decreased to the minimum.
     */
    if ( pass ) { /* ninth epoch */

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (2000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (int)(2000 * 512) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 60.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) { /* tenth epoch */

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (2000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2000 * 512) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 61.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) { /* eleventh epoch -- cache size reduction */

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (512 * 1024) ) ||
             ( cache_ptr->min_clean_size != (256 * 1024) ) ||
             ( cache_ptr->index_len != 2 ) ||
             ( cache_ptr->index_size !=
               MONSTER_ENTRY_SIZE + MEDIUM_ENTRY_SIZE ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 62.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) { /* twelth epoch -- at minimum size so no more ageouts */

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != at_min_size ) ||
             ( cache_ptr->max_cache_size != (512 * 1024) ) ||
             ( cache_ptr->min_clean_size != (256 * 1024) ) ||
             ( cache_ptr->index_len != 2 ) ||
             ( cache_ptr->index_size !=
               MONSTER_ENTRY_SIZE + MEDIUM_ENTRY_SIZE ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 63.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);


    /* Repeat the test again, this time using the age out with threshold
     * mode.  To simplify the testing, set epochs to eviction to 1.
     *
     * Again, there are some minor structural changes in the test.
     */

    if ( pass ) {

        auto_size_ctl.version                = H5C__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 8 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 8 * 1024 * 1024;
        auto_size_ctl.min_size               = 512 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C_incr__off;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (4 * 1024 * 1024);


        auto_size_ctl.decr_mode              = H5C_decr__age_out_with_threshold;

        auto_size_ctl.upper_hr_threshold     = 0.999; /* for ease of testing */

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = FALSE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 1; /* for ease of testing */

        auto_size_ctl.apply_empty_reserve    = FALSE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 7.\n";
        }
    }

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after set resize re-config 6.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);


    /* fill the cache with 4K byte entries -- increment mode is off,
     * so cache size reduction should kick in as soon as we get the
     * hit rate above .999.
     */
    if ( pass ) { /* first epoch -- hit rate 0 */

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, LARGE_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, LARGE_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 64.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) { /* second epoch -- hit rate 0 */

        rpt_fcn_called = FALSE;
        i = 1000;
        while ( ( pass ) && ( i < 2000 ) )
        {
            protect_entry(cache_ptr, LARGE_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, LARGE_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 65.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) { /* third epoch -- hit rate 1.0 -- should see decrease */

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, LARGE_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, LARGE_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (1001 * LARGE_ENTRY_SIZE) ) ||
             ( cache_ptr->min_clean_size != (1001 * LARGE_ENTRY_SIZE / 2) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 66.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* fourth epoch -- load up the cache again -- hit rate 0 */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (1001 * LARGE_ENTRY_SIZE) ) ||
             ( cache_ptr->min_clean_size != (1001 * LARGE_ENTRY_SIZE / 2) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 67.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* fifth epoch -- still loading up the cache -- hit rate 0 */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 1000;
        while ( ( pass ) && ( i < 2000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (1001 * LARGE_ENTRY_SIZE) ) ||
             ( cache_ptr->min_clean_size != (1001 * LARGE_ENTRY_SIZE / 2) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 68.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* sixth epoch -- force hit rate to .998 -- should be no reduction */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 1002;
        while ( ( pass ) && ( i < 2002 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (1001 * LARGE_ENTRY_SIZE) ) ||
             ( cache_ptr->min_clean_size != (1001 * LARGE_ENTRY_SIZE / 2) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 69.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* seventh epoch -- force hit rate to .999 -- should see reduction
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 1003;
        while ( ( pass ) && ( i < 2003 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (1000 * MEDIUM_ENTRY_SIZE) ) ||
             ( cache_ptr->min_clean_size != (1000 * MEDIUM_ENTRY_SIZE / 2) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 70.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);


    /* We have now tested all the major ageout modes individually.
     * Lets try them all together to look for unexpected interactions
     * and/or bugs.
     */

    if ( pass ) {

        auto_size_ctl.version                = H5C__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 8 * 1000 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 8 * 1000 * 1024;
        auto_size_ctl.min_size               = 512 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (4 * 1024 * 1024);


        auto_size_ctl.decr_mode              = H5C_decr__age_out_with_threshold;

        auto_size_ctl.upper_hr_threshold     = 0.999; /* for ease of testing */

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = TRUE;
        auto_size_ctl.max_decrement          = (1 * 1000 * 1024);

        auto_size_ctl.epochs_before_eviction = 1; /* for ease of testing */

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 0.5; /* for ease of testing */

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 8.\n";
        }
    }

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (8 * 1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1000 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after set resize re-config 7.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* fill the cache with 4K byte entries -- increment mode is threshold,
     * so the decrease code will not be executed until the hit rate exceeds
     * .75.
     */
    if ( pass ) { /* first epoch -- hit rate 0 */

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, LARGE_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, LARGE_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != at_max_size ) ||
             ( cache_ptr->max_cache_size != (8 * 1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1000 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 71.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) { /* second epoch -- hit rate 0 */

        rpt_fcn_called = FALSE;
        i = 1000;
        while ( ( pass ) && ( i < 2000 ) )
        {
            protect_entry(cache_ptr, LARGE_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, LARGE_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != at_max_size ) ||
             ( cache_ptr->max_cache_size != (8 * 1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1000 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 72.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* third epoch -- force the hit rate to 1.0.  Should be no change
     * in the cache size due to the combination of the empty reserve
     * and the max decrease.  Max decrease will limit the evictions
     * in any one epoch, and the empty reserve will not permit cache
     * size reduction unless the specified empty reserve is maintained.
     *
     * In this epoch, all we should see is a reduction in the index size.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, LARGE_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, LARGE_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (8 * 1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1000 * 1024) ) ||
             ( cache_ptr->index_size != (7 * 1000 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 73.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* fourth epoch -- hit rate still 1.0.  Index size should decrease,
     * but otherwise no change expected.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, LARGE_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, LARGE_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (8 * 1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1000 * 1024) ) ||
             ( cache_ptr->index_size != (6 * 1000 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 74.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* fifth epoch -- hit rate still 1.0.  Index size should decrease,
     * but otherwise no change expected.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, LARGE_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, LARGE_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (8 * 1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1000 * 1024) ) ||
             ( cache_ptr->index_size != (5 * 1000 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 75.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* sixth epoch -- hit rate still 1.0.  Index size should decrease,
     * but otherwise no change expected.  Note that the cache size is
     * now just on the edge of meeting the clean reserve.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, LARGE_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, LARGE_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (8 * 1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1000 * 1024) ) ||
             ( cache_ptr->index_size != (4 * 1000 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 76.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* seventh epoch -- hit rate still 1.0.  No change in index size expected.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, LARGE_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, LARGE_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (8 * 1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1000 * 1024) ) ||
             ( cache_ptr->index_size != (4 * 1000 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 77.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* eighth epoch -- start loading 1 KB entries.  Hit rate 0 so
     * decrease code shouldn't be called.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != at_max_size ) ||
             ( cache_ptr->max_cache_size != (8 * 1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1000 * 1024) ) ||
             ( cache_ptr->index_size != (5 * 1000 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 78.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* ninth epoch -- access the 1 KB entries again, driving the hit rate
     * to 1.0.  Decrease code should be triggered, but the max decrease
     * should prevent the empty reserve from being met in this epoch.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (8 * 1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1000 * 1024) ) ||
             ( cache_ptr->index_size != (4 * 1000 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 79.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* tenth epoch -- access the 1 KB entries yet again, forcing hit rate
     * to 1.0.  Decrease code should be triggered, and the empty reserve
     * should finally be met.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (7 * 1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (7 * 1000 * 1024 / 2) ) ||
             ( cache_ptr->index_size != (3 * 1000 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 80.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* eleventh epoch -- access the 1 KB entries yet again, forcing hit rate
     * to 1.0.  Decrease code should be triggered, and the empty reserve
     * should be met again.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (6 * 1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (3 * 1000 * 1024) ) ||
             ( cache_ptr->index_size != (2 * 1000 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 81.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* twelth  epoch -- hit rate 1.0 -- decrease as before.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (5 * 1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (5 * 1000 * 1024 / 2) ) ||
             ( cache_ptr->index_size != (1 * 1000 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 82.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* thirteenth  epoch -- hit rate 1.0 -- decrease as before.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (4 * 1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1000 * 1024) ) ||
             ( cache_ptr->index_size != (1 * 1000 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 83.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* fourteenth  epoch -- hit rate 1.0 -- decrease as before.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (3 * 1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (3 * 1000 * 1024 / 2) ) ||
             ( cache_ptr->index_size != (1 * 1000 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 84.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* fifteenth  epoch -- hit rate 1.0 -- decrease as before.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (2 * 1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (1 * 1000 * 1024) ) ||
             ( cache_ptr->index_size != (1 * 1000 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 85.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* sixteenth  epoch -- hit rate 1.0 -- should be stable now
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (2 * 1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (1 * 1000 * 1024) ) ||
             ( cache_ptr->index_size != (1 * 1000 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 86.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) {

        takedown_cache(cache_ptr, FALSE, FALSE);
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass ) {

        HDfprintf(stdout, "%s: failure_mssg = \"%s\".\n",
                  fcn_name, failure_mssg);
    }

    return;

} /* check_auto_cache_resize() */


/*-------------------------------------------------------------------------
 * Function:	check_auto_cache_resize_disable()
 *
 * Purpose:	Test the various ways in which the resize code can
 *		be disabled.  Unfortunately, there are quite a few of them.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              12/16/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
check_auto_cache_resize_disable(void)
{
    const char * fcn_name = "check_auto_cache_resize_disable()";
    hbool_t show_progress = FALSE;
    herr_t result;
    int32_t i;
    int32_t checkpoint = 0;
    H5C_t * cache_ptr = NULL;
    H5C_auto_size_ctl_t auto_size_ctl =
    {
        /* int32_t     version                = */ H5C__CURR_AUTO_SIZE_CTL_VER,
        /* H5C_auto_resize_report_fcn rpt_fcn = */ test_rpt_fcn,

        /* hbool_t     set_initial_size       = */ TRUE,
        /* size_t      initial_size           = */ (512 * 1024),

        /* double      min_clean_fraction     = */ 0.5,

        /* size_t      max_size               = */ (14 * 1024 * 1024),
        /* size_t      min_size               = */ (512 * 1024),

        /* int64_t     epoch_length           = */ 1000,


        /* enum H5C_cache_incr_mode incr_mode = */ H5C_incr__threshold,

        /* double     lower_hr_threshold      = */ 0.75,

        /* double      increment              = */ 2.0,

        /* hbool_t     apply_max_increment    = */ TRUE,
        /* size_t      max_increment          = */ (4 * 1024 * 1024),


        /* enum H5C_cache_decr_mode decr_mode = */ H5C_decr__threshold,

        /* double      upper_hr_threshold     = */ 0.995,

        /* double      decrement              = */ 0.1,

        /* hbool_t     apply_max_decrement    = */ TRUE,
        /* size_t      max_decrement          = */ (1 * 1024 * 1024),

        /* int32_t     epochs_before_eviction = */ 3,

        /* hbool_t     apply_empty_reserve    = */ TRUE,
        /* double      empty_reserve          = */ 0.05
    };

    TESTING("automatic cache resize disable");

    pass = TRUE;

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* allocate a cache, enable automatic cache resizing, and then force
     * the cache through all its operational modes.  Verify that all
     * performs as expected.
     */

    if ( pass ) {

        reset_entries();

        cache_ptr = setup_cache((size_t)(2 * 1024),
                                (size_t)(1 * 1024));
    }

    if ( pass ) {

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 1.\n";
        }
    }

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (512 * 1024) ) ||
             ( cache_ptr->min_clean_size != (256 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after initialization.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);


    /******************************************************************
     * So far, we have forced the auto cache resize through all modes
     * other than increase_disabled and decrease_disabled.  Force these
     * modes now.  Note that there are several ways we can reach these
     * modes.
     ******************************************************************/

    if ( pass ) {

        auto_size_ctl.version                = H5C__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 16 * 1024 * 1024;
        auto_size_ctl.min_size               = 1 * 1024 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 1.0; /* disable size increases */

        auto_size_ctl.apply_max_increment    = FALSE;
        auto_size_ctl.max_increment          = (4 * 1024 * 1024);


        auto_size_ctl.decr_mode              = H5C_decr__threshold;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = FALSE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 3;

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 2.\n";
        }
    }

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after set resize re-config 1.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate with cache full -- increase disabled so should
     * be no change in cache size, and result should be increase_disabled.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_increase_possible ) ||
             ( rpt_status != increase_disabled ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 1.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate -- make sure that we haven't disabled decreases.
     * should result in a decrease cache size from 4 to 2 Meg.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (2 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (1 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 2.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate again -- increase disabled so should
     * be no change in cache size, and result should be increase_disabled.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_increase_possible ) ||
             ( rpt_status != increase_disabled ) ||
             ( cache_ptr->max_cache_size != (2 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (1 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 3.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Repeat the above tests, disabling increase through the lower
     * threshold instead of the increment.
     */

    if ( pass ) {

        auto_size_ctl.version                = H5C__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 16 * 1024 * 1024;
        auto_size_ctl.min_size               = 1 * 1024 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.0; /* disable size increases */

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = FALSE;
        auto_size_ctl.max_increment          = (4 * 1024 * 1024);


        auto_size_ctl.decr_mode              = H5C_decr__threshold;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = FALSE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 3;

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 3.\n";
        }
    }

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after set resize re-config 2.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate with cache full -- increase disabled so should
     * be no change in cache size, and result should be in_spec.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_increase_possible ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 4.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate -- make sure that we haven't disabled decreases.
     * should result in a decrease cache size from 4 to 2 Meg.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (2 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (1 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 5.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate again -- increase disabled so should
     * be no change in cache size, and result should be increase_disabled.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_increase_possible ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (2 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (1 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 6.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Repeat the above tests yet again, disabling increase through the
     * incr_mode.
     */

    if ( pass ) {

        auto_size_ctl.version                = H5C__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 16 * 1024 * 1024;
        auto_size_ctl.min_size               = 1 * 1024 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C_incr__off;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = FALSE;
        auto_size_ctl.max_increment          = (4 * 1024 * 1024);


        auto_size_ctl.decr_mode              = H5C_decr__threshold;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = FALSE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 3;

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 4.\n";
        }
    }

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after set resize re-config 3.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate with cache full -- increase disabled so should
     * be no change in cache size, and result should be in_spec.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_increase_possible ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 7.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate -- make sure that we haven't disabled decreases.
     * should result in a decrease cache size from 4 to 2 Meg.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (2 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (1 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 8.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate again -- increase disabled so should
     * be no change in cache size, and result should be increase_disabled.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_increase_possible ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (2 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (1 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 9.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Now, disable size decreases, and repeat the above tests.
     */

    if ( pass ) {

        auto_size_ctl.version                = H5C__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 16 * 1024 * 1024;
        auto_size_ctl.min_size               = 1 * 1024 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        auto_size_ctl.decr_mode              = H5C_decr__threshold;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 1.0; /* disable size decreases */

        auto_size_ctl.apply_max_decrement    = TRUE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 3;

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 5.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after set resize re-config 4.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate -- should be no change in cache size,
     * and result should be decrease_disabled.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease_disabled ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 10.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate -- cache size should increase from 4 to 6 Meg.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != increase ) ||
             ( cache_ptr->max_cache_size != (6 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (3 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 11.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate again -- should be no change in cache size,
     * and result should be decrease_disabled.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease_disabled ) ||
             ( cache_ptr->max_cache_size != (6 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (3 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 12.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Repeat the above tests, disabling decrease through the upper
     * threshold instead of the decrement.
     */

    if ( pass ) {

        auto_size_ctl.version                = H5C__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 16 * 1024 * 1024;
        auto_size_ctl.min_size               = 1 * 1024 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        auto_size_ctl.decr_mode              = H5C_decr__threshold;

        auto_size_ctl.upper_hr_threshold     = 1.0; /* disable size decreases */

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = TRUE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 3;

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 6.\n";
        }
    }

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after set resize re-config 5.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate -- should be no change in cache size,
     * and result should be in_spec.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 13.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate -- cache size should increase from 4 to 6 Meg.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != increase ) ||
             ( cache_ptr->max_cache_size != (6 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (3 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 14.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate again -- should be no change in cache size,
     * and result should be in_spec.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (6 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (3 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 15.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Repeat the above tests, disabling decrease through the decr_mode.
     */

    if ( pass ) {

        auto_size_ctl.version                = H5C__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 16 * 1024 * 1024;
        auto_size_ctl.min_size               = 1 * 1024 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        auto_size_ctl.decr_mode              = H5C_decr__off;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = TRUE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 3;

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 7.\n";
        }
    }

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after set resize re-config 6.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate -- should be no change in cache size,
     * and result should be in_spec.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 16.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate -- cache size should increase from 4 to 6 Meg.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != increase ) ||
             ( cache_ptr->max_cache_size != (6 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (3 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 17.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate again -- should be no change in cache size,
     * and result should be in_spec.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (6 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (3 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 18.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Now do tests disabling size decrement in age out mode.
     *
     * Start by disabling size decrement by setting max_decrement to zero.
     */

    if ( pass ) {

        auto_size_ctl.version                = H5C__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 16 * 1024 * 1024;
        auto_size_ctl.min_size               = 1 * 1024 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        auto_size_ctl.decr_mode              = H5C_decr__age_out;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = TRUE;
        auto_size_ctl.max_decrement          = 0; /* disable decrement */

        auto_size_ctl.epochs_before_eviction = 1;

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 8.\n";
        }
    }

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after set resize re-config 7.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* flush the cache and destroy all entries so we start from a known point */
    flush_cache(cache_ptr, TRUE, FALSE, FALSE);

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* load up the cache with small entries.  Note that it will take an
     * epoch for the ageout code to initialize itself if it is enabled.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, SMALL_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, SMALL_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( rpt_status != not_full ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 19.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Load up some more small entries.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 1000;
        while ( ( pass ) && ( i < 2000 ) )
        {
            protect_entry(cache_ptr, SMALL_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, SMALL_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( rpt_status != not_full ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 20.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Now force a high hit rate so that the size increase code is
     * is satisfied.  We would see a decrease here if decrease were
     * possible.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, SMALL_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, SMALL_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( rpt_status != decrease_disabled ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 21.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate -- cache size should increase from 4 to 6 Meg.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != increase ) ||
             ( cache_ptr->max_cache_size != (6 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (3 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 22.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* just bang on a single entry.  This will see to it that there are
     * many entries that could be aged out were decreases enabled.
     * Should be no change in cache size, and result should be
     * decrease_disabled.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( rpt_status != decrease_disabled ) ||
             ( cache_ptr->max_cache_size != (6 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (3 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 23.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Now disable size decrement in age out mode via the empty reserve.
     */

    if ( pass ) {

        auto_size_ctl.version                = H5C__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 16 * 1024 * 1024;
        auto_size_ctl.min_size               = 1 * 1024 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        auto_size_ctl.decr_mode              = H5C_decr__age_out;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = TRUE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 1;

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 1.0; /* disable decrement */

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 9.\n";
        }
    }

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after set resize re-config 8.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* flush the cache and destroy all entries so we start from a known point */
    flush_cache(cache_ptr, TRUE, FALSE, FALSE);

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* load up the cache with small entries.  Note that it will take an
     * epoch for the ageout code to initialize itself if it is enabled.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, SMALL_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, SMALL_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( rpt_status != not_full ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 24.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Load up some more small entries.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 1000;
        while ( ( pass ) && ( i < 2000 ) )
        {
            protect_entry(cache_ptr, SMALL_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, SMALL_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( rpt_status != not_full ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 25.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Now force a high hit rate so that the size increase code is
     * is satisfied.  We would see a decrease here if decrease were
     * possible.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, SMALL_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, SMALL_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( rpt_status != decrease_disabled ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 26.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate -- cache size should increase from 4 to 6 Meg.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != increase ) ||
             ( cache_ptr->max_cache_size != (6 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (3 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 27.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* just bang on a single entry.  This will see to it that there are
     * many entries that could be aged out were decreases enabled.
     * Should be no change in cache size, and result should be
     * decrease_disabled.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( rpt_status != decrease_disabled ) ||
             ( cache_ptr->max_cache_size != (6 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (3 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 28.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Now work with age out with threshold.  One can argue that we should
     * repeat the above age out tests with age out with threshold, but the
     * same code is executed in both cases so I don't see the point.  If
     * that ever changes, this test should be updated.
     *
     * There is only one way of disabling decrements that is peculiar
     * to age out with threshold, which is to set the upper threshold
     * to 1.0.  Test this now.
     */

    if ( pass ) {

        auto_size_ctl.version                = H5C__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 16 * 1024 * 1024;
        auto_size_ctl.min_size               = 1 * 1024 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        auto_size_ctl.decr_mode              = H5C_decr__age_out_with_threshold;

        auto_size_ctl.upper_hr_threshold     = 1.0;

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = TRUE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 1;

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 10.\n";
        }
    }

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after set resize re-config 9.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* flush the cache and destroy all entries so we start from a known point */
    flush_cache(cache_ptr, TRUE, FALSE, FALSE);

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* load up the cache with small entries.  Note that it will take an
     * epoch for the ageout code to initialize itself if it is enabled.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, SMALL_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, SMALL_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( rpt_status != not_full ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 29.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Load up some more small entries.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 1000;
        while ( ( pass ) && ( i < 2000 ) )
        {
            protect_entry(cache_ptr, SMALL_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, SMALL_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( rpt_status != not_full ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 30.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Now force a high hit rate so that the size increase code is
     * is satisfied.  We would see a decrease here if decrease were
     * possible, but the upper threshold cannot be met, so no decrease.
     *
     * rpt_status should be decrease_disabled.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, SMALL_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, SMALL_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( rpt_status != decrease_disabled ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ||
             ( cache_ptr->index_len != 2000 ) ||
             ( cache_ptr->index_size != 2000 * SMALL_ENTRY_SIZE ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 31.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate -- cache size should increase from 4 to 6 Meg.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != increase ) ||
             ( cache_ptr->max_cache_size != (6 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (3 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 32.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* just bang on a single entry.  This keeps the hit rate high, and sees
     * to it that there are many entries that could be aged out were
     * decreases enabled.
     *
     * Should be no change in cache size, and result should be
     * decrease_disabled.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 999);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 999,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( rpt_status != decrease_disabled ) ||
             ( cache_ptr->max_cache_size != (6 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (3 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 33.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);


    /*********************************************************************
     * Finally, use the auto cache resize code to set the size of the
     * cache and keep it there.  Again, due to the complexity of the
     * interface, there are lots of ways of doing this.  We have to
     * check them all.
     *********************************************************************/

    if ( pass ) {

        auto_size_ctl.version                = H5C__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 2 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 16 * 1024 * 1024;
        auto_size_ctl.min_size               =  1 * 1024 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.0; /* disable size increases */

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        auto_size_ctl.decr_mode              = H5C_decr__threshold;

        auto_size_ctl.upper_hr_threshold     = 1.0; /* disable size decreases */

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = TRUE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 3;

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 11.\n";
        }
    }

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (2 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (1 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after set resize re-config 10.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate -- should be no response as the auto-resize
     * code should be disabled.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( rpt_fcn_called ) ||
             ( cache_ptr->resize_enabled ) ||
             ( cache_ptr->size_increase_possible ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( cache_ptr->max_cache_size != (2 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (1 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 34.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate -- should be no response as the auto-resize
     * code should be disabled.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( rpt_fcn_called ) ||
             ( cache_ptr->resize_enabled ) ||
             ( cache_ptr->size_increase_possible ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( cache_ptr->max_cache_size != (2 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (1 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 35.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) {

        auto_size_ctl.version                = H5C__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.25;

        auto_size_ctl.max_size               = 16 * 1024 * 1024;
        auto_size_ctl.min_size               =  1 * 1024 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 1.0; /* disable size increment */

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        auto_size_ctl.decr_mode              = H5C_decr__threshold;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 1.0; /* disable size decrement */

        auto_size_ctl.apply_max_decrement    = TRUE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 3;

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 12.\n";
        }
    }

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (1 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after set resize re-config 11.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate -- should be no response as the auto-resize
     * code should be disabled.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( rpt_fcn_called ) ||
             ( cache_ptr->resize_enabled ) ||
             ( cache_ptr->size_increase_possible ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (1 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 36.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate -- should be no response as the auto-resize
     * code should be disabled.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( rpt_fcn_called ) ||
             ( cache_ptr->resize_enabled ) ||
             ( cache_ptr->size_increase_possible ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (1 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 37.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) {

        auto_size_ctl.version                = H5C__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = FALSE;
        auto_size_ctl.initial_size           = 2 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 6 * 1024 * 1024; /* no resize */
        auto_size_ctl.min_size               = 6 * 1024 * 1024; /* no resize */

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        auto_size_ctl.decr_mode              = H5C_decr__threshold;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = TRUE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 3;

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 13.\n";
        }
    }

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (6 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (3 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after set resize re-config 12.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate -- should be no response as the auto-resize
     * code should be disabled.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( rpt_fcn_called ) ||
             ( cache_ptr->resize_enabled ) ||
             ( cache_ptr->size_increase_possible ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( cache_ptr->max_cache_size != (6 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (3 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 38.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate -- should be no response as the auto-resize
     * code should be disabled.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( rpt_fcn_called ) ||
             ( cache_ptr->resize_enabled ) ||
             ( cache_ptr->size_increase_possible ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( cache_ptr->max_cache_size != (6 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (3 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 39.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) {

        auto_size_ctl.version                = H5C__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.25;

        auto_size_ctl.max_size               = 16 * 1024 * 1024;
        auto_size_ctl.min_size               =  1 * 1024 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 1.0; /* disable size increment */

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        auto_size_ctl.decr_mode              = H5C_decr__threshold;

        auto_size_ctl.upper_hr_threshold     = 1.0; /* disable size decrement */

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = TRUE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 3;

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 14.\n";
        }
    }

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (1 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after set resize re-config 13.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate -- should be no response as the auto-resize
     * code should be disabled.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( rpt_fcn_called ) ||
             ( cache_ptr->resize_enabled ) ||
             ( cache_ptr->size_increase_possible ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (1 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 40.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate -- should be no response as the auto-resize
     * code should be disabled.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( rpt_fcn_called ) ||
             ( cache_ptr->resize_enabled ) ||
             ( cache_ptr->size_increase_possible ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (1 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 41.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) {

        auto_size_ctl.version                = H5C__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 16 * 1024 * 1024;
        auto_size_ctl.min_size               =  1 * 1024 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.0; /* disable size increment */

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        auto_size_ctl.decr_mode              = H5C_decr__threshold;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 1.0; /* disable size decrement */

        auto_size_ctl.apply_max_decrement    = TRUE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 3;

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 0.05;


        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 15.\n";
        }
    }

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after set resize re-config 14.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate -- should be no response as the auto-resize
     * code should be disabled.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( rpt_fcn_called ) ||
             ( cache_ptr->resize_enabled ) ||
             ( cache_ptr->size_increase_possible ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 42.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate -- should be no response as the auto-resize
     * code should be disabled.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( rpt_fcn_called ) ||
             ( cache_ptr->resize_enabled ) ||
             ( cache_ptr->size_increase_possible ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 43.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) {

        auto_size_ctl.version                = H5C__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 16 * 1024 * 1024;
        auto_size_ctl.min_size               =  1 * 1024 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C_incr__off;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        auto_size_ctl.decr_mode              = H5C_decr__off;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = TRUE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 3;

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 0.05;


        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 16.\n";
        }
    }

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after set resize re-config 15.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate -- should be no response as the auto-resize
     * code should be disabled.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( rpt_fcn_called ) ||
             ( cache_ptr->resize_enabled ) ||
             ( cache_ptr->size_increase_possible ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 44.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate -- should be no response as the auto-resize
     * code should be disabled.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass ) {
                unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( rpt_fcn_called ) ||
             ( cache_ptr->resize_enabled ) ||
             ( cache_ptr->size_increase_possible ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 45.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) {

        takedown_cache(cache_ptr, FALSE, FALSE);
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass ) {

        HDfprintf(stdout, "%s: failure_mssg = \"%s\".\n",
                  fcn_name, failure_mssg);
    }

    return;

} /* check_auto_cache_resize_disable() */


/*-------------------------------------------------------------------------
 * Function:	check_auto_cache_resize_epoch_markers()
 *
 * Purpose:	Verify that the auto-resize code manages epoch markers
 *		correctly.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              12/16/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
check_auto_cache_resize_epoch_markers(void)
{
    const char * fcn_name = "check_auto_cache_resize_epoch_markers()";
    hbool_t show_progress = FALSE;
    herr_t result;
    int32_t i;
    int32_t j;
    int32_t checkpoint = 0;
    H5C_t * cache_ptr = NULL;
    H5C_auto_size_ctl_t auto_size_ctl =
    {
        /* int32_t     version                = */ H5C__CURR_AUTO_SIZE_CTL_VER,
        /* H5C_auto_resize_report_fcn rpt_fcn = */ test_rpt_fcn,

        /* hbool_t     set_initial_size       = */ TRUE,
        /* size_t      initial_size           = */ (512 * 1024),

        /* double      min_clean_fraction     = */ 0.5,

        /* size_t      max_size               = */ (14 * 1024 * 1024),
        /* size_t      min_size               = */ (512 * 1024),

        /* int64_t     epoch_length           = */ 1000,


        /* enum H5C_cache_incr_mode incr_mode = */ H5C_incr__threshold,

        /* double     lower_hr_threshold      = */ 0.75,

        /* double      increment              = */ 2.0,

        /* hbool_t     apply_max_increment    = */ TRUE,
        /* size_t      max_increment          = */ (4 * 1024 * 1024),


        /* enum H5C_cache_decr_mode decr_mode = */ H5C_decr__threshold,

        /* double      upper_hr_threshold     = */ 0.995,

        /* double      decrement              = */ 0.1,

        /* hbool_t     apply_max_decrement    = */ TRUE,
        /* size_t      max_decrement          = */ (1 * 1024 * 1024),

        /* int32_t     epochs_before_eviction = */ 3,

        /* hbool_t     apply_empty_reserve    = */ TRUE,
        /* double      empty_reserve          = */ 0.05
    };

    TESTING("automatic cache resize epoch marker management");

    pass = TRUE;

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) {

        reset_entries();

        cache_ptr = setup_cache((size_t)(2 * 1024),
                                (size_t)(1 * 1024));
    }

    if ( pass ) {

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 1.\n";
        }
    }

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (512 * 1024) ) ||
             ( cache_ptr->min_clean_size != (256 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after initialization.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);


    /* Now make sure that we are managing the epoch markers correctly.
     */

    if ( pass ) {

        auto_size_ctl.version                = H5C__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 8 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 8 * 1024 * 1024;
        auto_size_ctl.min_size               = 512 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C_incr__off;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (4 * 1024 * 1024);


        auto_size_ctl.decr_mode              = H5C_decr__age_out;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = FALSE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 10;

        auto_size_ctl.apply_empty_reserve    = FALSE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 2.\n";
        }
    }

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after set resize re-config 1.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Since we just created the cache, there should be no epoch markers
     * active.  Verify that this is true.
     */

    if ( pass ) {

        if ( cache_ptr->epoch_markers_active != 0 ) {

            pass = FALSE;
            failure_mssg = "Unexpected # of epoch markers 1.\n";
        }
    }

    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->index_size != (1 * 1000 * MEDIUM_ENTRY_SIZE) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 0.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);


    if ( pass ) {

        j = 2;
        while ( ( pass ) && ( j <= 10 ) )
        {

            rpt_fcn_called = FALSE;
            i = (j - 2) * 1000;
            while ( ( pass ) && ( i < (j - 1) * 1000 ) )
            {
                protect_entry(cache_ptr, SMALL_ENTRY_TYPE, i);

                if ( pass ) {
                    unprotect_entry(cache_ptr, SMALL_ENTRY_TYPE, i,
                                    NO_CHANGE, H5C__NO_FLAGS_SET);
                }
                i++;
            }

            if ( ( ! rpt_fcn_called ) ||
                 ( rpt_status != in_spec ) ||
                 ( cache_ptr->epoch_markers_active != j ) ) {

                pass = FALSE;
                failure_mssg = "Unexpected # of epoch markers 2.\n";
            }

            j++;
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* we now have a full complement of epoch markers -- see if
     * we get the expected reduction.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 9000;
        while ( ( pass ) && ( i < 10000 ) )
        {
            protect_entry(cache_ptr, SMALL_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, SMALL_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size !=
               (10 * 1000 * SMALL_ENTRY_SIZE + MEDIUM_ENTRY_SIZE) ) ||
             ( cache_ptr->min_clean_size !=
               ((10 * 1000 * SMALL_ENTRY_SIZE + MEDIUM_ENTRY_SIZE) / 2) ) ||
             ( cache_ptr->index_size !=
               (10 * 1000 * SMALL_ENTRY_SIZE + MEDIUM_ENTRY_SIZE) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 1.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* now reduce the epochs before eviction, and see if the cache
     * deletes the extra markers
     */
    if ( pass ) {

        auto_size_ctl.version                = H5C__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 8 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 8 * 1024 * 1024;
        auto_size_ctl.min_size               = 512 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C_incr__off;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (4 * 1024 * 1024);


        auto_size_ctl.decr_mode              = H5C_decr__age_out;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = FALSE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 1;

        auto_size_ctl.apply_empty_reserve    = FALSE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 3.\n";
        }
    }

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after set resize re-config 2.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* There should be exactly one active epoch marker at present.
     */
    if ( pass ) {

        if ( cache_ptr->epoch_markers_active != 1 ) {

            pass = FALSE;
            failure_mssg = "Unexpected # of epoch markers 3.\n";
        }
    }

    /* Now do an epochs worth of accesses, and verify that everything
     * not accessed in this epoch gets evicted, and the cache size
     * is reduced.
     */
    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 9000;
        while ( ( pass ) && ( i < 10000 ) )
        {
            protect_entry(cache_ptr, SMALL_ENTRY_TYPE, i);

            if ( pass ) {
                unprotect_entry(cache_ptr, SMALL_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (512 * 1024) ) ||
             ( cache_ptr->min_clean_size != (256 * 1024) ) ||
             ( cache_ptr->index_size != (1 * 1000 * SMALL_ENTRY_SIZE) ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 2.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* There should be exactly one active epoch marker at present...
     */
    if ( pass ) {

        if ( cache_ptr->epoch_markers_active != 1 ) {

            pass = FALSE;
            failure_mssg = "Unexpected # of epoch markers 4.\n";
        }
    }

    /* shift the decrement mode to threshold, and verify that we remove
     * all epoch markers.
     */
    if ( pass ) {

        auto_size_ctl.version                = H5C__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 8 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 8 * 1024 * 1024;
        auto_size_ctl.min_size               = 512 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C_incr__off;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (4 * 1024 * 1024);


        auto_size_ctl.decr_mode              = H5C_decr__threshold;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = FALSE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 1;

        auto_size_ctl.apply_empty_reserve    = FALSE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 4.\n";
        }
    }

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after set resize re-config 3.\n";
        }
    }

    /* ... and now there should be none.
     */
    if ( pass ) {

        if ( cache_ptr->epoch_markers_active != 0 ) {

            pass = FALSE;
            failure_mssg = "Unexpected # of epoch markers 5.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* shift the decrement mode to age out with threshold.  Set epochs
     * before eviction to 10 again.
     */
    if ( pass ) {

        auto_size_ctl.version                = H5C__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 8 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 8 * 1024 * 1024;
        auto_size_ctl.min_size               = 512 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C_incr__off;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (4 * 1024 * 1024);


        auto_size_ctl.decr_mode              = H5C_decr__age_out_with_threshold;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = FALSE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 10;

        auto_size_ctl.apply_empty_reserve    = FALSE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 5.\n";
        }
    }

    /* Verify that there are no active epoch markers.
     */
    if ( pass ) {

        if ( cache_ptr->epoch_markers_active != 0 ) {

            pass = FALSE;
            failure_mssg = "Unexpected # of epoch markers 6.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* build up a full set of epoch markers. */
    if ( pass ) {

        j = 1;
        while ( ( pass ) && ( j <= 10 ) )
        {

            rpt_fcn_called = FALSE;
            i = (j - 1) * 1000;
            while ( ( pass ) && ( i < j * 1000 ) )
            {
                protect_entry(cache_ptr, SMALL_ENTRY_TYPE, i);

                if ( pass ) {
                    unprotect_entry(cache_ptr, SMALL_ENTRY_TYPE, i,
                                    NO_CHANGE, H5C__NO_FLAGS_SET);
                }
                i++;
            }

            if ( ( ! rpt_fcn_called ) ||
                 ( rpt_status != in_spec ) ||
                 ( cache_ptr->epoch_markers_active != j ) ) {

                pass = FALSE;
                failure_mssg = "Unexpected # of epoch markers 7.\n";
            }

            j++;
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Verify that there are now 10 active epoch markers.
     */
    if ( pass ) {

        if ( cache_ptr->epoch_markers_active != 10 ) {

            pass = FALSE;
            failure_mssg = "Unexpected # of epoch markers 8.\n";
        }
    }

    /* shift the decrement mode to off.  This should cause all epoch
     * markers to be removed.
     */
    if ( pass ) {

        auto_size_ctl.version                = H5C__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 8 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 8 * 1024 * 1024;
        auto_size_ctl.min_size               = 512 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C_incr__off;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (4 * 1024 * 1024);


        auto_size_ctl.decr_mode              = H5C_decr__off;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = FALSE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 10;

        auto_size_ctl.apply_empty_reserve    = FALSE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 6.\n";
        }
    }

    /* Verify that there are now no active epoch markers.
     */
    if ( pass ) {

        if ( cache_ptr->epoch_markers_active != 0 ) {

            pass = FALSE;
            failure_mssg = "Unexpected # of epoch markers 9.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* verify that we still have the expected number of entries in the cache,
     * and that the cache is of the expected size.
     */
    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) )||
             ( cache_ptr->index_size != (10 * 1000 * SMALL_ENTRY_SIZE) ) ||
             ( cache_ptr->index_len != 10000 ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache size change results 3.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) {

        takedown_cache(cache_ptr, FALSE, FALSE);
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass ) {

        HDfprintf(stdout, "%s: failure_mssg = \"%s\".\n",
                  fcn_name, failure_mssg);
    }

    return;

} /* check_auto_cache_resize_epoch_markers() */


/*-------------------------------------------------------------------------
 * Function:	check_auto_cache_resize_input_errs()
 *
 * Purpose:	Verify that H5C_set_cache_auto_resize_config() detects
 *		and rejects invalid input.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              10/29/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

#define RESIZE_CONFIGS_ARE_EQUAL(a, b, compare_init)              \
( ( (a).version                == (b).version ) &&                \
  ( (a).rpt_fcn                == (b).rpt_fcn ) &&                \
  ( ( ! compare_init ) ||                                         \
    ( (a).set_initial_size     == (b).set_initial_size ) ) &&     \
  ( ( ! compare_init ) ||                                         \
    ( (a).initial_size         == (b).initial_size ) ) &&         \
  ( (a).min_clean_fraction     == (b).min_clean_fraction ) &&     \
  ( (a).max_size               == (b).max_size ) &&               \
  ( (a).min_size               == (b).min_size ) &&               \
  ( (a).epoch_length           == (b).epoch_length ) &&           \
  ( (a).incr_mode              == (b).incr_mode ) &&              \
  ( (a).lower_hr_threshold     == (b).lower_hr_threshold ) &&     \
  ( (a).increment              == (b).increment ) &&              \
  ( (a).apply_max_increment    == (b).apply_max_increment ) &&    \
  ( (a).max_increment          == (b).max_increment ) &&          \
  ( (a).decr_mode              == (b).decr_mode ) &&              \
  ( (a).upper_hr_threshold     == (b).upper_hr_threshold ) &&     \
  ( (a).decrement              == (b).decrement ) &&              \
  ( (a).apply_max_decrement    == (b).apply_max_decrement ) &&    \
  ( (a).max_decrement          == (b).max_decrement ) &&          \
  ( (a).epochs_before_eviction == (b).epochs_before_eviction ) && \
  ( (a).apply_empty_reserve    == (b).apply_empty_reserve ) &&    \
  ( (a).empty_reserve          == (b).empty_reserve ) )

static void
check_auto_cache_resize_input_errs(void)
{
    const char * fcn_name = "check_auto_cache_resize_input_errs()";
    herr_t result;
    H5C_t * cache_ptr = NULL;
    H5C_auto_size_ctl_t ref_auto_size_ctl =
    {
        /* int32_t     version                = */ H5C__CURR_AUTO_SIZE_CTL_VER,
        /* H5C_auto_resize_report_fcn rpt_fcn = */ test_rpt_fcn,

        /* hbool_t     set_initial_size       = */ TRUE,
        /* size_t      initial_size           = */ (512 * 1024),

        /* double      min_clean_fraction     = */ 0.5,

        /* size_t      max_size               = */ (16 * 1024 * 1024),
        /* size_t      min_size               = */ (512 * 1024),

        /* int64_t     epoch_length           = */ 1000,


        /* enum H5C_cache_incr_mode incr_mode = */ H5C_incr__threshold,

        /* double     lower_hr_threshold      = */ 0.75,

        /* double      increment              = */ 2.0,

        /* hbool_t     apply_max_increment    = */ TRUE,
        /* size_t      max_increment          = */ (4 * 1024 * 1024),


        /* enum H5C_cache_decr_mode decr_mode = */ H5C_decr__threshold,

        /* double      upper_hr_threshold     = */ 0.995,

        /* double      decrement              = */ 0.1,

        /* hbool_t     apply_max_decrement    = */ TRUE,
        /* size_t      max_decrement          = */ (1 * 1024 * 1024),

        /* int32_t     epochs_before_eviction = */ 3,

        /* hbool_t     apply_empty_reserve    = */ TRUE,
        /* double      empty_reserve          = */ 0.05
    };

    H5C_auto_size_ctl_t invalid_auto_size_ctl;
    H5C_auto_size_ctl_t test_auto_size_ctl;

    TESTING("automatic cache resize input errors");

    pass = TRUE;

    /* allocate a cache, and set a reference automatic cache control
     * configuration.  Then feed H5C_set_cache_auto_resize_config()
     * invalid input, and verify that the correct error is returned,
     * and that the configuration is not modified.
     */

    if ( pass ) {

        reset_entries();

        cache_ptr = setup_cache((size_t)(2 * 1024),
                                (size_t)(1 * 1024));
    }

    if ( pass ) {

        result = H5C_set_cache_auto_resize_config(cache_ptr,
                                                  &ref_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 1.\n";
        }
    }

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (512 * 1024) ) ||
             ( cache_ptr->min_clean_size != (256 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after initialization.\n";
        }
    }

    if ( pass ) {

        result = H5C_get_cache_auto_resize_config(cache_ptr,
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_auto_resize_config failed 1.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass = FALSE;
            failure_mssg = "Unexpected auto resize config 1.";
        }
    }

    if ( pass ) {

        invalid_auto_size_ctl.version            = H5C__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.5;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;

        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.7;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        invalid_auto_size_ctl.decr_mode              = H5C_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.5;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(NULL,
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg =
                "H5C_set_cache_auto_resize_config accepted NULL cache_ptr.\n";
        }
    }

    if ( pass ) {

        result = H5C_get_cache_auto_resize_config(cache_ptr,
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_auto_resize_config failed 2.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass = FALSE;
            failure_mssg = "Unexpected auto resize config 2.";
        }
    }


    /* check bad version rejection. */

    if ( pass ) {

        invalid_auto_size_ctl.version                = -1; /* INVALID */
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.5;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;

        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.7;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        invalid_auto_size_ctl.decr_mode              = H5C_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.5;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr,
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg =
                "H5C_set_cache_auto_resize_config accepted bad version.\n";
        }
    }

    if ( pass ) {

        result = H5C_get_cache_auto_resize_config(cache_ptr,
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_auto_resize_config failed 3.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass = FALSE;
            failure_mssg = "Unexpected auto resize config 3.";
        }
    }


    /* check bad initial size rejection */

    if ( pass ) {

        invalid_auto_size_ctl.version            = H5C__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 16 * 1024 * 1024 + 1;
                                                       /* INVALID */

        invalid_auto_size_ctl.min_clean_fraction     = 0.5;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;

        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        invalid_auto_size_ctl.decr_mode              = H5C_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.5;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr,
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg =
                "H5C_set_cache_auto_resize_config accepted bad init size 1.\n";
        }
    }

    if ( pass ) {

        result = H5C_get_cache_auto_resize_config(cache_ptr,
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_auto_resize_config failed 4.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass = FALSE;
            failure_mssg = "Unexpected auto resize config 4.";
        }
    }

    if ( pass ) {

        invalid_auto_size_ctl.version            = H5C__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 1 * 1024 * 1024 - 1;
                                                       /* INVALID */

        invalid_auto_size_ctl.min_clean_fraction     = 0.5;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;

        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        invalid_auto_size_ctl.decr_mode              = H5C_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.5;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr,
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg =
                "H5C_set_cache_auto_resize_config accepted bad init size 2.\n";
        }
    }

    if ( pass ) {

        result = H5C_get_cache_auto_resize_config(cache_ptr,
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_auto_resize_config failed 5.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass = FALSE;
            failure_mssg = "Unexpected auto resize config 5.";
        }
    }


    /* test for invalid min clean fraction rejection. */

    if ( pass ) {

        invalid_auto_size_ctl.version            = H5C__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 1.00001; /* INVALID */

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;

        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        invalid_auto_size_ctl.decr_mode              = H5C_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.5;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr,
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg =
            "H5C_set_cache_auto_resize_config accepted bad min clean frac 1.\n";
        }
    }

    if ( pass ) {

        result = H5C_get_cache_auto_resize_config(cache_ptr,
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_auto_resize_config failed 6.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass = FALSE;
            failure_mssg = "Unexpected auto resize config 6.";
        }
    }

    if ( pass ) {

        invalid_auto_size_ctl.version            = H5C__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = -0.00001; /* INVALID */

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;

        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        invalid_auto_size_ctl.decr_mode              = H5C_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.5;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr,
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg =
            "H5C_set_cache_auto_resize_config accepted bad min clean frac 2.\n";
        }
    }

    if ( pass ) {

        result = H5C_get_cache_auto_resize_config(cache_ptr,
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_auto_resize_config failed 7.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass = FALSE;
            failure_mssg = "Unexpected auto resize config 7.";
        }
    }


    /* test for invalid max_size and/or min_size rejection */

    if ( pass ) {

        invalid_auto_size_ctl.version            = H5C__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.5;

        invalid_auto_size_ctl.max_size            = H5C__MAX_MAX_CACHE_SIZE + 1;
                                                    /* INVALID */
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;

        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        invalid_auto_size_ctl.decr_mode              = H5C_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.5;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr,
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg =
                "H5C_set_cache_auto_resize_config accepted bad max_size.\n";
        }
    }

    if ( pass ) {

        result = H5C_get_cache_auto_resize_config(cache_ptr,
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_auto_resize_config failed 8.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass = FALSE;
            failure_mssg = "Unexpected auto resize config 8.";
        }
    }

    if ( pass ) {

        invalid_auto_size_ctl.version            = H5C__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.5;

        invalid_auto_size_ctl.max_size           = 1 * 1024 * 1024;/* INVALID */
        invalid_auto_size_ctl.min_size           = 1 * 1024 * 1024 + 1;/*PAIR */

        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        invalid_auto_size_ctl.decr_mode              = H5C_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.5;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr,
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg =
                "H5C_set_cache_auto_resize_config accepted bad size pair.\n";
        }
    }

    if ( pass ) {

        result = H5C_get_cache_auto_resize_config(cache_ptr,
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_auto_resize_config failed 9.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass = FALSE;
            failure_mssg = "Unexpected auto resize config 9.";
        }
    }

    if ( pass ) {

        invalid_auto_size_ctl.version            = H5C__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.5;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size            = H5C__MIN_MAX_CACHE_SIZE - 1;
                                                    /* INVALID */
        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        invalid_auto_size_ctl.decr_mode              = H5C_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.5;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr,
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg =
                "H5C_set_cache_auto_resize_config accepted bad min_size.\n";
        }
    }

    if ( pass ) {

        result = H5C_get_cache_auto_resize_config(cache_ptr,
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_auto_resize_config failed 10.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass = FALSE;
            failure_mssg = "Unexpected auto resize config 10.";
        }
    }


    /* test for invalid epoch_length rejection */

    if ( pass ) {

        invalid_auto_size_ctl.version            = H5C__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.1;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;

        invalid_auto_size_ctl.epoch_length       = H5C__MAX_AR_EPOCH_LENGTH + 1;
                                                   /* INVALID */

        invalid_auto_size_ctl.incr_mode              = H5C_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        invalid_auto_size_ctl.decr_mode              = H5C_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.9;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr,
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg =
                "H5C_set_cache_auto_resize_config accepted bad epoch len 1.\n";
        }
    }

    if ( pass ) {

        result = H5C_get_cache_auto_resize_config(cache_ptr,
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_auto_resize_config failed 11.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass = FALSE;
            failure_mssg = "Unexpected auto resize config 11.";
        }
    }

    if ( pass ) {

        invalid_auto_size_ctl.version            = H5C__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.1;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;

        invalid_auto_size_ctl.epoch_length       = H5C__MIN_AR_EPOCH_LENGTH - 1;
                                                   /* INVALID */

        invalid_auto_size_ctl.incr_mode              = H5C_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        invalid_auto_size_ctl.decr_mode              = H5C_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.9;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr,
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg =
                "H5C_set_cache_auto_resize_config accepted bad epoch len 2.\n";
        }
    }

    if ( pass ) {

        result = H5C_get_cache_auto_resize_config(cache_ptr,
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_auto_resize_config failed 12.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass = FALSE;
            failure_mssg = "Unexpected auto resize config 12.";
        }
    }


    /* test for bad incr_mode rejection */

    if ( pass ) {

        invalid_auto_size_ctl.version            = H5C__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.1;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;

        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              =
        			(enum H5C_cache_incr_mode) -1; /* INVALID */

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        invalid_auto_size_ctl.decr_mode              = H5C_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.9;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr,
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg =
                "H5C_set_cache_auto_resize_config accepted bad incr_mode 1.\n";
        }
    }

    if ( pass ) {

        result = H5C_get_cache_auto_resize_config(cache_ptr,
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_auto_resize_config failed 13.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass = FALSE;
            failure_mssg = "Unexpected auto resize config 13.";
        }
    }

    if ( pass ) {

        invalid_auto_size_ctl.version            = H5C__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.1;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;

        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              =
        			(enum H5C_cache_incr_mode) 2; /* INVALID */

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        invalid_auto_size_ctl.decr_mode              = H5C_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.9;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr,
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg =
                "H5C_set_cache_auto_resize_config accepted bad incr_mode 2.\n";
        }
    }

    if ( pass ) {

        result = H5C_get_cache_auto_resize_config(cache_ptr,
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_auto_resize_config failed 14.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass = FALSE;
            failure_mssg = "Unexpected auto resize config 14.";
        }
    }


    /* check for bad upper and/or lower threshold rejection */

    if ( pass ) {

        invalid_auto_size_ctl.version            = H5C__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.5;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;

        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.7;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        invalid_auto_size_ctl.decr_mode              = H5C_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 1.01; /* INVALID */

        invalid_auto_size_ctl.decrement              = 0.5;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr,
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg =
             "H5C_set_cache_auto_resize_config accepted bad upper threshold.\n";
        }
    }

    if ( pass ) {

        result = H5C_get_cache_auto_resize_config(cache_ptr,
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_auto_resize_config failed 15.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass = FALSE;
            failure_mssg = "Unexpected auto resize config 15.";
        }
    }

    if ( pass ) {

        invalid_auto_size_ctl.version            = H5C__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.5;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;

        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.8; /* INVALID */

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        invalid_auto_size_ctl.decr_mode              = H5C_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.7; /* INVALID */

        invalid_auto_size_ctl.decrement              = 0.5;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr,
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg =
              "H5C_set_cache_auto_resize_config accepted bad threshold pair.\n";
        }
    }

    if ( pass ) {

        result = H5C_get_cache_auto_resize_config(cache_ptr,
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_auto_resize_config failed 16.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass = FALSE;
            failure_mssg = "Unexpected auto resize config 16.";
        }
    }

    if ( pass ) {

        invalid_auto_size_ctl.version            = H5C__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.5;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;

        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = -0.0001; /* INVALID */

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        invalid_auto_size_ctl.decr_mode              = H5C_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.5;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr,
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg =
             "H5C_set_cache_auto_resize_config accepted bad lower threshold.\n";
        }
    }

    if ( pass ) {

        result = H5C_get_cache_auto_resize_config(cache_ptr,
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_auto_resize_config failed 17.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass = FALSE;
            failure_mssg = "Unexpected auto resize config 17.";
        }
    }


    /* test for bad increment rejection */

    if ( pass ) {

        invalid_auto_size_ctl.version            = H5C__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.1;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;

        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 0.99999; /* INVALID */

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        invalid_auto_size_ctl.decr_mode              = H5C_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.5;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;


        result = H5C_set_cache_auto_resize_config(cache_ptr,
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg =
                "H5C_set_cache_auto_resize_config accepted bad increment.\n";
        }
    }

    if ( pass ) {

        result = H5C_get_cache_auto_resize_config(cache_ptr,
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_auto_resize_config failed 18.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass = FALSE;
            failure_mssg = "Unexpected auto resize config 18.";
        }
    }


    /* test for bad decr_mode rejection */

    if ( pass ) {

        invalid_auto_size_ctl.version            = H5C__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.1;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;

        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        invalid_auto_size_ctl.decr_mode              =
        			(enum H5C_cache_decr_mode) -1; /* INVALID */

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.9;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr,
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg =
                "H5C_set_cache_auto_resize_config accepted bad decr_mode 1.\n";
        }
    }

    if ( pass ) {

        result = H5C_get_cache_auto_resize_config(cache_ptr,
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_auto_resize_config failed 19.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass = FALSE;
            failure_mssg = "Unexpected auto resize config 19.";
        }
    }

    if ( pass ) {

        invalid_auto_size_ctl.version            = H5C__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.1;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;

        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        invalid_auto_size_ctl.decr_mode              =
        			(enum H5C_cache_decr_mode) 4; /* INVALID */

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.9;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr,
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg =
                "H5C_set_cache_auto_resize_config accepted bad decr_mode 2.\n";
        }
    }

    if ( pass ) {

        result = H5C_get_cache_auto_resize_config(cache_ptr,
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_auto_resize_config failed 20.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass = FALSE;
            failure_mssg = "Unexpected auto resize config 20.";
        }
    }


    /* check for bad decrement rejection */

    if ( pass ) {

        invalid_auto_size_ctl.version            = H5C__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.1;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;

        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        invalid_auto_size_ctl.decr_mode              = H5C_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 1.000001; /* INVALID */

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr,
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg =
                "H5C_set_cache_auto_resize_config accepted bad decrement 1.\n";
        }
    }

    if ( pass ) {

        result = H5C_get_cache_auto_resize_config(cache_ptr,
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_auto_resize_config failed 21.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass = FALSE;
            failure_mssg = "Unexpected auto resize config 21.";
        }
    }

    if ( pass ) {

        invalid_auto_size_ctl.version            = H5C__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.1;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;

        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        invalid_auto_size_ctl.decr_mode              = H5C_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = -0.000001; /* INVALID */

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr,
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg =
                "H5C_set_cache_auto_resize_config accepted bad decrement 2.\n";
        }
    }

    if ( pass ) {

        result = H5C_get_cache_auto_resize_config(cache_ptr,
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_auto_resize_config failed 22.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass = FALSE;
            failure_mssg = "Unexpected auto resize config 22.";
        }
    }


    /* check for rejection of bad epochs_before_eviction */

    if ( pass ) {

        invalid_auto_size_ctl.version            = H5C__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.1;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;

        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        invalid_auto_size_ctl.decr_mode              = H5C_decr__age_out;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.9;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 0; /* INVALID */

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr,
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config accepted bad epochs_before_eviction 1.\n";
        }
    }

    if ( pass ) {

        result = H5C_get_cache_auto_resize_config(cache_ptr,
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_auto_resize_config failed 23.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass = FALSE;
            failure_mssg = "Unexpected auto resize config 23.";
        }
    }

    if ( pass ) {

        invalid_auto_size_ctl.version            = H5C__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.1;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;

        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        invalid_auto_size_ctl.decr_mode      = H5C_decr__age_out_with_threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.9;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction =
                                       H5C__MAX_EPOCH_MARKERS + 1; /* INVALID */

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr,
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config accepted bad epochs_before_eviction 2.\n";
        }
    }

    if ( pass ) {

        result = H5C_get_cache_auto_resize_config(cache_ptr,
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_auto_resize_config failed 24.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass = FALSE;
            failure_mssg = "Unexpected auto resize config 24.";
        }
    }


    /* Check for bad apply_empty_reserve rejection */

    if ( pass ) {

        invalid_auto_size_ctl.version            = H5C__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.1;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;

        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        invalid_auto_size_ctl.decr_mode              = H5C_decr__age_out;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.9;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = -0.0000001; /* INVALID */

        result = H5C_set_cache_auto_resize_config(cache_ptr,
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config accepted bad empty_reserve 1.\n";
        }
    }

    if ( pass ) {

        result = H5C_get_cache_auto_resize_config(cache_ptr,
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_auto_resize_config failed 25.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass = FALSE;
            failure_mssg = "Unexpected auto resize config 25.";
        }
    }

    if ( pass ) {

        invalid_auto_size_ctl.version            = H5C__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.1;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;

        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);


        invalid_auto_size_ctl.decr_mode      = H5C_decr__age_out_with_threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.9;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction =
                                       H5C__MAX_EPOCH_MARKERS + 1; /* INVALID */

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C_set_cache_auto_resize_config(cache_ptr,
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config accepted bad empty_reserve 2.\n";
        }
    }

    if ( pass ) {

        result = H5C_get_cache_auto_resize_config(cache_ptr,
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_auto_resize_config failed 26.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass = FALSE;
            failure_mssg = "Unexpected auto resize config 26.";
        }
    }


    /* finally, before we finish, try feeding
     * H5C_get_cache_auto_resize_config invalid data.
     */

    if ( pass ) {

        result = H5C_get_cache_auto_resize_config(NULL, &test_auto_size_ctl);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg =
                "H5C_get_cache_auto_resize_config accepted NULL cache_ptr.\n";
        }
    }

    if ( pass ) {

        result = H5C_get_cache_auto_resize_config((H5C_t *)&test_auto_size_ctl,
                                                  &test_auto_size_ctl);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg =
                "H5C_get_cache_auto_resize_config accepted bad cache_ptr.\n";
        }
    }

    if ( pass ) {

        result = H5C_get_cache_auto_resize_config(cache_ptr, NULL);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg =
                "H5C_get_cache_auto_resize_config accepted NULL config ptr.\n";
        }
    }

    if ( pass ) {

        takedown_cache(cache_ptr, FALSE, FALSE);
    }

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass ) {

        HDfprintf(stdout, "%s: failure_mssg = \"%s\".\n",
                  fcn_name, failure_mssg);
    }

    return;

} /* check_auto_cache_resize_input_errs() */


/*-------------------------------------------------------------------------
 * Function:	check_auto_cache_resize_aux_fcns()
 *
 * Purpose:	Verify that the auxilary functions associated with
 *		the automatic cache resize capability are operating
 *		correctly.  These functions are:
 *
 *			H5C_get_cache_size()
 *			H5C_get_cache_hit_rate()
 *			H5C_reset_cache_hit_rate_stats()
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              11/4/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
check_auto_cache_resize_aux_fcns(void)
{
    const char * fcn_name = "check_auto_cache_resize_aux_fcns()";
    herr_t result;
    int32_t i;
    H5C_t * cache_ptr = NULL;
    double hit_rate;
    size_t max_size;
    size_t min_clean_size;
    size_t cur_size;
    int32_t cur_num_entries;
    H5C_auto_size_ctl_t auto_size_ctl =
    {
        /* int32_t     version                = */ H5C__CURR_AUTO_SIZE_CTL_VER,
#if 1
        /* H5C_auto_resize_report_fcn rpt_fcn = */ NULL,
#else
        /* H5C_auto_resize_report_fcn rpt_fcn = */ H5C_def_auto_resize_rpt_fcn,
#endif
        /* hbool_t     set_initial_size       = */ TRUE,
        /* size_t      initial_size           = */ (1 * 1024 * 1024),

        /* double      min_clean_fraction     = */ 0.5,

        /* size_t      max_size               = */ (16 * 1024 * 1025),
        /* size_t      min_size               = */ (512 * 1024),

        /* int64_t     epoch_length           = */ 50000,


        /* enum H5C_cache_incr_mode incr_mode = */ H5C_incr__off,

        /* double     lower_hr_threshold      = */ 0.75,

        /* double      increment              = */ 2.0,

        /* hbool_t     apply_max_increment    = */ TRUE,
        /* size_t      max_increment          = */ (4 * 1024 * 1024),


        /* enum H5C_cache_decr_mode decr_mode = */ H5C_decr__off,

        /* double      upper_hr_threshold     = */ 0.995,

        /* double      decrement              = */ 0.9,

        /* hbool_t     apply_max_decrement    = */ TRUE,
        /* size_t      max_decrement          = */ (1 * 1024 * 1024),

        /* int32_t     epochs_before_eviction = */ 3,

        /* hbool_t     apply_empty_reserve    = */ TRUE,
        /* double      empty_reserve          = */ 0.5
    };


    TESTING("automatic cache resize auxilary functions");

    pass = TRUE;

    /* allocate a cache, and then test the various auxilary functions.
     */

    if ( pass ) {

        reset_entries();

        cache_ptr = setup_cache((size_t)(2 * 1024),
                                (size_t)(1 * 1024));
    }

    if ( pass ) {

        result = H5C_set_cache_auto_resize_config(cache_ptr,
                                                  &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_set_cache_auto_resize_config failed 1.\n";
        }
    }

    if ( pass ) {

        if ( ( cache_ptr->max_cache_size != (1 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (512 * 1024) ) ) {

            pass = FALSE;
            failure_mssg = "bad cache size after initialization.\n";
        }
    }

    /* lets start with the H5C_get_cache_hit_rate(),
     * H5C_reset_cache_hit_rate_stats() pair.
     */

    if ( pass ) {

        if ( ( H5C_get_cache_hit_rate(NULL, &hit_rate) != FAIL ) ||
             ( H5C_get_cache_hit_rate(cache_ptr, NULL) != FAIL ) ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_hit_rate accepts bad params.\n";
        }
    }

    if ( pass ) {

        result = H5C_get_cache_hit_rate(cache_ptr, &hit_rate);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_hit_rate failed.\n";

        } else if ( hit_rate != 0.0 ) {

            pass = FALSE;
            failure_mssg =
                "H5C_get_cache_hit_rate returned unexpected hit rate 1.\n";
        }
    }

    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, PICO_ENTRY_TYPE, i);

            if ( pass ) {

                unprotect_entry(cache_ptr, PICO_ENTRY_TYPE, i,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }
    }

    if ( pass ) {

        result = H5C_get_cache_hit_rate(cache_ptr, &hit_rate);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_hit_rate failed.\n";

        } else if ( hit_rate != 0.0 ) {

            pass = FALSE;
            failure_mssg =
                "H5C_get_cache_hit_rate returned unexpected hit rate 2.\n";

        } else if ( ( cache_ptr->cache_accesses != 1000 ) ||
                    ( cache_ptr->cache_hits != 0 ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache hit rate stats.\n";

        } else if ( rpt_fcn_called ) {

            pass = FALSE;
            failure_mssg = "Report function called?.\n";

        }
    }

    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, PICO_ENTRY_TYPE, 0);

            if ( pass ) {

                unprotect_entry(cache_ptr, PICO_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }
    }

    if ( pass ) {

        result = H5C_get_cache_hit_rate(cache_ptr, &hit_rate);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_hit_rate failed.\n";

        } else if ( hit_rate != 0.5 ) {

            pass = FALSE;
            failure_mssg =
                "H5C_get_cache_hit_rate returned unexpected hit rate 3.\n";

        } else if ( ( cache_ptr->cache_accesses != 2000 ) ||
                    ( cache_ptr->cache_hits != 1000 ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache hit rate stats.\n";

        } else if ( rpt_fcn_called ) {

            pass = FALSE;
            failure_mssg = "Report function called?.\n";

        }
    }

    if ( pass ) {

        result = H5C_reset_cache_hit_rate_stats(NULL);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg =
                "H5C_reset_cache_hit_rate_stats accepted NULL cache_ptr.\n";

        } else if ( ( cache_ptr->cache_accesses != 2000 ) ||
                    ( cache_ptr->cache_hits != 1000 ) ) {

            pass = FALSE;
            failure_mssg =
              "Failed call to H5C_reset_cache_hit_rate_stats altered stats?\n";
        }
    }

    if ( pass ) {

        result = H5C_reset_cache_hit_rate_stats(cache_ptr);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_reset_cache_hit_rate_stats failed.\n";

        } else if ( ( cache_ptr->cache_accesses != 0 ) ||
                    ( cache_ptr->cache_hits != 0 ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache hit rate stats.\n";

        }
    }

    if ( pass ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass ) && ( i < 1000 ) )
        {
            protect_entry(cache_ptr, PICO_ENTRY_TYPE, i + 500);

            if ( pass ) {

                unprotect_entry(cache_ptr, PICO_ENTRY_TYPE, i + 500,
                                NO_CHANGE, H5C__NO_FLAGS_SET);
            }
            i++;
        }
    }


    if ( pass ) {

        result = H5C_get_cache_hit_rate(cache_ptr, &hit_rate);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_hit_rate failed.\n";

        } else if ( hit_rate != 0.5 ) {

            pass = FALSE;
            failure_mssg =
                "H5C_get_cache_hit_rate returned unexpected hit rate 4.\n";

        } else if ( ( cache_ptr->cache_accesses != 1000 ) ||
                    ( cache_ptr->cache_hits != 500 ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected cache hit rate stats.\n";

        } else if ( rpt_fcn_called ) {

            pass = FALSE;
            failure_mssg = "Report function called?.\n";

        }
    }

    /***************************************************
     * So much for testing H5C_get_cache_hit_rate() and
     * H5C_reset_cache_hit_rate_stats().  Now on to
     * H5C_get_cache_size().
     ***************************************************/

    if ( pass ) {

        result = H5C_get_cache_size(NULL, &max_size, &min_clean_size,
                                    &cur_size, &cur_num_entries);

        if ( result != FAIL ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_size accepted NULL cache_ptr.\n";
        }
    }

    if ( pass ) {

        max_size        = 0;
        min_clean_size  = 0;
        cur_size        = 0;
        cur_num_entries = 0;

        result = H5C_get_cache_size(cache_ptr, &max_size, &min_clean_size,
                                    &cur_size, &cur_num_entries);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_size failed 1.\n";

        } else if ( max_size != (1 * 1024 * 1024) ) {

            pass = FALSE;
            failure_mssg =
                "H5C_get_cache_size reports unexpected max_size 1.\n";

        } else if ( min_clean_size != (512 * 1024) ) {

            pass = FALSE;
            failure_mssg =
                "H5C_get_cache_size reports unexpected min_clean_size 1.\n";

        } else if ( cur_size != (1500 * PICO_ENTRY_SIZE) ) {

            pass = FALSE;
            failure_mssg =
                "H5C_get_cache_size reports unexpected cur_size 1.\n";

        } else if ( cur_num_entries != 1500 ) {

            pass = FALSE;
            failure_mssg =
                "H5C_get_cache_size reports unexpected cur_num_entries 1.\n";
        }
    }

    /* read a larger entry so that cur_size and cur_num_entries will be
     * different.
     */
    if ( pass ) {

        protect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0);
    }

    if ( pass ) {
        unprotect_entry(cache_ptr, MONSTER_ENTRY_TYPE, 0, NO_CHANGE,
                        H5C__NO_FLAGS_SET);
    }

    if ( pass ) {

        max_size        = 0;
        min_clean_size  = 0;
        cur_size        = 0;
        cur_num_entries = 0;

        result = H5C_get_cache_size(cache_ptr, &max_size, &min_clean_size,
                                    &cur_size, &cur_num_entries);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_size failed 2.\n";

        } else if ( max_size != (1 * 1024 * 1024) ) {

            pass = FALSE;
            failure_mssg =
                "H5C_get_cache_size reports unexpected max_size 2.\n";

        } else if ( min_clean_size != (512 * 1024) ) {

            pass = FALSE;
            failure_mssg =
                "H5C_get_cache_size reports unexpected min_clean_size 2.\n";

        } else if ( cur_size !=
                   ((1500 * PICO_ENTRY_SIZE) + MONSTER_ENTRY_SIZE) ) {

            pass = FALSE;
            failure_mssg =
                "H5C_get_cache_size reports unexpected cur_size 2.\n";

        } else if ( cur_num_entries != 1501 ) {

            pass = FALSE;
            failure_mssg =
                "H5C_get_cache_size reports unexpected cur_num_entries 2.\n";
        }
    }

    if ( pass ) {

        max_size        = 0;
        min_clean_size  = 0;
        cur_size        = 0;
        cur_num_entries = 0;

        result = H5C_get_cache_size(cache_ptr, &max_size, NULL, NULL, NULL);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_size failed 3.\n";

        } else if ( max_size != (1 * 1024 * 1024) ) {

            pass = FALSE;
            failure_mssg =
                "H5C_get_cache_size reports unexpected max_size 3.\n";

        } else if ( ( min_clean_size != 0 ) ||
                    ( cur_size != 0 ) ||
                    ( cur_num_entries != 0 ) ) {

            pass = FALSE;
            failure_mssg = "Phantom returns from H5C_get_cache_size?\n";

        }
    }

    if ( pass ) {

        max_size        = 0;
        min_clean_size  = 0;
        cur_size        = 0;
        cur_num_entries = 0;

        result = H5C_get_cache_size(cache_ptr, NULL, &min_clean_size,
                                    NULL, NULL);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_size failed 4.\n";

        } else if ( min_clean_size != (512 * 1024) ) {

            pass = FALSE;
            failure_mssg =
                "H5C_get_cache_size reports unexpected min_clean_size 4.\n";

        } else if ( ( max_size != 0 ) ||
                    ( cur_size != 0 ) ||
                    ( cur_num_entries != 0 ) ) {

            pass = FALSE;
            failure_mssg = "Phantom returns from H5C_get_cache_size?\n";

        }
    }

    if ( pass ) {

        max_size        = 0;
        min_clean_size  = 0;
        cur_size        = 0;
        cur_num_entries = 0;

        result = H5C_get_cache_size(cache_ptr, NULL, NULL, &cur_size, NULL);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_size failed 5.\n";

        } else if ( cur_size !=
                   ((1500 * PICO_ENTRY_SIZE) + MONSTER_ENTRY_SIZE) ) {

            pass = FALSE;
            failure_mssg =
                "H5C_get_cache_size reports unexpected cur_size 5.\n";

        } else if ( ( max_size != 0 ) ||
                    ( min_clean_size != 0 ) ||
                    ( cur_num_entries != 0 ) ) {

            pass = FALSE;
            failure_mssg = "Phantom returns from H5C_get_cache_size?\n";

        }
    }

    if ( pass ) {

        max_size        = 0;
        min_clean_size  = 0;
        cur_size        = 0;
        cur_num_entries = 0;

        result = H5C_get_cache_size(cache_ptr, NULL, NULL, NULL,
                                    &cur_num_entries);

        if ( result != SUCCEED ) {

            pass = FALSE;
            failure_mssg = "H5C_get_cache_size failed 6.\n";

        } else if ( cur_num_entries != 1501 ) {

            pass = FALSE;
            failure_mssg =
                "H5C_get_cache_size reports unexpected cur_num_entries 2.\n";

        } else if ( ( max_size != 0 ) ||
                    ( min_clean_size != 0 ) ||
                    ( cur_size != 0 ) ) {

            pass = FALSE;
            failure_mssg = "Phantom returns from H5C_get_cache_size?\n";

        }
    }

    if ( pass ) {

        takedown_cache(cache_ptr, FALSE, FALSE);
    }

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass ) {

        HDfprintf(stdout, "%s: failure_mssg = \"%s\".\n",
                  fcn_name, failure_mssg);
    }

    return;

} /* check_auto_cache_resize_aux_fcns() */


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Run tests on the cache code contained in H5C.c
 *
 * Return:	Success:
 *
 *		Failure:
 *
 * Programmer:	John Mainzer
 *              6/24/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

int
main(void)
{
    H5open();

    skip_long_tests = FALSE;

#ifdef NDEBUG
    run_full_test = TRUE;
#else /* NDEBUG */
    run_full_test = FALSE;
#endif /* NDEBUG */

#if 1
    smoke_check_1();
    smoke_check_2();
    smoke_check_3();
    smoke_check_4();
    smoke_check_5();
    smoke_check_6();
    smoke_check_7();
    smoke_check_8();
#endif
#if 1
    write_permitted_check();
    check_flush_cache();
    check_get_entry_status();
    check_expunge_entry();
    check_rename_entry();
    check_pin_protected_entry();
    check_resize_entry();
    check_flush_protected_err();
    check_destroy_pinned_err();
    check_destroy_protected_err();
    check_duplicate_insert_err();
    check_rename_err();
    check_double_pin_err();
    check_double_unpin_err();
    check_pin_entry_errs();
    check_double_protect_err();
    check_double_unprotect_err();
    check_mark_entry_dirty_errs();
    check_expunge_entry_errs();
    check_resize_entry_errs();
    check_auto_cache_resize();
    check_auto_cache_resize_disable();
    check_auto_cache_resize_epoch_markers();
    check_auto_cache_resize_input_errs();
    check_auto_cache_resize_aux_fcns();
#endif

    return(0);

} /* main() */
