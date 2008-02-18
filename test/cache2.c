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

/* Programmer:  John Mainzer
 *              6/9/04
 *
 *		This file contains tests for the cache implemented in
 *		H5C.c
 */
#include "h5test.h"
#include "H5Iprivate.h"
#include "H5ACprivate.h"
#include "cache2_common.h"

/* private function declarations: */

static void smoke_check_1(void);
static void smoke_check_2(void);
static void smoke_check_3(void);
static void smoke_check_4(void);
static void smoke_check_5(void);
static void smoke_check_6(void);
static void smoke_check_7(void);
static void smoke_check_8(void);
static void smoke_check_9(void);
static void smoke_check_10(void);
static void write_permitted_check(void);
static void check_insert_entry(void);
static void check_flush_cache(void);
static void check_flush_cache__empty_cache(H5C2_t * cache_ptr);
static void check_flush_cache__multi_entry(H5C2_t * cache_ptr);
static void check_flush_cache__multi_entry_test(H5C2_t * cache_ptr,
                                          int test_num,
                                          unsigned int flush_flags,
                                          int spec_size,
                                          struct flush_cache_test_spec spec[]);
static void check_flush_cache__pe_multi_entry_test(H5C2_t * cache_ptr,
                                        int test_num,
                                        unsigned int flush_flags,
                                        int spec_size,
                                        struct pe_flush_cache_test_spec spec[]);
static void check_flush_cache__single_entry(H5C2_t * cache_ptr);
static void check_flush_cache__single_entry_test(H5C2_t * cache_ptr,
                                                 int test_num,
                                                 int entry_type,
                                                 int entry_idx,
                                                 hbool_t insert_flag,
                                                 hbool_t dirty_flag,
                                                 unsigned int flags,
                                                 unsigned int flush_flags,
                                                 hbool_t expected_deserialized,
                                                 hbool_t expected_cleared,
                                                 hbool_t expected_serialized,
                                                 hbool_t expected_destroyed);
static void check_flush_cache__pinned_single_entry_test(H5C2_t * cache_ptr,
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
                                                 hbool_t expected_serialized,
                                                 hbool_t expected_destroyed);
static void check_flush_cache__flush_ops(H5C2_t * cache_ptr);
static void check_flush_cache__flush_op_test(H5C2_t * cache_ptr,
                                        int test_num,
                                        unsigned int flush_flags,
                                        int spec_size,
                                        struct fo_flush_cache_test_spec spec[],
				        int init_expected_index_len,
				        size_t init_expected_index_size,
				        int expected_index_len,
				        size_t expected_index_size,
					int check_size,
                                        struct fo_flush_entry_check check[]);
static void check_flush_cache__flush_op_eviction_test(H5C2_t * cache_ptr);
static void check_flush_protected_err(void);
static void check_get_entry_status(void);
static void check_expunge_entry(void);
static void check_multiple_read_protect(void);
static void check_rename_entry(void);
static void check_rename_entry__run_test(H5C2_t * cache_ptr, int test_num,
                                      struct rename_entry_test_spec * spec_ptr);
static void check_pin_protected_entry(void);
static void check_resize_entry(void);
static void check_evictions_enabled(void);
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
static void check_unprotect_ro_dirty_err(void);
static void check_protect_ro_rw_err(void);
static void check_check_evictions_enabled_err(void);
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
 *		Added code to skip this test if the skip_long_tests2 global
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
    H5C2_t * cache_ptr = NULL;

    TESTING("smoke check #1 -- all clean, ins, dest, ren, 4/2 MB cache");

    if ( skip_long_tests2 ) {

        SKIPPED();

        HDfprintf(stdout, "	Long tests disabled.\n");

        return;
    }

    pass2 = TRUE;

    if ( show_progress ) /* 1 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    reset_entries2();

    if ( show_progress ) /* 2 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    cache_ptr = setup_cache2((size_t)(4 * 1024 * 1024),
                            (size_t)(2 * 1024 * 1024));

    if ( show_progress ) /* 3 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    row_major_scan_forward2(/* cache_ptr              */ cache_ptr,
                            /* lag                    */ lag,
                            /* verbose                */ FALSE,
                            /* reset_stats            */ TRUE,
                            /* display_stats          */ display_stats,
                            /* display_detailed_stats */ FALSE,
                            /* do_inserts             */ TRUE,
                            /* dirty_inserts          */ dirty_inserts,
                            /* do_renames             */ TRUE,
                            /* rename_to_main_addr    */ FALSE,
                            /* do_destroys            */ TRUE,
			    /* do_mult_ro_protects    */ TRUE,
                            /* dirty_destroys         */ dirty_destroys,
                            /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 4 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    row_major_scan_backward2(/* cache_ptr              */ cache_ptr,
                             /* lag                    */ lag,
                             /* verbose                */ FALSE,
                             /* reset_stats            */ TRUE,
                             /* display_stats          */ display_stats,
                             /* display_detailed_stats */ FALSE,
                             /* do_inserts             */ FALSE,
                             /* dirty_inserts          */ dirty_inserts,
                             /* do_renames             */ TRUE,
                             /* rename_to_main_addr    */ TRUE,
                             /* do_destroys            */ FALSE,
			     /* do_mult_ro_protects    */ TRUE,
                             /* dirty_destroys         */ dirty_destroys,
                             /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 5 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    row_major_scan_forward2(/* cache_ptr              */ cache_ptr,
                            /* lag                    */ lag,
                            /* verbose                */ FALSE,
                            /* reset_stats            */ TRUE,
                            /* display_stats          */ display_stats,
                            /* display_detailed_stats */ FALSE,
                            /* do_inserts             */ TRUE,
                            /* dirty_inserts          */ dirty_inserts,
                            /* do_renames             */ TRUE,
                            /* rename_to_main_addr    */ FALSE,
                            /* do_destroys            */ FALSE,
			    /* do_mult_ro_protects    */ TRUE,
                            /* dirty_destroys         */ dirty_destroys,
                            /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 6 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    /* flush and destroy all entries in the cache: */

    flush_cache2(/* cache_ptr           */ cache_ptr,
                 /* destroy_entries     */ TRUE,
                 /* dump_stats          */ FALSE,
                 /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 7 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    col_major_scan_forward2(/* cache_ptr              */ cache_ptr,
                            /* lag                    */ lag,
                            /* verbose                */ FALSE,
                            /* reset_stats            */ TRUE,
                            /* display_stats          */ display_stats,
                            /* display_detailed_stats */ TRUE,
                            /* do_inserts             */ TRUE,
                            /* dirty_inserts          */ dirty_inserts,
                            /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 8 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    /* flush all entries in the cache: */

    flush_cache2(/* cache_ptr           */ cache_ptr,
                 /* destroy_entries     */ FALSE,
                 /* dump_stats          */ FALSE,
                 /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 9 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    col_major_scan_backward2(/* cache_ptr              */ cache_ptr,
                             /* lag                    */ lag,
                             /* verbose                */ FALSE,
                             /* reset_stats            */ TRUE,
                             /* display_stats          */ display_stats,
                             /* display_detailed_stats */ TRUE,
                             /* do_inserts             */ TRUE,
                             /* dirty_inserts          */ dirty_inserts,
                             /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 10 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    takedown_cache2(cache_ptr, display_stats, TRUE);

    if ( show_progress ) /* 11 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    verify_clean2();
    verify_unprotected2();

    if ( pass2 ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass2 ) {

        HDfprintf(stdout, "%s(): failure_mssg2 = \"%s\".\n",
                  fcn_name, failure_mssg2);
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
 *		Added code to skip this test if the skip_long_tests2 global
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
    H5C2_t * cache_ptr = NULL;

    TESTING("smoke check #2 -- ~1/2 dirty, ins, dest, ren, 4/2 MB cache");

    if ( skip_long_tests2 ) {

        SKIPPED();

        HDfprintf(stdout, "	Long tests disabled.\n");

        return;
    }

    pass2 = TRUE;

    if ( show_progress ) /* 1 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    reset_entries2();

    if ( show_progress ) /* 2 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    cache_ptr = setup_cache2((size_t)(4 * 1024 * 1024),
                            (size_t)(2 * 1024 * 1024));

    if ( show_progress ) /* 3 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    row_major_scan_forward2(/* cache_ptr              */ cache_ptr,
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
			    /* do_mult_ro_protects    */ TRUE,
                            /* dirty_destroys         */ dirty_destroys,
                            /* dirty_unprotects       */ dirty_unprotects);
 
    if ( show_progress ) /* 4 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    row_major_scan_backward2(/* cache_ptr              */ cache_ptr,
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
			     /* do_mult_ro_protects    */ TRUE,
                             /* dirty_destroys         */ dirty_destroys,
                             /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 5 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    row_major_scan_forward2(/* cache_ptr              */ cache_ptr,
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
			    /* do_mult_ro_protects    */ TRUE,
                            /* dirty_destroys         */ dirty_destroys,
                            /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 6 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    /* flush and destroy all entries in the cache: */

    flush_cache2(/* cache_ptr           */ cache_ptr,
                /* destroy_entries     */ TRUE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 7 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    col_major_scan_forward2(/* cache_ptr              */ cache_ptr,
                            /* lag                    */ lag,
                            /* verbose                */ FALSE,
                            /* reset_stats            */ TRUE,
                            /* display_stats          */ display_stats,
                            /* display_detailed_stats */ TRUE,
                            /* do_inserts             */ TRUE,
                            /* dirty_inserts          */ dirty_inserts,
                            /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 8 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    /* flush all entries in the cache: */

    flush_cache2(/* cache_ptr           */ cache_ptr,
                 /* destroy_entries     */ FALSE,
                 /* dump_stats          */ FALSE,
                 /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 9 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    col_major_scan_backward2(/* cache_ptr              */ cache_ptr,
                             /* lag                    */ lag,
                             /* verbose                */ FALSE,
                             /* reset_stats            */ TRUE,
                             /* display_stats          */ display_stats,
                             /* display_detailed_stats */ TRUE,
                             /* do_inserts             */ TRUE,
                             /* dirty_inserts          */ dirty_inserts,
                             /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 10 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    takedown_cache2(cache_ptr, display_stats, TRUE);

    if ( show_progress ) /* 11 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    verify_clean2();
    verify_unprotected2();

    if ( pass2 ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass2 ) {

        HDfprintf(stdout, "%s(): failure_mssg2 = \"%s\".\n",
                  fcn_name, failure_mssg2);
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
 *		Added code to skip this test if the skip_long_tests2 global
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
    H5C2_t * cache_ptr = NULL;

    TESTING("smoke check #3 -- all clean, ins, dest, ren, 2/1 KB cache");

    if ( skip_long_tests2 ) {

        SKIPPED();

        HDfprintf(stdout, "	Long tests disabled.\n");

        return;
    }

    pass2 = TRUE;

    if ( show_progress ) /* 1 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    reset_entries2();

    if ( show_progress ) /* 2 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    cache_ptr = setup_cache2((size_t)(2 * 1024),
                            (size_t)(1 * 1024));

    if ( show_progress ) /* 3 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    row_major_scan_forward2(/* cache_ptr              */ cache_ptr,
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
			   /* do_mult_ro_protects    */ TRUE,
                           /* dirty_destroys         */ dirty_destroys,
                           /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 4 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    row_major_scan_backward2(/* cache_ptr              */ cache_ptr,
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
			    /* do_mult_ro_protects    */ TRUE,
                            /* dirty_destroys         */ dirty_destroys,
                            /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 5 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    row_major_scan_forward2(/* cache_ptr              */ cache_ptr,
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
			   /* do_mult_ro_protects    */ TRUE,
                           /* dirty_destroys         */ dirty_destroys,
                           /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 6 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    /* flush and destroy all entries in the cache: */

    flush_cache2(/* cache_ptr           */ cache_ptr,
                /* destroy_entries     */ TRUE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 7 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    col_major_scan_forward2(/* cache_ptr              */ cache_ptr,
                           /* lag                    */ lag,
                           /* verbose                */ FALSE,
                           /* reset_stats            */ TRUE,
                           /* display_stats          */ display_stats,
                           /* display_detailed_stats */ TRUE,
                           /* do_inserts             */ TRUE,
                           /* dirty_inserts          */ dirty_inserts,
                           /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 8 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    /* flush all entries in the cache: */

    flush_cache2(/* cache_ptr           */ cache_ptr,
                /* destroy_entries     */ FALSE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 9 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    col_major_scan_backward2(/* cache_ptr              */ cache_ptr,
                            /* lag                    */ lag,
                            /* verbose                */ FALSE,
                            /* reset_stats            */ TRUE,
                            /* display_stats          */ display_stats,
                            /* display_detailed_stats */ TRUE,
                            /* do_inserts             */ TRUE,
                            /* dirty_inserts          */ dirty_inserts,
                            /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 10 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    takedown_cache2(cache_ptr, display_stats, TRUE);

    if ( show_progress ) /* 11 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    verify_clean2();
    verify_unprotected2();

    if ( pass2 ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass2 ) {

        HDfprintf(stdout, "%s(): failure_mssg2 = \"%s\".\n",
                  fcn_name, failure_mssg2);
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
 *		Added code to skip this test if the skip_long_tests2 global
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
    H5C2_t * cache_ptr = NULL;

    TESTING("smoke check #4 -- ~1/2 dirty, ins, dest, ren, 2/1 KB cache");

    if ( skip_long_tests2 ) {

        SKIPPED();

        HDfprintf(stdout, "	Long tests disabled.\n");

        return;
    }

    pass2 = TRUE;

    if ( show_progress ) /* 1 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    reset_entries2();

    if ( show_progress ) /* 2 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    cache_ptr = setup_cache2((size_t)(2 * 1024),
                            (size_t)(1 * 1024));

    if ( show_progress ) /* 3 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    row_major_scan_forward2(/* cache_ptr              */ cache_ptr,
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
			   /* do_mult_ro_protects    */ TRUE,
                           /* dirty_destroys         */ dirty_destroys,
                           /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 4 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    row_major_scan_backward2(/* cache_ptr              */ cache_ptr,
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
			    /* do_mult_ro_protects    */ TRUE,
                            /* dirty_destroys         */ dirty_destroys,
                            /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 5 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    row_major_scan_forward2(/* cache_ptr              */ cache_ptr,
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
			   /* do_mult_ro_protects    */ TRUE,
                           /* dirty_destroys         */ dirty_destroys,
                           /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 6 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    /* flush and destroy all entries in the cache: */

    flush_cache2(/* cache_ptr           */ cache_ptr,
                /* destroy_entries     */ TRUE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 7 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    col_major_scan_forward2(/* cache_ptr              */ cache_ptr,
                           /* lag                    */ lag,
                           /* verbose                */ FALSE,
                           /* reset_stats            */ TRUE,
                           /* display_stats          */ display_stats,
                           /* display_detailed_stats */ TRUE,
                           /* do_inserts             */ TRUE,
                           /* dirty_inserts          */ dirty_inserts,
                           /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 8 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    /* flush all entries in the cache: */

    flush_cache2(/* cache_ptr           */ cache_ptr,
                /* destroy_entries     */ FALSE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 9 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    col_major_scan_backward2(/* cache_ptr              */ cache_ptr,
                            /* lag                    */ lag,
                            /* verbose                */ FALSE,
                            /* reset_stats            */ TRUE,
                            /* display_stats          */ display_stats,
                            /* display_detailed_stats */ TRUE,
                            /* do_inserts             */ TRUE,
                            /* dirty_inserts          */ dirty_inserts,
                            /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 10 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    takedown_cache2(cache_ptr, display_stats, TRUE);

    if ( show_progress ) /* 11 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    verify_clean2();
    verify_unprotected2();

    if ( pass2 ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass2 ) {

        HDfprintf(stdout, "%s(): failure_mssg2 = \"%s\".\n",
                  fcn_name, failure_mssg2);
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
 *		Added code to skip this test if the skip_long_tests2 global
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
    H5C2_t * cache_ptr = NULL;
    H5C2_auto_size_ctl_t auto_size_ctl =
    {
        /* int32_t     version                = */ H5C2__CURR_AUTO_SIZE_CTL_VER,
#if 1
        /* H5C2_auto_resize_report_fcn rpt_fcn = */ NULL,
#else
        /* H5C2_auto_resize_report_fcn rpt_fcn = */ H5C2_def_auto_resize_rpt_fcn,
#endif
        /* hbool_t     set_initial_size       = */ TRUE,
        /* size_t      initial_size           = */ (2 * 1024 * 1024),

        /* double      min_clean_fraction     = */ 0.1,

        /* size_t      max_size               = */ (32 * 1024 * 1025),
        /* size_t      min_size               = */ (512 * 1024),

        /* int64_t     epoch_length           = */ 50000,


        /* enum H5C2_cache_incr_mode incr_mode = */ H5C2_incr__threshold,

        /* double     lower_hr_threshold      = */ 0.75,

        /* double      increment              = */ 2.0,

        /* hbool_t     apply_max_increment    = */ TRUE,
        /* size_t      max_increment          = */ (4 * 1024 * 1024),

        /* enum H5C2_cache_flash_incr_mode      */
	/*                    flash_incr_mode = */ H5C2_flash_incr__off,
	/* double      flash_multiple         = */ 2.0,
	/* double      flash_threshold        = */ 0.5,


        /* enum H5C2_cache_decr_mode decr_mode = */ H5C2_decr__threshold,

        /* double      upper_hr_threshold     = */ 0.995,

        /* double      decrement              = */ 0.9,

        /* hbool_t     apply_max_decrement    = */ TRUE,
        /* size_t      max_decrement          = */ (1 * 1024 * 1024),

        /* int32_t     epochs_before_eviction = */ 3,

        /* hbool_t     apply_empty_reserve    = */ TRUE,
        /* double      empty_reserve          = */ 0.5
    };

    TESTING("smoke check #5 -- all clean, ins, prot, unprot, AR cache 1");

    if ( skip_long_tests2 ) {

        SKIPPED();

        HDfprintf(stdout, "	Long tests disabled.\n");

        return;
    }

    if ( run_full_test2 ) {

        max_index = (10 * 1024) - 1;
    }

    pass2 = TRUE;

    if ( show_progress ) /* 1 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    reset_entries2();

    if ( show_progress ) /* 2 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    cache_ptr = setup_cache2((size_t)(2 * 1024),
                            (size_t)(1 * 1024));

    if ( pass2 ) {

        result = H5C2_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_set_cache_auto_resize_config failed 1.\n";
        }
    }

    if ( show_progress ) /* 3 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    hl_row_major_scan_forward2(/* cache_ptr              */ cache_ptr,
                              /* max_index              */ max_index,
                              /* verbose                */ FALSE,
                              /* reset_stats            */ TRUE,
                              /* display_stats          */ display_stats,
                              /* display_detailed_stats */ FALSE,
                              /* do_inserts             */ FALSE,
                              /* dirty_inserts          */ dirty_inserts);

    if ( show_progress ) /* 4 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    hl_row_major_scan_backward2(/* cache_ptr              */ cache_ptr,
                               /* max_index              */ max_index,
                               /* verbose                */ FALSE,
                               /* reset_stats            */ TRUE,
                               /* display_stats          */ display_stats,
                               /* display_detailed_stats */ FALSE,
                               /* do_inserts             */ FALSE,
                               /* dirty_inserts          */ dirty_inserts);

    if ( show_progress ) /* 5 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    hl_row_major_scan_forward2(/* cache_ptr              */ cache_ptr,
                              /* max_index              */ max_index,
                              /* verbose                */ FALSE,
                              /* reset_stats            */ TRUE,
                              /* display_stats          */ display_stats,
                              /* display_detailed_stats */ FALSE,
                              /* do_inserts             */ TRUE,
                              /* dirty_inserts          */ dirty_inserts);

    if ( show_progress ) /* 6 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    /* flush and destroy all entries in the cache: */

    flush_cache2(/* cache_ptr           */ cache_ptr,
                /* destroy_entries     */ TRUE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 7 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    hl_col_major_scan_forward2(/* cache_ptr              */ cache_ptr,
                              /* max_index              */ max_index,
                              /* verbose                */ FALSE,
                              /* reset_stats            */ TRUE,
                              /* display_stats          */ display_stats,
                              /* display_detailed_stats */ FALSE,
                              /* do_inserts             */ TRUE,
                              /* dirty_inserts          */ dirty_inserts,
                              /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 8 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    /* flush all entries in the cache: */

    flush_cache2(/* cache_ptr           */ cache_ptr,
                /* destroy_entries     */ FALSE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 9 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    hl_col_major_scan_backward2(/* cache_ptr              */ cache_ptr,
                               /* max_index              */ max_index,
                               /* verbose                */ FALSE,
                               /* reset_stats            */ TRUE,
                               /* display_stats          */ display_stats,
                               /* display_detailed_stats */ FALSE,
                               /* do_inserts             */ TRUE,
                               /* dirty_inserts          */ dirty_inserts,
                               /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 10 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    takedown_cache2(cache_ptr, display_stats, TRUE);

    if ( show_progress ) /* 11 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    verify_clean2();
    verify_unprotected2();

    if ( pass2 ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass2 ) {

        HDfprintf(stdout, "%s(): failure_mssg2 = \"%s\".\n",
                  fcn_name, failure_mssg2);
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
 *		Added code to skip this test if the skip_long_tests2 global
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
    H5C2_t * cache_ptr = NULL;
    H5C2_auto_size_ctl_t auto_size_ctl =
    {
        /* int32_t     version                = */ H5C2__CURR_AUTO_SIZE_CTL_VER,
#if 1
        /* H5C2_auto_resize_report_fcn rpt_fcn = */ NULL,
#else
        /* H5C2_auto_resize_report_fcn rpt_fcn = */ H5C2_def_auto_resize_rpt_fcn,
#endif
        /* hbool_t     set_initial_size       = */ TRUE,
        /* size_t      initial_size           = */ (2 * 1024 * 1024),

        /* double      min_clean_fraction     = */ 0.1,

        /* size_t      max_size               = */ (32 * 1024 * 1025),
        /* size_t      min_size               = */ (512 * 1024),

        /* int64_t     epoch_length           = */ 50000,


        /* enum H5C2_cache_incr_mode incr_mode = */ H5C2_incr__threshold,

        /* double     lower_hr_threshold      = */ 0.75,

        /* double      increment              = */ 2.0,

        /* hbool_t     apply_max_increment    = */ TRUE,
        /* size_t      max_increment          = */ (4 * 1024 * 1024),

        /* enum H5C2_cache_flash_incr_mode      */
	/*                    flash_incr_mode = */ H5C2_flash_incr__off,
	/* double      flash_multiple         = */ 2.0,
	/* double      flash_threshold        = */ 0.5,


        /* enum H5C2_cache_decr_mode decr_mode = */ H5C2_decr__threshold,

        /* double      upper_hr_threshold     = */ 0.995,

        /* double      decrement              = */ 0.9,

        /* hbool_t     apply_max_decrement    = */ TRUE,
        /* size_t      max_decrement          = */ (1 * 1024 * 1024),

        /* int32_t     epochs_before_eviction = */ 3,

        /* hbool_t     apply_empty_reserve    = */ TRUE,
        /* double      empty_reserve          = */ 0.05
    };

    TESTING("smoke check #6 -- ~1/2 dirty, ins, prot, unprot, AR cache 1");

    pass2 = TRUE;

    if ( skip_long_tests2 ) {

        SKIPPED();

        HDfprintf(stdout, "	Long tests disabled.\n");

        return;
    }

    if ( run_full_test2 ) {

        max_index = (10 * 1024) - 1;
    }

    if ( show_progress ) /* 1 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    reset_entries2();

    if ( show_progress ) /* 2 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    cache_ptr = setup_cache2((size_t)(2 * 1024),
                            (size_t)(1 * 1024));

    if ( pass2 ) {

        result = H5C2_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_set_cache_auto_resize_config failed 1.\n";
        }
    }

    if ( show_progress ) /* 3 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    hl_row_major_scan_forward2(/* cache_ptr              */ cache_ptr,
                              /* max_index              */ max_index,
                              /* verbose                */ FALSE,
                              /* reset_stats            */ TRUE,
                              /* display_stats          */ display_stats,
                              /* display_detailed_stats */ FALSE,
                              /* do_inserts             */ FALSE,
                              /* dirty_inserts          */ dirty_inserts);

    if ( show_progress ) /* 4 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    hl_row_major_scan_backward2(/* cache_ptr              */ cache_ptr,
                               /* max_index              */ max_index,
                               /* verbose                */ FALSE,
                               /* reset_stats            */ TRUE,
                               /* display_stats          */ display_stats,
                               /* display_detailed_stats */ FALSE,
                               /* do_inserts             */ FALSE,
                               /* dirty_inserts          */ dirty_inserts);

    if ( show_progress ) /* 5 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    hl_row_major_scan_forward2(/* cache_ptr              */ cache_ptr,
                              /* max_index              */ max_index,
                              /* verbose                */ FALSE,
                              /* reset_stats            */ TRUE,
                              /* display_stats          */ display_stats,
                              /* display_detailed_stats */ FALSE,
                              /* do_inserts             */ TRUE,
                              /* dirty_inserts          */ dirty_inserts);

    if ( show_progress ) /* 6 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    /* flush and destroy all entries in the cache: */

    flush_cache2(/* cache_ptr           */ cache_ptr,
                /* destroy_entries     */ TRUE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 7 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    hl_col_major_scan_forward2(/* cache_ptr              */ cache_ptr,
                              /* max_index              */ max_index,
                              /* verbose                */ FALSE,
                              /* reset_stats            */ TRUE,
                              /* display_stats          */ display_stats,
                              /* display_detailed_stats */ FALSE,
                              /* do_inserts             */ TRUE,
                              /* dirty_inserts          */ dirty_inserts,
                              /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 8 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    /* flush all entries in the cache: */

    flush_cache2(/* cache_ptr           */ cache_ptr,
                /* destroy_entries     */ FALSE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 9 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    hl_col_major_scan_backward2(/* cache_ptr              */ cache_ptr,
                               /* max_index              */ max_index,
                               /* verbose                */ FALSE,
                               /* reset_stats            */ TRUE,
                               /* display_stats          */ display_stats,
                               /* display_detailed_stats */ FALSE,
                               /* do_inserts             */ TRUE,
                               /* dirty_inserts          */ dirty_inserts,
                               /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 10 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    takedown_cache2(cache_ptr, display_stats, TRUE);

    if ( show_progress ) /* 11 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    verify_clean2();
    verify_unprotected2();

    if ( pass2 ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass2 ) {

        HDfprintf(stdout, "%s(): failure_mssg2 = \"%s\".\n",
                  fcn_name, failure_mssg2);
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
 *		Added code to skip this test if the skip_long_tests2 global
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
    H5C2_t * cache_ptr = NULL;
    H5C2_auto_size_ctl_t auto_size_ctl =
    {
        /* int32_t     version                = */ H5C2__CURR_AUTO_SIZE_CTL_VER,
#if 1
        /* H5C2_auto_resize_report_fcn rpt_fcn = */ NULL,
#else
        /* H5C2_auto_resize_report_fcn rpt_fcn = */ H5C2_def_auto_resize_rpt_fcn,
#endif
        /* hbool_t     set_initial_size       = */ TRUE,
        /* size_t      initial_size           = */ (2 * 1024 * 1024),

        /* double      min_clean_fraction     = */ 0.1,

        /* size_t      max_size               = */ (32 * 1024 * 1025),
        /* size_t      min_size               = */ (512 * 1024),

        /* int64_t     epoch_length           = */ 100000,


        /* enum H5C2_cache_incr_mode incr_mode = */ H5C2_incr__threshold,

        /* double     lower_hr_threshold      = */ 0.75,

        /* double      increment              = */ 2.0,

        /* hbool_t     apply_max_increment    = */ TRUE,
        /* size_t      max_increment          = */ (8 * 1024 * 1024),

        /* enum H5C2_cache_flash_incr_mode      */
	/*                    flash_incr_mode = */ H5C2_flash_incr__off,
	/* double      flash_multiple         = */ 2.0,
	/* double      flash_threshold        = */ 0.5,


        /* enum H5C2_cache_decr_mode decr_mode = */
                                             H5C2_decr__age_out_with_threshold,

        /* double      upper_hr_threshold     = */ 0.995,

        /* double      decrement              = */ 0.9,

        /* hbool_t     apply_max_decrement    = */ TRUE,
        /* size_t      max_decrement          = */ (1 * 1024 * 1024),

        /* int32_t     epochs_before_eviction = */ 3,

        /* hbool_t     apply_empty_reserve    = */ TRUE,
        /* double      empty_reserve          = */ 0.1
    };

    TESTING("smoke check #7 -- all clean, ins, prot, unprot, AR cache 2");

    if ( skip_long_tests2 ) {

        SKIPPED();

        HDfprintf(stdout, "	Long tests disabled.\n");

        return;
    }

    if ( run_full_test2 ) {

        max_index = (10 * 1024) - 1;
    }

    pass2 = TRUE;

    if ( show_progress ) /* 1 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    reset_entries2();

    if ( show_progress ) /* 2 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    cache_ptr = setup_cache2((size_t)(2 * 1024),
                            (size_t)(1 * 1024));

    if ( pass2 ) {

        result = H5C2_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_set_cache_auto_resize_config failed 1.\n";
        }
    }

    if ( show_progress ) /* 3 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    hl_row_major_scan_forward2(/* cache_ptr              */ cache_ptr,
                               /* max_index              */ max_index,
                               /* verbose                */ FALSE,
                               /* reset_stats            */ TRUE,
                               /* display_stats          */ display_stats,
                               /* display_detailed_stats */ FALSE,
                               /* do_inserts             */ FALSE,
                               /* dirty_inserts          */ dirty_inserts);

    if ( show_progress ) /* 4 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    hl_row_major_scan_backward2(/* cache_ptr              */ cache_ptr,
                               /* max_index              */ max_index,
                               /* verbose                */ FALSE,
                               /* reset_stats            */ TRUE,
                               /* display_stats          */ display_stats,
                               /* display_detailed_stats */ FALSE,
                               /* do_inserts             */ FALSE,
                               /* dirty_inserts          */ dirty_inserts);

    if ( show_progress ) /* 5 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    hl_row_major_scan_forward2(/* cache_ptr              */ cache_ptr,
                              /* max_index              */ max_index,
                              /* verbose                */ FALSE,
                              /* reset_stats            */ TRUE,
                              /* display_stats          */ display_stats,
                              /* display_detailed_stats */ FALSE,
                              /* do_inserts             */ TRUE,
                              /* dirty_inserts          */ dirty_inserts);

    if ( show_progress ) /* 6 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    /* flush and destroy all entries in the cache: */

    flush_cache2(/* cache_ptr           */ cache_ptr,
                /* destroy_entries     */ TRUE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 7 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    hl_col_major_scan_forward2(/* cache_ptr              */ cache_ptr,
                              /* max_index              */ max_index,
                              /* verbose                */ FALSE,
                              /* reset_stats            */ TRUE,
                              /* display_stats          */ display_stats,
                              /* display_detailed_stats */ FALSE,
                              /* do_inserts             */ TRUE,
                              /* dirty_inserts          */ dirty_inserts,
                              /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 8 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    /* flush all entries in the cache: */

    flush_cache2(/* cache_ptr           */ cache_ptr,
                /* destroy_entries     */ FALSE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 9 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    hl_col_major_scan_backward2(/* cache_ptr              */ cache_ptr,
                               /* max_index              */ max_index,
                               /* verbose                */ FALSE,
                               /* reset_stats            */ TRUE,
                               /* display_stats          */ display_stats,
                               /* display_detailed_stats */ FALSE,
                               /* do_inserts             */ TRUE,
                               /* dirty_inserts          */ dirty_inserts,
                               /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 10 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    takedown_cache2(cache_ptr, display_stats, TRUE);

    if ( show_progress ) /* 11 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    verify_clean2();
    verify_unprotected2();

    if ( pass2 ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass2 ) {

        HDfprintf(stdout, "%s(): failure_mssg2 = \"%s\".\n",
                  fcn_name, failure_mssg2);
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
 *		Added code to skip this test if the skip_long_tests2 global
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
    H5C2_t * cache_ptr = NULL;
    H5C2_auto_size_ctl_t auto_size_ctl =
    {
        /* int32_t     version                = */ H5C2__CURR_AUTO_SIZE_CTL_VER,
#if 1
        /* H5C2_auto_resize_report_fcn rpt_fcn = */ NULL,
#else
        /* H5C2_auto_resize_report_fcn rpt_fcn = */ H5C2_def_auto_resize_rpt_fcn,
#endif
        /* hbool_t     set_initial_size       = */ TRUE,
        /* size_t      initial_size           = */ (2 * 1024 * 1024),

        /* double      min_clean_fraction     = */ 0.1,

        /* size_t      max_size               = */ (32 * 1024 * 1025),
        /* size_t      min_size               = */ (512 * 1024),

        /* int64_t     epoch_length           = */ 100000,


        /* enum H5C2_cache_incr_mode incr_mode = */ H5C2_incr__threshold,

        /* double     lower_hr_threshold      = */ 0.75,

        /* double      increment              = */ 2.0,

        /* hbool_t     apply_max_increment    = */ TRUE,
        /* size_t      max_increment          = */ (4 * 1024 * 1024),

        /* enum H5C2_cache_flash_incr_mode      */
	/*                    flash_incr_mode = */ H5C2_flash_incr__off,
	/* double      flash_multiple         = */ 2.0,
	/* double      flash_threshold        = */ 0.5,


        /* enum H5C2_cache_decr_mode decr_mode = */
                                             H5C2_decr__age_out_with_threshold,

        /* double      upper_hr_threshold     = */ 0.995,

        /* double      decrement              = */ 0.9,

        /* hbool_t     apply_max_decrement    = */ TRUE,
        /* size_t      max_decrement          = */ (1 * 1024 * 1024),

        /* int32_t     epochs_before_eviction = */ 3,

        /* hbool_t     apply_empty_reserve    = */ TRUE,
        /* double      empty_reserve          = */ 0.1
    };

    TESTING("smoke check #8 -- ~1/2 dirty, ins, prot, unprot, AR cache 2");

    if ( skip_long_tests2 ) {

        SKIPPED();

        HDfprintf(stdout, "	Long tests disabled.\n");

        return;
    }

    if ( run_full_test2 ) {

        max_index = (10 * 1024) - 1;
    }

    pass2 = TRUE;

    if ( show_progress ) /* 1 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    reset_entries2();

    if ( show_progress ) /* 2 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    cache_ptr = setup_cache2((size_t)(2 * 1024),
                            (size_t)(1 * 1024));

    if ( pass2 ) {

        result = H5C2_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_set_cache_auto_resize_config failed 1.\n";
        }
    }

    if ( show_progress ) /* 3 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    hl_row_major_scan_forward2(/* cache_ptr              */ cache_ptr,
                              /* max_index              */ max_index,
                              /* verbose                */ FALSE,
                              /* reset_stats            */ TRUE,
                              /* display_stats          */ display_stats,
                              /* display_detailed_stats */ FALSE,
                              /* do_inserts             */ FALSE,
                              /* dirty_inserts          */ dirty_inserts);

    if ( show_progress ) /* 4 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    hl_row_major_scan_backward2(/* cache_ptr              */ cache_ptr,
                               /* max_index              */ max_index,
                               /* verbose                */ FALSE,
                               /* reset_stats            */ TRUE,
                               /* display_stats          */ display_stats,
                               /* display_detailed_stats */ FALSE,
                               /* do_inserts             */ FALSE,
                               /* dirty_inserts          */ dirty_inserts);

    if ( show_progress ) /* 5 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    hl_row_major_scan_forward2(/* cache_ptr              */ cache_ptr,
                              /* max_index              */ max_index,
                              /* verbose                */ FALSE,
                              /* reset_stats            */ TRUE,
                              /* display_stats          */ display_stats,
                              /* display_detailed_stats */ FALSE,
                              /* do_inserts             */ TRUE,
                              /* dirty_inserts          */ dirty_inserts);

    if ( show_progress ) /* 6 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    /* flush and destroy all entries in the cache: */

    flush_cache2(/* cache_ptr           */ cache_ptr,
                /* destroy_entries     */ TRUE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 7 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    hl_col_major_scan_forward2(/* cache_ptr              */ cache_ptr,
                              /* max_index              */ max_index,
                              /* verbose                */ FALSE,
                              /* reset_stats            */ TRUE,
                              /* display_stats          */ display_stats,
                              /* display_detailed_stats */ FALSE,
                              /* do_inserts             */ TRUE,
                              /* dirty_inserts          */ dirty_inserts,
                              /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 8 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    /* flush all entries in the cache: */

    flush_cache2(/* cache_ptr           */ cache_ptr,
                /* destroy_entries     */ FALSE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 9 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    hl_col_major_scan_backward2(/* cache_ptr              */ cache_ptr,
                               /* max_index              */ max_index,
                               /* verbose                */ FALSE,
                               /* reset_stats            */ TRUE,
                               /* display_stats          */ display_stats,
                               /* display_detailed_stats */ FALSE,
                               /* do_inserts             */ TRUE,
                               /* dirty_inserts          */ dirty_inserts,
                               /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 10 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    takedown_cache2(cache_ptr, display_stats, TRUE);

    if ( show_progress ) /* 11 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    verify_clean2();
    verify_unprotected2();

    if ( pass2 ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass2 ) {

        HDfprintf(stdout, "%s(): failure_mssg2 = \"%s\".\n",
                  fcn_name, failure_mssg2);
    }

    return;

} /* smoke_check_8() */


/*-------------------------------------------------------------------------
 * Function:	smoke_check_9()
 *
 * Purpose:	A repeat of smoke check 1, only with the cache corked
 * 		part of the time.
 *
 * 		Recall that smoke check 1 is a basic functional test, 
 * 		with inserts, destroys, and renames in the mix, along 
 * 		with repeated protects and unprotects.  All entries are 
 * 		marked as clean.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              8/1/07
 *
 * Modifications:
 *
 *		None.
 *
 *-------------------------------------------------------------------------
 */

static void
smoke_check_9(void)
{
    const char * fcn_name = "smoke_check_9";
    herr_t result;
    hbool_t show_progress = FALSE;
    hbool_t dirty_inserts = FALSE;
    int dirty_unprotects = FALSE;
    int dirty_destroys = FALSE;
    hbool_t display_stats = FALSE;
    hbool_t display_detailed_stats = FALSE;
    int32_t lag = 10;
    int mile_stone = 1;
    H5C2_t * cache_ptr = NULL;

    TESTING("smoke check #9 -- all clean, ins, dest, ren, 4/2 MB, corked");

    if ( skip_long_tests2 ) {

        SKIPPED();

        HDfprintf(stdout, "	Long tests disabled.\n");

        return;
    }

    pass2 = TRUE;

    if ( show_progress ) /* 1 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    reset_entries2();

    if ( show_progress ) /* 2 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    cache_ptr = setup_cache2((size_t)(4 * 1024 * 1024),
                            (size_t)(2 * 1024 * 1024));

    /* disable evictions */

    if ( show_progress ) /* 3 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    if ( pass2 ) {

        result = H5C2_set_evictions_enabled(cache_ptr, FALSE);

        if ( result < 0 ) {

             pass2 = FALSE;
	     failure_mssg2 = "can't disable evictions 1.\n";
	}
    }

    if ( show_progress ) /* 4 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d -- evictions disabled\n",
                  fcn_name, mile_stone++, (int)pass2);

    row_major_scan_forward2(/* cache_ptr              */ cache_ptr,
                           /* lag                    */ lag,
                           /* verbose                */ FALSE,
                           /* reset_stats            */ TRUE,
                           /* display_stats          */ display_stats,
                           /* display_detailed_stats */ display_detailed_stats,
                           /* do_inserts             */ TRUE,
                           /* dirty_inserts          */ dirty_inserts,
                           /* do_renames             */ TRUE,
                           /* rename_to_main_addr    */ FALSE,
                           /* do_destroys            */ TRUE,
			   /* do_mult_ro_protects    */ TRUE,
                           /* dirty_destroys         */ dirty_destroys,
                           /* dirty_unprotects       */ dirty_unprotects);

    /* enable evictions */

    if ( show_progress ) /* 5 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    if ( pass2 ) {

        result = H5C2_set_evictions_enabled(cache_ptr, TRUE);

        if ( result < 0 ) {

             pass2 = FALSE;
	     failure_mssg2 = "can't enable evictions 1.\n";
	}
    }

    if ( show_progress ) /* 6 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d -- evictions enabled \n",
                  fcn_name, mile_stone++, (int)pass2);

    row_major_scan_backward2(/* cache_ptr              */ cache_ptr,
                            /* lag                    */ lag,
                            /* verbose                */ FALSE,
                            /* reset_stats            */ TRUE,
                            /* display_stats          */ display_stats,
                            /* display_detailed_stats */ display_detailed_stats,
                            /* do_inserts             */ FALSE,
                            /* dirty_inserts          */ dirty_inserts,
                            /* do_renames             */ TRUE,
                            /* rename_to_main_addr    */ TRUE,
                            /* do_destroys            */ FALSE,
			    /* do_mult_ro_protects    */ TRUE,
                            /* dirty_destroys         */ dirty_destroys,
                            /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 7 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    if ( pass2 ) {

        result = H5C2_set_evictions_enabled(cache_ptr, FALSE);

        if ( result < 0 ) {

             pass2 = FALSE;
	     failure_mssg2 = "can't disable evictions 2.\n";
	}
    }

    if ( show_progress ) /* 8 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d -- evictions disabled \n",
                  fcn_name, mile_stone++, (int)pass2);

    row_major_scan_forward2(/* cache_ptr              */ cache_ptr,
                           /* lag                    */ lag,
                           /* verbose                */ FALSE,
                           /* reset_stats            */ TRUE,
                           /* display_stats          */ display_stats,
                           /* display_detailed_stats */ display_detailed_stats,
                           /* do_inserts             */ TRUE,
                           /* dirty_inserts          */ dirty_inserts,
                           /* do_renames             */ TRUE,
                           /* rename_to_main_addr    */ FALSE,
                           /* do_destroys            */ FALSE,
			   /* do_mult_ro_protects    */ TRUE,
                           /* dirty_destroys         */ dirty_destroys,
                           /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 9 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    if ( pass2 ) {

        result = H5C2_set_evictions_enabled(cache_ptr, TRUE);

        if ( result < 0 ) {

             pass2 = FALSE;
	     failure_mssg2 = "can't enable evictions 2.\n";
	}
    }

    if ( show_progress ) /* 10 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d -- evictions enabled \n",
                  fcn_name, mile_stone++, (int)pass2);

    /* flush and destroy all entries in the cache: */

    flush_cache2(/* cache_ptr           */ cache_ptr,
                /* destroy_entries     */ TRUE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 11 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    if ( pass2 ) {

        result = H5C2_set_evictions_enabled(cache_ptr, FALSE);

        if ( result < 0 ) {

             pass2 = FALSE;
	     failure_mssg2 = "can't disable evictions 3.\n";
	}
    }

    if ( show_progress ) /* 12 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d -- evictions disabled\n",
                  fcn_name, mile_stone++, (int)pass2);

    col_major_scan_forward2(/* cache_ptr              */ cache_ptr,
                           /* lag                    */ lag,
                           /* verbose                */ FALSE,
                           /* reset_stats            */ TRUE,
                           /* display_stats          */ display_stats,
                           /* display_detailed_stats */ display_detailed_stats,
                           /* do_inserts             */ TRUE,
                           /* dirty_inserts          */ dirty_inserts,
                           /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 13 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    /* flush all entries in the cache: */

    flush_cache2(/* cache_ptr           */ cache_ptr,
                /* destroy_entries     */ FALSE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 14 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    if ( pass2 ) {

        result = H5C2_set_evictions_enabled(cache_ptr, TRUE);

        if ( result < 0 ) {

             pass2 = FALSE;
	     failure_mssg2 = "can't enable evictions 3.\n";
	}
    }

    if ( show_progress ) /* 15 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d -- evictions enabled\n",
                  fcn_name, mile_stone++, (int)pass2);

    col_major_scan_backward2(/* cache_ptr              */ cache_ptr,
                            /* lag                    */ lag,
                            /* verbose                */ FALSE,
                            /* reset_stats            */ TRUE,
                            /* display_stats          */ display_stats,
                            /* display_detailed_stats */ display_detailed_stats,
                            /* do_inserts             */ TRUE,
                            /* dirty_inserts          */ dirty_inserts,
                            /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 16 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    if ( pass2 ) {

        result = H5C2_set_evictions_enabled(cache_ptr, FALSE);

        if ( result < 0 ) {

             pass2 = FALSE;
	     failure_mssg2 = "can't disable evictions 4.\n";
	}
    }


    if ( show_progress ) /* 17 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d -- evictions disabled\n",
                  fcn_name, mile_stone++, (int)pass2);

    takedown_cache2(cache_ptr, display_stats, TRUE);

    if ( show_progress ) /* 18 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    verify_clean2();
    verify_unprotected2();

    if ( pass2 ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass2 ) {

        HDfprintf(stdout, "%s(): failure_mssg2 = \"%s\".\n",
                  fcn_name, failure_mssg2);
    }

    return;

} /* smoke_check_9() */


/*-------------------------------------------------------------------------
 * Function:	smoke_check_10()
 *
 * Purpose:	A repeat of smoke check 2, only with the cache corked
 * 		part of the time.
 *
 * 		Recall that smoke check 2 is a basic functional test, 
 * 		with inserts, destroys, and renames in the mix, along 
 * 		with some repeated protects and unprotects.  About half 
 * 		the entries are marked as dirty.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              8/1/07
 *
 * Modifications:
 *
 *		None.
 *
 *-------------------------------------------------------------------------
 */

static void
smoke_check_10(void)
{
    const char * fcn_name = "smoke_check_10";
    herr_t result;
    hbool_t show_progress = FALSE;
    hbool_t dirty_inserts = TRUE;
    int dirty_unprotects = TRUE;
    int dirty_destroys = TRUE;
    hbool_t display_stats = FALSE;
    hbool_t display_detailed_stats = FALSE;
    int32_t lag = 10;
    int mile_stone = 1;
    H5C2_t * cache_ptr = NULL;

    TESTING("smoke check #10 -- ~1/2 dirty, ins, dest, ren, 4/2 MB, corked");

    if ( skip_long_tests2 ) {

        SKIPPED();

        HDfprintf(stdout, "	Long tests disabled.\n");

        return;
    }

    pass2 = TRUE;

    if ( show_progress ) /* 1 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    reset_entries2();

    if ( show_progress ) /* 2 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    cache_ptr = setup_cache2((size_t)(4 * 1024 * 1024),
                            (size_t)(2 * 1024 * 1024));

    if ( show_progress ) /* 3 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d -- evictions enabled\n",
                  fcn_name, mile_stone++, (int)pass2);

    row_major_scan_forward2(/* cache_ptr              */ cache_ptr,
                           /* lag                    */ lag,
                           /* verbose                */ FALSE,
                           /* reset_stats            */ TRUE,
                           /* display_stats          */ display_stats,
                           /* display_detailed_stats */ display_detailed_stats,
                           /* do_inserts             */ TRUE,
                           /* dirty_inserts          */ dirty_inserts,
                           /* do_renames             */ TRUE,
                           /* rename_to_main_addr    */ FALSE,
                           /* do_destroys            */ TRUE,
			   /* do_mult_ro_protects    */ TRUE,
                           /* dirty_destroys         */ dirty_destroys,
                           /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 4 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    if ( pass2 ) {

        result = H5C2_set_evictions_enabled(cache_ptr, FALSE);

        if ( result < 0 ) {

             pass2 = FALSE;
	     failure_mssg2 = "can't disable evictions 1.\n";
	}
    }

    if ( show_progress ) /* 5 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d -- evictions disabled\n",
                  fcn_name, mile_stone++, (int)pass2);

    row_major_scan_backward2(/* cache_ptr              */ cache_ptr,
                            /* lag                    */ lag,
                            /* verbose                */ FALSE,
                            /* reset_stats            */ TRUE,
                            /* display_stats          */ display_stats,
                            /* display_detailed_stats */ display_detailed_stats,
                            /* do_inserts             */ FALSE,
                            /* dirty_inserts          */ dirty_inserts,
                            /* do_renames             */ TRUE,
                            /* rename_to_main_addr    */ TRUE,
                            /* do_destroys            */ FALSE,
			    /* do_mult_ro_protects    */ TRUE,
                            /* dirty_destroys         */ dirty_destroys,
                            /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 6 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    if ( pass2 ) {

        result = H5C2_set_evictions_enabled(cache_ptr, TRUE);

        if ( result < 0 ) {

             pass2 = FALSE;
	     failure_mssg2 = "can't enable evictions 1.\n";
	}
    }

    if ( show_progress ) /* 7 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d -- evictions enabled\n",
                  fcn_name, mile_stone++, (int)pass2);

    row_major_scan_forward2(/* cache_ptr              */ cache_ptr,
                           /* lag                    */ lag,
                           /* verbose                */ FALSE,
                           /* reset_stats            */ TRUE,
                           /* display_stats          */ display_stats,
                           /* display_detailed_stats */ display_detailed_stats,
                           /* do_inserts             */ TRUE,
                           /* dirty_inserts          */ dirty_inserts,
                           /* do_renames             */ TRUE,
                           /* rename_to_main_addr    */ FALSE,
                           /* do_destroys            */ FALSE,
			   /* do_mult_ro_protects    */ TRUE,
                           /* dirty_destroys         */ dirty_destroys,
                           /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 8 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    if ( pass2 ) {

        result = H5C2_set_evictions_enabled(cache_ptr, FALSE);

        if ( result < 0 ) {

             pass2 = FALSE;
	     failure_mssg2 = "can't disable evictions 2.\n";
	}
    }

    if ( show_progress ) /* 9 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d -- evictions disabled\n",
                  fcn_name, mile_stone++, (int)pass2);

    /* flush and destroy all entries in the cache: */

    flush_cache2(/* cache_ptr           */ cache_ptr,
                /* destroy_entries     */ TRUE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 10 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    if ( pass2 ) {

        result = H5C2_set_evictions_enabled(cache_ptr, TRUE);

        if ( result < 0 ) {

             pass2 = FALSE;
	     failure_mssg2 = "can't enable evictions 2.\n";
	}
    }

    if ( show_progress ) /* 11 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d -- evictions enabled\n",
                  fcn_name, mile_stone++, (int)pass2);

    col_major_scan_forward2(/* cache_ptr              */ cache_ptr,
                           /* lag                    */ lag,
                           /* verbose                */ FALSE,
                           /* reset_stats            */ TRUE,
                           /* display_stats          */ display_stats,
                           /* display_detailed_stats */ display_detailed_stats,
                           /* do_inserts             */ TRUE,
                           /* dirty_inserts          */ dirty_inserts,
                           /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 12 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    if ( pass2 ) {

        result = H5C2_set_evictions_enabled(cache_ptr, FALSE);

        if ( result < 0 ) {

             pass2 = FALSE;
	     failure_mssg2 = "can't disable evictions 3.\n";
	}
    }

    if ( show_progress ) /* 13 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d -- evictions disabled\n",
                  fcn_name, mile_stone++, (int)pass2);

    /* flush all entries in the cache: */

    flush_cache2(/* cache_ptr           */ cache_ptr,
                /* destroy_entries     */ FALSE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 14 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    if ( pass2 ) {

        result = H5C2_set_evictions_enabled(cache_ptr, TRUE);

        if ( result < 0 ) {

             pass2 = FALSE;
	     failure_mssg2 = "can't enable evictions 3.\n";
	}
    }

    if ( show_progress ) /* 15 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d -- evictions enabled\n",
                  fcn_name, mile_stone++, (int)pass2);

    col_major_scan_backward2(/* cache_ptr              */ cache_ptr,
                            /* lag                    */ lag,
                            /* verbose                */ FALSE,
                            /* reset_stats            */ TRUE,
                            /* display_stats          */ display_stats,
                            /* display_detailed_stats */ display_detailed_stats,
                            /* do_inserts             */ TRUE,
                            /* dirty_inserts          */ dirty_inserts,
                            /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 16 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    if ( pass2 ) {

        result = H5C2_set_evictions_enabled(cache_ptr, FALSE);

        if ( result < 0 ) {

             pass2 = FALSE;
	     failure_mssg2 = "can't disable evictions 4.\n";
	}
    }

    if ( show_progress ) /* 17 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d -- evictions disabled\n",
                  fcn_name, mile_stone++, (int)pass2);

    takedown_cache2(cache_ptr, display_stats, TRUE);

    if ( show_progress ) /* 18 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    verify_clean2();
    verify_unprotected2();

    if ( pass2 ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass2 ) {

        HDfprintf(stdout, "%s(): failure_mssg2 = \"%s\".\n",
                  fcn_name, failure_mssg2);
    }

    return;

} /* smoke_check_10() */


/*-------------------------------------------------------------------------
 * Function:	write_permitted_check()
 *
 * Purpose:	A basic test of the write permitted function.  In essence,
 *		we load the cache up with dirty entryies, set
 *		write_permitted2 to FALSE, and then protect a bunch of
 *		entries.  If there are any writes while write_permitted2 is
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

#if H5C2_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS

    const char * fcn_name = "write_permitted_check";
    hbool_t show_progress = FALSE;
    hbool_t display_stats = FALSE;
    int32_t lag = 10;
    int mile_stone = 1;
    H5C2_t * cache_ptr = NULL;

#endif /* H5C2_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS */

    TESTING("write permitted check -- 1/0 MB cache");

#if H5C2_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS

    pass2 = TRUE;

    if ( show_progress ) /* 1 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    reset_entries2();

    if ( show_progress ) /* 2 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    cache_ptr = setup_cache2((size_t)(1 * 1024 * 1024),
                            (size_t)(0));

    if ( show_progress ) /* 3 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    row_major_scan_forward2(/* cache_ptr              */ cache_ptr,
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
			   /* do_mult_ro_protects    */ TRUE,
                           /* dirty_destroys         */ TRUE,
                           /* dirty_unprotects       */ TRUE);

    if ( show_progress ) /* 4 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    write_permitted2 = FALSE;

    row_major_scan_backward2(/* cache_ptr              */ cache_ptr,
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
			    /* do_mult_ro_protects    */ TRUE,
                            /* dirty_destroys         */ FALSE,
                            /* dirty_unprotects       */ NO_CHANGE);

    if ( show_progress ) /* 5 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    write_permitted2 = TRUE;

    row_major_scan_forward2(/* cache_ptr              */ cache_ptr,
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
			   /* do_mult_ro_protects    */ TRUE,
                           /* dirty_destroys         */ TRUE,
                           /* dirty_unprotects       */ TRUE);

    if ( show_progress ) /* 6 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    /* flush and destroy all entries in the cache: */

    flush_cache2(/* cache_ptr           */ cache_ptr,
                /* destroy_entries     */ TRUE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 7 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    col_major_scan_forward2(/* cache_ptr              */ cache_ptr,
                           /* lag                    */ lag,
                           /* verbose                */ FALSE,
                           /* reset_stats            */ TRUE,
                           /* display_stats          */ display_stats,
                           /* display_detailed_stats */ TRUE,
                           /* do_inserts             */ TRUE,
                           /* dirty_inserts          */ TRUE,
                           /* dirty_unprotects       */ TRUE);

    if ( show_progress ) /* 8 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    write_permitted2 = FALSE;

    col_major_scan_backward2(/* cache_ptr              */ cache_ptr,
                            /* lag                    */ lag,
                            /* verbose                */ FALSE,
                            /* reset_stats            */ TRUE,
                            /* display_stats          */ display_stats,
                            /* display_detailed_stats */ TRUE,
                            /* do_inserts             */ FALSE,
                            /* dirty_inserts          */ FALSE,
                            /* dirty_unprotects       */ NO_CHANGE);

    write_permitted2 = TRUE;

    if ( show_progress ) /* 9 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    takedown_cache2(cache_ptr, display_stats, TRUE);

    if ( show_progress ) /* 10 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    verify_clean2();
    verify_unprotected2();

    if ( pass2 ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass2 ) {

        HDfprintf(stdout, "%s(): failure_mssg2 = \"%s\".\n",
                  fcn_name, failure_mssg2);
    }

#else /* H5C2_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS */

    SKIPPED();

    HDfprintf(stdout, "	Clean and dirty LRU lists disabled.\n");

#endif /* H5C2_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS */

    return;

} /* write_permitted_check() */


/*-------------------------------------------------------------------------
 * Function:	check_insert_entry()
 *
 * Purpose:	Verify that H5C2_insert_entry behaves as expected.
 *		Test the behaviour with different flags.
 *
 *		This test was added primarily to test basic insert
 *		pinned entry functionallity, but I through in explicit
 *		tests for other functionallity that is tested implicitly
 *		elsewhere.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              8/10/06
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
check_insert_entry(void)
{
    const char *                fcn_name = "check_insert_entry";
    int                         entry_type = PICO_ENTRY_TYPE;
    int                         i;
    int				point = 0;
    int				subpoint = 0;
    herr_t                      result;
    hbool_t                     in_cache;
    hbool_t                     is_dirty;
    hbool_t                     is_protected;
    hbool_t                     is_pinned;
    hbool_t			show_progress = FALSE;
    size_t                      entry_size;
    H5C2_t *                    cache_ptr = NULL;
    test_entry_t *              base_addr;
    test_entry_t *              entry_ptr;
    struct H5C2_cache_entry_t * search_ptr;


    TESTING("H5C2_insert_entry() functionality");

    pass2 = TRUE;

    if ( show_progress ) {
        HDfprintf(stdout, "%s: point = %d\n", fcn_name, point++); /* 0 */
    }

    /* Allocate a cache, and insert entries into it using all 
     * combinations of flags.  Verify that the entries are inserted,
     * and that the flags have the desired effects.
     *
     * Note that the dirty parameter in insert_entry is no longer
     * used, as we have decided that all inserted entries are 
     * dirty by definition. (Which sounds very reasonable, but didn't
     * used to be the case.)
     */

    if ( pass2 ) {

        reset_entries2();

        cache_ptr = setup_cache2((size_t)(2 * 1024 * 1024),
                                (size_t)(1 * 1024 * 1024));
    }

    if ( show_progress ) {
        HDfprintf(stdout, "%s: point = %d\n", fcn_name, point++); /* 1 */
    }

    if ( pass2 ) {

        insert_entry2(cache_ptr, entry_type, 0, TRUE, H5C2__NO_FLAGS_SET);
        insert_entry2(cache_ptr, entry_type, 1, TRUE, 
                      H5C2__SET_FLUSH_MARKER_FLAG);
        insert_entry2(cache_ptr, entry_type, 2, TRUE, H5C2__PIN_ENTRY_FLAG);
        insert_entry2(cache_ptr, entry_type, 3, TRUE, 
 		      (H5C2__SET_FLUSH_MARKER_FLAG | H5C2__PIN_ENTRY_FLAG));
    }

    if ( show_progress ) {
        HDfprintf(stdout, "%s: point = %d\n", fcn_name, point++); /* 2 */
    }

    /* Verify that the entries are inserted as desired. */

    i = 0;
    base_addr = entries2[0];
    while ( ( pass2 ) && ( i < 4 ) )
    {
	subpoint = 0;

	entry_ptr = &(base_addr[i]);

	/* Start by checking everything we can via H5C2_get_entry_status() */

        if ( show_progress ) {
            HDfprintf(stdout, "%s:%d point = %d.%d\n", 
	 	      fcn_name, i, point, subpoint++);
	}

	result = H5C2_get_entry_status(cache_ptr, entry_ptr->addr, &entry_size,
			              &in_cache, &is_dirty, &is_protected, 
				      &is_pinned);

        if ( show_progress ) {
            HDfprintf(stdout, "%s:%d point = %d.%d\n", 
	 	      fcn_name, i, point, subpoint++);
	}

        if ( result < 0 ) {

            pass2 = FALSE;
            failure_mssg2 = "H5AC_get_entry_status() reports failure.";
        }

        if ( show_progress ) {
            HDfprintf(stdout, "%s:%d point = %d.%d\n", 
	 	      fcn_name, i, point, subpoint++);
	}

	if ( pass2 ) { 

	    /* check the universals */
	    if ( ( ! in_cache ) || ( ! is_dirty ) || ( is_protected ) || 
                 ( entry_size != entry_sizes2[entry_type] ) ) {

                pass2 = FALSE;
                failure_mssg2 = "Unexpected insert results 1.";
            }
	}

        if ( show_progress ) {
            HDfprintf(stdout, "%s:%d point = %d.%d\n", 
	 	      fcn_name, i, point, subpoint++);
	}

	if ( pass2 ) {

            /* verify that the pinned flag got set correctly */
	    if ( ( i == 2 ) || ( i == 3 ) ) {

		if ( ! is_pinned ) {

                    pass2 = FALSE;
                    failure_mssg2 = "Unexpected insert results 2.";
		}
	    } else if ( is_pinned ) {

                pass2 = FALSE;
                failure_mssg2 = "Unexpected insert results 3.";

	    } else if ( is_pinned != ((entry_ptr->header).is_pinned) ) {

                pass2 = FALSE;
                failure_mssg2 = "Unexpected insert results 4.";
            }
	}

        if ( show_progress ) {
            HDfprintf(stdout, "%s:%d point = %d.%d\n", 
	 	      fcn_name, i, point, subpoint++);
	}

	/* Thats all we can get from H5AC_get_entry_status().
	 * Now start looking at the cache data structures directly.
	 */

	if ( pass2 ) {

            /* Verify that the flush marker got set correctly */
	    if ( ( i == 1 ) || ( i == 3 ) ) {

		if ( ! ((entry_ptr->header).flush_marker) ) {

                    pass2 = FALSE;
                    failure_mssg2 = "Unexpected insert results 5.";
		}
	    } else if ( (entry_ptr->header).flush_marker ) {

                pass2 = FALSE;
                failure_mssg2 = "Unexpected insert results 6.";
	    }
	}

        if ( show_progress ) {
            HDfprintf(stdout, "%s:%d point = %d.%d\n", 
	 	      fcn_name, i, point, subpoint++);
	}

	if ( pass2 ) {

	    /* Verify that pinned entries are in the pinned entry list */
	    if ( (entry_ptr->header).is_pinned ) {

		search_ptr = cache_ptr->pel_head_ptr;

		while ( ( search_ptr != NULL ) &&
			( search_ptr != 
			  (struct H5C2_cache_entry_t *)entry_ptr ) )
		{
		    search_ptr = search_ptr->next;
		}

		if ( search_ptr == NULL ) {

                    pass2 = FALSE;
                    failure_mssg2 = "Unexpected insert results 7.";
		}
	    }
	}

        if ( show_progress ) {
            HDfprintf(stdout, "%s:%d point = %d.%d\n", 
	 	      fcn_name, i, point, subpoint++);
	}

	if ( pass2 ) {

	    /* Verify that unpinned entries are in the LRU list */
	    if ( ! ((entry_ptr->header).is_pinned) ) {

		search_ptr = cache_ptr->LRU_head_ptr;

		while ( ( search_ptr != NULL ) &&
			( search_ptr != 
			  (struct H5C2_cache_entry_t *)entry_ptr ) )
		{
		    search_ptr = search_ptr->next;
		}

		if ( search_ptr == NULL ) {

                    pass2 = FALSE;
                    failure_mssg2 = "Unexpected insert results 8.";
		}
	    }
	}

        if ( show_progress ) {
            HDfprintf(stdout, "%s:%d point = %d.%d\n", 
	 	      fcn_name, i, point, subpoint++);
	}

#if H5C2_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS
	if ( pass2 ) {

	    /* Verify that unpinned entries are in the dirty LRU list */
	    if ( ! ((entry_ptr->header).is_pinned) ) {

		search_ptr = cache_ptr->dLRU_head_ptr;

		while ( ( search_ptr != NULL ) &&
			( search_ptr != 
			  (struct H5C2_cache_entry_t *)entry_ptr ) )
		{
		    search_ptr = search_ptr->aux_next;
		}

		if ( search_ptr == NULL ) {

                    pass2 = FALSE;
                    failure_mssg2 = "Unexpected insert results 9.";
		}
	    }
	}
#endif /* H5C2_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS */

        if ( show_progress ) {
            HDfprintf(stdout, "%s:%d point = %d.%d\n", 
  	 	      fcn_name, i, point, subpoint++);
	}

	i++;

    } /* while */

    if ( show_progress ) {
	point++;
        HDfprintf(stdout, "%s: point = %d\n", fcn_name, point++);
    }

    /* So much for looking at the individual entries.  Now verify 
     * that the various counts and sized in the cache header are 
     * as expected.
     */

    if ( pass2 ) {

	if ( ( cache_ptr->index_len != 4 ) ||
	     ( cache_ptr->index_size != 4 * entry_sizes2[entry_type] ) ||
	     ( cache_ptr->slist_len != 4 ) ||
	     ( cache_ptr->slist_size != 4 * entry_sizes2[entry_type] ) ||
	     ( cache_ptr->pl_len != 0 ) ||
	     ( cache_ptr->pl_size != (size_t)0 ) ||
	     ( cache_ptr->pel_len != 2 ) ||
	     ( cache_ptr->pel_size != 2 * entry_sizes2[entry_type] ) ||
	     ( cache_ptr->LRU_list_len != 2 ) ||
	     ( cache_ptr->LRU_list_size != 2 * entry_sizes2[entry_type] ) ||
#if H5C2_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS
	     ( cache_ptr->dLRU_list_len != 2 ) ||
	     ( cache_ptr->dLRU_list_size != 2 * entry_sizes2[entry_type] ) ||
#endif /* H5C2_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS */
	     ( cache_ptr->cLRU_list_len != 0 ) ||
	     ( cache_ptr->cLRU_list_size != (size_t)0 ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected insert results 10.";
	}
    }

    if ( show_progress ) {
        HDfprintf(stdout, "%s: point = %d\n", fcn_name, point++);
    }

    /* Finally, if stats collection is enabled, verify that the expected
     * stats are collected.
     */
#if H5C2_COLLECT_CACHE_STATS
    if ( pass2 ) {

	if ( ( cache_ptr->insertions[entry_type] != 4 ) ||
	     ( cache_ptr->pinned_insertions[entry_type] != 2 ) ||
	     ( cache_ptr->pins[entry_type] != 2 ) ||
	     ( cache_ptr->unpins[entry_type] != 0 ) ||
             ( cache_ptr->dirty_pins[entry_type] != 0 ) ||
	     ( cache_ptr->max_index_len != 4 ) ||
	     ( cache_ptr->max_index_size != 4 * entry_sizes2[entry_type] ) ||
	     ( cache_ptr->max_slist_len != 4 ) ||
	     ( cache_ptr->max_slist_size != 4 * entry_sizes2[entry_type] ) ||
	     ( cache_ptr->max_pl_len != 0 ) ||
	     ( cache_ptr->max_pl_size != (size_t)0 ) ||
	     ( cache_ptr->max_pel_len != 2 ) ||
	     ( cache_ptr->max_pel_size != 2 * entry_sizes2[entry_type] ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected insert results 11.";
	}
    }
#endif /* H5C2_COLLECT_CACHE_STATS */

    if ( show_progress ) {
        HDfprintf(stdout, "%s: point = %d\n", fcn_name, point++);
    }

    /* Unpin the pinned entries so we can take down the cache cleanly. */

    if ( pass2 ) {

        unpin_entry2(cache_ptr, entry_type, 2);
	unpin_entry2(cache_ptr, entry_type, 3);
    }

    if ( show_progress ) {
        HDfprintf(stdout, "%s: point = %d\n", fcn_name, point++);
    }

    if ( pass2 ) {

        takedown_cache2(cache_ptr, FALSE, FALSE);
    }

    if ( show_progress ) {
        HDfprintf(stdout, "%s: point = %d\n", fcn_name, point++);
    }

    if ( pass2 ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass2 ) {

        HDfprintf(stdout, "%s(): failure_mssg2 = \"%s\".\n",
                  fcn_name, failure_mssg2);
    }

    return;

} /* check_insert_entry() */


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
    hbool_t	 show_progress = FALSE;
    int		 mile_stone = 0;
    H5C2_t *     cache_ptr = NULL;

    TESTING("H5C2_flush_cache() functionality");

    pass2 = TRUE;

    if ( show_progress ) {	/* 0 */
	HDfprintf(stdout, "\n%s: mile_stone = %d.\n", fcn_name, mile_stone++);
    }

    /* allocate a cache, and flush it under various circumstances.
     * To the extent possible, verify that the desired actions took
     * place.
     */

    if ( pass2 ) {

        reset_entries2();

        cache_ptr = setup_cache2((size_t)(2 * 1024 * 1024),
                                 (size_t)(1 * 1024 * 1024));
    }

    if ( show_progress ) {	/* 1 */
	HDfprintf(stdout, "%s: mile_stone = %d.\n", fcn_name, mile_stone++);
    }

    /* first test behaviour on an empty cache.  Can't do much sanity
     * checking in this case, so simply check the return values.
     */

    if ( pass2 ) {

        check_flush_cache__empty_cache(cache_ptr);
    }

    if ( show_progress ) {	/* 2 */
	HDfprintf(stdout, "%s: mile_stone = %d.\n", fcn_name, mile_stone++);
    }

    /* now do a series of similar tests with a cache with a single entry.
     * Start with a clean entry, with no flags set.
     */

    if ( pass2 ) {

        check_flush_cache__single_entry(cache_ptr);
    }

    if ( show_progress ) {	/* 3 */
	HDfprintf(stdout, "%s: mile_stone = %d.\n", fcn_name, mile_stone++);
    }

    if ( pass2 ) {

        check_flush_cache__multi_entry(cache_ptr);
    }

    if ( show_progress ) {	/* 4 */
	HDfprintf(stdout, "%s: mile_stone = %d.\n", fcn_name, mile_stone++);
    }

    if ( pass2 ) {

	check_flush_cache__flush_ops(cache_ptr);
    }

    if ( show_progress ) {	/* 5 */
	HDfprintf(stdout, "%s: mile_stone = %d.\n", fcn_name, mile_stone++);
    }

    if ( pass2 ) {

        takedown_cache2(cache_ptr, FALSE, FALSE);
    }

    if ( show_progress ) {	/* 6 */
	HDfprintf(stdout, "%s: mile_stone = %d.\n", fcn_name, mile_stone++);
    }

    if ( pass2 ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass2 ) {

        HDfprintf(stdout, "%s(): failure_mssg2 = \"%s\".\n",
                  fcn_name, failure_mssg2);
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
check_flush_cache__empty_cache(H5C2_t * cache_ptr)
{
    /* const char * fcn_name = "check_flush_cache__empty_cache"; */
    herr_t	 result;

    if ( cache_ptr == NULL ) {

        pass2 = FALSE;
        failure_mssg2 = "cache_ptr NULL on entry to empty cache case.";
    }
    else if ( ( cache_ptr->index_len != 0 ) ||
              ( cache_ptr->index_size != 0 ) ) {

        pass2 = FALSE;
        failure_mssg2 = "cache not empty at beginning of empty cache case.";
    }


    /* Test behaviour on an empty cache.  Can't do much sanity
     * checking in this case, so simply check the return values.
     */

    if ( pass2 ) {

        result = H5C2_flush_cache(cache_ptr, H5P_DATASET_XFER_DEFAULT, 
			          H5C2__NO_FLAGS_SET);

        if ( result < 0 ) {

            pass2 = FALSE;
            failure_mssg2 = "flush with flags = 0x00 failed on empty cache.\n";
        }
    }

    if ( pass2 ) {

        result = H5C2_flush_cache(cache_ptr, H5P_DATASET_XFER_DEFAULT,
                                  H5C2__FLUSH_INVALIDATE_FLAG);

        if ( result < 0 ) {

            pass2 = FALSE;
            failure_mssg2 = "flush with flags = 0x04 failed on empty cache.\n";
        }
    }

    if ( pass2 ) {

        result = H5C2_flush_cache(cache_ptr, H5P_DATASET_XFER_DEFAULT,
                                  H5C2__FLUSH_CLEAR_ONLY_FLAG);

        if ( result < 0 ) {

            pass2 = FALSE;
            failure_mssg2 = "flush with flags = 0x08 failed on empty cache.\n";
        }
    }


    if ( pass2 ) {

        result = H5C2_flush_cache(cache_ptr, H5P_DATASET_XFER_DEFAULT,
                                  H5C2__FLUSH_MARKED_ENTRIES_FLAG);

        if ( result < 0 ) {

            pass2 = FALSE;
            failure_mssg2 = "flush with flags = 0x10 failed on empty cache.\n";
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
check_flush_cache__multi_entry(H5C2_t * cache_ptr)
{
    /* const char * fcn_name = "check_flush_cache__multi_entry"; */

    if ( cache_ptr == NULL ) {

        pass2 = FALSE;
        failure_mssg2 = "cache_ptr NULL on entry to multi entry case.";
    }
    else if ( ( cache_ptr->index_len != 0 ) ||
              ( cache_ptr->index_size != 0 ) ) {

        pass2 = FALSE;
        failure_mssg2 = "cache not empty at beginning of multi entry case.";
    }

    if ( pass2 )
    {
        int test_num                         = 1;
        unsigned int flush_flags             = H5C2__NO_FLAGS_SET;
        int spec_size                        = 8;
        struct flush_cache_test_spec spec[8] =
        {
          {
            /* entry_num             = */ 0,
            /* entry_type            = */ PICO_ENTRY_TYPE,
            /* entry_index           = */ 100,
            /* insert_flag           = */ FALSE,
            /* dirty_flag            = */ FALSE,
            /* flags                 = */ H5C2__NO_FLAGS_SET,
            /* expected_deserialized = */ TRUE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ FALSE
          },
          {
            /* entry_num             = */ 1,
            /* entry_type            = */ PICO_ENTRY_TYPE,
            /* entry_index           = */ 75,
            /* insert_flag           = */ FALSE,
            /* dirty_flag            = */ TRUE,
            /* flags                 = */ H5C2__NO_FLAGS_SET,
            /* expected_deserialized = */ TRUE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ TRUE,
            /* expected_destroyed    = */ FALSE
          },
          {
            /* entry_num             = */ 2,
            /* entry_type            = */ PICO_ENTRY_TYPE,
            /* entry_index           = */ 25,
            /* insert_flag           = */ TRUE,
            /* dirty_flag            = */ FALSE,
            /* flags                 = */ H5C2__NO_FLAGS_SET,
            /* expected_deserialized = */ FALSE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ TRUE,
            /* expected_destroyed    = */ FALSE
          },
          {
            /* entry_num             = */ 3,
            /* entry_type            = */ PICO_ENTRY_TYPE,
            /* entry_index           = */ 50,
            /* insert_flag           = */ TRUE,
            /* dirty_flag            = */ TRUE,
            /* flags                 = */ H5C2__NO_FLAGS_SET,
            /* expected_deserialized = */ FALSE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ TRUE,
            /* expected_destroyed    = */ FALSE
          },
          {
            /* entry_num             = */ 4,
            /* entry_type            = */ MONSTER_ENTRY_TYPE,
            /* entry_index           = */ 10,
            /* insert_flag           = */ FALSE,
            /* dirty_flag            = */ FALSE,
            /* flags                 = */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* expected_deserialized = */ TRUE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ FALSE
          },
          {
            /* entry_num             = */ 5,
            /* entry_type            = */ MONSTER_ENTRY_TYPE,
            /* entry_index           = */ 20,
            /* insert_flag           = */ FALSE,
            /* dirty_flag            = */ TRUE,
            /* flags                 = */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* expected_deserialized = */ TRUE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ TRUE,
            /* expected_destroyed    = */ FALSE
          },
          {
            /* entry_num             = */ 6,
            /* entry_type            = */ MONSTER_ENTRY_TYPE,
            /* entry_index           = */ 30,
            /* insert_flag           = */ TRUE,
            /* dirty_flag            = */ FALSE,
            /* flags                 = */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* expected_deserialized = */ FALSE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ TRUE,
            /* expected_destroyed    = */ FALSE
          },
          {
            /* entry_num             = */ 7,
            /* entry_type            = */ MONSTER_ENTRY_TYPE,
            /* entry_index           = */ 40,
            /* insert_flag           = */ TRUE,
            /* dirty_flag            = */ TRUE,
            /* flags                 = */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* expected_deserialized = */ FALSE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ TRUE,
            /* expected_destroyed    = */ FALSE
          }
        };

        check_flush_cache__multi_entry_test(cache_ptr, test_num,
                                            flush_flags, spec_size, spec);
    }


    if ( pass2 )
    {
        int test_num                         = 2;
        unsigned int flush_flags             = H5C2__FLUSH_INVALIDATE_FLAG;
        int spec_size                        = 8;
        struct flush_cache_test_spec spec[8] =
        {
          {
            /* entry_num             = */ 0,
            /* entry_type            = */ PICO_ENTRY_TYPE,
            /* entry_index           = */ 100,
            /* insert_flag           = */ FALSE,
            /* dirty_flag            = */ FALSE,
            /* flags                 = */ H5C2__NO_FLAGS_SET,
            /* expected_deserialized = */ TRUE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ TRUE
          },
          {
            /* entry_num             = */ 1,
            /* entry_type            = */ PICO_ENTRY_TYPE,
            /* entry_index           = */ 75,
            /* insert_flag           = */ FALSE,
            /* dirty_flag            = */ TRUE,
            /* flags                 = */ H5C2__NO_FLAGS_SET,
            /* expected_deserialized = */ TRUE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ TRUE,
            /* expected_destroyed    = */ TRUE
          },
          {
            /* entry_num             = */ 2,
            /* entry_type            = */ PICO_ENTRY_TYPE,
            /* entry_index           = */ 25,
            /* insert_flag           = */ TRUE,
            /* dirty_flag            = */ FALSE,
            /* flags                 = */ H5C2__NO_FLAGS_SET,
            /* expected_deserialized = */ FALSE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ TRUE,
            /* expected_destroyed    = */ TRUE
          },
          {
            /* entry_num             = */ 3,
            /* entry_type            = */ PICO_ENTRY_TYPE,
            /* entry_index           = */ 50,
            /* insert_flag           = */ TRUE,
            /* dirty_flag            = */ TRUE,
            /* flags                 = */ H5C2__NO_FLAGS_SET,
            /* expected_deserialized = */ FALSE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ TRUE,
            /* expected_destroyed    = */ TRUE
          },
          {
            /* entry_num             = */ 4,
            /* entry_type            = */ MONSTER_ENTRY_TYPE,
            /* entry_index           = */ 10,
            /* insert_flag           = */ FALSE,
            /* dirty_flag            = */ FALSE,
            /* flags                 = */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* expected_deserialized = */ TRUE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ TRUE
          },
          {
            /* entry_num             = */ 5,
            /* entry_type            = */ MONSTER_ENTRY_TYPE,
            /* entry_index           = */ 20,
            /* insert_flag           = */ FALSE,
            /* dirty_flag            = */ TRUE,
            /* flags                 = */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* expected_deserialized = */ TRUE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ TRUE,
            /* expected_destroyed    = */ TRUE
          },
          {
            /* entry_num             = */ 6,
            /* entry_type            = */ MONSTER_ENTRY_TYPE,
            /* entry_index           = */ 30,
            /* insert_flag           = */ TRUE,
            /* dirty_flag            = */ FALSE,
            /* flags                 = */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* expected_deserialized = */ FALSE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ TRUE,
            /* expected_destroyed    = */ TRUE
          },
          {
            /* entry_num             = */ 7,
            /* entry_type            = */ MONSTER_ENTRY_TYPE,
            /* entry_index           = */ 40,
            /* insert_flag           = */ TRUE,
            /* dirty_flag            = */ TRUE,
            /* flags                 = */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* expected_deserialized = */ FALSE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ TRUE,
            /* expected_destroyed    = */ TRUE
          }
        };

        check_flush_cache__multi_entry_test(cache_ptr, test_num,
                                            flush_flags, spec_size, spec);
    }


    if ( pass2 )
    {
        int test_num                         = 3;
        unsigned int flush_flags             = H5C2__FLUSH_CLEAR_ONLY_FLAG;
        int spec_size                        = 8;
        struct flush_cache_test_spec spec[8] =
        {
          {
            /* entry_num             = */ 0,
            /* entry_type            = */ PICO_ENTRY_TYPE,
            /* entry_index           = */ 100,
            /* insert_flag           = */ FALSE,
            /* dirty_flag            = */ FALSE,
            /* flags                 = */ H5C2__NO_FLAGS_SET,
            /* expected_deserialized = */ TRUE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ FALSE
          },
          {
            /* entry_num             = */ 1,
            /* entry_type            = */ PICO_ENTRY_TYPE,
            /* entry_index           = */ 75,
            /* insert_flag           = */ FALSE,
            /* dirty_flag            = */ TRUE,
            /* flags                 = */ H5C2__NO_FLAGS_SET,
            /* expected_deserialized = */ TRUE,
            /* expected_cleared      = */ TRUE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ FALSE
          },
          {
            /* entry_num             = */ 2,
            /* entry_type            = */ PICO_ENTRY_TYPE,
            /* entry_index           = */ 25,
            /* insert_flag           = */ TRUE,
            /* dirty_flag            = */ FALSE,
            /* flags                 = */ H5C2__NO_FLAGS_SET,
            /* expected_deserialized = */ FALSE,
            /* expected_cleared      = */ TRUE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ FALSE
          },
          {
            /* entry_num             = */ 3,
            /* entry_type            = */ PICO_ENTRY_TYPE,
            /* entry_index           = */ 50,
            /* insert_flag           = */ TRUE,
            /* dirty_flag            = */ TRUE,
            /* flags                 = */ H5C2__NO_FLAGS_SET,
            /* expected_deserialized = */ FALSE,
            /* expected_cleared      = */ TRUE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ FALSE
          },
          {
            /* entry_num             = */ 4,
            /* entry_type            = */ MONSTER_ENTRY_TYPE,
            /* entry_index           = */ 10,
            /* insert_flag           = */ FALSE,
            /* dirty_flag            = */ FALSE,
            /* flags                 = */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* expected_deserialized = */ TRUE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ FALSE
          },
          {
            /* entry_num             = */ 5,
            /* entry_type            = */ MONSTER_ENTRY_TYPE,
            /* entry_index           = */ 20,
            /* insert_flag           = */ FALSE,
            /* dirty_flag            = */ TRUE,
            /* flags                 = */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* expected_deserialized = */ TRUE,
            /* expected_cleared      = */ TRUE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ FALSE
          },
          {
            /* entry_num             = */ 6,
            /* entry_type            = */ MONSTER_ENTRY_TYPE,
            /* entry_index           = */ 30,
            /* insert_flag           = */ TRUE,
            /* dirty_flag            = */ FALSE,
            /* flags                 = */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* expected_deserialized = */ FALSE,
            /* expected_cleared      = */ TRUE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ FALSE
          },
          {
            /* entry_num             = */ 7,
            /* entry_type            = */ MONSTER_ENTRY_TYPE,
            /* entry_index           = */ 40,
            /* insert_flag           = */ TRUE,
            /* dirty_flag            = */ TRUE,
            /* flags                 = */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* expected_deserialized = */ FALSE,
            /* expected_cleared      = */ TRUE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ FALSE
          }
        };

        check_flush_cache__multi_entry_test(cache_ptr, test_num,
                                            flush_flags, spec_size, spec);
    }


    if ( pass2 )
    {
        int test_num                         = 4;
        unsigned int flush_flags             = H5C2__FLUSH_MARKED_ENTRIES_FLAG;
        int spec_size                        = 8;
        struct flush_cache_test_spec spec[8] =
        {
          {
            /* entry_num             = */ 0,
            /* entry_type            = */ PICO_ENTRY_TYPE,
            /* entry_index           = */ 100,
            /* insert_flag           = */ FALSE,
            /* dirty_flag            = */ FALSE,
            /* flags                 = */ H5C2__NO_FLAGS_SET,
            /* expected_deserialized = */ TRUE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ FALSE
          },
          {
            /* entry_num             = */ 1,
            /* entry_type            = */ PICO_ENTRY_TYPE,
            /* entry_index           = */ 75,
            /* insert_flag           = */ FALSE,
            /* dirty_flag            = */ TRUE,
            /* flags                 = */ H5C2__NO_FLAGS_SET,
            /* expected_deserialized = */ TRUE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ FALSE
          },
          {
            /* entry_num             = */ 2,
            /* entry_type            = */ PICO_ENTRY_TYPE,
            /* entry_index           = */ 25,
            /* insert_flag           = */ TRUE,
            /* dirty_flag            = */ FALSE,
            /* flags                 = */ H5C2__NO_FLAGS_SET,
            /* expected_deserialized = */ FALSE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ FALSE
          },
          {
            /* entry_num             = */ 3,
            /* entry_type            = */ PICO_ENTRY_TYPE,
            /* entry_index           = */ 50,
            /* insert_flag           = */ TRUE,
            /* dirty_flag            = */ TRUE,
            /* flags                 = */ H5C2__NO_FLAGS_SET,
            /* expected_deserialized = */ FALSE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ FALSE
          },
          {
            /* entry_num             = */ 4,
            /* entry_type            = */ MONSTER_ENTRY_TYPE,
            /* entry_index           = */ 10,
            /* insert_flag           = */ FALSE,
            /* dirty_flag            = */ FALSE,
            /* flags                 = */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* expected_deserialized = */ TRUE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ FALSE
          },
          {
            /* entry_num             = */ 5,
            /* entry_type            = */ MONSTER_ENTRY_TYPE,
            /* entry_index           = */ 20,
            /* insert_flag           = */ FALSE,
            /* dirty_flag            = */ TRUE,
            /* flags                 = */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* expected_deserialized = */ TRUE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ TRUE,
            /* expected_destroyed    = */ FALSE
          },
          {
            /* entry_num             = */ 6,
            /* entry_type            = */ MONSTER_ENTRY_TYPE,
            /* entry_index           = */ 30,
            /* insert_flag           = */ TRUE,
            /* dirty_flag            = */ FALSE,
            /* flags                 = */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* expected_deserialized = */ FALSE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ TRUE,
            /* expected_destroyed    = */ FALSE
          },
          {
            /* entry_num             = */ 7,
            /* entry_type            = */ MONSTER_ENTRY_TYPE,
            /* entry_index           = */ 40,
            /* insert_flag           = */ TRUE,
            /* dirty_flag            = */ TRUE,
            /* flags                 = */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* expected_deserialized = */ FALSE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ TRUE,
            /* expected_destroyed    = */ FALSE
          }
        };

        check_flush_cache__multi_entry_test(cache_ptr, test_num,
                                            flush_flags, spec_size, spec);
    }


    if ( pass2 )
    {
        int test_num                         = 5;
        unsigned int flush_flags             = H5C2__FLUSH_INVALIDATE_FLAG |
                                               H5C2__FLUSH_CLEAR_ONLY_FLAG;
        int spec_size                        = 8;
        struct flush_cache_test_spec spec[8] =
        {
          {
            /* entry_num             = */ 0,
            /* entry_type            = */ PICO_ENTRY_TYPE,
            /* entry_index           = */ 100,
            /* insert_flag           = */ FALSE,
            /* dirty_flag            = */ FALSE,
            /* flags                 = */ H5C2__NO_FLAGS_SET,
            /* expected_deserialized = */ TRUE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ TRUE
          },
          {
            /* entry_num             = */ 1,
            /* entry_type            = */ PICO_ENTRY_TYPE,
            /* entry_index           = */ 75,
            /* insert_flag           = */ FALSE,
            /* dirty_flag            = */ TRUE,
            /* flags                 = */ H5C2__NO_FLAGS_SET,
            /* expected_deserialized = */ TRUE,
            /* expected_cleared      = */ TRUE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ TRUE
          },
          {
            /* entry_num             = */ 2,
            /* entry_type            = */ PICO_ENTRY_TYPE,
            /* entry_index           = */ 25,
            /* insert_flag           = */ TRUE,
            /* dirty_flag            = */ FALSE,
            /* flags                 = */ H5C2__NO_FLAGS_SET,
            /* expected_deserialized = */ FALSE,
            /* expected_cleared      = */ TRUE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ TRUE
          },
          {
            /* entry_num             = */ 3,
            /* entry_type            = */ PICO_ENTRY_TYPE,
            /* entry_index           = */ 50,
            /* insert_flag           = */ TRUE,
            /* dirty_flag            = */ TRUE,
            /* flags                 = */ H5C2__NO_FLAGS_SET,
            /* expected_deserialized = */ FALSE,
            /* expected_cleared      = */ TRUE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ TRUE
          },
          {
            /* entry_num             = */ 4,
            /* entry_type            = */ MONSTER_ENTRY_TYPE,
            /* entry_index           = */ 10,
            /* insert_flag           = */ FALSE,
            /* dirty_flag            = */ FALSE,
            /* flags                 = */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* expected_deserialized = */ TRUE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ TRUE
          },
          {
            /* entry_num             = */ 5,
            /* entry_type            = */ MONSTER_ENTRY_TYPE,
            /* entry_index           = */ 20,
            /* insert_flag           = */ FALSE,
            /* dirty_flag            = */ TRUE,
            /* flags                 = */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* expected_deserialized = */ TRUE,
            /* expected_cleared      = */ TRUE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ TRUE
          },
          {
            /* entry_num             = */ 6,
            /* entry_type            = */ MONSTER_ENTRY_TYPE,
            /* entry_index           = */ 30,
            /* insert_flag           = */ TRUE,
            /* dirty_flag            = */ FALSE,
            /* flags                 = */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* expected_deserialized = */ FALSE,
            /* expected_cleared      = */ TRUE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ TRUE
          },
          {
            /* entry_num             = */ 7,
            /* entry_type            = */ MONSTER_ENTRY_TYPE,
            /* entry_index           = */ 40,
            /* insert_flag           = */ TRUE,
            /* dirty_flag            = */ TRUE,
            /* flags                 = */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* expected_deserialized = */ FALSE,
            /* expected_cleared      = */ TRUE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ TRUE
          }
        };

        check_flush_cache__multi_entry_test(cache_ptr, test_num,
                                            flush_flags, spec_size, spec);
    }


    if ( pass2 )
    {
        int test_num                         = 6;
        unsigned int flush_flags             = H5C2__FLUSH_INVALIDATE_FLAG |
                                               H5C2__FLUSH_MARKED_ENTRIES_FLAG;
        int spec_size                        = 8;
        struct flush_cache_test_spec spec[8] =
        {
          {
            /* entry_num             = */ 0,
            /* entry_type            = */ PICO_ENTRY_TYPE,
            /* entry_index           = */ 100,
            /* insert_flag           = */ FALSE,
            /* dirty_flag            = */ FALSE,
            /* flags                 = */ H5C2__NO_FLAGS_SET,
            /* expected_deserialized = */ TRUE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ TRUE
          },
          {
            /* entry_num             = */ 1,
            /* entry_type            = */ PICO_ENTRY_TYPE,
            /* entry_index           = */ 75,
            /* insert_flag           = */ FALSE,
            /* dirty_flag            = */ TRUE,
            /* flags                 = */ H5C2__NO_FLAGS_SET,
            /* expected_deserialized = */ TRUE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ TRUE,
            /* expected_destroyed    = */ TRUE
          },
          {
            /* entry_num             = */ 2,
            /* entry_type            = */ PICO_ENTRY_TYPE,
            /* entry_index           = */ 25,
            /* insert_flag           = */ TRUE,
            /* dirty_flag            = */ FALSE,
            /* flags                 = */ H5C2__NO_FLAGS_SET,
            /* expected_deserialized = */ FALSE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ TRUE,
            /* expected_destroyed    = */ TRUE
          },
          {
            /* entry_num             = */ 3,
            /* entry_type            = */ PICO_ENTRY_TYPE,
            /* entry_index           = */ 50,
            /* insert_flag           = */ TRUE,
            /* dirty_flag            = */ TRUE,
            /* flags                 = */ H5C2__NO_FLAGS_SET,
            /* expected_deserialized = */ FALSE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ TRUE,
            /* expected_destroyed    = */ TRUE
          },
          {
            /* entry_num             = */ 4,
            /* entry_type            = */ MONSTER_ENTRY_TYPE,
            /* entry_index           = */ 10,
            /* insert_flag           = */ FALSE,
            /* dirty_flag            = */ FALSE,
            /* flags                 = */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* expected_deserialized = */ TRUE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ TRUE
          },
          {
            /* entry_num             = */ 5,
            /* entry_type            = */ MONSTER_ENTRY_TYPE,
            /* entry_index           = */ 20,
            /* insert_flag           = */ FALSE,
            /* dirty_flag            = */ TRUE,
            /* flags                 = */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* expected_deserialized = */ TRUE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ TRUE,
            /* expected_destroyed    = */ TRUE
          },
          {
            /* entry_num             = */ 6,
            /* entry_type            = */ MONSTER_ENTRY_TYPE,
            /* entry_index           = */ 30,
            /* insert_flag           = */ TRUE,
            /* dirty_flag            = */ FALSE,
            /* flags                 = */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* expected_deserialized = */ FALSE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ TRUE,
            /* expected_destroyed    = */ TRUE
          },
          {
            /* entry_num             = */ 7,
            /* entry_type            = */ MONSTER_ENTRY_TYPE,
            /* entry_index           = */ 40,
            /* insert_flag           = */ TRUE,
            /* dirty_flag            = */ TRUE,
            /* flags                 = */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* expected_deserialized = */ FALSE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ TRUE,
            /* expected_destroyed    = */ TRUE
          }
        };

        check_flush_cache__multi_entry_test(cache_ptr, test_num,
                                            flush_flags, spec_size, spec);
    }


    if ( pass2 )
    {
        int test_num                         = 7;
        unsigned int flush_flags             = H5C2__FLUSH_CLEAR_ONLY_FLAG |
                                               H5C2__FLUSH_MARKED_ENTRIES_FLAG;
        int spec_size                        = 8;
        struct flush_cache_test_spec spec[8] =
        {
          {
            /* entry_num             = */ 0,
            /* entry_type            = */ PICO_ENTRY_TYPE,
            /* entry_index           = */ 100,
            /* insert_flag           = */ FALSE,
            /* dirty_flag            = */ FALSE,
            /* flags                 = */ H5C2__NO_FLAGS_SET,
            /* expected_deserialized = */ TRUE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ FALSE
          },
          {
            /* entry_num             = */ 1,
            /* entry_type            = */ PICO_ENTRY_TYPE,
            /* entry_index           = */ 75,
            /* insert_flag           = */ FALSE,
            /* dirty_flag            = */ TRUE,
            /* flags                 = */ H5C2__NO_FLAGS_SET,
            /* expected_deserialized = */ TRUE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ FALSE
          },
          {
            /* entry_num             = */ 2,
            /* entry_type            = */ PICO_ENTRY_TYPE,
            /* entry_index           = */ 25,
            /* insert_flag           = */ TRUE,
            /* dirty_flag            = */ FALSE,
            /* flags                 = */ H5C2__NO_FLAGS_SET,
            /* expected_deserialized = */ FALSE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ FALSE
          },
          {
            /* entry_num             = */ 3,
            /* entry_type            = */ PICO_ENTRY_TYPE,
            /* entry_index           = */ 50,
            /* insert_flag           = */ TRUE,
            /* dirty_flag            = */ TRUE,
            /* flags                 = */ H5C2__NO_FLAGS_SET,
            /* expected_deserialized = */ FALSE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ FALSE
          },
          {
            /* entry_num             = */ 4,
            /* entry_type            = */ MONSTER_ENTRY_TYPE,
            /* entry_index           = */ 10,
            /* insert_flag           = */ FALSE,
            /* dirty_flag            = */ FALSE,
            /* flags                 = */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* expected_deserialized = */ TRUE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ FALSE
          },
          {
            /* entry_num             = */ 5,
            /* entry_type            = */ MONSTER_ENTRY_TYPE,
            /* entry_index           = */ 20,
            /* insert_flag           = */ FALSE,
            /* dirty_flag            = */ TRUE,
            /* flags                 = */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* expected_deserialized = */ TRUE,
            /* expected_cleared      = */ TRUE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ FALSE
          },
          {
            /* entry_num             = */ 6,
            /* entry_type            = */ MONSTER_ENTRY_TYPE,
            /* entry_index           = */ 30,
            /* insert_flag           = */ TRUE,
            /* dirty_flag            = */ FALSE,
            /* flags                 = */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* expected_deserialized = */ FALSE,
            /* expected_cleared      = */ TRUE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ FALSE
          },
          {
            /* entry_num             = */ 7,
            /* entry_type            = */ MONSTER_ENTRY_TYPE,
            /* entry_index           = */ 40,
            /* insert_flag           = */ TRUE,
            /* dirty_flag            = */ TRUE,
            /* flags                 = */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* expected_deserialized = */ FALSE,
            /* expected_cleared      = */ TRUE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ FALSE
          }
        };

        check_flush_cache__multi_entry_test(cache_ptr, test_num,
                                            flush_flags, spec_size, spec);
    }


    if ( pass2 )
    {
        int test_num                         = 8;
        unsigned int flush_flags             = H5C2__FLUSH_INVALIDATE_FLAG |
                                               H5C2__FLUSH_CLEAR_ONLY_FLAG |
                                               H5C2__FLUSH_MARKED_ENTRIES_FLAG;
        int spec_size                        = 8;
        struct flush_cache_test_spec spec[8] =
        {
          {
            /* entry_num             = */ 0,
            /* entry_type            = */ PICO_ENTRY_TYPE,
            /* entry_index           = */ 100,
            /* insert_flag           = */ FALSE,
            /* dirty_flag            = */ FALSE,
            /* flags                 = */ H5C2__NO_FLAGS_SET,
            /* expected_deserialized = */ TRUE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ TRUE
          },
          {
            /* entry_num             = */ 1,
            /* entry_type            = */ PICO_ENTRY_TYPE,
            /* entry_index           = */ 75,
            /* insert_flag           = */ FALSE,
            /* dirty_flag            = */ TRUE,
            /* flags                 = */ H5C2__NO_FLAGS_SET,
            /* expected_deserialized = */ TRUE,
            /* expected_cleared      = */ TRUE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ TRUE
          },
          {
            /* entry_num             = */ 2,
            /* entry_type            = */ PICO_ENTRY_TYPE,
            /* entry_index           = */ 25,
            /* insert_flag           = */ TRUE,
            /* dirty_flag            = */ FALSE,
            /* flags                 = */ H5C2__NO_FLAGS_SET,
            /* expected_deserialized = */ FALSE,
            /* expected_cleared      = */ TRUE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ TRUE
          },
          {
            /* entry_num             = */ 3,
            /* entry_type            = */ PICO_ENTRY_TYPE,
            /* entry_index           = */ 50,
            /* insert_flag           = */ TRUE,
            /* dirty_flag            = */ TRUE,
            /* flags                 = */ H5C2__NO_FLAGS_SET,
            /* expected_deserialized = */ FALSE,
            /* expected_cleared      = */ TRUE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ TRUE
          },
          {
            /* entry_num             = */ 4,
            /* entry_type            = */ MONSTER_ENTRY_TYPE,
            /* entry_index           = */ 10,
            /* insert_flag           = */ FALSE,
            /* dirty_flag            = */ FALSE,
            /* flags                 = */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* expected_deserialized = */ TRUE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ TRUE
          },
          {
            /* entry_num             = */ 5,
            /* entry_type            = */ MONSTER_ENTRY_TYPE,
            /* entry_index           = */ 20,
            /* insert_flag           = */ FALSE,
            /* dirty_flag            = */ TRUE,
            /* flags                 = */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* expected_deserialized = */ TRUE,
            /* expected_cleared      = */ TRUE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ TRUE
          },
          {
            /* entry_num             = */ 6,
            /* entry_type            = */ MONSTER_ENTRY_TYPE,
            /* entry_index           = */ 30,
            /* insert_flag           = */ TRUE,
            /* dirty_flag            = */ FALSE,
            /* flags                 = */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* expected_deserialized = */ FALSE,
            /* expected_cleared      = */ TRUE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ TRUE
          },
          {
            /* entry_num             = */ 7,
            /* entry_type            = */ MONSTER_ENTRY_TYPE,
            /* entry_index           = */ 40,
            /* insert_flag           = */ TRUE,
            /* dirty_flag            = */ TRUE,
            /* flags                 = */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* expected_deserialized = */ FALSE,
            /* expected_cleared      = */ TRUE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ TRUE
          }
        };

        check_flush_cache__multi_entry_test(cache_ptr, test_num,
                                            flush_flags, spec_size, spec);
    }


    /* verify that all other flags are ignored */
    if ( pass2 )
    {
        int test_num                         = 9;
        unsigned int flush_flags             = (unsigned)
                                               ~(H5C2__FLUSH_INVALIDATE_FLAG |
                                                H5C2__FLUSH_CLEAR_ONLY_FLAG |
                                                H5C2__FLUSH_MARKED_ENTRIES_FLAG);
        int spec_size                        = 8;
        struct flush_cache_test_spec spec[8] =
        {
          {
            /* entry_num             = */ 0,
            /* entry_type            = */ PICO_ENTRY_TYPE,
            /* entry_index           = */ 100,
            /* insert_flag           = */ FALSE,
            /* dirty_flag            = */ FALSE,
            /* flags                 = */ H5C2__NO_FLAGS_SET,
            /* expected_deserialized = */ TRUE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ FALSE
          },
          {
            /* entry_num             = */ 1,
            /* entry_type            = */ PICO_ENTRY_TYPE,
            /* entry_index           = */ 75,
            /* insert_flag           = */ FALSE,
            /* dirty_flag            = */ TRUE,
            /* flags                 = */ H5C2__NO_FLAGS_SET,
            /* expected_deserialized = */ TRUE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ TRUE,
            /* expected_destroyed    = */ FALSE
          },
          {
            /* entry_num             = */ 2,
            /* entry_type            = */ PICO_ENTRY_TYPE,
            /* entry_index           = */ 25,
            /* insert_flag           = */ TRUE,
            /* dirty_flag            = */ FALSE,
            /* flags                 = */ H5C2__NO_FLAGS_SET,
            /* expected_deserialized = */ FALSE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ TRUE,
            /* expected_destroyed    = */ FALSE
          },
          {
            /* entry_num             = */ 3,
            /* entry_type            = */ PICO_ENTRY_TYPE,
            /* entry_index           = */ 50,
            /* insert_flag           = */ TRUE,
            /* dirty_flag            = */ TRUE,
            /* flags                 = */ H5C2__NO_FLAGS_SET,
            /* expected_deserialized = */ FALSE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ TRUE,
            /* expected_destroyed    = */ FALSE
          },
          {
            /* entry_num             = */ 4,
            /* entry_type            = */ MONSTER_ENTRY_TYPE,
            /* entry_index           = */ 10,
            /* insert_flag           = */ FALSE,
            /* dirty_flag            = */ FALSE,
            /* flags                 = */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* expected_deserialized = */ TRUE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ FALSE
          },
          {
            /* entry_num             = */ 5,
            /* entry_type            = */ MONSTER_ENTRY_TYPE,
            /* entry_index           = */ 20,
            /* insert_flag           = */ FALSE,
            /* dirty_flag            = */ TRUE,
            /* flags                 = */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* expected_deserialized = */ TRUE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ TRUE,
            /* expected_destroyed    = */ FALSE
          },
          {
            /* entry_num             = */ 6,
            /* entry_type            = */ MONSTER_ENTRY_TYPE,
            /* entry_index           = */ 30,
            /* insert_flag           = */ TRUE,
            /* dirty_flag            = */ FALSE,
            /* flags                 = */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* expected_deserialized = */ FALSE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ TRUE,
            /* expected_destroyed    = */ FALSE
          },
          {
            /* entry_num             = */ 7,
            /* entry_type            = */ MONSTER_ENTRY_TYPE,
            /* entry_index           = */ 40,
            /* insert_flag           = */ TRUE,
            /* dirty_flag            = */ TRUE,
            /* flags                 = */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* expected_deserialized = */ FALSE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ TRUE,
            /* expected_destroyed    = */ FALSE
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

    if ( pass2 )
    {
        int test_num                            = 1;
        unsigned int flush_flags                = H5C2__NO_FLAGS_SET;
        int spec_size                           = 8;
        struct pe_flush_cache_test_spec spec[8] =
        {
          {
            /* entry_num             = */ 0,
            /* entry_type            = */ PICO_ENTRY_TYPE,
            /* entry_index           = */ 100,
            /* insert_flag           = */ FALSE,
            /* dirty_flag            = */ FALSE,
            /* flags                 = */ H5C2__NO_FLAGS_SET,
	    /* num_pins              = */ 0,
	    /* pin_type[MAX_PINS]    = */ {-1, -1, -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]     = */ {-1, -1, -1, -1, -1, -1, -1, -1},
            /* expected_deserialized = */ TRUE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ FALSE
          },
          {
            /* entry_num             = */ 1,
            /* entry_type            = */ PICO_ENTRY_TYPE,
            /* entry_index           = */ 75,
            /* insert_flag           = */ FALSE,
            /* dirty_flag            = */ TRUE,
            /* flags                 = */ H5C2__NO_FLAGS_SET,
	    /* num_pins              = */ 1,
	    /* pin_type[MAX_PINS]    = */ {PICO_ENTRY_TYPE,
		                           -1, -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]     = */ {100, -1, -1, -1, -1, -1, -1, -1},
            /* expected_deserialized = */ TRUE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ TRUE,
            /* expected_destroyed    = */ FALSE
          },
          {
            /* entry_num             = */ 2,
            /* entry_type            = */ PICO_ENTRY_TYPE,
            /* entry_index           = */ 25,
            /* insert_flag           = */ TRUE,
            /* dirty_flag            = */ FALSE,
            /* flags                 = */ H5C2__NO_FLAGS_SET,
	    /* num_pins              = */ 2,
	    /* pin_type[MAX_PINS]    = */ {PICO_ENTRY_TYPE,
		                           PICO_ENTRY_TYPE,
		                           -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]     = */ {100, 75, -1, -1, -1, -1, -1, -1},
            /* expected_deserialized = */ FALSE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ TRUE,
            /* expected_destroyed    = */ FALSE
          },
          {
            /* entry_num             = */ 3,
            /* entry_type            = */ PICO_ENTRY_TYPE,
            /* entry_index           = */ 50,
            /* insert_flag           = */ TRUE,
            /* dirty_flag            = */ TRUE,
            /* flags                 = */ H5C2__NO_FLAGS_SET,
	    /* num_pins              = */ 3,
	    /* pin_type[MAX_PINS]    = */ {PICO_ENTRY_TYPE,
		                           PICO_ENTRY_TYPE,
					   PICO_ENTRY_TYPE,
		                           -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]     = */ {100, 75, 25, -1, -1, -1, -1, -1},
            /* expected_deserialized = */ FALSE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ TRUE,
            /* expected_destroyed    = */ FALSE
          },
          {
            /* entry_num             = */ 4,
            /* entry_type            = */ MONSTER_ENTRY_TYPE,
            /* entry_index           = */ 10,
            /* insert_flag           = */ FALSE,
            /* dirty_flag            = */ FALSE,
            /* flags                 = */ H5C2__SET_FLUSH_MARKER_FLAG,
	    /* num_pins              = */ 4,
	    /* pin_type[MAX_PINS]    = */ {PICO_ENTRY_TYPE,
                                           PICO_ENTRY_TYPE,
                                           PICO_ENTRY_TYPE,
					   PICO_ENTRY_TYPE,
					   -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]     = */ {100, 75, 25, 50, -1, -1, -1, -1},
            /* expected_deserialized = */ TRUE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ FALSE
          },
          {
            /* entry_num             = */ 5,
            /* entry_type            = */ MONSTER_ENTRY_TYPE,
            /* entry_index           = */ 20,
            /* insert_flag           = */ FALSE,
            /* dirty_flag            = */ TRUE,
            /* flags                 = */ H5C2__SET_FLUSH_MARKER_FLAG,
	    /* num_pins              = */ 5,
	    /* pin_type[MAX_PINS]    = */ {PICO_ENTRY_TYPE,
                                           PICO_ENTRY_TYPE,
                                           PICO_ENTRY_TYPE,
					   PICO_ENTRY_TYPE,
					   MONSTER_ENTRY_TYPE,
					   -1, -1, -1},
	    /* pin_idx[MAX_PINS]     = */ {100, 75, 25, 50, 10, -1, -1, -1},
            /* expected_deserialized = */ TRUE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ TRUE,
            /* expected_destroyed    = */ FALSE
          },
          {
            /* entry_num             = */ 6,
            /* entry_type            = */ MONSTER_ENTRY_TYPE,
            /* entry_index           = */ 30,
            /* insert_flag           = */ TRUE,
            /* dirty_flag            = */ FALSE,
            /* flags                 = */ H5C2__SET_FLUSH_MARKER_FLAG,
	    /* num_pins              = */ 6,
	    /* pin_type[MAX_PINS]    = */ {PICO_ENTRY_TYPE,
                                           PICO_ENTRY_TYPE,
                                           PICO_ENTRY_TYPE,
					   PICO_ENTRY_TYPE,
				 	   MONSTER_ENTRY_TYPE,
					   MONSTER_ENTRY_TYPE,
					   -1, -1},
	    /* pin_idx[MAX_PINS]     = */ {100, 75, 25, 50, 10, 20, -1, -1},
            /* expected_deserialized = */ FALSE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ TRUE,
            /* expected_destroyed    = */ FALSE
          },
          {
            /* entry_num             = */ 7,
            /* entry_type            = */ MONSTER_ENTRY_TYPE,
            /* entry_index           = */ 40,
            /* insert_flag           = */ TRUE,
            /* dirty_flag            = */ TRUE,
            /* flags                 = */ H5C2__SET_FLUSH_MARKER_FLAG,
	    /* num_pins              = */ 7,
	    /* pin_type[MAX_PINS]    = */ {PICO_ENTRY_TYPE,
                                           PICO_ENTRY_TYPE,
                                           PICO_ENTRY_TYPE,
					   PICO_ENTRY_TYPE,
					   MONSTER_ENTRY_TYPE,
					   MONSTER_ENTRY_TYPE,
					   MONSTER_ENTRY_TYPE,
					   -1},
	    /* pin_idx[MAX_PINS]     = */ {100, 75, 25, 50, 10, 20, 30, -1},
            /* expected_deserialized = */ FALSE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ TRUE,
            /* expected_destroyed    = */ FALSE
          }
        };

        check_flush_cache__pe_multi_entry_test(cache_ptr, test_num,
                                               flush_flags, spec_size, spec);
    }


    if ( pass2 )
    {
        int test_num                            = 2;
        unsigned int flush_flags                = H5C2__FLUSH_INVALIDATE_FLAG;
        int spec_size                           = 8;
        struct pe_flush_cache_test_spec spec[8] =
        {
          {
            /* entry_num             = */ 0,
            /* entry_type            = */ PICO_ENTRY_TYPE,
            /* entry_index           = */ 100,
            /* insert_flag           = */ FALSE,
            /* dirty_flag            = */ FALSE,
            /* flags                 = */ H5C2__NO_FLAGS_SET,
	    /* num_pins              = */ 0,
	    /* pin_type[MAX_PINS]    = */ {-1, -1, -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]     = */ {-1, -1, -1, -1, -1, -1, -1, -1},
            /* expected_deserialized = */ TRUE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ TRUE
          },
          {
            /* entry_num             = */ 1,
            /* entry_type            = */ PICO_ENTRY_TYPE,
            /* entry_index           = */ 75,
            /* insert_flag           = */ FALSE,
            /* dirty_flag            = */ TRUE,
            /* flags                 = */ H5C2__NO_FLAGS_SET,
	    /* num_pins              = */ 1,
	    /* pin_type[MAX_PINS]    = */ {PICO_ENTRY_TYPE,
		                           -1, -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]     = */ {100, -1, -1, -1, -1, -1, -1, -1},
            /* expected_deserialized = */ TRUE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ TRUE,
            /* expected_destroyed    = */ TRUE
          },
          {
            /* entry_num             = */ 2,
            /* entry_type            = */ PICO_ENTRY_TYPE,
            /* entry_index           = */ 25,
            /* insert_flag           = */ TRUE,
            /* dirty_flag            = */ FALSE,
            /* flags                 = */ H5C2__NO_FLAGS_SET,
	    /* num_pins              = */ 2,
	    /* pin_type[MAX_PINS]    = */ {PICO_ENTRY_TYPE,
		                           PICO_ENTRY_TYPE,
		                           -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]  = */ {100, 75, -1, -1, -1, -1, -1, -1},
            /* expected_deserialized = */ FALSE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ TRUE,
            /* expected_destroyed    = */ TRUE
          },
          {
            /* entry_num             = */ 3,
            /* entry_type            = */ PICO_ENTRY_TYPE,
            /* entry_index           = */ 50,
            /* insert_flag           = */ TRUE,
            /* dirty_flag            = */ TRUE,
            /* flags                 = */ H5C2__NO_FLAGS_SET,
	    /* num_pins              = */ 3,
	    /* pin_type[MAX_PINS]    = */ {PICO_ENTRY_TYPE,
		                           PICO_ENTRY_TYPE,
					   PICO_ENTRY_TYPE,
		                           -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]     = */ {100, 75, 25, -1, -1, -1, -1, -1},
            /* expected_deserialized = */ FALSE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ TRUE,
            /* expected_destroyed    = */ TRUE
          },
          {
            /* entry_num             = */ 4,
            /* entry_type            = */ MONSTER_ENTRY_TYPE,
            /* entry_index           = */ 10,
            /* insert_flag           = */ FALSE,
            /* dirty_flag            = */ FALSE,
            /* flags                 = */ H5C2__SET_FLUSH_MARKER_FLAG,
	    /* num_pins              = */ 0,
	    /* pin_type[MAX_PINS]    = */ {-1, -1, -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]     = */ {-1, -1, -1, -1, -1, -1, -1, -1},
            /* expected_deserialized = */ TRUE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ TRUE
          },
          {
            /* entry_num             = */ 5,
            /* entry_type            = */ MONSTER_ENTRY_TYPE,
            /* entry_index           = */ 20,
            /* insert_flag           = */ FALSE,
            /* dirty_flag            = */ TRUE,
            /* flags                 = */ H5C2__SET_FLUSH_MARKER_FLAG,
	    /* num_pins              = */ 1,
	    /* pin_type[MAX_PINS]    = */ {MONSTER_ENTRY_TYPE,
					   -1, -1, -1, -1 -1, -1, -1},
	    /* pin_idx[MAX_PINS]     = */ {10, -1, -1, -1 -1, -1, -1, -1},
            /* expected_deserialized = */ TRUE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ TRUE,
            /* expected_destroyed    = */ TRUE
          },
          {
            /* entry_num             = */ 6,
            /* entry_type            = */ MONSTER_ENTRY_TYPE,
            /* entry_index           = */ 30,
            /* insert_flag           = */ TRUE,
            /* dirty_flag            = */ FALSE,
            /* flags                 = */ H5C2__SET_FLUSH_MARKER_FLAG,
	    /* num_pins              = */ 2,
	    /* pin_type[MAX_PINS]    = */ {MONSTER_ENTRY_TYPE,
					   MONSTER_ENTRY_TYPE,
					   -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]     = */ {10, 20, -1, -1, -1, -1, -1, -1},
            /* expected_deserialized = */ FALSE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ TRUE,
            /* expected_destroyed    = */ TRUE
          },
          {
            /* entry_num             = */ 7,
            /* entry_type            = */ MONSTER_ENTRY_TYPE,
            /* entry_index           = */ 40,
            /* insert_flag           = */ TRUE,
            /* dirty_flag            = */ TRUE,
            /* flags                 = */ H5C2__SET_FLUSH_MARKER_FLAG,
	    /* num_pins              = */ 3,
	    /* pin_type[MAX_PINS]    = */ {MONSTER_ENTRY_TYPE,
					   MONSTER_ENTRY_TYPE,
					   MONSTER_ENTRY_TYPE,
					   -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]     = */ {10, 20, 30, -1, -1, -1, -1, -1},
            /* expected_deserialized = */ FALSE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ TRUE,
            /* expected_destroyed    = */ TRUE
          }
        };

        check_flush_cache__pe_multi_entry_test(cache_ptr, test_num,
                                               flush_flags, spec_size, spec);
    }

    if ( pass2 )
    {
        int test_num                            = 3;
        unsigned int flush_flags                = H5C2__FLUSH_INVALIDATE_FLAG |
                                                  H5C2__FLUSH_CLEAR_ONLY_FLAG;
        int spec_size                           = 8;
        struct pe_flush_cache_test_spec spec[8] =
        {
          {
            /* entry_num             = */ 0,
            /* entry_type            = */ PICO_ENTRY_TYPE,
            /* entry_index           = */ 100,
            /* insert_flag           = */ FALSE,
            /* dirty_flag            = */ FALSE,
            /* flags                 = */ H5C2__NO_FLAGS_SET,
	    /* num_pins              = */ 0,
	    /* pin_type[MAX_PINS]    = */ {-1, -1, -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]     = */ {-1, -1, -1, -1, -1, -1, -1, -1},
            /* expected_deserialized = */ TRUE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ TRUE
          },
          {
            /* entry_num             = */ 1,
            /* entry_type            = */ PICO_ENTRY_TYPE,
            /* entry_index           = */ 75,
            /* insert_flag           = */ FALSE,
            /* dirty_flag            = */ TRUE,
            /* flags                 = */ H5C2__NO_FLAGS_SET,
	    /* num_pins              = */ 1,
	    /* pin_type[MAX_PINS]    = */ {PICO_ENTRY_TYPE,
		                           -1, -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]     = */ {100, -1, -1, -1, -1, -1, -1, -1},
            /* expected_deserialized = */ TRUE,
            /* expected_cleared      = */ TRUE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ TRUE
          },
          {
            /* entry_num             = */ 2,
            /* entry_type            = */ PICO_ENTRY_TYPE,
            /* entry_index           = */ 25,
            /* insert_flag           = */ TRUE,
            /* dirty_flag            = */ FALSE,
            /* flags                 = */ H5C2__NO_FLAGS_SET,
	    /* num_pins              = */ 1,
	    /* pin_type[MAX_PINS]    = */ {PICO_ENTRY_TYPE,
		                           -1, -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]     = */ {100, -1, -1, -1, -1, -1, -1, -1},
            /* expected_deserialized = */ FALSE,
            /* expected_cleared      = */ TRUE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ TRUE
          },
          {
            /* entry_num             = */ 3,
            /* entry_type            = */ PICO_ENTRY_TYPE,
            /* entry_index           = */ 50,
            /* insert_flag           = */ TRUE,
            /* dirty_flag            = */ TRUE,
            /* flags                 = */ H5C2__NO_FLAGS_SET,
	    /* num_pins              = */ 1,
	    /* pin_type[MAX_PINS]    = */ {PICO_ENTRY_TYPE,
		                           -1, -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]     = */ {100, -1, -1, -1, -1, -1, -1, -1},
            /* expected_deserialized = */ FALSE,
            /* expected_cleared      = */ TRUE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ TRUE
          },
          {
            /* entry_num             = */ 4,
            /* entry_type            = */ MONSTER_ENTRY_TYPE,
            /* entry_index           = */ 10,
            /* insert_flag           = */ FALSE,
            /* dirty_flag            = */ FALSE,
            /* flags                 = */ H5C2__SET_FLUSH_MARKER_FLAG,
	    /* num_pins              = */ 0,
	    /* pin_type[MAX_PINS]    = */ {-1, -1, -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]     = */ {-1, -1, -1, -1, -1, -1, -1, -1},
            /* expected_deserialized = */ TRUE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ TRUE
          },
          {
            /* entry_num             = */ 5,
            /* entry_type            = */ MONSTER_ENTRY_TYPE,
            /* entry_index           = */ 20,
            /* insert_flag           = */ FALSE,
            /* dirty_flag            = */ TRUE,
            /* flags                 = */ H5C2__SET_FLUSH_MARKER_FLAG,
	    /* num_pins              = */ 0,
	    /* pin_type[MAX_PINS]    = */ {-1, -1, -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]     = */ {-1, -1, -1, -1, -1, -1, -1, -1},
            /* expected_deserialized = */ TRUE,
            /* expected_cleared      = */ TRUE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ TRUE
          },
          {
            /* entry_num             = */ 6,
            /* entry_type            = */ MONSTER_ENTRY_TYPE,
            /* entry_index           = */ 30,
            /* insert_flag           = */ TRUE,
            /* dirty_flag            = */ FALSE,
            /* flags                 = */ H5C2__SET_FLUSH_MARKER_FLAG,
	    /* num_pins              = */ 0,
	    /* pin_type[MAX_PINS]    = */ {-1, -1, -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]     = */ {-1, -1, -1, -1, -1, -1, -1, -1},
            /* expected_deserialized = */ FALSE,
            /* expected_cleared      = */ TRUE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ TRUE
          },
          {
            /* entry_num             = */ 7,
            /* entry_type            = */ MONSTER_ENTRY_TYPE,
            /* entry_index           = */ 40,
            /* insert_flag           = */ TRUE,
            /* dirty_flag            = */ TRUE,
            /* flags                 = */ H5C2__SET_FLUSH_MARKER_FLAG,
	    /* num_pins              = */ 0,
	    /* pin_type[MAX_PINS]    = */ {-1, -1, -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]     = */ {-1, -1, -1, -1, -1, -1, -1, -1},
            /* expected_deserialized = */ FALSE,
            /* expected_cleared      = */ TRUE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ TRUE
          }
        };

        check_flush_cache__pe_multi_entry_test(cache_ptr, test_num,
                                               flush_flags, spec_size, spec);
    }


    if ( pass2 )
    {
        int test_num                            = 4;
        unsigned int flush_flags                = H5C2__FLUSH_INVALIDATE_FLAG |
                                                 H5C2__FLUSH_MARKED_ENTRIES_FLAG;
        int spec_size                           = 8;
        struct pe_flush_cache_test_spec spec[8] =
        {
          {
            /* entry_num             = */ 0,
            /* entry_type            = */ PICO_ENTRY_TYPE,
            /* entry_index           = */ 100,
            /* insert_flag           = */ FALSE,
            /* dirty_flag            = */ FALSE,
            /* flags                 = */ H5C2__NO_FLAGS_SET,
	    /* num_pins              = */ 0,
	    /* pin_type[MAX_PINS]    = */ {-1, -1, -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]     = */ {-1, -1, -1, -1, -1, -1, -1, -1},
            /* expected_deserialized = */ TRUE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ TRUE
          },
          {
            /* entry_num             = */ 1,
            /* entry_type            = */ PICO_ENTRY_TYPE,
            /* entry_index           = */ 75,
            /* insert_flag           = */ FALSE,
            /* dirty_flag            = */ TRUE,
            /* flags                 = */ H5C2__NO_FLAGS_SET,
	    /* num_pins              = */ 1,
	    /* pin_type[MAX_PINS]    = */ {PICO_ENTRY_TYPE,
		                           -1, -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]     = */ {100, -1, -1, -1, -1, -1, -1, -1},
            /* expected_deserialized = */ TRUE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ TRUE,
            /* expected_destroyed    = */ TRUE
          },
          {
            /* entry_num             = */ 2,
            /* entry_type            = */ PICO_ENTRY_TYPE,
            /* entry_index           = */ 25,
            /* insert_flag           = */ TRUE,
            /* dirty_flag            = */ FALSE,
            /* flags                 = */ H5C2__NO_FLAGS_SET,
	    /* num_pins              = */ 1,
	    /* pin_type[MAX_PINS]    = */ {PICO_ENTRY_TYPE,
		                           -1, -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]     = */ {100, -1, -1, -1, -1, -1, -1, -1},
            /* expected_deserialized = */ FALSE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ TRUE,
            /* expected_destroyed    = */ TRUE
          },
          {
            /* entry_num             = */ 3,
            /* entry_type            = */ PICO_ENTRY_TYPE,
            /* entry_index           = */ 50,
            /* insert_flag           = */ TRUE,
            /* dirty_flag            = */ TRUE,
            /* flags                 = */ H5C2__NO_FLAGS_SET,
	    /* num_pins              = */ 1,
	    /* pin_type[MAX_PINS]    = */ {PICO_ENTRY_TYPE,
		                           -1, -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]     = */ {100, -1, -1, -1, -1, -1, -1, -1},
            /* expected_deserialized = */ FALSE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ TRUE,
            /* expected_destroyed    = */ TRUE
          },
          {
            /* entry_num             = */ 4,
            /* entry_type            = */ MONSTER_ENTRY_TYPE,
            /* entry_index           = */ 10,
            /* insert_flag           = */ FALSE,
            /* dirty_flag            = */ FALSE,
            /* flags                 = */ H5C2__SET_FLUSH_MARKER_FLAG,
	    /* num_pins              = */ 0,
	    /* pin_type[MAX_PINS]    = */ {-1, -1, -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]     = */ {-1, -1, -1, -1, -1, -1, -1, -1},
            /* expected_deserialized = */ TRUE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ TRUE
          },
          {
            /* entry_num             = */ 5,
            /* entry_type            = */ MONSTER_ENTRY_TYPE,
            /* entry_index           = */ 20,
            /* insert_flag           = */ FALSE,
            /* dirty_flag            = */ TRUE,
            /* flags                 = */ H5C2__SET_FLUSH_MARKER_FLAG,
	    /* num_pins              = */ 4,
	    /* pin_type[MAX_PINS]    = */ {PICO_ENTRY_TYPE,
                                           PICO_ENTRY_TYPE,
                                           PICO_ENTRY_TYPE,
					   PICO_ENTRY_TYPE,
					   -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]     = */ {100, 75, 25, 50, -1, -1, -1, -1},
            /* expected_deserialized = */ TRUE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ TRUE,
            /* expected_destroyed    = */ TRUE
          },
          {
            /* entry_num             = */ 6,
            /* entry_type            = */ MONSTER_ENTRY_TYPE,
            /* entry_index           = */ 30,
            /* insert_flag           = */ TRUE,
            /* dirty_flag            = */ FALSE,
            /* flags                 = */ H5C2__SET_FLUSH_MARKER_FLAG,
	    /* num_pins              = */ 4,
	    /* pin_type[MAX_PINS]    = */ {PICO_ENTRY_TYPE,
                                           PICO_ENTRY_TYPE,
                                           PICO_ENTRY_TYPE,
					   PICO_ENTRY_TYPE,
					   -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]     = */ {100, 75, 25, 50, -1, -1, -1, -1},
            /* expected_deserialized = */ FALSE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ TRUE,
            /* expected_destroyed    = */ TRUE
          },
          {
            /* entry_num             = */ 7,
            /* entry_type            = */ MONSTER_ENTRY_TYPE,
            /* entry_index           = */ 40,
            /* insert_flag           = */ TRUE,
            /* dirty_flag            = */ TRUE,
            /* flags                 = */ H5C2__SET_FLUSH_MARKER_FLAG,
	    /* num_pins              = */ 0,
	    /* pin_type[MAX_PINS]    = */ {-1, -1, -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]     = */ {-1, -1, -1, -1, -1, -1, -1, -1},
            /* expected_deserialized = */ FALSE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ TRUE,
            /* expected_destroyed    = */ TRUE
          }
        };

        check_flush_cache__pe_multi_entry_test(cache_ptr, test_num,
                                               flush_flags, spec_size, spec);
    }


    if ( pass2 )
    {
        int test_num                            = 5;
        unsigned int flush_flags                = H5C2__FLUSH_INVALIDATE_FLAG |
                                                  H5C2__FLUSH_CLEAR_ONLY_FLAG |
                                                 H5C2__FLUSH_MARKED_ENTRIES_FLAG;
        int spec_size                           = 8;
        struct pe_flush_cache_test_spec spec[8] =
        {
          {
            /* entry_num             = */ 0,
            /* entry_type            = */ PICO_ENTRY_TYPE,
            /* entry_index           = */ 100,
            /* insert_flag           = */ FALSE,
            /* dirty_flag            = */ FALSE,
            /* flags                 = */ H5C2__NO_FLAGS_SET,
	    /* num_pins              = */ 0,
	    /* pin_type[MAX_PINS]    = */ {-1, -1, -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]     = */ {-1, -1, -1, -1, -1, -1, -1, -1},
            /* expected_deserialized = */ TRUE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ TRUE
          },
          {
            /* entry_num             = */ 1,
            /* entry_type            = */ PICO_ENTRY_TYPE,
            /* entry_index           = */ 75,
            /* insert_flag           = */ FALSE,
            /* dirty_flag            = */ TRUE,
            /* flags                 = */ H5C2__NO_FLAGS_SET,
	    /* num_pins              = */ 1,
	    /* pin_type[MAX_PINS]    = */ {PICO_ENTRY_TYPE,
		                           -1, -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]     = */ {100, -1, -1, -1, -1, -1, -1, -1},
            /* expected_deserialized = */ TRUE,
            /* expected_cleared      = */ TRUE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ TRUE
          },
          {
            /* entry_num             = */ 2,
            /* entry_type            = */ PICO_ENTRY_TYPE,
            /* entry_index           = */ 25,
            /* insert_flag           = */ TRUE,
            /* dirty_flag            = */ FALSE,
            /* flags                 = */ H5C2__NO_FLAGS_SET,
	    /* num_pins              = */ 1,
	    /* pin_type[MAX_PINS]    = */ {PICO_ENTRY_TYPE,
		                           -1, -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]     = */ {100, -1, -1, -1, -1, -1, -1, -1},
            /* expected_deserialized = */ FALSE,
            /* expected_cleared      = */ TRUE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ TRUE
          },
          {
            /* entry_num             = */ 3,
            /* entry_type            = */ PICO_ENTRY_TYPE,
            /* entry_index           = */ 50,
            /* insert_flag           = */ TRUE,
            /* dirty_flag            = */ TRUE,
            /* flags                 = */ H5C2__NO_FLAGS_SET,
	    /* num_pins              = */ 1,
	    /* pin_type[MAX_PINS]    = */ {PICO_ENTRY_TYPE,
		                           -1, -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]     = */ {100, -1, -1, -1, -1, -1, -1, -1},
            /* expected_deserialized = */ FALSE,
            /* expected_cleared      = */ TRUE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ TRUE
          },
          {
            /* entry_num             = */ 4,
            /* entry_type            = */ MONSTER_ENTRY_TYPE,
            /* entry_index           = */ 10,
            /* insert_flag           = */ FALSE,
            /* dirty_flag            = */ FALSE,
            /* flags                 = */ H5C2__SET_FLUSH_MARKER_FLAG,
	    /* num_pins              = */ 1,
	    /* pin_type[MAX_PINS]    = */ {PICO_ENTRY_TYPE,
		                           -1, -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]     = */ {100, -1, -1, -1, -1, -1, -1, -1},
            /* expected_deserialized = */ TRUE,
            /* expected_cleared      = */ FALSE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ TRUE
          },
          {
            /* entry_num             = */ 5,
            /* entry_type            = */ MONSTER_ENTRY_TYPE,
            /* entry_index           = */ 20,
            /* insert_flag           = */ FALSE,
            /* dirty_flag            = */ TRUE,
            /* flags                 = */ H5C2__SET_FLUSH_MARKER_FLAG,
	    /* num_pins              = */ 1,
	    /* pin_type[MAX_PINS]    = */ {PICO_ENTRY_TYPE,
		                           -1, -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]     = */ {100, -1, -1, -1, -1, -1, -1, -1},
            /* expected_deserialized = */ TRUE,
            /* expected_cleared      = */ TRUE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ TRUE
          },
          {
            /* entry_num             = */ 6,
            /* entry_type            = */ MONSTER_ENTRY_TYPE,
            /* entry_index           = */ 30,
            /* insert_flag           = */ TRUE,
            /* dirty_flag            = */ FALSE,
            /* flags                 = */ H5C2__SET_FLUSH_MARKER_FLAG,
	    /* num_pins              = */ 1,
	    /* pin_type[MAX_PINS]    = */ {PICO_ENTRY_TYPE,
		                           -1, -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]     = */ {100, -1, -1, -1, -1, -1, -1, -1},
            /* expected_deserialized = */ FALSE,
            /* expected_cleared      = */ TRUE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ TRUE
          },
          {
            /* entry_num             = */ 7,
            /* entry_type            = */ MONSTER_ENTRY_TYPE,
            /* entry_index           = */ 40,
            /* insert_flag           = */ TRUE,
            /* dirty_flag            = */ TRUE,
            /* flags                 = */ H5C2__SET_FLUSH_MARKER_FLAG,
	    /* num_pins              = */ 1,
	    /* pin_type[MAX_PINS]    = */ {PICO_ENTRY_TYPE,
		                           -1, -1, -1, -1, -1, -1, -1},
	    /* pin_idx[MAX_PINS]     = */ {100, -1, -1, -1, -1, -1, -1, -1},
            /* expected_deserialized = */ FALSE,
            /* expected_cleared      = */ TRUE,
            /* expected_serialized   = */ FALSE,
            /* expected_destroyed    = */ TRUE
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
check_flush_cache__multi_entry_test(H5C2_t * cache_ptr,
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

#if 0 /* JRM */
    /* This gets used a lot, so lets leave it in. */

    HDfprintf(stdout, "check_flush_cache__multi_entry_test: test %d\n",
	      test_num);
#endif /* JRM */

    if ( cache_ptr == NULL ) {

        pass2 = FALSE;
        HDsnprintf(msg, (size_t)128,
                   "cache_ptr NULL on entry to single entry test #%d.",
                   test_num);
        failure_mssg2 = msg;
    }
    else if ( ( cache_ptr->index_len != 0 ) ||
              ( cache_ptr->index_size != 0 ) ) {

        pass2 = FALSE;

        HDsnprintf(msg, (size_t)128,
                   "cache not empty at beginning of multi entry test #%d.",
                   test_num);
        failure_mssg2 = msg;
    }
    else if ( ( spec_size < 1 ) || ( spec == NULL ) ) {

        pass2 = FALSE;
        HDsnprintf(msg, (size_t)128,
                   "missing/bad test spec on entry to multi entry test #%d.",
                   test_num);
        failure_mssg2 = msg;
    }

    i = 0;
    while ( ( pass2 ) && ( i < spec_size ) )
    {
        if ( ( spec[i].entry_num != i ) ||
             ( spec[i].entry_type < 0 ) ||
             ( spec[i].entry_type >= NUMBER_OF_ENTRY_TYPES ) ||
             ( spec[i].entry_index < 0 ) ||
             ( spec[i].entry_index > max_indices2[spec[i].entry_type] ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "bad data in spec[%d] on entry to multi entry test #%d.",
                       i, test_num);
            failure_mssg2 = msg;
        }
        i++;
    }

    i = 0;
    while ( ( pass2 ) && ( i < spec_size ) )
    {
        if ( spec[i].insert_flag ) {

            insert_entry2(cache_ptr, spec[i].entry_type, spec[i].entry_index,
                         spec[i].dirty_flag, spec[i].flags);

        } else {

            protect_entry2(cache_ptr, spec[i].entry_type, spec[i].entry_index);

            unprotect_entry2(cache_ptr, spec[i].entry_type, spec[i].entry_index,
                            (int)(spec[i].dirty_flag), spec[i].flags);
        }

        total_entry_size += entry_sizes2[spec[i].entry_type];

        i++;
    }

    if ( pass2 ) {

        result = H5C2_flush_cache(cache_ptr, H5P_DATASET_XFER_DEFAULT, 
			          flush_flags);

        if ( result < 0 ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "flush with flags 0x%x failed in multi entry test #%d.",
                       flush_flags, test_num);
            failure_mssg2 = msg;
        }
    }

    i = 0;
    while ( ( pass2 ) && ( i < spec_size ) )
    {
        base_addr = entries2[spec[i].entry_type];
        entry_ptr = &(base_addr[spec[i].entry_index]);

#ifndef NDEBUG
        /* The clear_dirty_bits() callback is only called in debug mode --
         * thus we can only do our full test on the expected entry history
         * when debug is enabled.
         */
        if ( ( entry_ptr->deserialized != spec[i].expected_deserialized ) ||
             ( entry_ptr->cleared != spec[i].expected_cleared ) ||
             ( entry_ptr->serialized != spec[i].expected_serialized ) ||
             ( entry_ptr->destroyed != spec[i].expected_destroyed ) ) {
#else
        /* When in procduction mode, the clear_dirty_bits() callback is
         * not called, so entry_ptr->cleared should never be set.
         */
        if ( ( entry_ptr->deserialized != spec[i].expected_deserialized ) ||
             ( entry_ptr->cleared ) ||
             ( entry_ptr->serialized != spec[i].expected_serialized ) ||
             ( entry_ptr->destroyed != spec[i].expected_destroyed ) ) {
#endif /* NDEBUG */

#if 0 /* This is useful debugging code.  Lets keep it around. */

            HDfprintf(stdout,
              "deslzd = %d(%d), clrd = %d(%d), slzd = %d(%d), dest = %d(%d)\n",
              (int)(entry_ptr->deserialized),
              (int)(spec[i].expected_deserialized),
              (int)(entry_ptr->cleared),
              (int)(spec[i].expected_cleared),
              (int)(entry_ptr->serialized),
              (int)(spec[i].expected_serialized),
              (int)(entry_ptr->destroyed),
              (int)(spec[i].expected_destroyed));

#endif

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
                "Bad status on entry %d after flush in multi entry test #%d.",
                i, test_num);
            failure_mssg2 = msg;
        }
        i++;
    }

    if ( pass2 ) {

        if ( ( ( (flush_flags & H5C2__FLUSH_INVALIDATE_FLAG) == 0 )
               &&
               ( ( cache_ptr->index_len != spec_size )
                 ||
                 ( cache_ptr->index_size != total_entry_size )
               )
             )
             ||
             ( ( (flush_flags & H5C2__FLUSH_INVALIDATE_FLAG) != 0 )
               &&
               ( ( cache_ptr->index_len != 0 )
                 ||
                 ( cache_ptr->index_size != 0 )
               )
             )
           ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
              "Unexpected cache len/size after flush in multi entry test #%d.",
              test_num);
            failure_mssg2 = msg;
        }
    }

    /* clean up the cache to prep for the next test */
    if ( pass2 ) {

        result = H5C2_flush_cache(cache_ptr, H5P_DATASET_XFER_DEFAULT,
                                  H5C2__FLUSH_INVALIDATE_FLAG);

        if ( result < 0 ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "Flush failed on cleanup in multi entry test #%d.",
                       test_num);
            failure_mssg2 = msg;
        }
        else if ( ( cache_ptr->index_len != 0 ) ||
                  ( cache_ptr->index_size != 0 ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
            "Unexpected cache len/size after cleanup in multi entry test #%d.",
            test_num);
            failure_mssg2 = msg;

        }
    }

    i = 0;
    while ( ( pass2 ) && ( i < spec_size ) )
    {
        base_addr = entries2[spec[i].entry_type];
        entry_ptr = &(base_addr[spec[i].entry_index]);

        entry_ptr->deserialized = FALSE;
        entry_ptr->cleared      = FALSE;
        entry_ptr->serialized   = FALSE;
        entry_ptr->destroyed    = FALSE;

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
check_flush_cache__pe_multi_entry_test(H5C2_t * cache_ptr,
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

#if 0 /* JRM */
    /* This is useful debugging code.  Leave it in for now. */

    HDfprintf(stdout, "check_flush_cache__pe_multi_entry_test: test %d\n",
	      test_num);
#endif /* JRM */

    if ( cache_ptr == NULL ) {

        pass2 = FALSE;
        HDsnprintf(msg, (size_t)128,
                   "cache_ptr NULL on entry to pe multi entry test #%d.",
                   test_num);
        failure_mssg2 = msg;
    }
    else if ( ( cache_ptr->index_len != 0 ) ||
              ( cache_ptr->index_size != 0 ) ) {

        pass2 = FALSE;

        HDsnprintf(msg, (size_t)128,
                   "cache not empty at beginning of pe multi entry test #%d.",
                   test_num);
        failure_mssg2 = msg;
    }
    else if ( ( spec_size < 1 ) || ( spec == NULL ) ) {

        pass2 = FALSE;
        HDsnprintf(msg, (size_t)128,
                   "missing/bad test spec on entry to pe multi entry test #%d.",
                   test_num);
        failure_mssg2 = msg;
    }

    i = 0;
    while ( ( pass2 ) && ( i < spec_size ) )
    {
        if ( ( spec[i].entry_num != i ) ||
             ( spec[i].entry_type < 0 ) ||
             ( spec[i].entry_type >= NUMBER_OF_ENTRY_TYPES ) ||
             ( spec[i].entry_index < 0 ) ||
             ( spec[i].entry_index > max_indices2[spec[i].entry_type] ) ||
	     ( spec[i].num_pins < 0 ) ||
	     ( spec[i].num_pins > MAX_PINS ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
                    "bad data in spec[%d] on entry to pe multi entry test #%d.",
                    i, test_num);
            failure_mssg2 = msg;
        }
        i++;
    }

    i = 0;
    while ( ( pass2 ) && ( i < spec_size ) )
    {
        if ( spec[i].insert_flag ) {

            insert_entry2(cache_ptr, spec[i].entry_type, spec[i].entry_index,
                         spec[i].dirty_flag, spec[i].flags);

        } else {

            protect_entry2(cache_ptr, spec[i].entry_type, spec[i].entry_index);

            unprotect_entry2(cache_ptr, spec[i].entry_type, spec[i].entry_index,
                            (int)(spec[i].dirty_flag), spec[i].flags);
        }

        total_entry_size += entry_sizes2[spec[i].entry_type];

	for ( j = 0; j < spec[i].num_pins; j++ )
	{
            create_pinned_entry_dependency2(cache_ptr,
		                           spec[i].entry_type,
					   spec[i].entry_index,
					   spec[i].pin_type[j],
					   spec[i].pin_idx[j]);
	}

        i++;
    }

    if ( pass2 ) {

        result = H5C2_flush_cache(cache_ptr, H5P_DATASET_XFER_DEFAULT, 
			          flush_flags);

        if ( result < 0 ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
                     "flush with flags 0x%x failed in pe multi entry test #%d.",
                     flush_flags, test_num);
            failure_mssg2 = msg;
        }
    }

    i = 0;
    while ( ( pass2 ) && ( i < spec_size ) )
    {
        base_addr = entries2[spec[i].entry_type];
        entry_ptr = &(base_addr[spec[i].entry_index]);

#ifndef NDEBUG
        /* The clear_dirty_bits() callback is only called in debug mode --
         * thus we can only do our full test on the expected entry history
         * when debug is enabled.
         */
        if ( ( entry_ptr->deserialized != spec[i].expected_deserialized ) ||
             ( entry_ptr->cleared != spec[i].expected_cleared ) ||
             ( entry_ptr->serialized != spec[i].expected_serialized ) ||
             ( entry_ptr->destroyed != spec[i].expected_destroyed ) ) {
#else
        /* When in procduction mode, the clear_dirty_bits() callback is
         * not called, so entry_ptr->cleared should never be set.
         */
        if ( ( entry_ptr->deserialized != spec[i].expected_deserialized ) ||
             ( entry_ptr->cleared ) ||
             ( entry_ptr->serialized != spec[i].expected_serialized ) ||
             ( entry_ptr->destroyed != spec[i].expected_destroyed ) ) {
#endif /* NDEBUG */

#if 0 /* This is useful debugging code.  Lets keep it around. */

            HDfprintf(stdout,
             "desrlzd = %d(%d), clrd = %d(%d), srlzd = %d(%d), dest = %d(%d)\n",
             (int)(entry_ptr->deserialized),
             (int)(spec[i].expected_deserialized),
             (int)(entry_ptr->cleared),
             (int)(spec[i].expected_cleared),
             (int)(entry_ptr->serialized),
             (int)(spec[i].expected_serialized),
             (int)(entry_ptr->destroyed),
             (int)(spec[i].expected_destroyed));

#endif

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
               "Bad status on entry %d after flush in pe multi entry test #%d.",
               i, test_num);
            failure_mssg2 = msg;
        }
        i++;
    }

    if ( pass2 ) {

        if ( ( ( (flush_flags & H5C2__FLUSH_INVALIDATE_FLAG) == 0 )
               &&
               ( ( cache_ptr->index_len != spec_size )
                 ||
                 ( cache_ptr->index_size != total_entry_size )
               )
             )
             ||
             ( ( (flush_flags & H5C2__FLUSH_INVALIDATE_FLAG) != 0 )
               &&
               ( ( cache_ptr->index_len != 0 )
                 ||
                 ( cache_ptr->index_size != 0 )
               )
             )
           ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
            "Unexpected cache len/size after flush in pe multi entry test #%d.",
            test_num);
            failure_mssg2 = msg;
        }
    }

    /* clean up the cache to prep for the next test */
    if ( pass2 ) {

        result = H5C2_flush_cache(cache_ptr, H5P_DATASET_XFER_DEFAULT,
                                  H5C2__FLUSH_INVALIDATE_FLAG);

        if ( result < 0 ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "Flush failed on cleanup in pe multi entry test #%d.",
                       test_num);
            failure_mssg2 = msg;
        }
        else if ( ( cache_ptr->index_len != 0 ) ||
                  ( cache_ptr->index_size != 0 ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
          "Unexpected cache len/size after cleanup in pe multi entry test #%d.",
            test_num);
            failure_mssg2 = msg;

        }
    }

    i = 0;
    while ( ( pass2 ) && ( i < spec_size ) )
    {
        base_addr = entries2[spec[i].entry_type];
        entry_ptr = &(base_addr[spec[i].entry_index]);

        entry_ptr->deserialized = FALSE;
        entry_ptr->cleared      = FALSE;
        entry_ptr->serialized   = FALSE;
        entry_ptr->destroyed    = FALSE;

        i++;
    }

    return;

} /* check_flush_cache__pe_multi_entry_test() */


/*-------------------------------------------------------------------------
 * Function:	check_flush_cache__flush_ops()
 *
 * Purpose:	Run the flush ops cache tests. 
 *
 * 		These are tests that test the cache's ability to handle 
 * 		the case in which the flush callback dirties, resizes,
 * 		and/or renames entries.
 *
 * 		Do nothing if pass2 is FALSE on entry.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              9/3/06
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
check_flush_cache__flush_ops(H5C2_t * cache_ptr)
{
    /* const char *   fcn_name = "check_flush_cache__flush_ops"; */

    if ( cache_ptr == NULL ) {

        pass2 = FALSE;
        failure_mssg2 = "cache_ptr NULL on entry to flush ops test.";
    }
    else if ( ( cache_ptr->index_len != 0 ) ||
              ( cache_ptr->index_size != 0 ) ) {

        pass2 = FALSE;
        failure_mssg2 = "cache not empty at beginning of flush ops test.";
    }

    if ( pass2 ) /* test #1 */
    {
	/* start with a very simple test, in which there are two entries
	 * resident in cache, and the second entry dirties the first in 
	 * the flush callback.  No size changes, and no flush flags.
	 */
        int test_num			= 1;
	unsigned int flush_flags	= H5C2__NO_FLAGS_SET;
	int spec_size			= 2;
	int init_expected_index_len	= 2;
	size_t init_expected_index_size	= 2 * PICO_ENTRY_SIZE;
	int expected_index_len		= 2;
	size_t expected_index_size	= 2 * PICO_ENTRY_SIZE;
	struct fo_flush_cache_test_spec spec[2] = 
	{
          { 
            /* entry_num          = */ 0,
            /* entry_type         = */ 0,
	    /* entry_index        = */ 0,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__NO_FLAGS_SET,
	    /* new_size           = */ 0,
	    /* num_pins           = */ 0,
	    /* pin_type           = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 0,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ FALSE
	  },
          { 
            /* entry_num          = */ 1,
            /* entry_type         = */ 0,
	    /* entry_index        = */ 1,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__DIRTIED_FLAG,
	    /* new_size           = */ 0,
	    /* num_pins           = */ 0,
	    /* pin_type           = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 1,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__DIRTY,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ FALSE
          }
	};
	int check_size = 0;
	struct fo_flush_entry_check checks[1] =
	{
	  {
	    /* entry_num          = */ 0,
	    /* entry_type         = */ 0,
	    /* entry_index        = */ 0,
	    /* expected_size      = */ (size_t)0,
	    /* in_cache           = */ FALSE,
	    /* at_main_addr       = */ FALSE,
	    /* is_dirty	          = */ FALSE,
	    /* is_protected       = */ FALSE,
	    /* is_pinned          = */ FALSE,
	    /* expected_loaded    = */ FALSE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ FALSE,
	    /* expected_destroyed = */ FALSE
	  }
	};

        check_flush_cache__flush_op_test(cache_ptr,
                                         test_num,
                                         flush_flags,
                                         spec_size,
                                         spec,
				         init_expected_index_len,
				         init_expected_index_size,
				         expected_index_len,
				         expected_index_size,
					 check_size,
					 checks);
    }

    if ( pass2 ) /* test #2 */
    {
	/* Same as test 1, only this time set the flush invalidate flag.
	 * Note that we must repeat all tests with the flush invalidate flag
	 * as this triggers a different set of code to execute the flush.
	 *
	 * Create two entries resident in cache, and have the second entry 
	 * dirty the first in the flush callback.  
	 */
        int test_num			= 2;
	unsigned int flush_flags	= H5C2__FLUSH_INVALIDATE_FLAG;
	int spec_size			= 2;
	int init_expected_index_len	= 2;
	size_t init_expected_index_size	= 2 * PICO_ENTRY_SIZE;
	int expected_index_len		= 0;
	size_t expected_index_size	= 0;
	struct fo_flush_cache_test_spec spec[2] = 
	{
          { 
            /* entry_num          = */ 0,
            /* entry_type         = */ PICO_ENTRY_TYPE,
	    /* entry_index        = */ 0,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__NO_FLAGS_SET,
	    /* new_size           = */ 0,
	    /* num_pins           = */ 0,
	    /* pin_type           = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 0,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ TRUE
	  },
          { 
            /* entry_num          = */ 1,
            /* entry_type         = */ PICO_ENTRY_TYPE,
	    /* entry_index        = */ 1,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__DIRTIED_FLAG,
	    /* new_size           = */ 0,
	    /* num_pins           = */ 0,
	    /* pin_type           = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 1,
	    /* flush_ops          = */
	    /*	op_code:		type:		idx:	flag:	size: */
	    { { FLUSH_OP__DIRTY,	PICO_ENTRY_TYPE,0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,		0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,		0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,		0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,		0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,		0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,		0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,		0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,		0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,		0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ TRUE
          }
	};
	int check_size = 0;
	struct fo_flush_entry_check checks[1] =
	{
	  {
	    /* entry_num          = */ 0,
	    /* entry_type         = */ 0,
	    /* entry_index        = */ 0,
	    /* expected_size      = */ (size_t)0,
	    /* in_cache           = */ FALSE,
	    /* at_main_addr       = */ FALSE,
	    /* is_dirty	          = */ FALSE,
	    /* is_protected       = */ FALSE,
	    /* is_pinned          = */ FALSE,
	    /* expected_loaded    = */ FALSE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ FALSE,
	    /* expected_destroyed = */ FALSE
	  }
	};

        check_flush_cache__flush_op_test(cache_ptr,
                                         test_num,
                                         flush_flags,
                                         spec_size,
                                         spec,
				         init_expected_index_len,
				         init_expected_index_size,
				         expected_index_len,
				         expected_index_size,
					 check_size,
					 checks);
    }

    if ( pass2 ) /* test #3 */
    {
	/* Single entry test verifying that the cache can handle the case in
	 * which the call back function resizes the entry for which it has
	 * been called.
	 */
        int test_num			= 3;
	unsigned int flush_flags	= H5C2__NO_FLAGS_SET;
	int spec_size			= 1;
	int init_expected_index_len	= 1;
	size_t init_expected_index_size	= VARIABLE_ENTRY_SIZE / 4;
	int expected_index_len		= 1;
	size_t expected_index_size	= VARIABLE_ENTRY_SIZE / 2;
	struct fo_flush_cache_test_spec spec[1] = 
	{
          { 
            /* entry_num          = */ 0,
            /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 0,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__SIZE_CHANGED_FLAG,
	    /* new_size           = */ VARIABLE_ENTRY_SIZE / 4,
	    /* num_pins           = */ 0,
	    /* pin_type           = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 1,
	    /* flush_ops          = */
	    /*	op_code:		type:			idx:	flag:	size: */
	    { { FLUSH_OP__RESIZE,	VARIABLE_ENTRY_TYPE,	0,	FALSE,	VARIABLE_ENTRY_SIZE / 2 },
	      { FLUSH_OP__NO_OP,	0,			0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,			0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,			0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,			0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,			0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,			0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,			0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,			0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,			0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ FALSE
          }
	};
	int check_size = 0;
	struct fo_flush_entry_check checks[1] =
	{
	  {
	    /* entry_num          = */ 0,
	    /* entry_type         = */ 0,
	    /* entry_index        = */ 0,
	    /* expected_size      = */ (size_t)0,
	    /* in_cache           = */ FALSE,
	    /* at_main_addr       = */ FALSE,
	    /* is_dirty	          = */ FALSE,
	    /* is_protected       = */ FALSE,
	    /* is_pinned          = */ FALSE,
	    /* expected_loaded    = */ FALSE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ FALSE,
	    /* expected_destroyed = */ FALSE
	  }
	};

        check_flush_cache__flush_op_test(cache_ptr,
                                         test_num,
                                         flush_flags,
                                         spec_size,
                                         spec,
				         init_expected_index_len,
				         init_expected_index_size,
				         expected_index_len,
				         expected_index_size,
					 check_size,
					 checks);
    }

    if ( pass2 ) /* test #4 */
    {
	/* Repeat test #4 with the flush invalidate flag.
	 *
	 * Single entry test verifying that the cache can handle the case in
	 * which the call back function resizes the entry for which it has
	 * been called.
	 */
        int test_num			= 4;
	unsigned int flush_flags	= H5C2__FLUSH_INVALIDATE_FLAG;
	int spec_size			= 1;
	int init_expected_index_len	= 1;
	size_t init_expected_index_size	= VARIABLE_ENTRY_SIZE / 4;
	int expected_index_len		= 0;
	size_t expected_index_size	= 0;
	struct fo_flush_cache_test_spec spec[1] = 
	{
          { 
            /* entry_num          = */ 0,
            /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 0,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__SIZE_CHANGED_FLAG,
	    /* new_size           = */ VARIABLE_ENTRY_SIZE / 4,
	    /* num_pins           = */ 0,
	    /* pin_type           = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 1,
	    /* flush_ops          = */
	    /*	op_code:		type:			idx:	flag:	size: */
	    { { FLUSH_OP__RESIZE,	VARIABLE_ENTRY_TYPE,	0,	FALSE,	VARIABLE_ENTRY_SIZE / 2 },
	      { FLUSH_OP__NO_OP,	0,			0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,			0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,			0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,			0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,			0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,			0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,			0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,			0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,			0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ TRUE
          }
	};
	int check_size = 0;
	struct fo_flush_entry_check checks[1] =
	{
	  {
	    /* entry_num          = */ 0,
	    /* entry_type         = */ 0,
	    /* entry_index        = */ 0,
	    /* expected_size      = */ (size_t)0,
	    /* in_cache           = */ FALSE,
	    /* at_main_addr       = */ FALSE,
	    /* is_dirty	          = */ FALSE,
	    /* is_protected       = */ FALSE,
	    /* is_pinned          = */ FALSE,
	    /* expected_loaded    = */ FALSE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ FALSE,
	    /* expected_destroyed = */ FALSE
	  }
	};

        check_flush_cache__flush_op_test(cache_ptr,
                                         test_num,
                                         flush_flags,
                                         spec_size,
                                         spec,
				         init_expected_index_len,
				         init_expected_index_size,
				         expected_index_len,
				         expected_index_size,
					 check_size,
					 checks);
    }

    if ( pass2 ) /* test #5 & #6 */
    {
	/* Single entry test verifying that the cache can handle the case in
	 * which the call back function first resizes and then renames the 
	 * entry for which it has been called.
	 *
	 * Run this entry twice, as the first run moves the entry to its 
	 * alternate address, and the second moves it back.  
         *
         * 10/8/07 -- JRM 
         * Added a resize operation to this test to satisfy the new 
         * requiremnt that any resize of an entry on flush will always
         * be accompanied by a resize.  Note that as a result, this 
         * test becomes redundant with later tests.
	 */
        int test_num			= 5; /* and 6 */
	unsigned int flush_flags	= H5C2__NO_FLAGS_SET;
	int spec_size			= 1;
	int init_expected_index_len	= 1;
	size_t init_expected_index_size	= VARIABLE_ENTRY_SIZE;
	int expected_index_len		= 1;
	size_t expected_index_size	= VARIABLE_ENTRY_SIZE / 2;
	struct fo_flush_cache_test_spec spec[1] = 
	{
          { 
            /* entry_num          = */ 0,
            /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 0,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__DIRTIED_FLAG,
	    /* new_size           = */ 0,
	    /* num_pins           = */ 0,
	    /* pin_type           = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 2,
	    /* flush_ops          = */
	    /*	op_code:		type:			idx:	flag:	size: */
	    { { FLUSH_OP__RESIZE,	VARIABLE_ENTRY_TYPE,	0,	FALSE,	VARIABLE_ENTRY_SIZE / 2 },
	      { FLUSH_OP__RENAME,	VARIABLE_ENTRY_TYPE,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,			0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,			0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,			0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,			0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,			0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,			0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,			0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,			0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ FALSE
          }
	};
	int check_size = 0;
	struct fo_flush_entry_check checks[1] =
	{
	  {
	    /* entry_num          = */ 0,
	    /* entry_type         = */ 0,
	    /* entry_index        = */ 0,
	    /* expected_size      = */ (size_t)0,
	    /* in_cache           = */ FALSE,
	    /* at_main_addr       = */ FALSE,
	    /* is_dirty	          = */ FALSE,
	    /* is_protected       = */ FALSE,
	    /* is_pinned          = */ FALSE,
	    /* expected_loaded    = */ FALSE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ FALSE,
	    /* expected_destroyed = */ FALSE
	  }
	};

        check_flush_cache__flush_op_test(cache_ptr,
                                         test_num,
                                         flush_flags,
                                         spec_size,
                                         spec,
				         init_expected_index_len,
				         init_expected_index_size,
				         expected_index_len,
				         expected_index_size,
					 check_size,
					 checks);

	/* this change forces the rename to move the target entry back to its 
	 * main address.  The first test moved it to its alternate address.
	 *
	 * Note that these two tests are not the same, as in the first test,
	 * the renamed entry is moved forward in the slist.  In the second 
	 * it is moved backwards.
	 *
	 * Since there is only one entry in the cache, this doesn't really
	 * matter in this case.  But we will do similar tests later with 
	 * other entries in the cache.
	 */
	if ( pass2 ) {

	    spec[0].flush_ops[1].flag = TRUE;
	    test_num = 6;

            check_flush_cache__flush_op_test(cache_ptr,
                                             test_num,
                                             flush_flags,
                                             spec_size,
                                             spec,
                                             init_expected_index_len,
                                             init_expected_index_size,
                                             expected_index_len,
                                             expected_index_size,
					     check_size,
					     checks);
	}
    }

    if ( pass2 ) /* test #7 & #8 */
    {
	/* Run tests 5 & 6 again, using the flush invalidate flag on the 
	 * second test.
	 *
	 * Single entry test verifying that the cache can handle the case in
	 * which the call back function renames the entry for which it has
	 * been called.
	 *
	 * Run this entry twice, as the first run moves the entry to its 
	 * alternate address, and the second moves it back.  
         *
         * 10/8/07 -- JRM 
         * Added a resize operation to this test to satisfy the new 
         * requiremnt that any resize of an entry on flush will always
         * be accompanied by a resize.  Note that as a result, this 
         * test becomes redundant with later tests.
	 */
        int test_num			= 7; /* and 8 */
	unsigned int flush_flags	= H5C2__NO_FLAGS_SET;
	int spec_size			= 1;
	int init_expected_index_len	= 1;
	size_t init_expected_index_size	= VARIABLE_ENTRY_SIZE;
	int expected_index_len		= 1;
	size_t expected_index_size	= VARIABLE_ENTRY_SIZE / 2;
	struct fo_flush_cache_test_spec spec[1] = 
	{
          { 
            /* entry_num          = */ 0,
            /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 0,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__DIRTIED_FLAG,
	    /* new_size           = */ 0,
	    /* num_pins           = */ 0,
	    /* pin_type           = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 2,
	    /* flush_ops          = */
	    /*	op_code:		type:			idx:	flag:	size: */
	    { { FLUSH_OP__RESIZE,	VARIABLE_ENTRY_TYPE,	0,	FALSE,	VARIABLE_ENTRY_SIZE / 2 },
	      { FLUSH_OP__RENAME,	VARIABLE_ENTRY_TYPE,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,			0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,			0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,			0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,			0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,			0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,			0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,			0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,			0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ FALSE
          }
	};
	int check_size = 0;
	struct fo_flush_entry_check checks[1] =
	{
	  {
	    /* entry_num          = */ 0,
	    /* entry_type         = */ 0,
	    /* entry_index        = */ 0,
	    /* expected_size      = */ (size_t)0,
	    /* in_cache           = */ FALSE,
	    /* at_main_addr       = */ FALSE,
	    /* is_dirty	          = */ FALSE,
	    /* is_protected       = */ FALSE,
	    /* is_pinned          = */ FALSE,
	    /* expected_loaded    = */ FALSE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ FALSE,
	    /* expected_destroyed = */ FALSE
	  }
	};

        check_flush_cache__flush_op_test(cache_ptr,
                                         test_num,
                                         flush_flags,
                                         spec_size,
                                         spec,
				         init_expected_index_len,
				         init_expected_index_size,
				         expected_index_len,
				         expected_index_size,
					 check_size,
					 checks);

	/* this change forces the rename to move the target entry back to its 
	 * main address.  The first test moved it to its alternate address.
	 *
	 * Note that these two tests are not the same, as in the first test,
	 * the renamed entry is moved forward in the slist.  In the second 
	 * it is moved backwards.
	 *
	 * Since there is only one entry in the cache, this doesn't really
	 * matter in this case.  But we will do similar tests later with 
	 * other entries in the cache.
	 */

	if ( pass2 ) {

            test_num = 8;
	    flush_flags = H5C2__FLUSH_INVALIDATE_FLAG;
	    expected_index_len = 0;
	    expected_index_size	= 0;
	    spec[0].flush_ops[1].flag = TRUE;
	    spec[0].expected_destroyed = TRUE;

            check_flush_cache__flush_op_test(cache_ptr,
                                             test_num,
                                             flush_flags,
                                             spec_size,
                                             spec,
                                             init_expected_index_len,
                                             init_expected_index_size,
                                             expected_index_len,
                                             expected_index_size,
					     check_size,
					     checks);
	}
    }

    if ( pass2 ) /* test #9 & #10 */
    {
	/* Single entry test verifying that the cache can handle the case in
	 * which the call back function both resizes and renames the entry 
	 * for which it has been called.
	 *
	 * Again, we run this entry twice, as the first run moves the entry 
         * to its alternate address, and the second moves it back.  
	 */
        int test_num			= 9; /* and 10 */
	unsigned int flush_flags	= H5C2__NO_FLAGS_SET;
	int spec_size			= 1;
	int init_expected_index_len	= 1;
	size_t init_expected_index_size	= VARIABLE_ENTRY_SIZE / 2;
	int expected_index_len		= 1;
	size_t expected_index_size	= VARIABLE_ENTRY_SIZE / 4;
	struct fo_flush_cache_test_spec spec[1] = 
	{
          { 
            /* entry_num          = */ 0,
            /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 0,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__SIZE_CHANGED_FLAG,
	    /* new_size           = */ VARIABLE_ENTRY_SIZE / 2,
	    /* num_pins           = */ 0,
	    /* pin_type           = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 2,
	    /* flush_ops          = */
	    /*	op_code:		type:			idx:	flag:	size: */
	    { { FLUSH_OP__RESIZE,	VARIABLE_ENTRY_TYPE,	0,	FALSE,	VARIABLE_ENTRY_SIZE / 4 },
	      { FLUSH_OP__RENAME,	VARIABLE_ENTRY_TYPE,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,			0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,			0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,			0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,			0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,			0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,			0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,			0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,			0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ FALSE
          }
	};
	int check_size = 0;
	struct fo_flush_entry_check checks[1] =
	{
	  {
	    /* entry_num          = */ 0,
	    /* entry_type         = */ 0,
	    /* entry_index        = */ 0,
	    /* expected_size      = */ (size_t)0,
	    /* in_cache           = */ FALSE,
	    /* at_main_addr       = */ FALSE,
	    /* is_dirty	          = */ FALSE,
	    /* is_protected       = */ FALSE,
	    /* is_pinned          = */ FALSE,
	    /* expected_loaded    = */ FALSE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ FALSE,
	    /* expected_destroyed = */ FALSE
	  }
	};

        check_flush_cache__flush_op_test(cache_ptr,
                                         test_num,
                                         flush_flags,
                                         spec_size,
                                         spec,
				         init_expected_index_len,
				         init_expected_index_size,
				         expected_index_len,
				         expected_index_size,
					 check_size,
					 checks);

	/* this change forces the rename to move the target entry back to its 
	 * main address.  The first test moved it to its alternate address.
	 *
	 * Note that these two tests are not the same, as in the first test,
	 * the renamed entry is moved forward in the slist.  In the second 
	 * it is moved backwards.
	 *
	 * Since there is only one entry in the cache, this doesn't really
	 * matter in this case.  But we will do similar tests later with 
	 * other entries in the cache.
	 */
	if ( pass2 ) {

	    spec[0].flush_ops[1].flag = TRUE;
	    test_num = 10;

            check_flush_cache__flush_op_test(cache_ptr,
                                             test_num,
                                             flush_flags,
                                             spec_size,
                                             spec,
                                             init_expected_index_len,
                                             init_expected_index_size,
                                             expected_index_len,
                                             expected_index_size,
					     check_size,
					     checks);
	}
    }

    if ( pass2 ) /* test #11 & #12 */
    {
	/* Repeat the previous test with the flush invalidate flag on the
	 * second test.
	 *
	 * Single entry test verifying that the cache can handle the case in
	 * which the call back function both resizes and renames the entry 
	 * for which it has been called.
	 *
	 * Again, we run this entry twice, as the first run moves the entry to its 
	 * alternate address, and the second moves it back.  
	 */
        int test_num			= 11; /* and 12 */
	unsigned int flush_flags	= H5C2__NO_FLAGS_SET;
	int spec_size			= 1;
	int init_expected_index_len	= 1;
	size_t init_expected_index_size	= VARIABLE_ENTRY_SIZE / 2;
	int expected_index_len		= 1;
	size_t expected_index_size	= VARIABLE_ENTRY_SIZE / 4;
	struct fo_flush_cache_test_spec spec[1] = 
	{
          { 
            /* entry_num          = */ 0,
            /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 0,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__SIZE_CHANGED_FLAG,
	    /* new_size           = */ VARIABLE_ENTRY_SIZE / 2,
	    /* num_pins           = */ 0,
	    /* pin_type           = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 2,
	    /* flush_ops          = */
	    /*	op_code:		type:			idx:	flag:	size: */
	    { { FLUSH_OP__RESIZE,	VARIABLE_ENTRY_TYPE,	0,	FALSE,	VARIABLE_ENTRY_SIZE / 4 },
	      { FLUSH_OP__RENAME,	VARIABLE_ENTRY_TYPE,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,			0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,			0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,			0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,			0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,			0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,			0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,			0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,			0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ FALSE
          }
	};
	int check_size = 0;
	struct fo_flush_entry_check checks[1] =
	{
	  {
	    /* entry_num          = */ 0,
	    /* entry_type         = */ 0,
	    /* entry_index        = */ 0,
	    /* expected_size      = */ (size_t)0,
	    /* in_cache           = */ FALSE,
	    /* at_main_addr       = */ FALSE,
	    /* is_dirty	          = */ FALSE,
	    /* is_protected       = */ FALSE,
	    /* is_pinned          = */ FALSE,
	    /* expected_loaded    = */ FALSE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ FALSE,
	    /* expected_destroyed = */ FALSE
	  }
	};

        check_flush_cache__flush_op_test(cache_ptr,
                                         test_num,
                                         flush_flags,
                                         spec_size,
                                         spec,
				         init_expected_index_len,
				         init_expected_index_size,
				         expected_index_len,
				         expected_index_size,
					 check_size,
					 checks);

	/* this change forces the rename to move the target entry back to its 
	 * main address.  The first test moved it to its alternate address.
	 *
	 * Note that these two tests are not the same, as in the first test,
	 * the renamed entry is moved forward in the slist.  In the second 
	 * it is moved backwards.
	 *
	 * Since there is only one entry in the cache, this doesn't really
	 * matter in this case.  But we will do similar tests later with 
	 * other entries in the cache.
	 */
	if ( pass2 ) {

            test_num = 12;
	    flush_flags = H5C2__FLUSH_INVALIDATE_FLAG;
	    expected_index_len = 0;
	    expected_index_size	= 0;
	    spec[0].flush_ops[1].flag = TRUE;
	    spec[0].expected_destroyed = TRUE;


            check_flush_cache__flush_op_test(cache_ptr,
                                             test_num,
                                             flush_flags,
                                             spec_size,
                                             spec,
                                             init_expected_index_len,
                                             init_expected_index_size,
                                             expected_index_len,
                                             expected_index_size,
					     check_size,
					     checks);
	}
    }

    if ( pass2 ) /* test #13 */
    {
	/* Test the ability of the cache to handle the case in which 
	 * the flush function of an entry that is resident in cache 
	 * dirties two entries that are not in cache.  No size 
	 * changes.
	 *
	 * At present, I am assured that this case will never occur, but
	 * lets make sure we can handle it regardless.
	 */
        int test_num			= 13;
	unsigned int flush_flags	= H5C2__NO_FLAGS_SET;
	int spec_size			= 1;
	int init_expected_index_len	= 1;
	size_t init_expected_index_size	= 1 * PICO_ENTRY_SIZE;
	int expected_index_len		= 3;
	size_t expected_index_size	= 3 * PICO_ENTRY_SIZE;
	struct fo_flush_cache_test_spec spec[1] = 
	{
          { 
            /* entry_num          = */ 0,
            /* entry_type         = */ 0,
	    /* entry_index        = */ 1,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__DIRTIED_FLAG,
	    /* new_size           = */ 0,
	    /* num_pins           = */ 0,
	    /* pin_type           = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 2,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__DIRTY,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__DIRTY,	0,	2,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ FALSE
          }
	};
	int check_size = 2;
	struct fo_flush_entry_check checks[2] =
	{
	  {
	    /* entry_num          = */ 0,
	    /* entry_type         = */ PICO_ENTRY_TYPE,
	    /* entry_index        = */ 0,
	    /* expected_size      = */ PICO_ENTRY_SIZE,
	    /* in_cache           = */ TRUE,
	    /* at_main_addr       = */ TRUE,
	    /* is_dirty	          = */ FALSE,
	    /* is_protected       = */ FALSE,
	    /* is_pinned          = */ FALSE,
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ FALSE
	  },
	  {
	    /* entry_num          = */ 1,
	    /* entry_type         = */ PICO_ENTRY_TYPE,
	    /* entry_index        = */ 2,
	    /* expected_size      = */ PICO_ENTRY_SIZE,
	    /* in_cache           = */ TRUE,
	    /* at_main_addr       = */ TRUE,
	    /* is_dirty	          = */ FALSE,
	    /* is_protected       = */ FALSE,
	    /* is_pinned          = */ FALSE,
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ FALSE
	  }
	};

        check_flush_cache__flush_op_test(cache_ptr,
                                         test_num,
                                         flush_flags,
                                         spec_size,
                                         spec,
				         init_expected_index_len,
				         init_expected_index_size,
				         expected_index_len,
				         expected_index_size,
					 check_size,
					 checks);
    }

    if ( pass2 ) /* test #14 */
    {
	/* Repeat previous test with the flush invalidate flag.
	 *
	 * Test the ability of the cache to handle the case in which 
	 * the flush function of an entry that is resident in cache 
	 * dirties two entries that are not in cache.  No size 
	 * changes.
	 *
	 * At present, I am assured that this case will never occur, but
	 * lets make sure we can handle it regardless.
	 */
        int test_num			= 14;
	unsigned int flush_flags	= H5C2__FLUSH_INVALIDATE_FLAG;
	int spec_size			= 1;
	int init_expected_index_len	= 1;
	size_t init_expected_index_size	= 1 * PICO_ENTRY_SIZE;
	int expected_index_len		= 0;
	size_t expected_index_size	= (size_t)0;
	struct fo_flush_cache_test_spec spec[1] = 
	{
          { 
            /* entry_num          = */ 0,
            /* entry_type         = */ 0,
	    /* entry_index        = */ 1,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__DIRTIED_FLAG,
	    /* new_size           = */ 0,
	    /* num_pins           = */ 0,
	    /* pin_type           = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 2,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__DIRTY,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__DIRTY,	0,	2,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ TRUE
          }
	};
	int check_size = 2;
	struct fo_flush_entry_check checks[2] =
	{
	  {
	    /* entry_num          = */ 0,
	    /* entry_type         = */ PICO_ENTRY_TYPE,
	    /* entry_index        = */ 0,
	    /* expected_size      = */ PICO_ENTRY_SIZE,
	    /* in_cache           = */ FALSE,
	    /* at_main_addr       = */ TRUE,
	    /* is_dirty	          = */ FALSE,
	    /* is_protected       = */ FALSE,
	    /* is_pinned          = */ FALSE,
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ TRUE
	  },
	  {
	    /* entry_num          = */ 1,
	    /* entry_type         = */ PICO_ENTRY_TYPE,
	    /* entry_index        = */ 2,
	    /* expected_size      = */ PICO_ENTRY_SIZE,
	    /* in_cache           = */ FALSE,
	    /* at_main_addr       = */ TRUE,
	    /* is_dirty	          = */ FALSE,
	    /* is_protected       = */ FALSE,
	    /* is_pinned          = */ FALSE,
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ TRUE
	  }
	};

        check_flush_cache__flush_op_test(cache_ptr,
                                         test_num,
                                         flush_flags,
                                         spec_size,
                                         spec,
				         init_expected_index_len,
				         init_expected_index_size,
				         expected_index_len,
				         expected_index_size,
					 check_size,
					 checks);
    }

    if ( pass2 ) /* test #15 */
    {
	/* Test the ability of the cache to handle the case in which 
	 * the flush function of an entry that is resident in cache 
	 * resizes and dirties two entries that are not in cache.
	 *
	 * At present, I am assured that this case will never occur, but
	 * lets make sure we can handle it regardless.
	 */
        int test_num			= 15;
	unsigned int flush_flags	= H5C2__NO_FLAGS_SET;
	int spec_size			= 1;
	int init_expected_index_len	= 1;
	size_t init_expected_index_size	= 1 * VARIABLE_ENTRY_SIZE;
	int expected_index_len		= 3;
	size_t expected_index_size	= VARIABLE_ENTRY_SIZE +
		                          (VARIABLE_ENTRY_SIZE / 4) +
					  (VARIABLE_ENTRY_SIZE / 2);
	struct fo_flush_cache_test_spec spec[1] = 
	{
          { 
            /* entry_num          = */ 0,
            /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 1,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__DIRTIED_FLAG,
	    /* new_size           = */ 0,
	    /* num_pins           = */ 0,
	    /* pin_type           = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 4,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__RESIZE,	VARIABLE_ENTRY_TYPE,	0,	FALSE,	VARIABLE_ENTRY_SIZE / 4 },
	      { FLUSH_OP__DIRTY,	VARIABLE_ENTRY_TYPE,	0,	FALSE,	0 },
	      { FLUSH_OP__DIRTY,	VARIABLE_ENTRY_TYPE,	2,	FALSE,	0 },
	      { FLUSH_OP__RESIZE,	VARIABLE_ENTRY_TYPE,	2,	FALSE,	VARIABLE_ENTRY_SIZE / 2 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ FALSE
          }
	};
	int check_size = 2;
	struct fo_flush_entry_check checks[2] =
	{
	  {
	    /* entry_num          = */ 0,
	    /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 0,
	    /* expected_size      = */ VARIABLE_ENTRY_SIZE / 4,
	    /* in_cache           = */ TRUE,
	    /* at_main_addr       = */ TRUE,
	    /* is_dirty	          = */ FALSE,
	    /* is_protected       = */ FALSE,
	    /* is_pinned          = */ FALSE,
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ FALSE
	  },
	  {
	    /* entry_num          = */ 1,
	    /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 2,
	    /* expected_size      = */ VARIABLE_ENTRY_SIZE / 2,
	    /* in_cache           = */ TRUE,
	    /* at_main_addr       = */ TRUE,
	    /* is_dirty	          = */ FALSE,
	    /* is_protected       = */ FALSE,
	    /* is_pinned          = */ FALSE,
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ FALSE
	  }
	};

        check_flush_cache__flush_op_test(cache_ptr,
                                         test_num,
                                         flush_flags,
                                         spec_size,
                                         spec,
				         init_expected_index_len,
				         init_expected_index_size,
				         expected_index_len,
				         expected_index_size,
					 check_size,
					 checks);
    }

    if ( pass2 ) /* test #16 */
    {
	/* Repeat previous test with the flush invalidate flag.
	 *
	 * Test the ability of the cache to handle the case in which 
	 * the flush function of an entry that is resident in cache 
	 * resizes and dirties two entries that are not in cache.
	 *
	 * At present, I am assured that this case will never occur, but
	 * lets make sure we can handle it regardless.
	 */
        int test_num			= 16;
	unsigned int flush_flags	= H5C2__FLUSH_INVALIDATE_FLAG;
	int spec_size			= 1;
	int init_expected_index_len	= 1;
	size_t init_expected_index_size	= 1 * VARIABLE_ENTRY_SIZE;
	int expected_index_len		= 0;
	size_t expected_index_size	= (size_t)0;
	struct fo_flush_cache_test_spec spec[1] = 
	{
          { 
            /* entry_num          = */ 0,
            /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 1,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__DIRTIED_FLAG,
	    /* new_size           = */ 0,
	    /* num_pins           = */ 0,
	    /* pin_type           = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 4,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__RESIZE,	VARIABLE_ENTRY_TYPE,	0,	FALSE,	VARIABLE_ENTRY_SIZE / 4 },
	      { FLUSH_OP__DIRTY,	VARIABLE_ENTRY_TYPE,	0,	FALSE,	0 },
	      { FLUSH_OP__DIRTY,	VARIABLE_ENTRY_TYPE,	2,	FALSE,	0 },
	      { FLUSH_OP__RESIZE,	VARIABLE_ENTRY_TYPE,	2,	FALSE,	VARIABLE_ENTRY_SIZE / 2 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ TRUE
          }
	};
	int check_size = 2;
	struct fo_flush_entry_check checks[2] =
	{
	  {
	    /* entry_num          = */ 0,
	    /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 0,
	    /* expected_size      = */ VARIABLE_ENTRY_SIZE / 4,
	    /* in_cache           = */ FALSE,
	    /* at_main_addr       = */ TRUE,
	    /* is_dirty	          = */ FALSE,
	    /* is_protected       = */ FALSE,
	    /* is_pinned          = */ FALSE,
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ TRUE
	  },
	  {
	    /* entry_num          = */ 1,
	    /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 2,
	    /* expected_size      = */ VARIABLE_ENTRY_SIZE / 2,
	    /* in_cache           = */ FALSE,
	    /* at_main_addr       = */ TRUE,
	    /* is_dirty	          = */ FALSE,
	    /* is_protected       = */ FALSE,
	    /* is_pinned          = */ FALSE,
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ TRUE
	  }
	};

        check_flush_cache__flush_op_test(cache_ptr,
                                         test_num,
                                         flush_flags,
                                         spec_size,
                                         spec,
				         init_expected_index_len,
				         init_expected_index_size,
				         expected_index_len,
				         expected_index_size,
					 check_size,
					 checks);
    }

    if ( pass2 ) /* test #17 & #18 */
    {
	/* Test the ability of the cache to handle the case in which 
	 * the flush function of an entry that is resident in cache 
	 * resizes, dirties, and renames two entries that are not in cache.
	 *
	 * At present, I am assured that this case will never occur, but
	 * lets make sure we can handle it regardless.
	 */
        int test_num			= 17; /* and 18 */
	unsigned int flush_flags	= H5C2__NO_FLAGS_SET;
	int spec_size			= 1;
	int init_expected_index_len	= 1;
	size_t init_expected_index_size	= 1 * VARIABLE_ENTRY_SIZE;
	int expected_index_len		= 3;
	size_t expected_index_size	= VARIABLE_ENTRY_SIZE +
		                          (VARIABLE_ENTRY_SIZE / 4) +
					  (VARIABLE_ENTRY_SIZE / 2);
	struct fo_flush_cache_test_spec spec[1] = 
	{
          { 
            /* entry_num          = */ 0,
            /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 1,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__DIRTIED_FLAG,
	    /* new_size           = */ 0,
	    /* num_pins           = */ 0,
	    /* pin_type           = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 6,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__RESIZE,	VARIABLE_ENTRY_TYPE,	0,	FALSE,	VARIABLE_ENTRY_SIZE / 4 },
	      { FLUSH_OP__DIRTY,	VARIABLE_ENTRY_TYPE,	0,	FALSE,	0 },
	      { FLUSH_OP__RENAME,	VARIABLE_ENTRY_TYPE,	0,	FALSE,	0 },
	      { FLUSH_OP__DIRTY,	VARIABLE_ENTRY_TYPE,	2,	FALSE,	0 },
	      { FLUSH_OP__RESIZE,	VARIABLE_ENTRY_TYPE,	2,	FALSE,	VARIABLE_ENTRY_SIZE / 2 },
	      { FLUSH_OP__RENAME,	VARIABLE_ENTRY_TYPE,	2,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ FALSE
          }
	};
	int check_size = 2;
	struct fo_flush_entry_check checks[2] =
	{
	  {
	    /* entry_num          = */ 0,
	    /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 0,
	    /* expected_size      = */ VARIABLE_ENTRY_SIZE / 4,
	    /* in_cache           = */ TRUE,
	    /* at_main_addr       = */ FALSE,
	    /* is_dirty	          = */ FALSE,
	    /* is_protected       = */ FALSE,
	    /* is_pinned          = */ FALSE,
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ FALSE
	  },
	  {
	    /* entry_num          = */ 1,
	    /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 2,
	    /* expected_size      = */ VARIABLE_ENTRY_SIZE / 2,
	    /* in_cache           = */ TRUE,
	    /* at_main_addr       = */ FALSE,
	    /* is_dirty	          = */ FALSE,
	    /* is_protected       = */ FALSE,
	    /* is_pinned          = */ FALSE,
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ FALSE
	  }
	};
	
        check_flush_cache__flush_op_test(cache_ptr,
                                         test_num,
                                         flush_flags,
                                         spec_size,
                                         spec,
				         init_expected_index_len,
				         init_expected_index_size,
				         expected_index_len,
				         expected_index_size,
					 check_size,
					 checks);

	/* this change forces the renames to move the target entries back to 
	 * their main address.  The first test moved them to their alternate 
	 * address.
	 *
	 * Note that these two tests are not the same, as in the first test,
	 * the renamed entries are moved forward in the slist.  In the second 
	 * they are moved backwards.
	 */
	if ( pass2 ) {

	    test_num = 18;
	    spec[0].flush_ops[2].flag = TRUE;
	    spec[0].flush_ops[5].flag = TRUE;
	    checks[0].at_main_addr = TRUE;
	    checks[1].at_main_addr = TRUE;

            check_flush_cache__flush_op_test(cache_ptr,
                                             test_num,
                                             flush_flags,
                                             spec_size,
                                             spec,
                                             init_expected_index_len,
                                             init_expected_index_size,
                                             expected_index_len,
                                             expected_index_size,
					     check_size,
					     checks);
	}
    }

    if ( pass2 ) /* test #19 & #20 */
    {
	/* Repeat the above test with the flush invalidate flag on the
	 * second test.
	 *
	 * Test the ability of the cache to handle the case in which 
	 * the flush function of an entry that is resident in cache 
	 * resizes, dirties, and renames two entries that are not in cache.
	 *
	 * At present, I am assured that this case will never occur, but
	 * lets make sure we can handle it regardless.
	 */
        int test_num			= 19; /* and 20 */
	unsigned int flush_flags	= H5C2__NO_FLAGS_SET;
	int spec_size			= 1;
	int init_expected_index_len	= 1;
	size_t init_expected_index_size	= 1 * VARIABLE_ENTRY_SIZE;
	int expected_index_len		= 3;
	size_t expected_index_size	= VARIABLE_ENTRY_SIZE +
		                          (VARIABLE_ENTRY_SIZE / 4) +
					  (VARIABLE_ENTRY_SIZE / 2);
	struct fo_flush_cache_test_spec spec[1] = 
	{
          { 
            /* entry_num          = */ 0,
            /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 1,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__DIRTIED_FLAG,
	    /* new_size           = */ 0,
	    /* num_pins           = */ 0,
	    /* pin_type           = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 6,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__RESIZE,	VARIABLE_ENTRY_TYPE,	0,	FALSE,	VARIABLE_ENTRY_SIZE / 4 },
	      { FLUSH_OP__DIRTY,	VARIABLE_ENTRY_TYPE,	0,	FALSE,	0 },
	      { FLUSH_OP__RENAME,	VARIABLE_ENTRY_TYPE,	0,	FALSE,	0 },
	      { FLUSH_OP__DIRTY,	VARIABLE_ENTRY_TYPE,	2,	FALSE,	0 },
	      { FLUSH_OP__RESIZE,	VARIABLE_ENTRY_TYPE,	2,	FALSE,	VARIABLE_ENTRY_SIZE / 2 },
	      { FLUSH_OP__RENAME,	VARIABLE_ENTRY_TYPE,	2,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ FALSE
          }
	};
	int check_size = 2;
	struct fo_flush_entry_check checks[2] =
	{
	  {
	    /* entry_num          = */ 0,
	    /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 0,
	    /* expected_size      = */ VARIABLE_ENTRY_SIZE / 4,
	    /* in_cache           = */ TRUE,
	    /* at_main_addr       = */ FALSE,
	    /* is_dirty	          = */ FALSE,
	    /* is_protected       = */ FALSE,
	    /* is_pinned          = */ FALSE,
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ FALSE
	  },
	  {
	    /* entry_num          = */ 1,
	    /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 2,
	    /* expected_size      = */ VARIABLE_ENTRY_SIZE / 2,
	    /* in_cache           = */ TRUE,
	    /* at_main_addr       = */ FALSE,
	    /* is_dirty	          = */ FALSE,
	    /* is_protected       = */ FALSE,
	    /* is_pinned          = */ FALSE,
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ FALSE
	  }
	};

        check_flush_cache__flush_op_test(cache_ptr,
                                         test_num,
                                         flush_flags,
                                         spec_size,
                                         spec,
				         init_expected_index_len,
				         init_expected_index_size,
				         expected_index_len,
				         expected_index_size,
					 check_size,
					 checks);

	/* this change forces the renames to move the target entries back to 
	 * their main address.  The first test moved them to their alternate 
	 * address.
	 *
	 * Note that these two tests are not the same, as in the first test,
	 * the renamed entries are moved forward in the slist.  In the second 
	 * they are moved backwards.
	 */
	if ( pass2 ) {

	    test_num = 20;
	    flush_flags	= H5C2__FLUSH_INVALIDATE_FLAG;
	    expected_index_len = 0;
	    expected_index_size	= (size_t)0;
            spec[0].expected_destroyed = TRUE;
	    spec[0].flush_ops[2].flag = TRUE;
	    spec[0].flush_ops[5].flag = TRUE;
	    checks[0].at_main_addr = TRUE;
	    checks[0].in_cache = FALSE;
	    checks[0].expected_destroyed = TRUE;
	    checks[1].at_main_addr = TRUE;
	    checks[1].in_cache = FALSE;
	    checks[1].expected_destroyed = TRUE;

            check_flush_cache__flush_op_test(cache_ptr,
                                             test_num,
                                             flush_flags,
                                             spec_size,
                                             spec,
                                             init_expected_index_len,
                                             init_expected_index_size,
                                             expected_index_len,
                                             expected_index_size,
					     check_size,
					     checks);
	}
    }

    if ( pass2 ) /* test #21 */
    {
	/* Now mix things up a bit.
	 *
	 * Load several entries, two of which have flush functions that 
	 * resize, dirty, and rename two entries that are not in the 
	 * cache.  Mark only one of these entries, and then flush the 
	 * cache with the flush marked entries flag.
	 *
	 * This is the only test in which we test the 
	 * H5C2__FLUSH_MARKED_ENTRIES_FLAG.  The hope is that since
	 * we test the two features extensively by themselves, so 
	 * it should be sufficient to verify that they play together
	 * as expected.
	 */
        int test_num			= 21;
	unsigned int flush_flags	= H5C2__FLUSH_MARKED_ENTRIES_FLAG;
	int spec_size			= 4;
	int init_expected_index_len	= 4;
	size_t init_expected_index_size	= (2 * VARIABLE_ENTRY_SIZE) + (2 * PICO_ENTRY_SIZE);
	int expected_index_len		= 6;
	size_t expected_index_size	= (2 * VARIABLE_ENTRY_SIZE) +
		                          (VARIABLE_ENTRY_SIZE / 4) +
					  (VARIABLE_ENTRY_SIZE / 2) +
					  (2 * PICO_ENTRY_SIZE);
	struct fo_flush_cache_test_spec spec[4] = 
	{
          { 
            /* entry_num          = */ 0,
            /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 1,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__DIRTIED_FLAG | H5C2__SET_FLUSH_MARKER_FLAG,
	    /* new_size           = */ 0,
	    /* num_pins           = */ 0,
	    /* pin_type           = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 6,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__RESIZE,	VARIABLE_ENTRY_TYPE,	0,	FALSE,	VARIABLE_ENTRY_SIZE / 4 },
	      { FLUSH_OP__DIRTY,	VARIABLE_ENTRY_TYPE,	0,	FALSE,	0 },
	      { FLUSH_OP__RENAME,	VARIABLE_ENTRY_TYPE,	0,	FALSE,	0 },
	      { FLUSH_OP__DIRTY,	VARIABLE_ENTRY_TYPE,	2,	FALSE,	0 },
	      { FLUSH_OP__RESIZE,	VARIABLE_ENTRY_TYPE,	2,	FALSE,	VARIABLE_ENTRY_SIZE / 2 },
	      { FLUSH_OP__RENAME,	VARIABLE_ENTRY_TYPE,	2,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ FALSE
          },
          { 
            /* entry_num          = */ 1,
            /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 11,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__DIRTIED_FLAG,
	    /* new_size           = */ 0,
	    /* num_pins           = */ 0,
	    /* pin_type           = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 6,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__RESIZE,	VARIABLE_ENTRY_TYPE,	10,	FALSE,	VARIABLE_ENTRY_SIZE / 4 },
	      { FLUSH_OP__DIRTY,	VARIABLE_ENTRY_TYPE,	10,	FALSE,	0 },
	      { FLUSH_OP__RENAME,	VARIABLE_ENTRY_TYPE,	10,	FALSE,	0 },
	      { FLUSH_OP__DIRTY,	VARIABLE_ENTRY_TYPE,	12,	FALSE,	0 },
	      { FLUSH_OP__RESIZE,	VARIABLE_ENTRY_TYPE,	12,	FALSE,	VARIABLE_ENTRY_SIZE / 2 },
	      { FLUSH_OP__RENAME,	VARIABLE_ENTRY_TYPE,	12,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ FALSE,
	    /* expected_destroyed = */ FALSE
          },
          { 
            /* entry_num          = */ 2,
            /* entry_type         = */ PICO_ENTRY_TYPE,
	    /* entry_index        = */ 0,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__DIRTIED_FLAG | H5C2__SET_FLUSH_MARKER_FLAG,
	    /* new_size           = */ 0,
	    /* num_pins           = */ 0,
	    /* pin_type           = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 0,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ FALSE
          },
          { 
            /* entry_num          = */ 3,
            /* entry_type         = */ PICO_ENTRY_TYPE,
	    /* entry_index        = */ 1,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__DIRTIED_FLAG,
	    /* new_size           = */ 0,
	    /* num_pins           = */ 0,
	    /* pin_type           = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 0,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ FALSE,
	    /* expected_destroyed = */ FALSE
          }
	};
	int check_size = 4;
	struct fo_flush_entry_check checks[4] =
	{
	  {
	    /* entry_num          = */ 0,
	    /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 0,
	    /* expected_size      = */ VARIABLE_ENTRY_SIZE / 4,
	    /* in_cache           = */ TRUE,
	    /* at_main_addr       = */ FALSE,
	    /* is_dirty	          = */ TRUE,
	    /* is_protected       = */ FALSE,
	    /* is_pinned          = */ FALSE,
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ FALSE,
	    /* expected_destroyed = */ FALSE
	  },
	  {
	    /* entry_num          = */ 1,
	    /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 2,
	    /* expected_size      = */ VARIABLE_ENTRY_SIZE / 2,
	    /* in_cache           = */ TRUE,
	    /* at_main_addr       = */ FALSE,
	    /* is_dirty	          = */ TRUE,
	    /* is_protected       = */ FALSE,
	    /* is_pinned          = */ FALSE,
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ FALSE,
	    /* expected_destroyed = */ FALSE
	  },
	  {
	    /* entry_num          = */ 2,
	    /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 10,
	    /* expected_size      = */ VARIABLE_ENTRY_SIZE,
	    /* in_cache           = */ FALSE,
	    /* at_main_addr       = */ TRUE,
	    /* is_dirty	          = */ FALSE,
	    /* is_protected       = */ FALSE,
	    /* is_pinned          = */ FALSE,
	    /* expected_loaded    = */ FALSE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ FALSE,
	    /* expected_destroyed = */ FALSE
	  },
	  {
	    /* entry_num          = */ 3,
	    /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 12,
	    /* expected_size      = */ VARIABLE_ENTRY_SIZE,
	    /* in_cache           = */ FALSE,
	    /* at_main_addr       = */ TRUE,
	    /* is_dirty	          = */ FALSE,
	    /* is_protected       = */ FALSE,
	    /* is_pinned          = */ FALSE,
	    /* expected_loaded    = */ FALSE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ FALSE,
	    /* expected_destroyed = */ FALSE
	  }
	};

        check_flush_cache__flush_op_test(cache_ptr,
                                         test_num,
                                         flush_flags,
                                         spec_size,
                                         spec,
				         init_expected_index_len,
				         init_expected_index_size,
				         expected_index_len,
				         expected_index_size,
					 check_size,
					 checks);
	reset_entries2();
    }

    if ( pass2 ) /* test #22 */
    {
	/* Mix things up some more.
	 *
	 * Load lots of entries, some of which have flush functions that 
	 * resize, dirty, and rename two entries that are not in the 
	 * cache.  
	 *
	 * Also load entries that have flush ops on entries that are in
	 * cache.
	 */
        int test_num			= 22;
	unsigned int flush_flags	= H5C2__NO_FLAGS_SET;
	int spec_size			= 6;
	int init_expected_index_len	= 6;
	size_t init_expected_index_size	= (2 * VARIABLE_ENTRY_SIZE) + (4 * PICO_ENTRY_SIZE);
	int expected_index_len		= 10;
	size_t expected_index_size	= (2 * VARIABLE_ENTRY_SIZE) +
		                          (2 * (VARIABLE_ENTRY_SIZE / 4)) +
					  (2 * (VARIABLE_ENTRY_SIZE / 2)) +
					  (4 * PICO_ENTRY_SIZE);
	struct fo_flush_cache_test_spec spec[6] = 
	{
          { 
            /* entry_num          = */ 0,
            /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 1,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__DIRTIED_FLAG,
	    /* new_size           = */ 0,
	    /* num_pins           = */ 0,
	    /* pin_type           = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 6,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__RESIZE,	VARIABLE_ENTRY_TYPE,	0,	FALSE,	VARIABLE_ENTRY_SIZE / 4 },
	      { FLUSH_OP__DIRTY,	VARIABLE_ENTRY_TYPE,	0,	FALSE,	0 },
	      { FLUSH_OP__RENAME,	VARIABLE_ENTRY_TYPE,	0,	FALSE,	0 },
	      { FLUSH_OP__DIRTY,	VARIABLE_ENTRY_TYPE,	2,	FALSE,	0 },
	      { FLUSH_OP__RESIZE,	VARIABLE_ENTRY_TYPE,	2,	FALSE,	VARIABLE_ENTRY_SIZE / 2 },
	      { FLUSH_OP__RENAME,	VARIABLE_ENTRY_TYPE,	2,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ FALSE
          },
          { 
            /* entry_num          = */ 1,
            /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 11,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__DIRTIED_FLAG,
	    /* new_size           = */ 0,
	    /* num_pins           = */ 0,
	    /* pin_type           = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 6,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__RESIZE,	VARIABLE_ENTRY_TYPE,	10,	FALSE,	VARIABLE_ENTRY_SIZE / 4 },
	      { FLUSH_OP__DIRTY,	VARIABLE_ENTRY_TYPE,	10,	FALSE,	0 },
	      { FLUSH_OP__RENAME,	VARIABLE_ENTRY_TYPE,	10,	FALSE,	0 },
	      { FLUSH_OP__DIRTY,	VARIABLE_ENTRY_TYPE,	12,	FALSE,	0 },
	      { FLUSH_OP__RESIZE,	VARIABLE_ENTRY_TYPE,	12,	FALSE,	VARIABLE_ENTRY_SIZE / 2 },
	      { FLUSH_OP__RENAME,	VARIABLE_ENTRY_TYPE,	12,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ FALSE
          },
          { 
            /* entry_num          = */ 2,
            /* entry_type         = */ PICO_ENTRY_TYPE,
	    /* entry_index        = */ 0,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__NO_FLAGS_SET,
	    /* new_size           = */ 0,
	    /* num_pins           = */ 0,
	    /* pin_type           = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 0,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ FALSE
          },
          { 
            /* entry_num          = */ 3,
            /* entry_type         = */ PICO_ENTRY_TYPE,
	    /* entry_index        = */ 1,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__NO_FLAGS_SET,
	    /* new_size           = */ 0,
	    /* num_pins           = */ 0,
	    /* pin_type           = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 0,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ FALSE,
	    /* expected_destroyed = */ FALSE
          },
          { 
            /* entry_num          = */ 4,
            /* entry_type         = */ PICO_ENTRY_TYPE,
	    /* entry_index        = */ 10,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__DIRTIED_FLAG,
	    /* new_size           = */ 0,
	    /* num_pins           = */ 0,
	    /* pin_type           = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 1,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__DIRTY,	PICO_ENTRY_TYPE,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ FALSE
          },
          { 
            /* entry_num          = */ 5,
            /* entry_type         = */ PICO_ENTRY_TYPE,
	    /* entry_index        = */ 20,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__DIRTIED_FLAG,
	    /* new_size           = */ 0,
	    /* num_pins           = */ 0,
	    /* pin_type           = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 1,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__DIRTY,	PICO_ENTRY_TYPE,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ FALSE
          }
	};
	int check_size = 4;
	struct fo_flush_entry_check checks[4] =
	{
	  {
	    /* entry_num          = */ 0,
	    /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 0,
	    /* expected_size      = */ VARIABLE_ENTRY_SIZE / 4,
	    /* in_cache           = */ TRUE,
	    /* at_main_addr       = */ FALSE,
	    /* is_dirty	          = */ FALSE,
	    /* is_protected       = */ FALSE,
	    /* is_pinned          = */ FALSE,
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ FALSE
	  },
	  {
	    /* entry_num          = */ 1,
	    /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 2,
	    /* expected_size      = */ VARIABLE_ENTRY_SIZE / 2,
	    /* in_cache           = */ TRUE,
	    /* at_main_addr       = */ FALSE,
	    /* is_dirty	          = */ FALSE,
	    /* is_protected       = */ FALSE,
	    /* is_pinned          = */ FALSE,
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ FALSE
	  },
	  {
	    /* entry_num          = */ 2,
	    /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 10,
	    /* expected_size      = */ VARIABLE_ENTRY_SIZE / 4,
	    /* in_cache           = */ TRUE,
	    /* at_main_addr       = */ FALSE,
	    /* is_dirty	          = */ FALSE,
	    /* is_protected       = */ FALSE,
	    /* is_pinned          = */ FALSE,
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ FALSE
	  },
	  {
	    /* entry_num          = */ 3,
	    /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 12,
	    /* expected_size      = */ VARIABLE_ENTRY_SIZE / 2,
	    /* in_cache           = */ TRUE,
	    /* at_main_addr       = */ FALSE,
	    /* is_dirty	          = */ FALSE,
	    /* is_protected       = */ FALSE,
	    /* is_pinned          = */ FALSE,
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ FALSE
	  }
	};

        check_flush_cache__flush_op_test(cache_ptr,
                                         test_num,
                                         flush_flags,
                                         spec_size,
                                         spec,
				         init_expected_index_len,
				         init_expected_index_size,
				         expected_index_len,
				         expected_index_size,
					 check_size,
					 checks);
	reset_entries2();
    }

    if ( pass2 ) /* test #23 */
    {
	/* Repeat test #23 with the flush invalidate flag set.
	 *
	 * Mix things up some more.
	 *
	 * Load lots of entries, some of which have flush functions that 
	 * resize, dirty, and rename two entries that are not in the 
	 * cache.  
	 *
	 * Also load entries that have flush ops on entries that are in
	 * cache.
	 */
        int test_num			= 23;
	unsigned int flush_flags	= H5C2__FLUSH_INVALIDATE_FLAG;
	int spec_size			= 6;
	int init_expected_index_len	= 6;
	size_t init_expected_index_size	= (2 * VARIABLE_ENTRY_SIZE) + (4 * PICO_ENTRY_SIZE);
	int expected_index_len		= 0;
	size_t expected_index_size	= 0;
	struct fo_flush_cache_test_spec spec[6] = 
	{
          { 
            /* entry_num          = */ 0,
            /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 1,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__DIRTIED_FLAG,
	    /* new_size           = */ 0,
	    /* num_pins           = */ 0,
	    /* pin_type           = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 6,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__RESIZE,	VARIABLE_ENTRY_TYPE,	0,	FALSE,	VARIABLE_ENTRY_SIZE / 4 },
	      { FLUSH_OP__DIRTY,	VARIABLE_ENTRY_TYPE,	0,	FALSE,	0 },
	      { FLUSH_OP__RENAME,	VARIABLE_ENTRY_TYPE,	0,	FALSE,	0 },
	      { FLUSH_OP__DIRTY,	VARIABLE_ENTRY_TYPE,	2,	FALSE,	0 },
	      { FLUSH_OP__RESIZE,	VARIABLE_ENTRY_TYPE,	2,	FALSE,	VARIABLE_ENTRY_SIZE / 2 },
	      { FLUSH_OP__RENAME,	VARIABLE_ENTRY_TYPE,	2,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ TRUE
          },
          { 
            /* entry_num          = */ 1,
            /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 11,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__DIRTIED_FLAG,
	    /* new_size           = */ 0,
	    /* num_pins           = */ 0,
	    /* pin_type           = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 6,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__RESIZE,	VARIABLE_ENTRY_TYPE,	10,	FALSE,	VARIABLE_ENTRY_SIZE / 4 },
	      { FLUSH_OP__DIRTY,	VARIABLE_ENTRY_TYPE,	10,	FALSE,	0 },
	      { FLUSH_OP__RENAME,	VARIABLE_ENTRY_TYPE,	10,	FALSE,	0 },
	      { FLUSH_OP__DIRTY,	VARIABLE_ENTRY_TYPE,	12,	FALSE,	0 },
	      { FLUSH_OP__RESIZE,	VARIABLE_ENTRY_TYPE,	12,	FALSE,	VARIABLE_ENTRY_SIZE / 2 },
	      { FLUSH_OP__RENAME,	VARIABLE_ENTRY_TYPE,	12,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ TRUE
          },
          { 
            /* entry_num          = */ 2,
            /* entry_type         = */ PICO_ENTRY_TYPE,
	    /* entry_index        = */ 0,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__NO_FLAGS_SET,
	    /* new_size           = */ 0,
	    /* num_pins           = */ 0,
	    /* pin_type           = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 0,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ TRUE
          },
          { 
            /* entry_num          = */ 3,
            /* entry_type         = */ PICO_ENTRY_TYPE,
	    /* entry_index        = */ 1,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__NO_FLAGS_SET,
	    /* new_size           = */ 0,
	    /* num_pins           = */ 0,
	    /* pin_type           = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 0,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ FALSE,
	    /* expected_destroyed = */ TRUE
          },
          { 
            /* entry_num          = */ 4,
            /* entry_type         = */ PICO_ENTRY_TYPE,
	    /* entry_index        = */ 10,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__DIRTIED_FLAG,
	    /* new_size           = */ 0,
	    /* num_pins           = */ 0,
	    /* pin_type           = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 1,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__DIRTY,	PICO_ENTRY_TYPE,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ TRUE
          },
          { 
            /* entry_num          = */ 5,
            /* entry_type         = */ PICO_ENTRY_TYPE,
	    /* entry_index        = */ 20,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__DIRTIED_FLAG,
	    /* new_size           = */ 0,
	    /* num_pins           = */ 0,
	    /* pin_type           = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 1,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__DIRTY,	PICO_ENTRY_TYPE,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ TRUE
          }
	};
	int check_size = 4;
	struct fo_flush_entry_check checks[4] =
	{
	  {
	    /* entry_num          = */ 0,
	    /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 0,
	    /* expected_size      = */ VARIABLE_ENTRY_SIZE / 4,
	    /* in_cache           = */ FALSE,
	    /* at_main_addr       = */ FALSE,
	    /* is_dirty	          = */ FALSE,
	    /* is_protected       = */ FALSE,
	    /* is_pinned          = */ FALSE,
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ TRUE
	  },
	  {
	    /* entry_num          = */ 1,
	    /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 2,
	    /* expected_size      = */ VARIABLE_ENTRY_SIZE / 2,
	    /* in_cache           = */ FALSE,
	    /* at_main_addr       = */ FALSE,
	    /* is_dirty	          = */ FALSE,
	    /* is_protected       = */ FALSE,
	    /* is_pinned          = */ FALSE,
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ TRUE
	  },
	  {
	    /* entry_num          = */ 2,
	    /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 10,
	    /* expected_size      = */ VARIABLE_ENTRY_SIZE / 4,
	    /* in_cache           = */ FALSE,
	    /* at_main_addr       = */ FALSE,
	    /* is_dirty	          = */ FALSE,
	    /* is_protected       = */ FALSE,
	    /* is_pinned          = */ FALSE,
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ TRUE
	  },
	  {
	    /* entry_num          = */ 3,
	    /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 12,
	    /* expected_size      = */ VARIABLE_ENTRY_SIZE / 2,
	    /* in_cache           = */ FALSE,
	    /* at_main_addr       = */ FALSE,
	    /* is_dirty	          = */ FALSE,
	    /* is_protected       = */ FALSE,
	    /* is_pinned          = */ FALSE,
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ TRUE
	  }
	};

        check_flush_cache__flush_op_test(cache_ptr,
                                         test_num,
                                         flush_flags,
                                         spec_size,
                                         spec,
				         init_expected_index_len,
				         init_expected_index_size,
				         expected_index_len,
				         expected_index_size,
					 check_size,
					 checks);
	reset_entries2();
    }

    /* So much for tests involving only flush operations.
     *
     * Now create some tests mixing flush ops and pins.
     */
    if ( pass2 ) /* test #24 */
    {
	/* Pico entries 50 and 150 pin pico entry 100, and also dirty
	 * pico entry 100 on flush.
	 */
        int test_num			= 24;
	unsigned int flush_flags	= H5C2__NO_FLAGS_SET;
	int spec_size			= 3;
	int init_expected_index_len	= 3;
	size_t init_expected_index_size	= 3 * PICO_ENTRY_SIZE;
	int expected_index_len		= 3;
	size_t expected_index_size	= 3 * PICO_ENTRY_SIZE;
	struct fo_flush_cache_test_spec spec[3] = 
	{
          { 
            /* entry_num          = */ 0,
            /* entry_type         = */ PICO_ENTRY_TYPE,
	    /* entry_index        = */ 100,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__NO_FLAGS_SET,
	    /* new_size           = */ 0,
	    /* num_pins           = */ 0,
	    /* pin_type           = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 0,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ FALSE
          },
	  {
            /* entry_num          = */ 1,
            /* entry_type         = */ PICO_ENTRY_TYPE,
	    /* entry_index        = */ 50,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__DIRTIED_FLAG,
	    /* new_size           = */ 0,
	    /* num_pins           = */ 1,
	    /* pin_type           = */ {PICO_ENTRY_TYPE, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {100, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 1,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__DIRTY,	PICO_ENTRY_TYPE,	100,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ FALSE
          },
	  {
            /* entry_num          = */ 2,
            /* entry_type         = */ PICO_ENTRY_TYPE,
	    /* entry_index        = */ 150,
	    /* insert_flag        = */ TRUE,
	    /* flags		  = */ H5C2__NO_FLAGS_SET,
	    /* new_size           = */ 0,
	    /* num_pins           = */ 1,
	    /* pin_type           = */ {PICO_ENTRY_TYPE, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {100, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 1,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__DIRTY,	PICO_ENTRY_TYPE,	100,	FALSE,	0 },
	      { FLUSH_OP__DIRTY,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ FALSE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ FALSE
          }
	};
	int check_size = 0;
	struct fo_flush_entry_check checks[1] =
	{
	  {
	    /* entry_num          = */ 0,
	    /* entry_type         = */ 0,
	    /* entry_index        = */ 0,
	    /* expected_size      = */ (size_t)0,
	    /* in_cache           = */ FALSE,
	    /* at_main_addr       = */ FALSE,
	    /* is_dirty	          = */ FALSE,
	    /* is_protected       = */ FALSE,
	    /* is_pinned          = */ FALSE,
	    /* expected_loaded    = */ FALSE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ FALSE,
	    /* expected_destroyed = */ FALSE
	  }
	};

        check_flush_cache__flush_op_test(cache_ptr,
                                         test_num,
                                         flush_flags,
                                         spec_size,
                                         spec,
				         init_expected_index_len,
				         init_expected_index_size,
				         expected_index_len,
				         expected_index_size,
					 check_size,
					 checks);
    }

    if ( pass2 ) /* test #25 */
    {
	/* Repeat the previous test with the flush invalidate flag.
	 *
	 * Pico entries 50 and 150 pin pico entry 100, and also dirty
	 * pico entry 100 on flush.
	 */
        int test_num			= 25;
	unsigned int flush_flags	= H5C2__FLUSH_INVALIDATE_FLAG;
	int spec_size			= 3;
	int init_expected_index_len	= 3;
	size_t init_expected_index_size	= 3 * PICO_ENTRY_SIZE;
	int expected_index_len		= 0;
	size_t expected_index_size	= (size_t)0;
	struct fo_flush_cache_test_spec spec[3] = 
	{
          { 
            /* entry_num          = */ 0,
            /* entry_type         = */ PICO_ENTRY_TYPE,
	    /* entry_index        = */ 100,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__NO_FLAGS_SET,
	    /* new_size           = */ 0,
	    /* num_pins           = */ 0,
	    /* pin_type           = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 0,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ TRUE
          },
	  {
            /* entry_num          = */ 1,
            /* entry_type         = */ PICO_ENTRY_TYPE,
	    /* entry_index        = */ 50,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__DIRTIED_FLAG,
	    /* new_size           = */ 0,
	    /* num_pins           = */ 1,
	    /* pin_type           = */ {PICO_ENTRY_TYPE, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {100, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 1,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__DIRTY,	PICO_ENTRY_TYPE,	100,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ TRUE
          },
	  {
            /* entry_num          = */ 2,
            /* entry_type         = */ PICO_ENTRY_TYPE,
	    /* entry_index        = */ 150,
	    /* insert_flag        = */ TRUE,
	    /* flags		  = */ H5C2__NO_FLAGS_SET,
	    /* new_size           = */ 0,
	    /* num_pins           = */ 1,
	    /* pin_type           = */ {PICO_ENTRY_TYPE, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {100, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 1,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__DIRTY,	PICO_ENTRY_TYPE,	100,	FALSE,	0 },
	      { FLUSH_OP__DIRTY,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ FALSE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ TRUE
          }
	};
	int check_size = 0;
	struct fo_flush_entry_check checks[1] =
	{
	  {
	    /* entry_num          = */ 0,
	    /* entry_type         = */ 0,
	    /* entry_index        = */ 0,
	    /* expected_size      = */ (size_t)0,
	    /* in_cache           = */ FALSE,
	    /* at_main_addr       = */ FALSE,
	    /* is_dirty	          = */ FALSE,
	    /* is_protected       = */ FALSE,
	    /* is_pinned          = */ FALSE,
	    /* expected_loaded    = */ FALSE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ FALSE,
	    /* expected_destroyed = */ FALSE
	  }
	};

        check_flush_cache__flush_op_test(cache_ptr,
                                         test_num,
                                         flush_flags,
                                         spec_size,
                                         spec,
				         init_expected_index_len,
				         init_expected_index_size,
				         expected_index_len,
				         expected_index_size,
					 check_size,
					 checks);
    }

    if ( pass2 ) /* test #26 */
    {
	/* This one is complex.  
	 *
	 * In the following overvies table, VET stands for 
	 * VARIABLE_ENTRY_TYPE.
	 *
	 * In trying to follow what happens when we flush the
	 * set of entries constructed below, recall that each
	 * flush operation is executed the first time the 
	 * entry is flushed, and then not executed again.
	 * This may be a weakness in the tests, but that 
	 * is the way it is for now.
	 *
	 * After thinking about it for a while, I'm not sure that 
	 * the interaction between pins and flush operations needs 
	 * all that much testing, as the two are essentially 
	 * orthoginal.  Thus this is a bit of a smoke check to 
	 * verify that we get the expected results.
	 *
	 * (VET, 100)	initially not resident in cache
	 *
	 * (VET, 200)	initially clean and resident in cache
	 *
	 * (VET, 300)	initially not resident in cache
	 *
	 * (VET, 2100)	initially clean and resident in cache
	 *
	 * (VET, 2200)	initially not resident in cache
	 *
	 * (VET, 2300)	initially clean and resident in cache
	 *
	 * (VET, 1000)	initially clean, and in cache
	 * 		dirties (VET, 100)
	 * 		resizes (VET, 200)
	 * 		dirty	(VET, 300) -- dirty first to bring into cache.
	 * 		renames (VET, 300)
	 *
	 * (VET, 2000)	initially clean, and in cache
	 * 		dirties (VET, 2100)
	 * 		resizes (VET, 2200)
	 * 		renames (VET, 2300)
	 *
	 * (VET, 350)	initially clean, and in cache
	 * 		pins	(VET, 1000)
	 * 		dirties (VET, 1000)
	 * 		resizes (VET, 350)
	 * 		pins	(VET, 2000)
	 * 		dirties (VET, 2000)
	 *
	 * (VET, 450)	initially dirty, and in cache
	 * 		pins	(VET, 1000)
	 * 		dirties	(VET, 1000)
	 * 		renames (VET, 450)
	 * 		pins	(VET, 2000)
	 * 		dirties (VET, 2000)
	 *
	 * (VET, 650)	initially clean, and in cache
	 * 		pins	(VET, 1000)
	 * 		dirties (VET, 1000)
	 * 		resizes (VET, 650)
	 * 		pins	(VET, 2000)
	 * 		dirties (VET, 2000)
	 *
	 * (VET, 750)	initially dirty, and in cache
	 * 		pins	(VET, 1000)
	 * 		dirties (VET, 1000)
	 * 		resizes (VET, 750)
	 * 		pins	(VET, 2000)
	 * 		dirties	(VET, 2000)
	 *
	 * (VET, 500)	initially dirty, and in cache
	 * 		dirties	(VET, 350)
	 * 		dirties	(VET, 450)
	 * 		dirties	(VET, 650)
	 * 		dirties (VET, 750)
	 */
        int test_num			= 26;
	unsigned int flush_flags	= H5C2__NO_FLAGS_SET;
	int spec_size			= 10;
	int init_expected_index_len	= 10;
	size_t init_expected_index_size	= 10 * VARIABLE_ENTRY_SIZE;
	int expected_index_len		= 13;
	size_t expected_index_size	= 9 * VARIABLE_ENTRY_SIZE;
	struct fo_flush_cache_test_spec spec[10] = 
	{
          { 
            /* entry_num          = */ 0,
            /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 200,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__NO_FLAGS_SET,
	    /* new_size           = */ 0,
	    /* num_pins           = */ 0,
	    /* pin_type           = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 0,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ FALSE
          },
	  {
            /* entry_num          = */ 1,
            /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 2100,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__NO_FLAGS_SET,
	    /* new_size           = */ 0,
	    /* num_pins           = */ 0,
	    /* pin_type           = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 0,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ FALSE
          },
	  {
            /* entry_num          = */ 2,
            /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 2300,
	    /* insert_flag        = */ TRUE,
	    /* flags		  = */ H5C2__NO_FLAGS_SET,
	    /* new_size           = */ 0,
	    /* num_pins           = */ 0,
	    /* pin_type           = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 0,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ FALSE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ FALSE
          },
	  {
            /* entry_num          = */ 3,
            /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 1000,
	    /* insert_flag        = */ TRUE,
	    /* flags		  = */ H5C2__NO_FLAGS_SET,
	    /* new_size           = */ 0,
	    /* num_pins           = */ 0,
	    /* pin_type           = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 4,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__DIRTY,	VARIABLE_ENTRY_TYPE,	100,	FALSE,	0 },
	      { FLUSH_OP__RESIZE,	VARIABLE_ENTRY_TYPE,	200,	FALSE,	VARIABLE_ENTRY_SIZE / 2 },
	      { FLUSH_OP__DIRTY,	VARIABLE_ENTRY_TYPE,	300,	FALSE,	0 },
	      { FLUSH_OP__RENAME,	VARIABLE_ENTRY_TYPE,	300,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ FALSE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ FALSE
          },
	  {
            /* entry_num          = */ 4,
            /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 2000,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__NO_FLAGS_SET,
	    /* new_size           = */ 0,
	    /* num_pins           = */ 0,
	    /* pin_type           = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 3,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__DIRTY,	VARIABLE_ENTRY_TYPE,	2100,	FALSE,	0 },
	      { FLUSH_OP__RESIZE,	VARIABLE_ENTRY_TYPE,	2200,	FALSE,	VARIABLE_ENTRY_SIZE / 2 },
	      { FLUSH_OP__RENAME,	VARIABLE_ENTRY_TYPE,	2300,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ FALSE
          },
	  {
            /* entry_num          = */ 5,
            /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 350,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__NO_FLAGS_SET,
	    /* new_size           = */ 0,
	    /* num_pins           = */ 2,
	    /* pin_type           = */ {VARIABLE_ENTRY_TYPE, VARIABLE_ENTRY_TYPE, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {1000, 2000, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 3,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__DIRTY,	VARIABLE_ENTRY_TYPE,	1000,	FALSE,	0 },
	      { FLUSH_OP__DIRTY,	VARIABLE_ENTRY_TYPE,	2000,	FALSE,	0 },
	      { FLUSH_OP__RESIZE,	VARIABLE_ENTRY_TYPE,	350,	FALSE,	VARIABLE_ENTRY_SIZE / 4 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ FALSE
          },
	  {
            /* entry_num          = */ 6,
            /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 450,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__DIRTIED_FLAG,
	    /* new_size           = */ 0,
	    /* num_pins           = */ 2,
	    /* pin_type           = */ {VARIABLE_ENTRY_TYPE, VARIABLE_ENTRY_TYPE, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {1000, 2000, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 3,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__DIRTY,	VARIABLE_ENTRY_TYPE,	1000,	FALSE,	0 },
	      { FLUSH_OP__DIRTY,	VARIABLE_ENTRY_TYPE,	2000,	FALSE,	0 },
	      { FLUSH_OP__RESIZE,	VARIABLE_ENTRY_TYPE,	450,	FALSE,	VARIABLE_ENTRY_SIZE / 4 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ FALSE
          },
	  {
            /* entry_num          = */ 7,
            /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 650,
	    /* insert_flag        = */ TRUE,
	    /* flags		  = */ H5C2__NO_FLAGS_SET,
	    /* new_size           = */ 0,
	    /* num_pins           = */ 2,
	    /* pin_type           = */ {VARIABLE_ENTRY_TYPE, VARIABLE_ENTRY_TYPE, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {1000, 2000, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 3,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__DIRTY,	VARIABLE_ENTRY_TYPE,	1000,	FALSE,	0 },
	      { FLUSH_OP__DIRTY,	VARIABLE_ENTRY_TYPE,	2000,	FALSE,	0 },
	      { FLUSH_OP__RESIZE,	VARIABLE_ENTRY_TYPE,	650,	FALSE,	VARIABLE_ENTRY_SIZE / 4 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ FALSE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ FALSE
          },
	  {
            /* entry_num          = */ 8,
            /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 750,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__DIRTIED_FLAG,
	    /* new_size           = */ 0,
	    /* num_pins           = */ 2,
	    /* pin_type           = */ {VARIABLE_ENTRY_TYPE, VARIABLE_ENTRY_TYPE, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {1000, 2000, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 3,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__DIRTY,	VARIABLE_ENTRY_TYPE,	1000,	FALSE,	0 },
	      { FLUSH_OP__DIRTY,	VARIABLE_ENTRY_TYPE,	2000,	FALSE,	0 },
	      { FLUSH_OP__RESIZE,	VARIABLE_ENTRY_TYPE,	750,	FALSE,	VARIABLE_ENTRY_SIZE / 4 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ FALSE
          },
	  {
            /* entry_num          = */ 9,
            /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 500,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__DIRTIED_FLAG,
	    /* new_size           = */ 0,
	    /* num_pins           = */ 0,
	    /* pin_type           = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 4,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__DIRTY,	VARIABLE_ENTRY_TYPE,	350,	FALSE,	0 },
	      { FLUSH_OP__DIRTY,	VARIABLE_ENTRY_TYPE,	450,	FALSE,	0 },
	      { FLUSH_OP__DIRTY,	VARIABLE_ENTRY_TYPE,	650,	FALSE,	0 },
	      { FLUSH_OP__DIRTY,	VARIABLE_ENTRY_TYPE,	750,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ FALSE
          }
	};
	int check_size = 3;
	struct fo_flush_entry_check checks[3] =
	{
	  {
	    /* entry_num          = */ 0,
	    /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 100,
	    /* expected_size      = */ VARIABLE_ENTRY_SIZE,
	    /* in_cache           = */ TRUE,
	    /* at_main_addr       = */ TRUE,
	    /* is_dirty	          = */ FALSE,
	    /* is_protected       = */ FALSE,
	    /* is_pinned          = */ FALSE,
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ FALSE
	  },
	  {
	    /* entry_num          = */ 1,
	    /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 300,
	    /* expected_size      = */ VARIABLE_ENTRY_SIZE,
	    /* in_cache           = */ TRUE,
	    /* at_main_addr       = */ FALSE,
	    /* is_dirty	          = */ FALSE,
	    /* is_protected       = */ FALSE,
	    /* is_pinned          = */ FALSE,
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ FALSE
	  },
	  {
	    /* entry_num          = */ 2,
	    /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 2200,
	    /* expected_size      = */ VARIABLE_ENTRY_SIZE / 2,
	    /* in_cache           = */ TRUE,
	    /* at_main_addr       = */ TRUE,
	    /* is_dirty	          = */ FALSE,
	    /* is_protected       = */ FALSE,
	    /* is_pinned          = */ FALSE,
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ FALSE
	  }

	};

        check_flush_cache__flush_op_test(cache_ptr,
                                         test_num,
                                         flush_flags,
                                         spec_size,
                                         spec,
				         init_expected_index_len,
				         init_expected_index_size,
				         expected_index_len,
				         expected_index_size,
					 check_size,
					 checks);

	reset_entries2();
    }

    if ( pass2 ) /* test #27 */
    {
	/* Repeat test #26 with the flush invalidate flag.
	 *
	 * In the following overview table, VET stands for 
	 * VARIABLE_ENTRY_TYPE.
	 *
	 * In trying to follow what happens when we flush the
	 * set of entries constructed below, recall that each
	 * flush operation is executed the first time the 
	 * entry is flushed, and then not executed again.
	 * This may be a weakness in the tests, but that 
	 * is the way it is for now.
	 *
	 * After thinking about it for a while, I'm not sure that 
	 * the interaction between pins and flush operations needs 
	 * all that much testing, as the two are essentially 
	 * orthoginal.  The big thing is to verify that flushes of
	 * pinned entries with flush ops result in the expected 
	 * updates of the cache.
	 *
	 * Thus this is a bit of a smoke check to * verify that we 
	 * get the expected results.
	 *
	 * (VET, 100)	initially not resident in cache
	 *
	 * (VET, 200)	initially clean and resident in cache
	 *
	 * (VET, 300)	initially not resident in cache
	 *
	 * (VET, 2100)	initially clean and resident in cache
	 *
	 * (VET, 2200)	initially not resident in cache
	 *
	 * (VET, 2300)	initially clean and resident in cache
	 *
	 * (VET, 1000)	initially clean, and in cache
	 * 		dirties (VET, 100)
	 * 		resizes (VET, 200)
	 * 		dirty	(VET, 300) -- dirty first to bring into cache.
	 * 		renames (VET, 300)
	 *
	 * (VET, 2000)	initially clean, and in cache
	 * 		dirties (VET, 2100)
	 * 		resizes (VET, 2200)
	 * 		renames (VET, 2300)
	 *
	 * (VET, 350)	initially clean, and in cache
	 * 		pins	(VET, 1000)
	 * 		dirties (VET, 1000)
	 * 		resizes (VET, 350)
	 * 		pins	(VET, 2000)
	 * 		dirties (VET, 2000)
	 *
	 * (VET, 450)	initially dirty, and in cache
	 * 		pins	(VET, 1000)
	 * 		dirties	(VET, 1000)
	 * 		renames (VET, 450)
	 * 		pins	(VET, 2000)
	 * 		dirties (VET, 2000)
	 *
	 * (VET, 650)	initially clean, and in cache
	 * 		pins	(VET, 1000)
	 * 		dirties (VET, 1000)
	 * 		resizes (VET, 650)
	 * 		pins	(VET, 2000)
	 * 		dirties (VET, 2000)
	 *
	 * (VET, 750)	initially dirty, and in cache
	 * 		pins	(VET, 1000)
	 * 		dirties (VET, 1000)
	 * 		resizes (VET, 750)
	 * 		pins	(VET, 2000)
	 * 		dirties	(VET, 2000)
	 *
	 * (VET, 500)	initially dirty, and in cache
	 * 		dirties	(VET, 350)
	 * 		dirties	(VET, 450)
	 * 		dirties	(VET, 650)
	 * 		dirties (VET, 750)
	 */
        int test_num			= 27;
	unsigned int flush_flags	= H5C2__FLUSH_INVALIDATE_FLAG;
	int spec_size			= 10;
	int init_expected_index_len	= 10;
	size_t init_expected_index_size	= 10 * VARIABLE_ENTRY_SIZE;
	int expected_index_len		= 0;
	size_t expected_index_size	= (size_t)0;
	struct fo_flush_cache_test_spec spec[10] = 
	{
          { 
            /* entry_num          = */ 0,
            /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 200,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__NO_FLAGS_SET,
	    /* new_size           = */ 0,
	    /* num_pins           = */ 0,
	    /* pin_type           = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 0,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ TRUE
          },
	  {
            /* entry_num          = */ 1,
            /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 2100,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__NO_FLAGS_SET,
	    /* new_size           = */ 0,
	    /* num_pins           = */ 0,
	    /* pin_type           = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 0,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ TRUE
          },
	  {
            /* entry_num          = */ 2,
            /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 2300,
	    /* insert_flag        = */ TRUE,
	    /* flags		  = */ H5C2__NO_FLAGS_SET,
	    /* new_size           = */ 0,
	    /* num_pins           = */ 0,
	    /* pin_type           = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 0,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ FALSE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ TRUE
          },
	  {
            /* entry_num          = */ 3,
            /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 1000,
	    /* insert_flag        = */ TRUE,
	    /* flags		  = */ H5C2__NO_FLAGS_SET,
	    /* new_size           = */ 0,
	    /* num_pins           = */ 0,
	    /* pin_type           = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 4,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__DIRTY,	VARIABLE_ENTRY_TYPE,	100,	FALSE,	0 },
	      { FLUSH_OP__RESIZE,	VARIABLE_ENTRY_TYPE,	200,	FALSE,	VARIABLE_ENTRY_SIZE / 2 },
	      { FLUSH_OP__DIRTY,	VARIABLE_ENTRY_TYPE,	300,	FALSE,	0 },
	      { FLUSH_OP__RENAME,	VARIABLE_ENTRY_TYPE,	300,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ FALSE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ TRUE
          },
	  {
            /* entry_num          = */ 4,
            /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 2000,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__NO_FLAGS_SET,
	    /* new_size           = */ 0,
	    /* num_pins           = */ 0,
	    /* pin_type           = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 3,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__DIRTY,	VARIABLE_ENTRY_TYPE,	2100,	FALSE,	0 },
	      { FLUSH_OP__RESIZE,	VARIABLE_ENTRY_TYPE,	2200,	FALSE,	VARIABLE_ENTRY_SIZE / 2 },
	      { FLUSH_OP__RENAME,	VARIABLE_ENTRY_TYPE,	2300,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ TRUE
          },
	  {
            /* entry_num          = */ 5,
            /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 350,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__NO_FLAGS_SET,
	    /* new_size           = */ 0,
	    /* num_pins           = */ 2,
	    /* pin_type           = */ {VARIABLE_ENTRY_TYPE, VARIABLE_ENTRY_TYPE, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {1000, 2000, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 3,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__DIRTY,	VARIABLE_ENTRY_TYPE,	1000,	FALSE,	0 },
	      { FLUSH_OP__DIRTY,	VARIABLE_ENTRY_TYPE,	2000,	FALSE,	0 },
	      { FLUSH_OP__RESIZE,	VARIABLE_ENTRY_TYPE,	350,	FALSE,	VARIABLE_ENTRY_SIZE / 4 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ TRUE
          },
	  {
            /* entry_num          = */ 6,
            /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 450,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__DIRTIED_FLAG,
	    /* new_size           = */ 0,
	    /* num_pins           = */ 2,
	    /* pin_type           = */ {VARIABLE_ENTRY_TYPE, VARIABLE_ENTRY_TYPE, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {1000, 2000, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 3,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__DIRTY,	VARIABLE_ENTRY_TYPE,	1000,	FALSE,	0 },
	      { FLUSH_OP__DIRTY,	VARIABLE_ENTRY_TYPE,	2000,	FALSE,	0 },
	      { FLUSH_OP__RESIZE,	VARIABLE_ENTRY_TYPE,	450,	FALSE,	VARIABLE_ENTRY_SIZE / 4 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ TRUE
          },
	  {
            /* entry_num          = */ 7,
            /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 650,
	    /* insert_flag        = */ TRUE,
	    /* flags		  = */ H5C2__NO_FLAGS_SET,
	    /* new_size           = */ 0,
	    /* num_pins           = */ 2,
	    /* pin_type           = */ {VARIABLE_ENTRY_TYPE, VARIABLE_ENTRY_TYPE, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {1000, 2000, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 3,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__DIRTY,	VARIABLE_ENTRY_TYPE,	1000,	FALSE,	0 },
	      { FLUSH_OP__DIRTY,	VARIABLE_ENTRY_TYPE,	2000,	FALSE,	0 },
	      { FLUSH_OP__RESIZE,	VARIABLE_ENTRY_TYPE,	650,	FALSE,	VARIABLE_ENTRY_SIZE / 4 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ FALSE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ TRUE
          },
	  {
            /* entry_num          = */ 8,
            /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 750,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__DIRTIED_FLAG,
	    /* new_size           = */ 0,
	    /* num_pins           = */ 2,
	    /* pin_type           = */ {VARIABLE_ENTRY_TYPE, VARIABLE_ENTRY_TYPE, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {1000, 2000, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 3,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__DIRTY,	VARIABLE_ENTRY_TYPE,	1000,	FALSE,	0 },
	      { FLUSH_OP__DIRTY,	VARIABLE_ENTRY_TYPE,	2000,	FALSE,	0 },
	      { FLUSH_OP__RESIZE,	VARIABLE_ENTRY_TYPE,	750,	FALSE,	VARIABLE_ENTRY_SIZE / 4 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ TRUE
          },
	  {
            /* entry_num          = */ 9,
            /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 500,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__DIRTIED_FLAG,
	    /* new_size           = */ 0,
	    /* num_pins           = */ 0,
	    /* pin_type           = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 4,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__DIRTY,	VARIABLE_ENTRY_TYPE,	350,	FALSE,	0 },
	      { FLUSH_OP__DIRTY,	VARIABLE_ENTRY_TYPE,	450,	FALSE,	0 },
	      { FLUSH_OP__DIRTY,	VARIABLE_ENTRY_TYPE,	650,	FALSE,	0 },
	      { FLUSH_OP__DIRTY,	VARIABLE_ENTRY_TYPE,	750,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ TRUE
          }
	};
	int check_size = 3;
	struct fo_flush_entry_check checks[3] =
	{
	  {
	    /* entry_num          = */ 0,
	    /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 100,
	    /* expected_size      = */ VARIABLE_ENTRY_SIZE,
	    /* in_cache           = */ FALSE,
	    /* at_main_addr       = */ TRUE,
	    /* is_dirty	          = */ FALSE,
	    /* is_protected       = */ FALSE,
	    /* is_pinned          = */ FALSE,
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ TRUE
	  },
	  {
	    /* entry_num          = */ 1,
	    /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 300,
	    /* expected_size      = */ VARIABLE_ENTRY_SIZE,
	    /* in_cache           = */ FALSE,
	    /* at_main_addr       = */ FALSE,
	    /* is_dirty	          = */ FALSE,
	    /* is_protected       = */ FALSE,
	    /* is_pinned          = */ FALSE,
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ TRUE
	  },
	  {
	    /* entry_num          = */ 2,
	    /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 2200,
	    /* expected_size      = */ VARIABLE_ENTRY_SIZE / 2,
	    /* in_cache           = */ FALSE,
	    /* at_main_addr       = */ TRUE,
	    /* is_dirty	          = */ FALSE,
	    /* is_protected       = */ FALSE,
	    /* is_pinned          = */ FALSE,
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ TRUE
	  }

	};

        check_flush_cache__flush_op_test(cache_ptr,
                                         test_num,
                                         flush_flags,
                                         spec_size,
                                         spec,
				         init_expected_index_len,
				         init_expected_index_size,
				         expected_index_len,
				         expected_index_size,
					 check_size,
					 checks);

	reset_entries2();
    }

    if ( pass2 ) /* test #28 */
    {
	/* Test the expected fheap case, in which an entry dirties
	 * and resizes itself, and dirties an entry which it has 
	 * pinned.
	 */
        int test_num			= 28;
	unsigned int flush_flags	= H5C2__NO_FLAGS_SET;
	int spec_size			= 5;
	int init_expected_index_len	= 5;
	size_t init_expected_index_size	= 3 * VARIABLE_ENTRY_SIZE;
	int expected_index_len		= 5;
	size_t expected_index_size	= 4 * VARIABLE_ENTRY_SIZE;
	struct fo_flush_cache_test_spec spec[5] = 
	{
          { 
            /* entry_num          = */ 0,
            /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 100,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__NO_FLAGS_SET,
	    /* new_size           = */ 0,
	    /* num_pins           = */ 0,
	    /* pin_type           = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 0,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ FALSE
          },
	  {
            /* entry_num          = */ 1,
            /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 200,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__DIRTIED_FLAG | H5C2__SIZE_CHANGED_FLAG,
	    /* new_size           = */ VARIABLE_ENTRY_SIZE / 2,
	    /* num_pins           = */ 1,
	    /* pin_type           = */ {VARIABLE_ENTRY_TYPE, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {100, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 3,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__DIRTY,	VARIABLE_ENTRY_TYPE,	100,	FALSE,	0 },
	      { FLUSH_OP__RESIZE,	VARIABLE_ENTRY_TYPE,	200,	FALSE,	VARIABLE_ENTRY_SIZE },
	      { FLUSH_OP__RENAME,	VARIABLE_ENTRY_TYPE,	200,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ FALSE
          },
	  {
            /* entry_num          = */ 2,
            /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 300,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__DIRTIED_FLAG | H5C2__SIZE_CHANGED_FLAG,
	    /* new_size           = */ VARIABLE_ENTRY_SIZE / 4,
	    /* num_pins           = */ 1,
	    /* pin_type           = */ {VARIABLE_ENTRY_TYPE, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {400, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 3,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__DIRTY,	VARIABLE_ENTRY_TYPE,	400,	FALSE,	0 },
	      { FLUSH_OP__RESIZE,	VARIABLE_ENTRY_TYPE,	300,	FALSE,	VARIABLE_ENTRY_SIZE / 2 },
	      { FLUSH_OP__RENAME,	VARIABLE_ENTRY_TYPE,	300,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ FALSE
          },
	  {
            /* entry_num          = */ 3,
            /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 400,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__NO_FLAGS_SET,
	    /* new_size           = */ 0,
	    /* num_pins           = */ 0,
	    /* pin_type           = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 0,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ FALSE
          },
	  {
            /* entry_num          = */ 4,
            /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 500,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__DIRTIED_FLAG | H5C2__SIZE_CHANGED_FLAG,
	    /* new_size           = */ VARIABLE_ENTRY_SIZE / 4,
	    /* num_pins           = */ 1,
	    /* pin_type           = */ {VARIABLE_ENTRY_TYPE, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {100, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 3,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__DIRTY,	VARIABLE_ENTRY_TYPE,	100,	FALSE,	0 },
	      { FLUSH_OP__RESIZE,	VARIABLE_ENTRY_TYPE,	500,	FALSE,	VARIABLE_ENTRY_SIZE / 2 },
	      { FLUSH_OP__RENAME,	VARIABLE_ENTRY_TYPE,	500,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ FALSE
          }
	};
	int check_size = 0;
	struct fo_flush_entry_check checks[1] =
	{
	  {
	    /* entry_num          = */ 0,
	    /* entry_type         = */ 0,
	    /* entry_index        = */ 0,
	    /* expected_size      = */ 0,
	    /* in_cache           = */ FALSE,
	    /* at_main_addr       = */ FALSE,
	    /* is_dirty	          = */ FALSE,
	    /* is_protected       = */ FALSE,
	    /* is_pinned          = */ FALSE,
	    /* expected_loaded    = */ FALSE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ FALSE,
	    /* expected_destroyed = */ FALSE
	  }
	};

        check_flush_cache__flush_op_test(cache_ptr,
                                         test_num,
                                         flush_flags,
                                         spec_size,
                                         spec,
				         init_expected_index_len,
				         init_expected_index_size,
				         expected_index_len,
				         expected_index_size,
					 check_size,
					 checks);

	reset_entries2();
    }

    if ( pass2 ) /* test #29 */
    {
	/* Repeat test #28 with the flush invalidate flag.
	 *
	 * Test the expected fheap case, in which an entry dirties
	 * and resizes itself, and dirties an entry which it has 
	 * pinned.
	 */
        int test_num			= 29;
	unsigned int flush_flags	= H5C2__FLUSH_INVALIDATE_FLAG;
	int spec_size			= 5;
	int init_expected_index_len	= 5;
	size_t init_expected_index_size	= 3 * VARIABLE_ENTRY_SIZE;
	int expected_index_len		= 0;
	size_t expected_index_size	= 0;
	struct fo_flush_cache_test_spec spec[5] = 
	{
          { 
            /* entry_num          = */ 0,
            /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 100,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__NO_FLAGS_SET,
	    /* new_size           = */ 0,
	    /* num_pins           = */ 0,
	    /* pin_type           = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 0,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ TRUE
          },
	  {
            /* entry_num          = */ 1,
            /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 200,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__DIRTIED_FLAG | H5C2__SIZE_CHANGED_FLAG,
	    /* new_size           = */ VARIABLE_ENTRY_SIZE / 2,
	    /* num_pins           = */ 1,
	    /* pin_type           = */ {VARIABLE_ENTRY_TYPE, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {100, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 3,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__DIRTY,	VARIABLE_ENTRY_TYPE,	100,	FALSE,	0 },
	      { FLUSH_OP__RESIZE,	VARIABLE_ENTRY_TYPE,	200,	FALSE,	VARIABLE_ENTRY_SIZE },
	      { FLUSH_OP__RENAME,	VARIABLE_ENTRY_TYPE,	200,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ TRUE
          },
	  {
            /* entry_num          = */ 2,
            /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 300,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__DIRTIED_FLAG | H5C2__SIZE_CHANGED_FLAG,
	    /* new_size           = */ VARIABLE_ENTRY_SIZE / 4,
	    /* num_pins           = */ 1,
	    /* pin_type           = */ {VARIABLE_ENTRY_TYPE, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {400, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 3,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__DIRTY,	VARIABLE_ENTRY_TYPE,	400,	FALSE,	0 },
	      { FLUSH_OP__RESIZE,	VARIABLE_ENTRY_TYPE,	300,	FALSE,	VARIABLE_ENTRY_SIZE / 2 },
	      { FLUSH_OP__RENAME,	VARIABLE_ENTRY_TYPE,	300,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ TRUE
          },
	  {
            /* entry_num          = */ 3,
            /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 400,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__NO_FLAGS_SET,
	    /* new_size           = */ 0,
	    /* num_pins           = */ 0,
	    /* pin_type           = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {0, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 0,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ TRUE
          },
	  {
            /* entry_num          = */ 4,
            /* entry_type         = */ VARIABLE_ENTRY_TYPE,
	    /* entry_index        = */ 500,
	    /* insert_flag        = */ FALSE,
	    /* flags		  = */ H5C2__DIRTIED_FLAG | H5C2__SIZE_CHANGED_FLAG,
	    /* new_size           = */ VARIABLE_ENTRY_SIZE / 4,
	    /* num_pins           = */ 1,
	    /* pin_type           = */ {VARIABLE_ENTRY_TYPE, 0, 0, 0, 0, 0, 0, 0},
	    /* pin_idx            = */ {100, 0, 0, 0, 0, 0, 0, 0},
	    /* num_flush_ops      = */ 3,
	    /* flush_ops          = */
	    /*	op_code:		type:	idx:	flag:	size: */
	    { { FLUSH_OP__DIRTY,	VARIABLE_ENTRY_TYPE,	100,	FALSE,	0 },
	      { FLUSH_OP__RESIZE,	VARIABLE_ENTRY_TYPE,	500,	FALSE,	VARIABLE_ENTRY_SIZE / 2 },
	      { FLUSH_OP__RENAME,	VARIABLE_ENTRY_TYPE,	500,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 },
	      { FLUSH_OP__NO_OP,	0,	0,	FALSE,	0 } },
	    /* expected_loaded    = */ TRUE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ TRUE,
	    /* expected_destroyed = */ TRUE
          }
	};
	int check_size = 0;
	struct fo_flush_entry_check checks[1] =
	{
	  {
	    /* entry_num          = */ 0,
	    /* entry_type         = */ 0,
	    /* entry_index        = */ 0,
	    /* expected_size      = */ 0,
	    /* in_cache           = */ FALSE,
	    /* at_main_addr       = */ FALSE,
	    /* is_dirty	          = */ FALSE,
	    /* is_protected       = */ FALSE,
	    /* is_pinned          = */ FALSE,
	    /* expected_loaded    = */ FALSE,
	    /* expected_cleared   = */ FALSE,
	    /* expected_flushed   = */ FALSE,
	    /* expected_destroyed = */ FALSE
	  }
	};

        check_flush_cache__flush_op_test(cache_ptr,
                                         test_num,
                                         flush_flags,
                                         spec_size,
                                         spec,
				         init_expected_index_len,
				         init_expected_index_size,
				         expected_index_len,
				         expected_index_size,
					 check_size,
					 checks);

	reset_entries2();
    }

    /* finally finish up with the flush ops eviction test */
    check_flush_cache__flush_op_eviction_test(cache_ptr);

    return;

} /* check_flush_cache__flush_ops() */


/*-------------------------------------------------------------------------
 * Function:	check_flush_cache__flush_op_test()
 *
 * Purpose:	Run a flush op flush cache test.  Of the nature of 
 * 		flush operations, this is a multi-entry test.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              9/3/06
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
check_flush_cache__flush_op_test(H5C2_t * cache_ptr,
                                 int test_num,
                                 unsigned int flush_flags,
                                 int spec_size,
                                 struct fo_flush_cache_test_spec spec[],
				 int init_expected_index_len,
				 size_t init_expected_index_size,
				 int expected_index_len,
				 size_t expected_index_size,
				 int check_size,
				 struct fo_flush_entry_check check[])
{
    /* const char *   fcn_name = "check_flush_cache__flush_op_test"; */
    static char    msg[128];
    herr_t	   result;
    int            i;
    int            j;
    test_entry_t * base_addr;
    test_entry_t * entry_ptr;

#if 0 /* This is useful debugging code -- lets keep it around. */
    HDfprintf(stdout, "check_flush_cache__flush_op_test: test %d\n",
	      test_num);
#endif 

    if ( cache_ptr == NULL ) {

        pass2 = FALSE;
        HDsnprintf(msg, (size_t)128,
                   "cache_ptr NULL on entry to flush op test #%d.",
                   test_num);
        failure_mssg2 = msg;
    }
    else if ( ( cache_ptr->index_len != 0 ) ||
              ( cache_ptr->index_size != 0 ) ) {
#if 0 /* JRM */
        HDfprintf(stdout, "%s:(1) index_len = %ld, index_size = %ld.\n",
                  fcn_name, (long)(cache_ptr->index_len),
                  (long)(cache_ptr->index_size));
#endif /* JRM */
        pass2 = FALSE;

        HDsnprintf(msg, (size_t)128,
                   "cache not empty at beginning of flush op test #%d.",
                   test_num);
        failure_mssg2 = msg;
    }
    else if ( ( spec_size < 1 ) || ( spec == NULL ) ) {

        pass2 = FALSE;
        HDsnprintf(msg, (size_t)128,
                   "missing/bad test spec on entry to flush op test #%d.",
                   test_num);
        failure_mssg2 = msg;
    }

    i = 0;
    while ( ( pass2 ) && ( i < spec_size ) )
    {
        if ( ( spec[i].entry_num != i ) ||
             ( spec[i].entry_type < 0 ) ||
             ( spec[i].entry_type >= NUMBER_OF_ENTRY_TYPES ) ||
             ( spec[i].entry_index < 0 ) ||
             ( spec[i].entry_index > max_indices2[spec[i].entry_type] ) ||
	     ( spec[i].num_pins < 0 ) ||
	     ( spec[i].num_pins > MAX_PINS ) ||
	     ( spec[i].num_flush_ops < 0 ) ||
	     ( spec[i].num_flush_ops > MAX_FLUSH_OPS ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
                    "bad data in spec[%d] on entry to flush op test #%d.",
                    i, test_num);
            failure_mssg2 = msg;
        }
        i++;
    }

    i = 0;
    while ( ( pass2 ) && ( i < check_size ) )
    {
        if ( ( check[i].entry_num != i ) ||
             ( check[i].entry_type < 0 ) ||
             ( check[i].entry_type >= NUMBER_OF_ENTRY_TYPES ) ||
             ( check[i].entry_index < 0 ) ||
             ( check[i].entry_index > max_indices2[check[i].entry_type] ) ||
             ( check[i].expected_size <= (size_t)0 ) ||
	     ( ( check[i].in_cache != TRUE ) && 
	       ( check[i].in_cache != FALSE ) ) ||
	     ( ( check[i].at_main_addr != TRUE ) && 
	       ( check[i].at_main_addr != FALSE ) ) ||
	     ( ( check[i].is_dirty != TRUE ) && 
	       ( check[i].is_dirty != FALSE ) ) ||
	     ( ( check[i].is_protected != TRUE ) && 
	       ( check[i].is_protected != FALSE ) ) ||
	     ( ( check[i].is_pinned != TRUE ) && 
	       ( check[i].is_pinned != FALSE ) ) ||
	     ( ( check[i].expected_deserialized != TRUE ) && 
	       ( check[i].expected_deserialized != FALSE ) ) ||
	     ( ( check[i].expected_cleared != TRUE ) && 
	       ( check[i].expected_cleared != FALSE ) ) ||
	     ( ( check[i].expected_serialized != TRUE ) && 
	       ( check[i].expected_serialized != FALSE ) ) ||
	     ( ( check[i].expected_destroyed != TRUE ) && 
	       ( check[i].expected_destroyed != FALSE ) ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
                    "bad data in check[%d] on entry to flush op test #%d.",
                    i, test_num);
            failure_mssg2 = msg;
        }
        i++;
    }

    i = 0;
    while ( ( pass2 ) && ( i < spec_size ) )
    {
        if ( spec[i].insert_flag ) {

            insert_entry2(cache_ptr, spec[i].entry_type, spec[i].entry_index,
                         TRUE, spec[i].flags);

        } else {

            protect_entry2(cache_ptr, spec[i].entry_type, spec[i].entry_index);

            unprotect_entry_with_size_change2(cache_ptr, spec[i].entry_type,
			                     spec[i].entry_index,
					     spec[i].flags, spec[i].new_size);
        }

	for ( j = 0; j < spec[i].num_pins; j++ )
	{
            create_pinned_entry_dependency2(cache_ptr,
		                           spec[i].entry_type,
					   spec[i].entry_index,
					   spec[i].pin_type[j],
					   spec[i].pin_idx[j]);
	}

	for ( j = 0; j < spec[i].num_flush_ops; j++ )
	{
	    add_flush_op2(spec[i].entry_type, 
			 spec[i].entry_index,
			 spec[i].flush_ops[j].op_code,
			 spec[i].flush_ops[j].type,
			 spec[i].flush_ops[j].idx,
			 spec[i].flush_ops[j].flag,
			 spec[i].flush_ops[j].size);
	}

        i++;
    }

    if ( pass2 ) {

        if ( ( cache_ptr->index_len != init_expected_index_len ) ||
             ( cache_ptr->index_size != init_expected_index_size ) ) {

            pass2 = FALSE;
            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
                "Unexpected cache len/size before flush in flush op test #%d.",
                test_num);
            failure_mssg2 = msg;
        }
    }

    if ( pass2 ) {

        result = H5C2_flush_cache(cache_ptr, H5P_DATASET_XFER_DEFAULT, 
			          flush_flags);

        if ( result < 0 ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
                     "flush with flags 0x%x failed in flush op test #%d.",
                     flush_flags, test_num);
            failure_mssg2 = msg;
        }
    }


    i = 0;
    while ( ( pass2 ) && ( i < spec_size ) )
    {
        base_addr = entries2[spec[i].entry_type];
        entry_ptr = &(base_addr[spec[i].entry_index]);

#ifndef NDEBUG
        /* The clear_dirty_bits() callback is only called in debug mode --
         * thus we can only do our full test on the expected entry history
         * when debug is enabled.
         */
        if ( ( entry_ptr->deserialized != spec[i].expected_deserialized ) ||
             ( entry_ptr->cleared != spec[i].expected_cleared ) ||
             ( entry_ptr->serialized != spec[i].expected_serialized ) ||
             ( entry_ptr->destroyed != spec[i].expected_destroyed ) ) {
#else
        /* When in procduction mode, the clear_dirty_bits() callback is
         * not called, so entry_ptr->cleared should never be set.
         */
        if ( ( entry_ptr->deserialized != spec[i].expected_deserialized ) ||
             ( entry_ptr->cleared ) ||
             ( entry_ptr->serialized != spec[i].expected_serialized ) ||
             ( entry_ptr->destroyed != spec[i].expected_destroyed ) ) {
#endif /* NDEBUG */

#if 0 /* This is useful debugging code.  Lets keep it around. */

            HDfprintf(stdout,
             "desrlzd = %d(%d), clrd = %d(%d), srlzd = %d(%d), dest = %d(%d)\n",
             (int)(entry_ptr->deserialized),
             (int)(spec[i].expected_deserialized),
             (int)(entry_ptr->cleared),
             (int)(spec[i].expected_cleared),
             (int)(entry_ptr->serialized),
             (int)(spec[i].expected_serialized),
             (int)(entry_ptr->destroyed),
             (int)(spec[i].expected_destroyed));

	    HDfprintf(stdout, "entry_ptr->header.is_dirty = %d\n",
		      (int)(entry_ptr->header.is_dirty));
#endif

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "Bad status on entry %d after flush op test #%d.",
                       i, test_num);
            failure_mssg2 = msg;
        }
        i++;
    }

    if ( pass2 ) {

        i = 0;
        while ( ( pass2 ) && ( i < check_size ) )
        {
	    if ( check[i].in_cache != entry_in_cache2(cache_ptr, 
				                      check[i].entry_type,
						      check[i].entry_index) ) {

                pass2 = FALSE;
                HDsnprintf(msg, (size_t)128,
                           "Check1 failed on entry %d after flush op test #%d.",
                           i, test_num);
                failure_mssg2 = msg;
	    }

            base_addr = entries2[check[i].entry_type];
            entry_ptr = &(base_addr[check[i].entry_index]);

#ifndef NDEBUG
        /* The clear_dirty_bits() callback is only called in debug mode --
         * thus we can only do our full test on the expected entry status
         * and histry when debug is enabled.
         */
	    if ( ( entry_ptr->size != check[i].expected_size ) ||
		 ( ( ! entry_ptr->header.destroy_in_progress ) &&
		   ( check[i].in_cache ) &&
		   ( entry_ptr->header.size != check[i].expected_size ) ) ||
		 ( entry_ptr->at_main_addr != check[i].at_main_addr ) ||
		 ( entry_ptr->is_dirty != check[i].is_dirty ) ||
		 ( entry_ptr->header.is_dirty != check[i].is_dirty ) ||
		 ( entry_ptr->is_protected != check[i].is_protected ) ||
		 ( entry_ptr->header.is_protected != check[i].is_protected ) ||
                 ( entry_ptr->is_pinned != check[i].is_pinned ) ||
                 ( entry_ptr->header.is_pinned != check[i].is_pinned ) ||
		 ( entry_ptr->deserialized != 
		   check[i].expected_deserialized ) ||
		 ( entry_ptr->cleared != check[i].expected_cleared ) ||
		 ( entry_ptr->serialized != check[i].expected_serialized ) ||
		 ( entry_ptr->destroyed != check[i].expected_destroyed ) ) {
#else
        /* When in procduction mode, the clear_dirty_bits() callback is
         * not called, so entry_ptr->cleared should never be set.
         */
	    if ( ( entry_ptr->size != check[i].expected_size ) ||
		 ( ( ! entry_ptr->header.destroy_in_progress ) &&
		   ( check[i].in_cache ) &&
		   ( entry_ptr->header.size != check[i].expected_size ) ) ||
		 ( entry_ptr->at_main_addr != check[i].at_main_addr ) ||
		 ( entry_ptr->is_dirty != check[i].is_dirty ) ||
		 ( entry_ptr->header.is_dirty != check[i].is_dirty ) ||
		 ( entry_ptr->is_protected != check[i].is_protected ) ||
		 ( entry_ptr->header.is_protected != check[i].is_protected ) ||
                 ( entry_ptr->is_pinned != check[i].is_pinned ) ||
                 ( entry_ptr->header.is_pinned != check[i].is_pinned ) ||
		 ( entry_ptr->deserialized != 
		   check[i].expected_deserialized ) ||
		 ( entry_ptr->cleared ) ||
		 ( entry_ptr->serialized != check[i].expected_serialized ) ||
		 ( entry_ptr->destroyed != check[i].expected_destroyed ) ) {
#endif /* NDEBUG */


#if 0 /* This is useful debugging code.  Lets keep it around for a while. */

		if ( entry_ptr->size != check[i].expected_size ) {
		    HDfprintf(stdout, "entry_ptr->size (expected) = %d (%d).\n",
			      (int)(entry_ptr->size),
			      (int)(check[i].expected_size));
		}
		if ( ( ! entry_ptr->header.destroy_in_progress ) &&
		     ( check[i].in_cache ) &&
                     ( entry_ptr->header.size != check[i].expected_size ) ) {
                    HDfprintf(stdout, 
                              "(!destroy in progress and in cache and size (expected) = %d (%d).\n",
                              (int)(entry_ptr->header.size),
			      (int)(check[i].expected_size));
		}
		if ( entry_ptr->at_main_addr != check[i].at_main_addr ) {
		    HDfprintf(stdout, "(%d,%d) at main addr (expected) = %d (%d).\n",
			      (int)(check[i].entry_type),
			      (int)(check[i].entry_index),
                              (int)(entry_ptr->at_main_addr),
			      (int)(check[i].at_main_addr));
                }
		if ( entry_ptr->is_dirty != check[i].is_dirty ) {
		    HDfprintf(stdout, "entry_ptr->is_dirty (expected) = %d (%d).\n",
		              (int)(entry_ptr->is_dirty),
			      (int)(check[i].is_dirty));
		}
		if ( entry_ptr->header.is_dirty != check[i].is_dirty ) {
		    HDfprintf(stdout, "entry_ptr->header.is_dirty (expected) = %d (%d).\n",
		              (int)(entry_ptr->header.is_dirty),
			      (int)(check[i].is_dirty));
		}
	        if ( entry_ptr->is_protected != check[i].is_protected ) {
                    HDfprintf(stdout, "entry_ptr->is_protected (expected) = %d (%d).\n",
			      (int)(entry_ptr->is_protected),
			      (int)(check[i].is_protected));
		}
	        if ( entry_ptr->header.is_protected != check[i].is_protected ) {
                    HDfprintf(stdout, "entry_ptr->header.is_protected (expected) = %d (%d).\n",
			      (int)(entry_ptr->is_protected),
			      (int)(check[i].is_protected));
		}
		if ( entry_ptr->is_pinned != check[i].is_pinned ) {
		    HDfprintf(stdout, "entry_ptr->is_pinned (expected) = %d (%d).\n",
			      (int)(entry_ptr->is_pinned),
			      (int)(check[i].is_pinned));
		}
		if ( entry_ptr->header.is_pinned != check[i].is_pinned ) {
		    HDfprintf(stdout, "entry_ptr->header.is_pinned (expected) = %d (%d).\n",
			      (int)(entry_ptr->header.is_pinned),
			      (int)(check[i].is_pinned));
		}
		if ( entry_ptr->deserialized != 
				check[i].expected_deserialized ) {
		    HDfprintf(stdout, 
		 	      "entry_ptr->deserialized (expected) = %d (%d).\n",
			      (int)(entry_ptr->deserialized),
			      (int)(check[i].expected_deserialized));
		}
		if ( entry_ptr->cleared != check[i].expected_cleared ) {
		    HDfprintf(stdout, "entry_ptr->cleared (expected) = %d (%d).\n",
			      (int)(entry_ptr->cleared),
			      (int)(check[i].expected_cleared));
		}
		if ( entry_ptr->serialized != check[i].expected_serialized ) {
		    HDfprintf(stdout, 
			      "entry_ptr->serialized (expected) = %d (%d).\n",
			      (int)(entry_ptr->serialized),
			      (int)(check[i].expected_serialized));
		}
		if ( entry_ptr->destroyed != check[i].expected_destroyed ) {
		    HDfprintf(stdout, "entry_ptr->destroyed (expected) = %d (%d).\n",
			      (int)(entry_ptr->destroyed),
			      (int)(check[i].expected_destroyed));
		}
#endif
                pass2 = FALSE;
                HDsnprintf(msg, (size_t)128,
                           "Check2 failed on entry %d after flush op test #%d.",
                           i, test_num);
                failure_mssg2 = msg;
	    }
	    i++;
        }
    }

    if ( pass2 ) {

        if ( ( ( (flush_flags & H5C2__FLUSH_INVALIDATE_FLAG) == 0 )
               &&
               ( ( cache_ptr->index_len != expected_index_len )
                 ||
                 ( cache_ptr->index_size != expected_index_size )
               )
             )
             ||
             ( ( (flush_flags & H5C2__FLUSH_INVALIDATE_FLAG) != 0 )
               &&
               ( ( cache_ptr->index_len != 0 )
                 ||
                 ( cache_ptr->index_size != 0 )
               )
             )
           ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
                 "Unexpected cache len/size after flush in flush op test #%d.",
                 test_num);
            failure_mssg2 = msg;
        }
    }

    /* clean up the cache to prep for the next test */
    if ( pass2 ) {

        result = H5C2_flush_cache(cache_ptr, H5P_DATASET_XFER_DEFAULT,
                                  H5C2__FLUSH_INVALIDATE_FLAG);

        if ( result < 0 ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "Flush failed on cleanup in flush op test #%d.",
                       test_num);
            failure_mssg2 = msg;
        }
        else if ( ( cache_ptr->index_len != 0 ) ||
                  ( cache_ptr->index_size != 0 ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
            "Unexpected cache len/size after cleanup in flush op test #%d.",
            test_num);
            failure_mssg2 = msg;

        }
    }

    i = 0;
    while ( ( pass2 ) && ( i < spec_size ) )
    {
        base_addr = entries2[spec[i].entry_type];
        entry_ptr = &(base_addr[spec[i].entry_index]);

	entry_ptr->size      = entry_sizes2[spec[i].entry_type];

        entry_ptr->deserialized = FALSE;
        entry_ptr->cleared      = FALSE;
        entry_ptr->serialized   = FALSE;
        entry_ptr->destroyed    = FALSE;

        i++;
    }

    i = 0;
    while ( ( pass2 ) && ( i < check_size ) )
    {
        base_addr = entries2[check[i].entry_type];
        entry_ptr = &(base_addr[check[i].entry_index]);

	entry_ptr->size      = entry_sizes2[check[i].entry_type];

        entry_ptr->deserialized = FALSE;
        entry_ptr->cleared      = FALSE;
        entry_ptr->serialized   = FALSE;
        entry_ptr->destroyed    = FALSE;

        i++;
    }

    return;

} /* check_flush_cache__flush_op_test() */


/*-------------------------------------------------------------------------
 * Function:	check_flush_cache__flush_op_eviction_test()
 *
 * Purpose:	Verify that flush operations work as expected when an
 *              entry is evicted.
 *
 *              Do nothing if pass2 is FALSE on entry.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              10/3/06
 *
 * Modifications:
 *
 *              JRM -- 10/9/07
 *		Modified test to accomodate changes in the cache API.  
 *		In particular, since the cache is now reading and writing
 *		its own entries, the serialize callback is not called on
 *		entry eviction unless the entry is dirty.  
 *
 *		This fact broke some basic assumptions of the test, and 
 *		necessitated a substantial re-write.
 *
 *-------------------------------------------------------------------------
 */

static void
check_flush_cache__flush_op_eviction_test(H5C2_t * cache_ptr)
{
    /* const char *   fcn_name = "check_flush_cache__flush_op_eviction_test"; */
    int		   i;
    int		   num_variable_entries = 10;
    int		   num_monster_entries = 31;
    int		   num_large_entries = 0;
    herr_t	   result;
    test_entry_t * entry_ptr;
    test_entry_t * base_addr;
    struct expected_entry_status expected[10 + 31 + 14] =
    {
      /* the expected array is used to maintain a table of the expected status of every 
       * entry used in this test.  Note that since the function that processes this
       * array only processes as much of it as it is told to, we don't have to 
       * worry about maintaining the status of entries that we haven't used yet.
       */
      /* entry			entry				in	at main                                                       */
      /* type:			index:	size:			cache:	addr:	dirty:	prot:	pinned:	dsrlzd:	clrd:	srlzd:	dest: */
      { VARIABLE_ENTRY_TYPE,	0,	VARIABLE_ENTRY_SIZE/4,	TRUE,	TRUE,	TRUE,	FALSE,	TRUE,	TRUE,	FALSE,	FALSE,	FALSE },
      { VARIABLE_ENTRY_TYPE,	1,	VARIABLE_ENTRY_SIZE/4,	TRUE,	TRUE,	TRUE,	FALSE,	FALSE,	TRUE,	FALSE,	FALSE,	FALSE },
      { VARIABLE_ENTRY_TYPE,	2,	VARIABLE_ENTRY_SIZE,	TRUE,	TRUE,	FALSE,	FALSE,	FALSE,	TRUE,	FALSE,	FALSE,	FALSE },
      { VARIABLE_ENTRY_TYPE,	3,	VARIABLE_ENTRY_SIZE/4,	TRUE,	TRUE,	TRUE,	FALSE,	FALSE,	TRUE,	FALSE,	FALSE,	FALSE },
      { VARIABLE_ENTRY_TYPE,	4,	VARIABLE_ENTRY_SIZE,	TRUE,	TRUE,	FALSE,	FALSE,	FALSE,	TRUE,	FALSE,	FALSE,	FALSE },
      { VARIABLE_ENTRY_TYPE,	5,	VARIABLE_ENTRY_SIZE/4,	TRUE,	TRUE,	TRUE,	FALSE,	FALSE,	TRUE,	FALSE,	FALSE,	FALSE },
      { VARIABLE_ENTRY_TYPE,	6,	VARIABLE_ENTRY_SIZE/2,	TRUE,	TRUE,	TRUE,	FALSE,	FALSE,	TRUE,	FALSE,	FALSE,	FALSE },
      { VARIABLE_ENTRY_TYPE,	7,	VARIABLE_ENTRY_SIZE/2,	TRUE,	TRUE,	TRUE,	FALSE,	FALSE,	TRUE,	FALSE,	FALSE,	FALSE },
      { VARIABLE_ENTRY_TYPE,	8,	VARIABLE_ENTRY_SIZE,	TRUE,	TRUE,	FALSE,	FALSE,	FALSE,	TRUE,	FALSE,	FALSE,	FALSE },
      { VARIABLE_ENTRY_TYPE,	9,	VARIABLE_ENTRY_SIZE,	TRUE,	TRUE,	FALSE,	FALSE,	TRUE,	TRUE,	FALSE,	FALSE,	FALSE },
      { MONSTER_ENTRY_TYPE,	0,	MONSTER_ENTRY_SIZE,	TRUE,	TRUE,	TRUE,	FALSE,	FALSE,	TRUE,	FALSE,	FALSE,	FALSE },
      { MONSTER_ENTRY_TYPE,	1,	MONSTER_ENTRY_SIZE,	TRUE,	TRUE,	TRUE,	FALSE,	FALSE,	TRUE,	FALSE,	FALSE,	FALSE },
      { MONSTER_ENTRY_TYPE,	2,	MONSTER_ENTRY_SIZE,	TRUE,	TRUE,	TRUE,	FALSE,	FALSE,	TRUE,	FALSE,	FALSE,	FALSE },
      { MONSTER_ENTRY_TYPE,	3,	MONSTER_ENTRY_SIZE,	TRUE,	TRUE,	TRUE,	FALSE,	FALSE,	TRUE,	FALSE,	FALSE,	FALSE },
      { MONSTER_ENTRY_TYPE,	4,	MONSTER_ENTRY_SIZE,	TRUE,	TRUE,	TRUE,	FALSE,	FALSE,	TRUE,	FALSE,	FALSE,	FALSE },
      { MONSTER_ENTRY_TYPE,	5,	MONSTER_ENTRY_SIZE,	TRUE,	TRUE,	TRUE,	FALSE,	FALSE,	TRUE,	FALSE,	FALSE,	FALSE },
      { MONSTER_ENTRY_TYPE,	6,	MONSTER_ENTRY_SIZE,	TRUE,	TRUE,	TRUE,	FALSE,	FALSE,	TRUE,	FALSE,	FALSE,	FALSE },
      { MONSTER_ENTRY_TYPE,	7,	MONSTER_ENTRY_SIZE,	TRUE,	TRUE,	TRUE,	FALSE,	FALSE,	TRUE,	FALSE,	FALSE,	FALSE },
      { MONSTER_ENTRY_TYPE,	8,	MONSTER_ENTRY_SIZE,	TRUE,	TRUE,	TRUE,	FALSE,	FALSE,	TRUE,	FALSE,	FALSE,	FALSE },
      { MONSTER_ENTRY_TYPE,	9,	MONSTER_ENTRY_SIZE,	TRUE,	TRUE,	TRUE,	FALSE,	FALSE,	TRUE,	FALSE,	FALSE,	FALSE },
      { MONSTER_ENTRY_TYPE,	10,	MONSTER_ENTRY_SIZE,	TRUE,	TRUE,	TRUE,	FALSE,	FALSE,	TRUE,	FALSE,	FALSE,	FALSE },
      { MONSTER_ENTRY_TYPE,	11,	MONSTER_ENTRY_SIZE,	TRUE,	TRUE,	TRUE,	FALSE,	FALSE,	TRUE,	FALSE,	FALSE,	FALSE },
      { MONSTER_ENTRY_TYPE,	12,	MONSTER_ENTRY_SIZE,	TRUE,	TRUE,	TRUE,	FALSE,	FALSE,	TRUE,	FALSE,	FALSE,	FALSE },
      { MONSTER_ENTRY_TYPE,	13,	MONSTER_ENTRY_SIZE,	TRUE,	TRUE,	TRUE,	FALSE,	FALSE,	TRUE,	FALSE,	FALSE,	FALSE },
      { MONSTER_ENTRY_TYPE,	14,	MONSTER_ENTRY_SIZE,	TRUE,	TRUE,	TRUE,	FALSE,	FALSE,	TRUE,	FALSE,	FALSE,	FALSE },
      { MONSTER_ENTRY_TYPE,	15,	MONSTER_ENTRY_SIZE,	TRUE,	TRUE,	TRUE,	FALSE,	FALSE,	TRUE,	FALSE,	FALSE,	FALSE },
      { MONSTER_ENTRY_TYPE,	16,	MONSTER_ENTRY_SIZE,	TRUE,	TRUE,	TRUE,	FALSE,	FALSE,	TRUE,	FALSE,	FALSE,	FALSE },
      { MONSTER_ENTRY_TYPE,	17,	MONSTER_ENTRY_SIZE,	TRUE,	TRUE,	TRUE,	FALSE,	FALSE,	TRUE,	FALSE,	FALSE,	FALSE },
      { MONSTER_ENTRY_TYPE,	18,	MONSTER_ENTRY_SIZE,	TRUE,	TRUE,	TRUE,	FALSE,	FALSE,	TRUE,	FALSE,	FALSE,	FALSE },
      { MONSTER_ENTRY_TYPE,	19,	MONSTER_ENTRY_SIZE,	TRUE,	TRUE,	TRUE,	FALSE,	FALSE,	TRUE,	FALSE,	FALSE,	FALSE },
      { MONSTER_ENTRY_TYPE,	20,	MONSTER_ENTRY_SIZE,	TRUE,	TRUE,	TRUE,	FALSE,	FALSE,	TRUE,	FALSE,	FALSE,	FALSE },
      { MONSTER_ENTRY_TYPE,	21,	MONSTER_ENTRY_SIZE,	TRUE,	TRUE,	TRUE,	FALSE,	FALSE,	TRUE,	FALSE,	FALSE,	FALSE },
      { MONSTER_ENTRY_TYPE,	22,	MONSTER_ENTRY_SIZE,	TRUE,	TRUE,	TRUE,	FALSE,	FALSE,	TRUE,	FALSE,	FALSE,	FALSE },
      { MONSTER_ENTRY_TYPE,	23,	MONSTER_ENTRY_SIZE,	TRUE,	TRUE,	TRUE,	FALSE,	FALSE,	TRUE,	FALSE,	FALSE,	FALSE },
      { MONSTER_ENTRY_TYPE,	24,	MONSTER_ENTRY_SIZE,	TRUE,	TRUE,	TRUE,	FALSE,	FALSE,	TRUE,	FALSE,	FALSE,	FALSE },
      { MONSTER_ENTRY_TYPE,	25,	MONSTER_ENTRY_SIZE,	TRUE,	TRUE,	TRUE,	FALSE,	FALSE,	TRUE,	FALSE,	FALSE,	FALSE },
      { MONSTER_ENTRY_TYPE,	26,	MONSTER_ENTRY_SIZE,	TRUE,	TRUE,	TRUE,	FALSE,	FALSE,	TRUE,	FALSE,	FALSE,	FALSE },
      { MONSTER_ENTRY_TYPE,	27,	MONSTER_ENTRY_SIZE,	TRUE,	TRUE,	TRUE,	FALSE,	FALSE,	TRUE,	FALSE,	FALSE,	FALSE },
      { MONSTER_ENTRY_TYPE,	28,	MONSTER_ENTRY_SIZE,	TRUE,	TRUE,	TRUE,	FALSE,	FALSE,	TRUE,	FALSE,	FALSE,	FALSE },
      { MONSTER_ENTRY_TYPE,	29,	MONSTER_ENTRY_SIZE,	TRUE,	TRUE,	TRUE,	FALSE,	FALSE,	TRUE,	FALSE,	FALSE,	FALSE },
      { MONSTER_ENTRY_TYPE,	30,	MONSTER_ENTRY_SIZE,	TRUE,	TRUE,	TRUE,	FALSE,	FALSE,	TRUE,	FALSE,	FALSE,	FALSE },
      { LARGE_ENTRY_TYPE,	0,	LARGE_ENTRY_SIZE,	TRUE,	TRUE,	TRUE,	FALSE,	FALSE,	TRUE,	FALSE,	FALSE,	FALSE },
      { LARGE_ENTRY_TYPE,	1,	LARGE_ENTRY_SIZE,	TRUE,	TRUE,	TRUE,	FALSE,	FALSE,	TRUE,	FALSE,	FALSE,	FALSE },
      { LARGE_ENTRY_TYPE,	2,	LARGE_ENTRY_SIZE,	TRUE,	TRUE,	TRUE,	FALSE,	FALSE,	TRUE,	FALSE,	FALSE,	FALSE },
      { LARGE_ENTRY_TYPE,	3,	LARGE_ENTRY_SIZE,	TRUE,	TRUE,	TRUE,	FALSE,	FALSE,	TRUE,	FALSE,	FALSE,	FALSE },
      { LARGE_ENTRY_TYPE,	4,	LARGE_ENTRY_SIZE,	TRUE,	TRUE,	TRUE,	FALSE,	FALSE,	TRUE,	FALSE,	FALSE,	FALSE },
      { LARGE_ENTRY_TYPE,	5,	LARGE_ENTRY_SIZE,	TRUE,	TRUE,	TRUE,	FALSE,	FALSE,	TRUE,	FALSE,	FALSE,	FALSE },
      { LARGE_ENTRY_TYPE,	6,	LARGE_ENTRY_SIZE,	TRUE,	TRUE,	TRUE,	FALSE,	FALSE,	TRUE,	FALSE,	FALSE,	FALSE },
      { LARGE_ENTRY_TYPE,	7,	LARGE_ENTRY_SIZE,	TRUE,	TRUE,	TRUE,	FALSE,	FALSE,	TRUE,	FALSE,	FALSE,	FALSE },
      { LARGE_ENTRY_TYPE,	8,	LARGE_ENTRY_SIZE,	TRUE,	TRUE,	TRUE,	FALSE,	FALSE,	TRUE,	FALSE,	FALSE,	FALSE },
      { LARGE_ENTRY_TYPE,	9,	LARGE_ENTRY_SIZE,	TRUE,	TRUE,	TRUE,	FALSE,	FALSE,	TRUE,	FALSE,	FALSE,	FALSE },
      { LARGE_ENTRY_TYPE,	10,	LARGE_ENTRY_SIZE,	TRUE,	TRUE,	TRUE,	FALSE,	FALSE,	TRUE,	FALSE,	FALSE,	FALSE },
      { LARGE_ENTRY_TYPE,	11,	LARGE_ENTRY_SIZE,	TRUE,	TRUE,	TRUE,	FALSE,	FALSE,	TRUE,	FALSE,	FALSE,	FALSE },
      { LARGE_ENTRY_TYPE,	12,	LARGE_ENTRY_SIZE,	TRUE,	TRUE,	TRUE,	FALSE,	FALSE,	TRUE,	FALSE,	FALSE,	FALSE },
      { LARGE_ENTRY_TYPE,	13,	LARGE_ENTRY_SIZE,	TRUE,	TRUE,	TRUE,	FALSE,	FALSE,	TRUE,	FALSE,	FALSE,	FALSE }
    };

    if ( cache_ptr == NULL ) {

        pass2 = FALSE;
        failure_mssg2 = "cache_ptr NULL on entry to flush ops test.";
    }
    else if ( ( cache_ptr->index_len != 0 ) ||
              ( cache_ptr->index_size != 0 ) ) {

        pass2 = FALSE;
        failure_mssg2 = "cache not empty at start of flush ops eviction test.";
    }
    else if ( ( cache_ptr->max_cache_size != (2 * 1024 * 1024 ) ) ||
              ( cache_ptr->min_clean_size != (1 * 1024 * 1024 ) ) ) {

	pass2 = FALSE;
	failure_mssg2 = "unexpected cache config at start of flush op eviction test.";

    } else {

        /* set min clean size to zero for this test as it simplifies
	 * computing the expected cache size after each operation.
	 */

        cache_ptr->min_clean_size = 0;
    }

    if ( pass2 ) {

        /* the basic idea in this test is to insert a bunch of entries
         * with flush operations associated with them, and then load 
         * other entries into the cache until the cache is full.  At 
         * that point, load yet more entries into the cache, and see 
         * if the flush operations are performed as expected.
	 *
	 * To make things a bit more interesting, we also include a
	 * couple of pins.
         */

	/* reset the stats before we start.  If stats are enabled, we will
	 * check to see if they are as expected at the end.
	 */
	H5C2_stats__reset(cache_ptr);


	/* load a few entries with pin relationships and flush ops.
	 * Start by just loading the entries.
	 */

	protect_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 0);
	unprotect_entry_with_size_change2(cache_ptr, VARIABLE_ENTRY_TYPE, 0, 
			           H5C2__DIRTIED_FLAG | H5C2__SIZE_CHANGED_FLAG,
				   (VARIABLE_ENTRY_SIZE / 4));

	protect_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 1);
	unprotect_entry_with_size_change2(cache_ptr, VARIABLE_ENTRY_TYPE, 1, 
			                 H5C2__DIRTIED_FLAG | H5C2__SIZE_CHANGED_FLAG, 
					 (VARIABLE_ENTRY_SIZE / 4));

	protect_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 2);
	unprotect_entry_with_size_change2(cache_ptr, VARIABLE_ENTRY_TYPE, 2, 
			                 H5C2__NO_FLAGS_SET, (size_t)0);

	protect_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 3);
	unprotect_entry_with_size_change2(cache_ptr, VARIABLE_ENTRY_TYPE, 3, 
			                  H5C2__DIRTIED_FLAG | H5C2__SIZE_CHANGED_FLAG, 
					  (VARIABLE_ENTRY_SIZE / 4));

	protect_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 4);
	unprotect_entry_with_size_change2(cache_ptr, VARIABLE_ENTRY_TYPE, 4, 
			                  H5C2__NO_FLAGS_SET, (size_t)0);

	protect_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 5);
	unprotect_entry_with_size_change2(cache_ptr, VARIABLE_ENTRY_TYPE, 5, 
			           H5C2__DIRTIED_FLAG | H5C2__SIZE_CHANGED_FLAG,
				   (VARIABLE_ENTRY_SIZE / 4));

	protect_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 6);
	unprotect_entry_with_size_change2(cache_ptr, VARIABLE_ENTRY_TYPE, 6, 
			           H5C2__DIRTIED_FLAG | H5C2__SIZE_CHANGED_FLAG,
				   (VARIABLE_ENTRY_SIZE / 2));

	protect_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 7);
	unprotect_entry_with_size_change2(cache_ptr, VARIABLE_ENTRY_TYPE, 7, 
			           H5C2__DIRTIED_FLAG | H5C2__SIZE_CHANGED_FLAG,
				   (VARIABLE_ENTRY_SIZE / 2));

	protect_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 8);
	unprotect_entry_with_size_change2(cache_ptr, VARIABLE_ENTRY_TYPE, 8, 
			                 H5C2__NO_FLAGS_SET, (size_t)0);

	protect_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 9);
	unprotect_entry_with_size_change2(cache_ptr, VARIABLE_ENTRY_TYPE, 9, 
			                 H5C2__NO_FLAGS_SET, (size_t)0);

	if ( ( cache_ptr->index_len != 10 ) ||
             ( cache_ptr->index_size != (4 * (VARIABLE_ENTRY_SIZE / 4)) +
	                                (2 * (VARIABLE_ENTRY_SIZE / 2)) + 
	                                (4 * VARIABLE_ENTRY_SIZE) ) ) {

            pass2 = FALSE;
	    failure_mssg2 = "unexpected size/len in flush op eviction test 1.";
	}
    }

    if ( pass2 ) {

	/* Now set up the pinning relationships: 
	 *
	 * Briefly, (VET, 0) is pinned by (VET, 1), (VET, 3), and (VET, 5)
	 *          (VET, 9) is pinned by (VET, 5), and (VET, 7)
	 */
	create_pinned_entry_dependency2(cache_ptr, VARIABLE_ENTRY_TYPE, 1,
			                VARIABLE_ENTRY_TYPE, 0);
	create_pinned_entry_dependency2(cache_ptr, VARIABLE_ENTRY_TYPE, 3,
			                VARIABLE_ENTRY_TYPE, 0);
	create_pinned_entry_dependency2(cache_ptr, VARIABLE_ENTRY_TYPE, 5,
			                VARIABLE_ENTRY_TYPE, 0);
	create_pinned_entry_dependency2(cache_ptr, VARIABLE_ENTRY_TYPE, 5,
			                VARIABLE_ENTRY_TYPE, 9);
	create_pinned_entry_dependency2(cache_ptr, VARIABLE_ENTRY_TYPE, 7,
			                VARIABLE_ENTRY_TYPE, 9);

	/* Next, set up the flush operations:
	 *
	 * Briefly, (VET, 1) dirties (VET, 0)
	 *                   resizes (VET, 0) to 3/4 VARIABLE_ENTRY_SIZE
	 *
	 *          (VET, 3) dirties (VET, 0)
	 *                   resizes (VET, 0) to VARIABLE_ENTRY_SIZE
	 *                   renames (VET, 0) to its alternate address
	 *
	 *          (VET, 5) dirties (VET, 0)
	 *                   resizes itself to VARIABLE_ENTRY_SIZE / 2
         *
         *          (VET, 7) dirties (VET, 9)
	 *
	 *          (VET, 9) dirties (VET, 8)
	 */

        add_flush_op2(VARIABLE_ENTRY_TYPE, 1, FLUSH_OP__DIRTY, 
                     VARIABLE_ENTRY_TYPE, 0, FALSE, (size_t)0);
        add_flush_op2(VARIABLE_ENTRY_TYPE, 1, FLUSH_OP__RESIZE, 
                     VARIABLE_ENTRY_TYPE, 0, FALSE, 
		     3 * VARIABLE_ENTRY_SIZE / 4);

        add_flush_op2(VARIABLE_ENTRY_TYPE, 3, FLUSH_OP__DIRTY, 
                     VARIABLE_ENTRY_TYPE, 0, FALSE, (size_t)0);
        add_flush_op2(VARIABLE_ENTRY_TYPE, 3, FLUSH_OP__RESIZE, 
                     VARIABLE_ENTRY_TYPE, 0, FALSE, VARIABLE_ENTRY_SIZE);
        add_flush_op2(VARIABLE_ENTRY_TYPE, 3, FLUSH_OP__RENAME, 
                     VARIABLE_ENTRY_TYPE, 0, FALSE, (size_t)0);

        add_flush_op2(VARIABLE_ENTRY_TYPE, 5, FLUSH_OP__DIRTY, 
                     VARIABLE_ENTRY_TYPE, 0, FALSE, (size_t)0);
        add_flush_op2(VARIABLE_ENTRY_TYPE, 5, FLUSH_OP__RESIZE, 
                     VARIABLE_ENTRY_TYPE, 5, FALSE, VARIABLE_ENTRY_SIZE / 2);

        add_flush_op2(VARIABLE_ENTRY_TYPE, 7, FLUSH_OP__DIRTY, 
                      VARIABLE_ENTRY_TYPE, 9, FALSE, (size_t)0);

        add_flush_op2(VARIABLE_ENTRY_TYPE, 9, FLUSH_OP__DIRTY, 
                     VARIABLE_ENTRY_TYPE, 8, FALSE, (size_t)0);
    }

    if ( pass2 ) {

	/* to summarize, at present the following variable size entries 
	 * are in cache with the following characteristics:
	 *
	 * 		in
	 * entry:	cache?	size:	dirty?	pinned?	pins:	flush operations:
	 * 
	 * (VET, 0) 	Y	2.5 KB	Y	Y	-	-
	 *
	 * (VET, 1)	Y	2.5 KB	Y	N	0	dirty (VET, 0), 
	 * 							resize (VET, 0) to 7.5 KB
	 * 
	 * (VET, 2) 	Y	10 KB	N	N	-	-
	 *
	 *
	 * (VET, 3)	Y	2.5 KB	N	N	0	dirty (VET, 0)
	 * 							resize (VET, 0) to 10 KB
	 * 							rename (VET, 0) to its alternate address
	 * 
	 * (VET, 4) 	Y	10 KB	N	N	-	-
	 *
	 *
	 * (VET, 5)	Y	2.5 KB	Y	N	0, 9	dirty (VET, 0)
	 * 							resize (VET, 5) to 5 KB
	 *
	 * (VET, 6)	Y	 5 KB	Y	N	-	-
	 *
	 * (VET, 7)	Y	 5 KB	Y	N	9	dirty (VET, 9)
	 *
	 * (VET, 8)	Y	10 KB	N	N	-	-
	 * 
	 * (VET, 9)	Y	10 KB	N	N	-	dirty (VET, 8)
	 *
	 * Recall that in this test bed, flush operations are excuted the 
	 * first time the associated entry is flushed, and are then 
	 * deleted.
	 */

        /* Now fill up the cache with other, unrelated entries */
	for ( i = 0; i < 31; i++ )
	{
	    protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i);
	    unprotect_entry_with_size_change2(cache_ptr, MONSTER_ENTRY_TYPE, i, 
			                     H5C2__DIRTIED_FLAG, (size_t)0);
	}

	for ( i = 0; i < 1; i++ )
	{
	    protect_entry2(cache_ptr, LARGE_ENTRY_TYPE, i);
	    unprotect_entry_with_size_change2(cache_ptr, LARGE_ENTRY_TYPE, i, 
			                     H5C2__DIRTIED_FLAG, (size_t)0);
	}

	/* The cache should now be exactly full */
	if ( ( cache_ptr->index_len != 42 ) ||
             ( cache_ptr->index_size != 2 * 1024 * 1024 ) ||
	     ( cache_ptr->index_size != ((4 * VARIABLE_ENTRY_SIZE / 4) +
					 (2 * VARIABLE_ENTRY_SIZE / 2) +
	                                 (4 * VARIABLE_ENTRY_SIZE) +
	                                 (31 * MONSTER_ENTRY_SIZE) +
					 (1 * LARGE_ENTRY_SIZE)) ) ) {

            pass2 = FALSE;
	    failure_mssg2 = "unexpected size/len in flush op eviction test 2.";

	} else {

	    /* verify the expected status of all entries we have loaded to date: */
            num_large_entries = 1;
	    verify_entry_status2(cache_ptr, 
			         0,
			         (num_variable_entries + num_monster_entries + num_large_entries), 
				 expected);
	}
    }


    if ( pass2 ) {

	/* Now load a large entry.  This should result in the eviction 
	 * of (VET,2), and the increase in the size of (VET, 0) from .25 
	 * VARIABLE_ENTRY_SIZE to .75 VARIABLE_ENTRY_SIZE.
	 *
	 * The following table illustrates the intended state of affairs
	 * after the eviction:
	 *
	 * 		in
	 * entry:	cache?	size:	dirty?	pinned?	pins:	flush operations:
	 * 
	 * (VET, 0) 	Y	7.5 KB	Y	Y	-	-
	 *
	 * (VET, 1)	Y	2.5 KB	N	N	-	-
	 *
	 * (VET, 2)	N	10 KB	N	N	-	-
	 *
	 * (VET, 3)	Y	2.5 KB	Y	N	0	dirty (VET, 0)
	 * 							resize (VET, 0) to 10 KB
	 * 							rename (VET, 0) to its alternate address
	 *
	 * (VET, 4)	Y	10 KB	N	N	-	-
	 *
	 * (VET, 5)	Y	2.5 KB	Y	N	0, 9	dirty (VET, 0)
	 * 							resize (VET, 5) to 5 KB
	 *
	 * (VET, 6)	Y	 5 KB	Y	N	-	-
	 *
	 * (VET, 7)	Y	 5 KB	Y	N	9	dirty (VET, 9)
	 *
	 * (VET, 8)	Y	10 KB	N	N	-	-
	 * 
	 * (VET, 9)	Y	10 KB	N	Y	-	dirty (VET, 8)
	 *
	 * Start by updating the expected table for the expected changes in entry status:
	 */
	expected[0].size       = 3 * VARIABLE_ENTRY_SIZE / 4;
	expected[1].is_dirty   = FALSE;
	expected[1].serialized = TRUE;
	expected[2].in_cache   = FALSE;
	expected[2].destroyed  = TRUE;

        num_large_entries = 2;

	protect_entry2(cache_ptr, LARGE_ENTRY_TYPE, 1);
	unprotect_entry_with_size_change2(cache_ptr, LARGE_ENTRY_TYPE, 1, 
                                         H5C2__DIRTIED_FLAG, (size_t)0);

	if ( ( cache_ptr->index_len != 42 ) ||
             ( cache_ptr->index_size != (2 * 1024 * 1024) -
	                                (VARIABLE_ENTRY_SIZE) +
	                                (VARIABLE_ENTRY_SIZE / 2) +
	                                (LARGE_ENTRY_SIZE) ) ||
	     ( cache_ptr->index_size != ((1 * (3 * VARIABLE_ENTRY_SIZE / 4)) +
					 (3 * VARIABLE_ENTRY_SIZE / 4) +
					 (2 * VARIABLE_ENTRY_SIZE / 2) +
	                                 (3 * VARIABLE_ENTRY_SIZE) +
	                                 (31 * MONSTER_ENTRY_SIZE) +
					 (2 * LARGE_ENTRY_SIZE)) ) ) {
            pass2 = FALSE;
	    failure_mssg2 = "unexpected size/len in flush op eviction test 3.";
	}

	/* verify entry status */
	verify_entry_status2(cache_ptr, 
			    1,
                            (num_variable_entries + num_monster_entries + num_large_entries), 
			    expected);
    }

    if ( pass2 ) {

	/* Now load another large entry.  This should result in the eviction 
	 * of (VET, 4), the increase in the size of (VET, 0) from .75
	 * VARIABLE_ENTRY_SIZE to 1.0 VARIABLE_ENTRY_SIZE, and the renaming
	 * of (VET, 0) to its alternate address.
	 *
	 * The following table shows the expected states of the variable 
	 * size entries after the test.
	 *
	 * 		in
	 * entry:	cache?	size:	dirty?	pinned?	pins:	flush operations:
	 * 
	 * (VET, 0) 	Y	10 KB	Y	Y	-	-
	 *
	 * (VET, 1)	Y	2.5 KB	N	N	-	-
	 *
	 * (VET, 2)	N	10 KB	N	N	-	-
	 *
	 * (VET, 3)	Y	2.5 KB	N	N	-	-
	 *
	 * (VET, 4)	N	10 KB	N	N	-	-
	 *
	 * (VET, 5)	Y	2.5 KB	Y	N	0, 9	dirty (VET, 0)
	 * 							resize (VET, 5) to 5 KB
	 *
	 * (VET, 6)	Y	 5 KB	Y	N	-	-
	 *
	 * (VET, 7)	Y	 5 KB	Y	N	9	dirty (VET, 9)
	 *
	 * (VET, 8)	Y	10 KB	N	N	-	-
	 * 
	 * (VET, 9)	Y	10 KB	N	Y	-	dirty (VET, 8)
	 *
	 * Start by updating the expected table for the expected changes in entry status:
	 */
	expected[0].size         = VARIABLE_ENTRY_SIZE;
	expected[0].at_main_addr = FALSE;
	expected[3].is_dirty     = FALSE;
	expected[3].serialized   = TRUE;
	expected[4].in_cache     = FALSE;
	expected[4].destroyed    = TRUE;

        num_large_entries = 3;

	protect_entry2(cache_ptr, LARGE_ENTRY_TYPE, 2);
	unprotect_entry_with_size_change2(cache_ptr, LARGE_ENTRY_TYPE, 2, 
                                         H5C2__DIRTIED_FLAG, (size_t)0);

	if ( ( cache_ptr->index_len != 42 ) ||
             ( cache_ptr->index_size != (2 * 1024 * 1024) -
	                                (2 * VARIABLE_ENTRY_SIZE) +
	                                (3 * VARIABLE_ENTRY_SIZE / 4) +
	                                (2 * LARGE_ENTRY_SIZE) ) ||
	     ( cache_ptr->index_size != ((3 * VARIABLE_ENTRY_SIZE / 4) +
					 (2 * VARIABLE_ENTRY_SIZE / 2) +
				         (3 * VARIABLE_ENTRY_SIZE) +
				         (31 * MONSTER_ENTRY_SIZE) + 
				         (3 * LARGE_ENTRY_SIZE)) ) ) {

            pass2 = FALSE;
	    failure_mssg2 = "unexpected size/len in flush op eviction test 4.";
	}

	/* verify entry status */
	verify_entry_status2(cache_ptr, 
			    2,
                            (num_variable_entries + num_monster_entries + num_large_entries), 
			    expected);
    }

    if ( pass2 ) {

	/* load two more large entries.  This should result in (VET, 5) being
	 * flushed, and increasing its size from 1/4 VARIABLE_ENTRY_SIZE to
	 * VARIABLE_ENTRY_SIZE.
	 *
	 * As a result of this size increase, the cache will have to look 
	 * for another entry to evict.  After flushing (VET, 6) and (VET, 7),
	 * it should evict (VET, 8), yielding the needed memory and dirtying
         * (VET, 9).
	 *
	 * The following table shows the expected states of the variable 
	 * size entries after the test.
	 *
	 * 		in
	 * entry:	cache?	size:	dirty?	pinned?	pins:	flush operations:
	 * 
	 * (VET, 0) 	Y	10 KB	Y	Y	-	-
	 *
	 * (VET, 1)	Y	2.5 KB	N	N	-	-
	 *
	 * (VET, 2)	N	10 KB	N	N	-	-
	 *
	 * (VET, 3)	Y	2.5 KB	N	N	-	-
	 *
	 * (VET, 4)	N	10 KB	N	N	-	-
	 *
	 * (VET, 5)	Y	 5 KB	N	N	0, 9	-
	 *
	 * (VET, 6)	Y	 5 KB	N	N	-	-
	 *
	 * (VET, 7)	Y	 5 KB	N	N	9	-
	 *
	 * (VET, 8)	N	10 KB	N	N	-	-
	 * 
	 * (VET, 9)	Y	10 KB	N	Y	-	dirty (VET, 8)
	 *
	 * Start by updating the expected table for the expected changes in entry status:
	 */

	expected[5].size         = VARIABLE_ENTRY_SIZE / 2;
	expected[5].is_dirty	 = FALSE;
	expected[5].serialized   = TRUE;
	expected[6].is_dirty	 = FALSE;
	expected[6].serialized	 = TRUE;
	expected[7].is_dirty	 = FALSE;
	expected[7].serialized	 = TRUE;
	expected[8].in_cache	 = FALSE;
        expected[8].destroyed	 = TRUE;
        expected[9].is_dirty	 = TRUE;

        num_large_entries = 5;

	protect_entry2(cache_ptr, LARGE_ENTRY_TYPE, 3);
	unprotect_entry_with_size_change2(cache_ptr, LARGE_ENTRY_TYPE, 3, 
                                         H5C2__DIRTIED_FLAG, (size_t)0);

	protect_entry2(cache_ptr, LARGE_ENTRY_TYPE, 4);
	unprotect_entry_with_size_change2(cache_ptr, LARGE_ENTRY_TYPE, 4, 
                                         H5C2__DIRTIED_FLAG, (size_t)0);

        /* verify cache size */
	if ( ( cache_ptr->index_len != 43 ) ||
             ( cache_ptr->index_size != (2 * 1024 * 1024) -
	                                (3 * VARIABLE_ENTRY_SIZE) +
	                                (1 * VARIABLE_ENTRY_SIZE / 4) +
	                                (3 * VARIABLE_ENTRY_SIZE / 4) +
	                                (4 * LARGE_ENTRY_SIZE) ) ||
	     ( cache_ptr->index_size != ((2 * VARIABLE_ENTRY_SIZE / 4) +
					 (3 * VARIABLE_ENTRY_SIZE / 2) +
				         (2 * VARIABLE_ENTRY_SIZE) +
				         (31 * MONSTER_ENTRY_SIZE) +
				         (5 * LARGE_ENTRY_SIZE)) ) ) {

            pass2 = FALSE;
	    failure_mssg2 = "unexpected size/len in flush op eviction test 5.";
	}

	/* verify entry status */
	verify_entry_status2(cache_ptr, 
			    3,
                            (num_variable_entries + num_monster_entries + num_large_entries), 
			    expected);
    }

    if ( pass2 ) {

        /* now touch all the non VARIABLE_ENTRY_TYPE entries in the
	 * cache to bring all the VARIABLE_ENTRY_TYPE entries to the 
	 * end of the LRU list.
	 *
	 * Note that we don't have to worry about (VET, 0) and (VET, 9)
	 * as they are pinned and thus not in the LRU list to begin with.
	 */
	for ( i = 0; i < 31; i++ )
	{
	    protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i);
	    unprotect_entry_with_size_change2(cache_ptr, MONSTER_ENTRY_TYPE, i, 
			                     H5C2__DIRTIED_FLAG, (size_t)0);
	}

	for ( i = 0; i < 5; i++ )
	{
	    protect_entry2(cache_ptr, LARGE_ENTRY_TYPE, i);
	    unprotect_entry_with_size_change2(cache_ptr, LARGE_ENTRY_TYPE, i, 
			                     H5C2__DIRTIED_FLAG, (size_t)0);
	}

        /* verify cache size */
	if ( ( cache_ptr->index_len != 43 ) ||
             ( cache_ptr->index_size != (2 * 1024 * 1024) -
	                                (3 * VARIABLE_ENTRY_SIZE) +
	                                (1 * VARIABLE_ENTRY_SIZE / 4) +
	                                (3 * VARIABLE_ENTRY_SIZE / 4) +
	                                (4 * LARGE_ENTRY_SIZE) ) ||
	     ( cache_ptr->index_size != ((2 * VARIABLE_ENTRY_SIZE / 4) +
					 (3 * VARIABLE_ENTRY_SIZE / 2) +
				         (2 * VARIABLE_ENTRY_SIZE) +
				         (31 * MONSTER_ENTRY_SIZE) +
				         (5 * LARGE_ENTRY_SIZE)) ) ) {

            pass2 = FALSE;
	    failure_mssg2 = "unexpected size/len in flush op eviction test 6.";
	}

	/* verify entry status */
	verify_entry_status2(cache_ptr, 
			    4,
                            (num_variable_entries + num_monster_entries + num_large_entries), 
			    expected);
    }

    if ( pass2 ) {

	/* Now load three more large entries.  This should result
	 * in the evictions of (VET, 1), (VET, 3), and (VET, 5), and the 
         * unpinning of (VET, 0)
	 *
	 * The following table shows the expected states of the variable 
	 * size entries after the test.
	 *
	 * 		in
	 * entry:	cache?	size:	dirty?	pinned?	pins:	flush operations:
	 * 
	 * (VET, 0) 	Y	10 KB	Y	N	-	-
	 *
	 * (VET, 1)	N	2.5 KB	N	N	-	-
	 *
	 * (VET, 2)	N	10 KB	N	N	-	-
	 *
	 * (VET, 3)	N	2.5 KB	N	N	-	-
	 *
	 * (VET, 4)	N	10 KB	N	N	-	-
	 *
	 * (VET, 5)	N	 5 KB	N	N	-	-
	 *
	 * (VET, 6)	Y	 5 KB	N	N	-	-
	 *
	 * (VET, 7)	Y	 5 KB	N	N	9	-
	 *
	 * (VET, 8)	N	10 KB	N	N	-	-
	 * 
	 * (VET, 9)	Y	10 KB	N	Y	-	dirty (VET, 8)
	 *
	 * Start by updating the expected table for the expected changes in entry status:
	 */

	expected[0].is_pinned    = FALSE;
	expected[1].in_cache     = FALSE;
	expected[1].destroyed    = TRUE;
	expected[3].in_cache     = FALSE;
	expected[3].destroyed    = TRUE;
	expected[5].in_cache     = FALSE;
	expected[5].destroyed    = TRUE;

        num_large_entries = 8;

	for ( i = 5; i < 8; i++ )
	{
	    protect_entry2(cache_ptr, LARGE_ENTRY_TYPE, i);
	    unprotect_entry_with_size_change2(cache_ptr, LARGE_ENTRY_TYPE, i, 
			                     H5C2__DIRTIED_FLAG, (size_t)0);
	}

        /* verify cache size */
	if ( ( cache_ptr->index_len != 43 ) ||
             ( cache_ptr->index_size != (2 * 1024 * 1024) -
	                                (4 * VARIABLE_ENTRY_SIZE) +
	                                (1 * VARIABLE_ENTRY_SIZE / 4) +
	                                (3 * VARIABLE_ENTRY_SIZE / 4) +
	                                (7 * LARGE_ENTRY_SIZE) ) ||
	     ( cache_ptr->index_size != ((2 * VARIABLE_ENTRY_SIZE / 2) +
					 (2 * VARIABLE_ENTRY_SIZE) +
					 (31 * MONSTER_ENTRY_SIZE) +
					 (8 * LARGE_ENTRY_SIZE)) ) ) {

            pass2 = FALSE;
	    failure_mssg2 = "unexpected size/len in flush op eviction test 7.";
	}

	/* verify entry status */
	verify_entry_status2(cache_ptr, 
			    5,
                            (num_variable_entries + num_monster_entries + num_large_entries), 
			    expected);
    }

    if ( pass2 ) {

	/* load another large entry.  (VET, 6) should be evicted.
	 *
	 * The following table shows the expected states of the variable 
	 * size entries after the test.
	 *
	 * 		in
	 * entry:	cache?	size:	dirty?	pinned?	pins:	flush operations:
	 * 
	 * (VET, 0) 	Y	10 KB	Y	N	-	-
	 *
	 * (VET, 1)	N	2.5 KB	N	N	-	-
	 *
	 * (VET, 2)	N	10 KB	N	N	-	-
	 *
	 * (VET, 3)	N	2.5 KB	N	N	-	-
	 *
	 * (VET, 4)	N	10 KB	N	N	-	-
	 *
	 * (VET, 5)	N	 5 KB	N	N	-	-
	 *
	 * (VET, 6)	N	 5 KB	N	N	-	-
	 *
	 * (VET, 7)	Y	 5 KB	N	N	9	-
	 *
	 * (VET, 8)	N	10 KB	N	N	-	-
	 * 
	 * (VET, 9)	Y	10 KB	N	Y	-	dirty (VET, 8)
	 *
	 * Start by updating the expected table for the expected changes in entry status:
	 */

	expected[6].in_cache     = FALSE;
	expected[6].destroyed    = TRUE;

        num_large_entries = 9;

	for ( i = 8; i < 9; i++ )
	{
	    protect_entry2(cache_ptr, LARGE_ENTRY_TYPE, i);
	    unprotect_entry_with_size_change2(cache_ptr, LARGE_ENTRY_TYPE, i, 
			                     H5C2__DIRTIED_FLAG, (size_t)0);
	}

        /* verify cache size */
	if ( ( cache_ptr->index_len != 43 ) ||
             ( cache_ptr->index_size != (2 * 1024 * 1024) -
	                                (3 * VARIABLE_ENTRY_SIZE) -
					(VARIABLE_ENTRY_SIZE / 2) +
	                                (8 * LARGE_ENTRY_SIZE) ) ||
	     ( cache_ptr->index_size != ((1 * VARIABLE_ENTRY_SIZE / 2) +
				         (2 * VARIABLE_ENTRY_SIZE) +
				         (31 * MONSTER_ENTRY_SIZE) +
				         (9 * LARGE_ENTRY_SIZE)) ) ) {

            pass2 = FALSE;
	    failure_mssg2 = "unexpected size/len in flush op eviction test 8.";
	}

	/* verify entry status */
	verify_entry_status2(cache_ptr, 
			    6,
                            (num_variable_entries + num_monster_entries + num_large_entries), 
			    expected);
    }

    if ( pass2 ) {

	/* Load another large entry.  
	 *
	 * (VET, 7) should be evicted, and (VET, 9) should be unpinned. 
	 *
	 * The following table shows the expected states of the variable 
	 * size entries after the test.
	 *
	 * 		in
	 * entry:	cache?	size:	dirty?	pinned?	pins:	flush operations:
	 * 
	 * (VET, 0) 	Y	10 KB	Y	N	-	-
	 *
	 * (VET, 1)	N	2.5 KB	N	N	-	-
	 *
	 * (VET, 2)	N	10 KB	N	N	-	-
	 *
	 * (VET, 3)	N	2.5 KB	N	N	-	-
	 *
	 * (VET, 4)	N	10 KB	N	N	-	-
	 *
	 * (VET, 5)	N	 5 KB	N	N	-	-
	 *
	 * (VET, 6)	N	 5 KB	N	N	-	-
	 *
	 * (VET, 7)	N	 5 KB	N	N	-	-
	 *
	 * (VET, 8)	N	10 KB	N	N	-	-
	 * 
	 * (VET, 9)	Y	10 KB	Y	N	-	dirty (VET, 8)
	 *
	 * Start by updating the expected table for the expected changes in entry status:
	 */

	expected[7].in_cache     = FALSE;
	expected[7].destroyed    = TRUE;
	expected[9].is_pinned    = FALSE;

        num_large_entries = 10;

	for ( i = 9; i < 10; i++ )
	{
	    protect_entry2(cache_ptr, LARGE_ENTRY_TYPE, i);
	    unprotect_entry_with_size_change2(cache_ptr, LARGE_ENTRY_TYPE, i, 
			                     H5C2__DIRTIED_FLAG, (size_t)0);
	}

        /* verify cache size */
	if ( ( cache_ptr->index_len != 43 ) ||
             ( cache_ptr->index_size != (2 * 1024 * 1024) -
	                                (4 * VARIABLE_ENTRY_SIZE) +
	                                (9 * LARGE_ENTRY_SIZE) ) ||
	     ( cache_ptr->index_size != ((2 * VARIABLE_ENTRY_SIZE) +
				         (31 * MONSTER_ENTRY_SIZE) +
				         (10 * LARGE_ENTRY_SIZE)) ) ) {

            pass2 = FALSE;
	    failure_mssg2 = "unexpected size/len in flush op eviction test 9.";
	}

	/* verify entry status */
	verify_entry_status2(cache_ptr, 
			    7,
                            (num_variable_entries + num_monster_entries + num_large_entries), 
			    expected);
    }

    if ( pass2 ) {

        /* Again, touch all the non VARIABLE_ENTRY_TYPE entries in the
	 * cache to bring all the VARIABLE_ENTRY_TYPE entries to the 
	 * end of the LRU list.
	 *
	 * Both (VET, 0) and (VET, 7) have been unpinned, so they are
	 * now in the LRU list.
	 */
	for ( i = 0; i < 31; i++ )
	{
	    protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i);
	    unprotect_entry_with_size_change2(cache_ptr, MONSTER_ENTRY_TYPE, i, 
			                     H5C2__DIRTIED_FLAG, (size_t)0);
	}

	for ( i = 0; i < 10; i++ )
	{
	    protect_entry2(cache_ptr, LARGE_ENTRY_TYPE, i);
	    unprotect_entry_with_size_change2(cache_ptr, LARGE_ENTRY_TYPE, i, 
			                     H5C2__DIRTIED_FLAG, (size_t)0);
	}

        /* verify cache size */
	if ( ( cache_ptr->index_len != 43 ) ||
             ( cache_ptr->index_size != (2 * 1024 * 1024) -
	                                (4 * VARIABLE_ENTRY_SIZE) +
	                                (9 * LARGE_ENTRY_SIZE) ) ||
	     ( cache_ptr->index_size != ((2 * VARIABLE_ENTRY_SIZE) +
				         (31 * MONSTER_ENTRY_SIZE) +
				         (10 * LARGE_ENTRY_SIZE)) ) ) {

            pass2 = FALSE;
	    failure_mssg2 = "unexpected size/len in flush op eviction test 10.";
	}

	/* verify entry status */
	verify_entry_status2(cache_ptr, 
			    8,
                            (num_variable_entries + num_monster_entries + num_large_entries), 
			    expected);
    }

    if ( pass2 ) {

	/* load two more large entries.  Things get a bit complicated here, 
	 * so I'll go through the operation step by step.
         *
         * Initially, the cache has 4 KB of empty space, so the first entry 
	 * (LET, 10) is loaded via calls to H5C2_protect() H5C2_unprotect() 
	 * without causing any evictions.
         *
         * However, this is not the case for the call of H5C2_protect() on 
	 * (LET, 11).
         *
         * Before inserting (LET, 11), H5C2_protect(LET, 11) must try to 
	 * free up at least 4  KB of space.  To do this, it starts scanning 
	 * up the LRU list to find entries to evict.
         *
         * (VET, 0) is at the bottom of the LRU list, and thus is the first 
	 * entry considered.  However, it is dirty, so it is moved to the 
	 * top of the LRU list, flushed to disk, and marked clean.
         *
         * (VET, 9) is the next entry on the bottom of the LRU list. It is 
	 * dirty too, so the cache moves it to the top of the LRU list, 
	 * and calls its serialize callback function to construct an on 
	 * disk image of the entry.
         *
         * However, this serialize function needs to modify (VET, 8), which 
	 * is currently not in cache.  Thus it calls H5C2_protect(VET, 8) 
	 * to gain access to it.  H5C2_protect(VET, 8) loads (VET, 8), and 
	 * then attempts to evict entries to make space for it.  (VET, 9) 
	 * has already been moved to the head of the LRU list, so the next 
	 * entries on the LRU are (MET, 0) thru (MET, 30) and (LET, 0) thru 
	 * (LET, 10) -- all of which are dirty, and are therefore flushed 
	 * and moved to the head of the LRU list.
         *
         * The next entry on the bottom of the LRU list is (VET, 0), which 
	 * is clean, and is therefore evicted to make space for (VET, 8).  
	 * This space is sufficient, so H5C2_protect(VET, 8) inserts 
	 * (VET, 8) into the cache's index, marks it as protected, and 
	 * returns to the serialize function for (VET, 9).
         *
         * When the serialize function for (VET, 9) is done with (VET, 8), it 
         * calls H5C2_unprotect(VET, 8), which markes (VET, 8) as dirty and 
         * unprotected, and places it at the head of the LRU.
         *
         * The serialize function for (VET, 9) then returns, and (VET, 9) is 
         * is written to disk, and marked clean.  
         * 
         * At this point, the cache is still full (since (VET, 8) took the 
	 * space created by the eviction of (VET, 0)). Thus 
	 * H5C2_protect(LET, 11) continues to look for space.  While 
	 * (MET, 0) was the next item on the LRU list when it called the
	 * serialize function for (VET, 9), the function notices that the
	 * LRU has been modified, and restarts its search for candidates
	 * for eviction at the bottom of the LRU.
	 *
	 * (VET, 0) is now at the bottom of the LRU, and is clean.  Thus
	 * it is evicted.  This makes sufficient space for (LET, 11), so
	 * H5C2_protect(LET, 11) inserts it into the cache, marks it as 
	 * protected, and returns.  
	 *
	 * H5C2_unprotect(VET, 11) marks (VET, 11) as unprotected, and then
	 * returns as well.
	 *
	 * The following table shows the expected states of the variable 
	 * size entries after the test.
	 *
	 * 		in
	 * entry:	cache?	size:	dirty?	pinned?	pins:	flush operations:
	 * 
	 * (VET, 0) 	N	10 KB	N	N	-	-
	 *
	 * (VET, 1)	N	2.5 KB	N	N	-	-
	 *
	 * (VET, 2)	N	10 KB	N	N	-	-
	 *
	 * (VET, 3)	N	2.5 KB	N	N	-	-
	 *
	 * (VET, 4)	N	10 KB	N	N	-	-
	 *
	 * (VET, 5)	N	 5 KB	N	N	-	-
	 *
	 * (VET, 6)	N	 5 KB	N	N	-	-
	 *
	 * (VET, 7)	N	 5 KB	N	N	-	-
	 *
	 * (VET, 8)	Y	10 KB	Y	N	-	-
	 * 
	 * (VET, 9)	N	10 KB	N	N	-	-
	 *
	 * Start by updating the expected table for the expected changes in 
	 * entry status:
	 *
	 * Note that we reset the loaded, cleared, flushed, and destroyed 
	 * fields of (VET,8) so we can track what is happening.
	 */
	base_addr = entries2[VARIABLE_ENTRY_TYPE];
	entry_ptr = &(base_addr[8]);
	entry_ptr->deserialized = FALSE;
	entry_ptr->cleared      = FALSE;
	entry_ptr->deserialized = FALSE;
	entry_ptr->destroyed    = FALSE;

	expected[0].in_cache     = FALSE;
	expected[0].is_dirty     = FALSE;
	expected[0].serialized   = TRUE;
	expected[0].destroyed    = TRUE;
	expected[8].in_cache	 = TRUE;
	expected[8].is_dirty     = TRUE;
	expected[8].deserialized = TRUE;
	expected[8].serialized   = FALSE;
	expected[8].destroyed    = FALSE;
	expected[9].in_cache     = FALSE;
	expected[9].is_dirty     = FALSE;
	expected[9].serialized   = TRUE;
	expected[9].destroyed    = TRUE;

        num_large_entries = 12;

	/* a newly loaded entry is not inserted in the cache until after 
	 * space has been made for it.  Thus (LET, 11) will not be flushed.
	 */
	for ( i = num_variable_entries; 
	      i < num_variable_entries + num_monster_entries + num_large_entries - 1;
	      i++ )
	{
            expected[i].is_dirty   = FALSE;
	    expected[i].serialized = TRUE;
	}

	for ( i = 10; i < 12; i++ )
	{
	    protect_entry2(cache_ptr, LARGE_ENTRY_TYPE, i);
	    unprotect_entry_with_size_change2(cache_ptr, LARGE_ENTRY_TYPE, i, 
			                     H5C2__DIRTIED_FLAG, (size_t)0);
	}

        /* verify cache size  */
	if ( ( cache_ptr->index_len != 44 ) ||
             ( cache_ptr->index_size != (2 * 1024 * 1024) -
	                                (5 * VARIABLE_ENTRY_SIZE) +
	                                (11 * LARGE_ENTRY_SIZE) ) ||
	     ( cache_ptr->index_size != ((1 * VARIABLE_ENTRY_SIZE) +
					 (31 * MONSTER_ENTRY_SIZE) +
					 (12 * LARGE_ENTRY_SIZE)) ) ) {

            pass2 = FALSE;
	    failure_mssg2 = "unexpected size/len in flush op eviction test 11.";
	}

	/* verify entry status */
	verify_entry_status2(cache_ptr, 
			    9,
                            (num_variable_entries + num_monster_entries + num_large_entries), 
			    expected);
    }

    if ( pass2 ) {

        /* Again, touch all the non VARIABLE_ENTRY_TYPE entries in the
	 * cache to bring the last remaining VARIABLE_ENTRY_TYPE entry to the 
	 * end of the LRU list. 
	 */
	for ( i = 0; i < num_monster_entries; i++ )
	{
	    protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i);
	    unprotect_entry_with_size_change2(cache_ptr, MONSTER_ENTRY_TYPE, i, 
			                     H5C2__DIRTIED_FLAG, (size_t)0);
	}

	for ( i = 0; i < num_large_entries; i++ )
	{
	    protect_entry2(cache_ptr, LARGE_ENTRY_TYPE, i);
	    unprotect_entry_with_size_change2(cache_ptr, LARGE_ENTRY_TYPE, i, 
			                     H5C2__DIRTIED_FLAG, (size_t)0);
	}

	/* update the expected array to mark all these entries dirty again. */
	for ( i = num_variable_entries; 
	      i < num_variable_entries + num_monster_entries + num_large_entries - 1;
	      i++ )
	{
            expected[i].is_dirty = TRUE;
	}

        /* verify cache size */
	if ( ( cache_ptr->index_len != 44 ) ||
             ( cache_ptr->index_size != (2 * 1024 * 1024) -
	                                (5 * VARIABLE_ENTRY_SIZE) +
	                                (11 * LARGE_ENTRY_SIZE) ) ||
	     ( cache_ptr->index_size != ((1 * VARIABLE_ENTRY_SIZE) +
					 (31 * MONSTER_ENTRY_SIZE) +
					 (12 * LARGE_ENTRY_SIZE)) ) ) {

            pass2 = FALSE;
	    failure_mssg2 = "unexpected size/len in flush op eviction test 12.";
	}

	/* verify entry status */
	verify_entry_status2(cache_ptr, 
			    10,
                            (num_variable_entries + num_monster_entries + num_large_entries), 
			    expected);
    }

    if ( pass2 ) {
	    
        /* Load two more large entries.  
	 *
	 * Since (VET, 8) is dirty, at first this will just cause (VET, 8) 
	 * to be flushed.
	 *
	 * But all other entries in the cache are dirty, so the cache will 
	 * flush them all, and then evict (VET, 8) on the second pass.
	 *
	 * The following table shows the expected states of the variable 
	 * size entries after the test.
	 *
	 * 		in
	 * entry:	cache?	size:	dirty?	pinned?	pins:	flush operations:
	 * 
	 * (VET, 0) 	N	10 KB	N	N	-	-
	 *
	 * (VET, 1)	N	2.5 KB	N	N	-	-
	 *
	 * (VET, 2)	N	10 KB	N	N	-	-
	 *
	 * (VET, 3)	N	2.5 KB	N	N	-	-
	 *
	 * (VET, 4)	N	10 KB	N	N	-	-
	 *
	 * (VET, 5)	N	 5 KB	N	N	-	-
	 *
	 * (VET, 6)	N	 5 KB	N	N	-	-
	 *
	 * (VET, 7)	N	 5 KB	N	N	-	-
	 *
	 * (VET, 8)	N	10 KB	N	N	-	-
	 * 
	 * (VET, 9)	N	10 KB	N	N	-	-
	 *
	 * Start by updating the expected table for the expected changes in 
	 * entry status:
	 */

	expected[8].in_cache	 = FALSE;
	expected[8].is_dirty     = FALSE;
	expected[8].serialized   = TRUE;
	expected[8].destroyed    = TRUE;

        num_large_entries = 14;

	/* a newly loaded entry is not inserted in the cache until after 
	 * space has been made for it.  Thus (LET, 13) will not be flushed.
	 */
	for ( i = num_variable_entries; 
	      i < num_variable_entries + num_monster_entries + num_large_entries - 1;
	      i++ )
	{
            expected[i].is_dirty   = FALSE;
	    expected[i].serialized = TRUE;
	}

	for ( i = 12; i < 14; i++ )
	{
	    protect_entry2(cache_ptr, LARGE_ENTRY_TYPE, i);
	    unprotect_entry_with_size_change2(cache_ptr, LARGE_ENTRY_TYPE, i, 
			                     H5C2__DIRTIED_FLAG, (size_t)0);
	}

        /* verify cache size  */
	if ( ( cache_ptr->index_len != 45 ) ||
             ( cache_ptr->index_size != (2 * 1024 * 1024) -
	                                (6 * VARIABLE_ENTRY_SIZE) +
	                                (13 * LARGE_ENTRY_SIZE) ) ||
	     ( cache_ptr->index_size != ((31 * MONSTER_ENTRY_SIZE) +
					 (14 * LARGE_ENTRY_SIZE)) ) ) {

            pass2 = FALSE;
	    failure_mssg2 = "unexpected size/len in flush op eviction test 13.";
	}

	/* verify entry status */
	verify_entry_status2(cache_ptr, 
			    11,
                            (num_variable_entries + num_monster_entries + num_large_entries), 
			    expected);
    }

    /* at this point we have cycled all the variable size entries through 
     * the cache.
     *
     * flush the cache and end the test.
     */

    if ( pass2 ) {

        result = H5C2_flush_cache(cache_ptr, H5P_DATASET_XFER_DEFAULT,
                                  H5C2__FLUSH_INVALIDATE_FLAG);

        if ( result < 0 ) {

            pass2 = FALSE;
            failure_mssg2 = "Cache flush invalidate failed after flush op eviction test";
        }
        else if ( ( cache_ptr->index_len != 0 ) ||
                  ( cache_ptr->index_size != 0 ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache len/size after cleanup of flush op eviction test";

        }
    }

#if H5C2_COLLECT_CACHE_STATS 
    /* If we are collecting stats, check to see if we get the expected
     * values.
     *
     * Testing the stats code is fairly new, but given the extent
     * to which I find myself depending on the stats, I've decided 
     * to start testing the stats whenever it is convenient to do
     * so.
     */

    if ( pass2 ) {

	if ( ( cache_ptr->insertions[VARIABLE_ENTRY_TYPE] != 0 ) ||
             ( cache_ptr->pinned_insertions[VARIABLE_ENTRY_TYPE] != 0 ) ||
             ( cache_ptr->clears[VARIABLE_ENTRY_TYPE] != 0 ) ||
             ( cache_ptr->flushes[VARIABLE_ENTRY_TYPE] != 19 ) ||
             ( cache_ptr->evictions[VARIABLE_ENTRY_TYPE] != 11 ) ||
             ( cache_ptr->renames[VARIABLE_ENTRY_TYPE] != 1 ) ||
             ( cache_ptr->entry_flush_renames[VARIABLE_ENTRY_TYPE] != 0 ) ||
             ( cache_ptr->cache_flush_renames[VARIABLE_ENTRY_TYPE] != 0 ) ||
             ( cache_ptr->pins[VARIABLE_ENTRY_TYPE] != 2 ) ||
             ( cache_ptr->unpins[VARIABLE_ENTRY_TYPE] != 2 ) ||
             ( cache_ptr->dirty_pins[VARIABLE_ENTRY_TYPE] != 0 ) ||
             ( cache_ptr->pinned_flushes[VARIABLE_ENTRY_TYPE] != 0 ) ||
             ( cache_ptr->pinned_clears[VARIABLE_ENTRY_TYPE] != 0 ) ||
             ( cache_ptr->size_increases[VARIABLE_ENTRY_TYPE] != 3 ) ||
             ( cache_ptr->size_decreases[VARIABLE_ENTRY_TYPE] != 6 ) ||
             ( cache_ptr->entry_flush_size_changes[VARIABLE_ENTRY_TYPE] != 1 ) ||
             ( cache_ptr->cache_flush_size_changes[VARIABLE_ENTRY_TYPE] != 0 ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected variable size entry stats.";
        }
    }

    if ( pass2 ) {

	if ( ( cache_ptr->insertions[LARGE_ENTRY_TYPE] != 0 ) ||
             ( cache_ptr->pinned_insertions[LARGE_ENTRY_TYPE] != 0 ) ||
             ( cache_ptr->clears[LARGE_ENTRY_TYPE] != 0 ) ||
             ( cache_ptr->flushes[LARGE_ENTRY_TYPE] != 38 ) ||
             ( cache_ptr->evictions[LARGE_ENTRY_TYPE] != 14 ) ||
             ( cache_ptr->renames[LARGE_ENTRY_TYPE] != 0 ) ||
             ( cache_ptr->entry_flush_renames[LARGE_ENTRY_TYPE] != 0 ) ||
             ( cache_ptr->cache_flush_renames[LARGE_ENTRY_TYPE] != 0 ) ||
             ( cache_ptr->pins[LARGE_ENTRY_TYPE] != 0 ) ||
             ( cache_ptr->unpins[LARGE_ENTRY_TYPE] != 0 ) ||
             ( cache_ptr->dirty_pins[LARGE_ENTRY_TYPE] != 0 ) ||
             ( cache_ptr->pinned_flushes[LARGE_ENTRY_TYPE] != 0 ) ||
             ( cache_ptr->pinned_clears[LARGE_ENTRY_TYPE] != 0 ) ||
             ( cache_ptr->size_increases[LARGE_ENTRY_TYPE] != 0 ) ||
             ( cache_ptr->size_decreases[LARGE_ENTRY_TYPE] != 0 ) ||
             ( cache_ptr->entry_flush_size_changes[LARGE_ENTRY_TYPE] != 0 ) ||
             ( cache_ptr->cache_flush_size_changes[LARGE_ENTRY_TYPE] != 0 ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected monster entry stats.";
        }
    }

    if ( pass2 ) {

	if ( ( cache_ptr->insertions[MONSTER_ENTRY_TYPE] != 0 ) ||
             ( cache_ptr->pinned_insertions[MONSTER_ENTRY_TYPE] != 0 ) ||
             ( cache_ptr->clears[MONSTER_ENTRY_TYPE] != 0 ) ||
             ( cache_ptr->flushes[MONSTER_ENTRY_TYPE] != 93 ) ||
             ( cache_ptr->evictions[MONSTER_ENTRY_TYPE] != 31 ) ||
             ( cache_ptr->renames[MONSTER_ENTRY_TYPE] != 0 ) ||
             ( cache_ptr->entry_flush_renames[MONSTER_ENTRY_TYPE] != 0 ) ||
             ( cache_ptr->cache_flush_renames[MONSTER_ENTRY_TYPE] != 0 ) ||
             ( cache_ptr->pins[MONSTER_ENTRY_TYPE] != 0 ) ||
             ( cache_ptr->unpins[MONSTER_ENTRY_TYPE] != 0 ) ||
             ( cache_ptr->dirty_pins[MONSTER_ENTRY_TYPE] != 0 ) ||
             ( cache_ptr->pinned_flushes[MONSTER_ENTRY_TYPE] != 0 ) ||
             ( cache_ptr->pinned_clears[MONSTER_ENTRY_TYPE] != 0 ) ||
             ( cache_ptr->size_increases[MONSTER_ENTRY_TYPE] != 0 ) ||
             ( cache_ptr->size_decreases[MONSTER_ENTRY_TYPE] != 0 ) ||
             ( cache_ptr->entry_flush_size_changes[MONSTER_ENTRY_TYPE] != 0 ) ||
             ( cache_ptr->cache_flush_size_changes[MONSTER_ENTRY_TYPE] != 0 ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected monster entry stats.";
        }
    }
#endif /* H5C2_COLLECT_CACHE_STATS */

    if ( pass2 ) {

	reset_entries2();
    }

    return;

} /* check_flush_cache__flush_op_eviction_test() */


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
 * 		the new H5C2_mark_pinned_or_protected_entry_dirty()
 * 		call.
 *
 *-------------------------------------------------------------------------
 */

static void
check_flush_cache__single_entry(H5C2_t * cache_ptr)
{
    /* const char * fcn_name = "check_flush_cache__single_entry"; */

    if ( cache_ptr == NULL ) {

        pass2 = FALSE;
        failure_mssg2 = "cache_ptr NULL on entry to single entry case.";
    }
    else if ( ( cache_ptr->index_len != 0 ) ||
              ( cache_ptr->index_size != 0 ) ) {

        pass2 = FALSE;
        failure_mssg2 = "cache not empty at beginning of single entry case.";
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 1,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ FALSE,
            /* dirty_flag            */ FALSE,
            /* flags                 */ H5C2__NO_FLAGS_SET,
            /* flush_flags           */ H5C2__NO_FLAGS_SET,
            /* expected_deserialized */ TRUE,
            /* expected_cleared      */ FALSE,
            /* expected_serialized   */ FALSE,
            /* expected_destroyed    */ FALSE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 2,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ FALSE,
            /* dirty_flag            */ TRUE,
            /* flags                 */ H5C2__NO_FLAGS_SET,
            /* flush_flags           */ H5C2__NO_FLAGS_SET,
            /* expected_deserialized */ TRUE,
            /* expected_cleared      */ FALSE,
            /* expected_serialized   */ TRUE,
            /* expected_destroyed    */ FALSE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 3,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ FALSE,
            /* dirty_flag            */ FALSE,
            /* flags                 */ H5C2__NO_FLAGS_SET,
            /* flush_flags           */ H5C2__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_deserialized */ TRUE,
            /* expected_cleared      */ FALSE,
            /* expected_serialized   */ FALSE,
            /* expected_destroyed    */ FALSE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 4,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ FALSE,
            /* dirty_flag            */ TRUE,
            /* flags                 */ H5C2__NO_FLAGS_SET,
            /* flush_flags           */ H5C2__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_deserialized */ TRUE,
            /* expected_cleared      */ TRUE,
            /* expected_serialized   */ FALSE,
            /* expected_destroyed    */ FALSE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 5,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ FALSE,
            /* dirty_flag            */ FALSE,
            /* flags                 */ H5C2__NO_FLAGS_SET,
            /* flush_flags           */ H5C2__FLUSH_INVALIDATE_FLAG,
            /* expected_deserialized */ TRUE,
            /* expected_cleared      */ FALSE,
            /* expected_serialized   */ FALSE,
            /* expected_destroyed    */ TRUE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 6,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ FALSE,
            /* dirty_flag            */ TRUE,
            /* flags                 */ H5C2__NO_FLAGS_SET,
            /* flush_flags           */ H5C2__FLUSH_INVALIDATE_FLAG,
            /* expected_deserialized */ TRUE,
            /* expected_cleared      */ FALSE,
            /* expected_serialized   */ TRUE,
            /* expected_destroyed    */ TRUE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 7,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ FALSE,
            /* dirty_flag            */ FALSE,
            /* flags                 */ H5C2__NO_FLAGS_SET,
            /* flush_flags           */ H5C2__FLUSH_MARKED_ENTRIES_FLAG,
            /* expected_deserialized */ TRUE,
            /* expected_cleared      */ FALSE,
            /* expected_serialized   */ FALSE,
            /* expected_destroyed    */ FALSE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 8,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ FALSE,
            /* dirty_flag            */ TRUE,
            /* flags                 */ H5C2__NO_FLAGS_SET,
            /* flush_flags           */ H5C2__FLUSH_MARKED_ENTRIES_FLAG,
            /* expected_deserialized */ TRUE,
            /* expected_cleared      */ FALSE,
            /* expected_serialized   */ FALSE,
            /* expected_destroyed    */ FALSE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 9,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ FALSE,
            /* dirty_flag            */ FALSE,
            /* flags                 */ H5C2__NO_FLAGS_SET,
            /* flush_flags           */ H5C2__FLUSH_INVALIDATE_FLAG |
                                        H5C2__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_deserialized */ TRUE,
            /* expected_cleared      */ FALSE,
            /* expected_serialized   */ FALSE,
            /* expected_destroyed    */ TRUE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 10,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ FALSE,
            /* dirty_flag            */ TRUE,
            /* flags                 */ H5C2__NO_FLAGS_SET,
            /* flush_flags           */ H5C2__FLUSH_INVALIDATE_FLAG |
                                        H5C2__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_deserialized */ TRUE,
            /* expected_cleared      */ TRUE,
            /* expected_serialized   */ FALSE,
            /* expected_destroyed    */ TRUE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 11,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ FALSE,
            /* dirty_flag            */ FALSE,
            /* flags                 */ H5C2__NO_FLAGS_SET,
            /* flush_flags           */ H5C2__FLUSH_MARKED_ENTRIES_FLAG |
                                        H5C2__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_deserialized */ TRUE,
            /* expected_cleared      */ FALSE,
            /* expected_serialized   */ FALSE,
            /* expected_destroyed    */ FALSE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 12,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ FALSE,
            /* dirty_flag            */ TRUE,
            /* flags                 */ H5C2__NO_FLAGS_SET,
            /* flush_flags           */ H5C2__FLUSH_MARKED_ENTRIES_FLAG |
                                        H5C2__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_deserialized */ TRUE,
            /* expected_cleared      */ FALSE,
            /* expected_serialized   */ FALSE,
            /* expected_destroyed    */ FALSE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 13,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ FALSE,
            /* dirty_flag            */ FALSE,
            /* flags                 */ H5C2__NO_FLAGS_SET,
            /* flush_flags           */ H5C2__FLUSH_MARKED_ENTRIES_FLAG |
                                        H5C2__FLUSH_INVALIDATE_FLAG,
            /* expected_deserialized */ TRUE,
            /* expected_cleared      */ FALSE,
            /* expected_serialized   */ FALSE,
            /* expected_destroyed    */ TRUE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 14,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ FALSE,
            /* dirty_flag            */ TRUE,
            /* flags                 */ H5C2__NO_FLAGS_SET,
            /* flush_flags           */ H5C2__FLUSH_MARKED_ENTRIES_FLAG |
                                        H5C2__FLUSH_INVALIDATE_FLAG,
            /* expected_deserialized */ TRUE,
            /* expected_cleared      */ FALSE,
            /* expected_serialized   */ TRUE,
            /* expected_destroyed    */ TRUE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 15,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ FALSE,
            /* dirty_flag            */ FALSE,
            /* flags                 */ H5C2__NO_FLAGS_SET,
            /* flush_flags           */ H5C2__FLUSH_INVALIDATE_FLAG |
                                        H5C2__FLUSH_CLEAR_ONLY_FLAG |
                                        H5C2__FLUSH_MARKED_ENTRIES_FLAG,
            /* expected_deserialized */ TRUE,
            /* expected_cleared      */ FALSE,
            /* expected_serialized   */ FALSE,
            /* expected_destroyed    */ TRUE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 16,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ FALSE,
            /* dirty_flag            */ TRUE,
            /* flags                 */ H5C2__NO_FLAGS_SET,
            /* flush_flags           */ H5C2__FLUSH_INVALIDATE_FLAG |
                                        H5C2__FLUSH_CLEAR_ONLY_FLAG |
                                        H5C2__FLUSH_MARKED_ENTRIES_FLAG,
            /* expected_deserialized */ TRUE,
            /* expected_cleared      */ TRUE,
            /* expected_serialized   */ FALSE,
            /* expected_destroyed    */ TRUE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 17,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ FALSE,
            /* dirty_flag            */ FALSE,
            /* flags                 */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* flush_flags           */ H5C2__NO_FLAGS_SET,
            /* expected_deserialized */ TRUE,
            /* expected_cleared      */ FALSE,
            /* expected_serialized   */ FALSE,
            /* expected_destroyed    */ FALSE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 18,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ FALSE,
            /* dirty_flag            */ TRUE,
            /* flags                 */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* flush_flags           */ H5C2__NO_FLAGS_SET,
            /* expected_deserialized */ TRUE,
            /* expected_cleared      */ FALSE,
            /* expected_serialized   */ TRUE,
            /* expected_destroyed    */ FALSE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 19,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ FALSE,
            /* dirty_flag            */ FALSE,
            /* flags                 */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* flush_flags           */ H5C2__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_deserialized */ TRUE,
            /* expected_cleared      */ FALSE,
            /* expected_serialized   */ FALSE,
            /* expected_destroyed    */ FALSE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 20,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ FALSE,
            /* dirty_flag            */ TRUE,
            /* flags                 */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* flush_flags           */ H5C2__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_deserialized */ TRUE,
            /* expected_cleared      */ TRUE,
            /* expected_serialized   */ FALSE,
            /* expected_destroyed    */ FALSE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 21,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ FALSE,
            /* dirty_flag            */ FALSE,
            /* flags                 */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* flush_flags           */ H5C2__FLUSH_INVALIDATE_FLAG,
            /* expected_deserialized */ TRUE,
            /* expected_cleared      */ FALSE,
            /* expected_serialized   */ FALSE,
            /* expected_destroyed    */ TRUE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 22,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ FALSE,
            /* dirty_flag            */ TRUE,
            /* flags                 */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* flush_flags           */ H5C2__FLUSH_INVALIDATE_FLAG,
            /* expected_deserialized */ TRUE,
            /* expected_cleared      */ FALSE,
            /* expected_serialized   */ TRUE,
            /* expected_destroyed    */ TRUE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 23,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ FALSE,
            /* dirty_flag            */ FALSE,
            /* flags                 */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* flush_flags           */ H5C2__FLUSH_MARKED_ENTRIES_FLAG,
            /* expected_deserialized */ TRUE,
            /* expected_cleared      */ FALSE,
            /* expected_serialized   */ FALSE,
            /* expected_destroyed    */ FALSE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 24,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ FALSE,
            /* dirty_flag            */ TRUE,
            /* flags                 */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* flush_flags           */ H5C2__FLUSH_MARKED_ENTRIES_FLAG,
            /* expected_deserialized */ TRUE,
            /* expected_cleared      */ FALSE,
            /* expected_serialized   */ TRUE,
            /* expected_destroyed    */ FALSE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 25,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ FALSE,
            /* dirty_flag            */ FALSE,
            /* flags                 */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* flush_flags           */ H5C2__FLUSH_INVALIDATE_FLAG |
                                        H5C2__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_deserialized */ TRUE,
            /* expected_cleared      */ FALSE,
            /* expected_serialized   */ FALSE,
            /* expected_destroyed    */ TRUE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 26,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ FALSE,
            /* dirty_flag            */ TRUE,
            /* flags                 */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* flush_flags           */ H5C2__FLUSH_INVALIDATE_FLAG |
                                        H5C2__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_deserialized */ TRUE,
            /* expected_cleared      */ TRUE,
            /* expected_serialized   */ FALSE,
            /* expected_destroyed    */ TRUE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 27,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ FALSE,
            /* dirty_flag            */ FALSE,
            /* flags                 */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* flush_flags           */ H5C2__FLUSH_MARKED_ENTRIES_FLAG |
                                        H5C2__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_deserialized */ TRUE,
            /* expected_cleared      */ FALSE,
            /* expected_serialized   */ FALSE,
            /* expected_destroyed    */ FALSE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 28,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ FALSE,
            /* dirty_flag            */ TRUE,
            /* flags                 */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* flush_flags           */ H5C2__FLUSH_MARKED_ENTRIES_FLAG |
                                        H5C2__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_deserialized */ TRUE,
            /* expected_cleared      */ TRUE,
            /* expected_serialized   */ FALSE,
            /* expected_destroyed    */ FALSE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 29,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ FALSE,
            /* dirty_flag            */ FALSE,
            /* flags                 */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* flush_flags           */ H5C2__FLUSH_MARKED_ENTRIES_FLAG |
                                        H5C2__FLUSH_INVALIDATE_FLAG,
            /* expected_deserialized */ TRUE,
            /* expected_cleared      */ FALSE,
            /* expected_serialized   */ FALSE,
            /* expected_destroyed    */ TRUE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 30,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ FALSE,
            /* dirty_flag            */ TRUE,
            /* flags                 */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* flush_flags           */ H5C2__FLUSH_MARKED_ENTRIES_FLAG |
                                        H5C2__FLUSH_INVALIDATE_FLAG,
            /* expected_deserialized */ TRUE,
            /* expected_cleared      */ FALSE,
            /* expected_serialized   */ TRUE,
            /* expected_destroyed    */ TRUE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 31,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ FALSE,
            /* dirty_flag            */ FALSE,
            /* flags                 */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* flush_flags           */ H5C2__FLUSH_INVALIDATE_FLAG |
                                        H5C2__FLUSH_CLEAR_ONLY_FLAG |
                                        H5C2__FLUSH_MARKED_ENTRIES_FLAG,
            /* expected_deserialized */ TRUE,
            /* expected_cleared      */ FALSE,
            /* expected_serialized   */ FALSE,
            /* expected_destroyed    */ TRUE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 32,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ FALSE,
            /* dirty_flag            */ TRUE,
            /* flags                 */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* flush_flags           */ H5C2__FLUSH_INVALIDATE_FLAG |
                                       H5C2__FLUSH_CLEAR_ONLY_FLAG |
                                       H5C2__FLUSH_MARKED_ENTRIES_FLAG,
            /* expected_deserialized */ TRUE,
            /* expected_cleared      */ TRUE,
            /* expected_serialized   */ FALSE,
            /* expected_destroyed    */ TRUE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 33,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ TRUE,
            /* dirty_flag            */ FALSE,
            /* flags                 */ H5C2__NO_FLAGS_SET,
            /* flush_flags           */ H5C2__NO_FLAGS_SET,
            /* expected_deserialized */ FALSE,
            /* expected_cleared      */ FALSE,
            /* expected_serialized   */ TRUE,
            /* expected_destroyed    */ FALSE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 34,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ TRUE,
            /* dirty_flag            */ TRUE,
            /* flags                 */ H5C2__NO_FLAGS_SET,
            /* flush_flags           */ H5C2__NO_FLAGS_SET,
            /* expected_deserialized */ FALSE,
            /* expected_cleared      */ FALSE,
            /* expected_serialized   */ TRUE,
            /* expected_destroyed    */ FALSE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 35,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ TRUE,
            /* dirty_flag            */ FALSE,
            /* flags                 */ H5C2__NO_FLAGS_SET,
            /* flush_flags           */ H5C2__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_deserialized */ FALSE,
            /* expected_cleared      */ TRUE,
            /* expected_serialized   */ FALSE,
            /* expected_destroyed    */ FALSE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 36,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ TRUE,
            /* dirty_flag            */ TRUE,
            /* flags                 */ H5C2__NO_FLAGS_SET,
            /* flush_flags           */ H5C2__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_deserialized */ FALSE,
            /* expected_cleared      */ TRUE,
            /* expected_serialized   */ FALSE,
            /* expected_destroyed    */ FALSE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 37,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ TRUE,
            /* dirty_flag            */ FALSE,
            /* flags                 */ H5C2__NO_FLAGS_SET,
            /* flush_flags           */ H5C2__FLUSH_INVALIDATE_FLAG,
            /* expected_deserialized */ FALSE,
            /* expected_cleared      */ FALSE,
            /* expected_serialized   */ TRUE,
            /* expected_destroyed    */ TRUE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 38,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ TRUE,
            /* dirty_flag            */ TRUE,
            /* flags                 */ H5C2__NO_FLAGS_SET,
            /* flush_flags           */ H5C2__FLUSH_INVALIDATE_FLAG,
            /* expected_deserialized */ FALSE,
            /* expected_cleared      */ FALSE,
            /* expected_serialized   */ TRUE,
            /* expected_destroyed    */ TRUE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 39,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ TRUE,
            /* dirty_flag            */ FALSE,
            /* flags                 */ H5C2__NO_FLAGS_SET,
            /* flush_flags           */ H5C2__FLUSH_MARKED_ENTRIES_FLAG,
            /* expected_deserialized */ FALSE,
            /* expected_cleared      */ FALSE,
            /* expected_serialized   */ FALSE,
            /* expected_destroyed    */ FALSE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 40,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ TRUE,
            /* dirty_flag            */ TRUE,
            /* flags                 */ H5C2__NO_FLAGS_SET,
            /* flush_flags           */ H5C2__FLUSH_MARKED_ENTRIES_FLAG,
            /* expected_deserialized */ FALSE,
            /* expected_cleared      */ FALSE,
            /* expected_serialized   */ FALSE,
            /* expected_destroyed    */ FALSE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 41,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ TRUE,
            /* dirty_flag            */ FALSE,
            /* flags                 */ H5C2__NO_FLAGS_SET,
            /* flush_flags           */ H5C2__FLUSH_INVALIDATE_FLAG |
                                        H5C2__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_deserialized */ FALSE,
            /* expected_cleared      */ TRUE,
            /* expected_serialized   */ FALSE,
            /* expected_destroyed    */ TRUE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 42,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ TRUE,
            /* dirty_flag            */ TRUE,
            /* flags                 */ H5C2__NO_FLAGS_SET,
            /* flush_flags           */ H5C2__FLUSH_INVALIDATE_FLAG |
                                        H5C2__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_deserialized */ FALSE,
            /* expected_cleared      */ TRUE,
            /* expected_serialized   */ FALSE,
            /* expected_destroyed    */ TRUE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 43,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ TRUE,
            /* dirty_flag            */ FALSE,
            /* flags                 */ H5C2__NO_FLAGS_SET,
            /* flush_flags           */ H5C2__FLUSH_MARKED_ENTRIES_FLAG |
                                        H5C2__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_deserialized */ FALSE,
            /* expected_cleared      */ FALSE,
            /* expected_serialized   */ FALSE,
            /* expected_destroyed    */ FALSE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 44,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ TRUE,
            /* dirty_flag            */ TRUE,
            /* flags                 */ H5C2__NO_FLAGS_SET,
            /* flush_flags           */ H5C2__FLUSH_MARKED_ENTRIES_FLAG |
                                        H5C2__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_deserialized */ FALSE,
            /* expected_cleared      */ FALSE,
            /* expected_serialized   */ FALSE,
            /* expected_destroyed    */ FALSE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 45,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ TRUE,
            /* dirty_flag            */ FALSE,
            /* flags                 */ H5C2__NO_FLAGS_SET,
            /* flush_flags           */ H5C2__FLUSH_MARKED_ENTRIES_FLAG |
                                        H5C2__FLUSH_INVALIDATE_FLAG,
            /* expected_deserialized */ FALSE,
            /* expected_cleared      */ FALSE,
            /* expected_serialized   */ TRUE,
            /* expected_destroyed    */ TRUE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 46,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ TRUE,
            /* dirty_flag            */ TRUE,
            /* flags                 */ H5C2__NO_FLAGS_SET,
            /* flush_flags           */ H5C2__FLUSH_MARKED_ENTRIES_FLAG |
                                        H5C2__FLUSH_INVALIDATE_FLAG,
            /* expected_deserialized */ FALSE,
            /* expected_cleared      */ FALSE,
            /* expected_serialized   */ TRUE,
            /* expected_destroyed    */ TRUE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 47,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ TRUE,
            /* dirty_flag            */ FALSE,
            /* flags                 */ H5C2__NO_FLAGS_SET,
            /* flush_flags           */ H5C2__FLUSH_INVALIDATE_FLAG |
                                        H5C2__FLUSH_CLEAR_ONLY_FLAG |
                                        H5C2__FLUSH_MARKED_ENTRIES_FLAG,
            /* expected_deserialized */ FALSE,
            /* expected_cleared      */ TRUE,
            /* expected_serialized   */ FALSE,
            /* expected_destroyed    */ TRUE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 48,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ TRUE,
            /* dirty_flag            */ TRUE,
            /* flags                 */ H5C2__NO_FLAGS_SET,
            /* flush_flags           */ H5C2__FLUSH_INVALIDATE_FLAG |
                                        H5C2__FLUSH_CLEAR_ONLY_FLAG |
                                        H5C2__FLUSH_MARKED_ENTRIES_FLAG,
            /* expected_deserialized */ FALSE,
            /* expected_cleared      */ TRUE,
            /* expected_serialized   */ FALSE,
            /* expected_destroyed    */ TRUE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 49,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ TRUE,
            /* dirty_flag            */ FALSE,
            /* flags                 */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* flush_flags           */ H5C2__NO_FLAGS_SET,
            /* expected_deserialized */ FALSE,
            /* expected_cleared      */ FALSE,
            /* expected_serialized   */ TRUE,
            /* expected_destroyed    */ FALSE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 50,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ TRUE,
            /* dirty_flag            */ TRUE,
            /* flags                 */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* flush_flags           */ H5C2__NO_FLAGS_SET,
            /* expected_deserialized */ FALSE,
            /* expected_cleared      */ FALSE,
            /* expected_serialized   */ TRUE,
            /* expected_destroyed    */ FALSE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 51,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ TRUE,
            /* dirty_flag            */ FALSE,
            /* flags                 */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* flush_flags           */ H5C2__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_deserialized */ FALSE,
            /* expected_cleared      */ TRUE,
            /* expected_serialized   */ FALSE,
            /* expected_destroyed    */ FALSE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 52,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ TRUE,
            /* dirty_flag            */ TRUE,
            /* flags                 */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* flush_flags           */ H5C2__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_deserialized */ FALSE,
            /* expected_cleared      */ TRUE,
            /* expected_serialized   */ FALSE,
            /* expected_destroyed    */ FALSE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 53,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ TRUE,
            /* dirty_flag            */ FALSE,
            /* flags                 */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* flush_flags           */ H5C2__FLUSH_INVALIDATE_FLAG,
            /* expected_deserialized */ FALSE,
            /* expected_cleared      */ FALSE,
            /* expected_serialized   */ TRUE,
            /* expected_destroyed    */ TRUE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 54,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ TRUE,
            /* dirty_flag            */ TRUE,
            /* flags                 */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* flush_flags           */ H5C2__FLUSH_INVALIDATE_FLAG,
            /* expected_deserialized */ FALSE,
            /* expected_cleared      */ FALSE,
            /* expected_serialized   */ TRUE,
            /* expected_destroyed    */ TRUE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 55,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ TRUE,
            /* dirty_flag            */ FALSE,
            /* flags                 */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* flush_flags           */ H5C2__FLUSH_MARKED_ENTRIES_FLAG,
            /* expected_deserialized */ FALSE,
            /* expected_cleared      */ FALSE,
            /* expected_serialized   */ TRUE,
            /* expected_destroyed    */ FALSE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 56,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ TRUE,
            /* dirty_flag            */ TRUE,
            /* flags                 */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* flush_flags           */ H5C2__FLUSH_MARKED_ENTRIES_FLAG,
            /* expected_deserialized */ FALSE,
            /* expected_cleared      */ FALSE,
            /* expected_serialized   */ TRUE,
            /* expected_destroyed    */ FALSE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 57,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ TRUE,
            /* dirty_flag            */ FALSE,
            /* flags                 */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* flush_flags           */ H5C2__FLUSH_INVALIDATE_FLAG |
                                        H5C2__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_deserialized */ FALSE,
            /* expected_cleared      */ TRUE,
            /* expected_serialized   */ FALSE,
            /* expected_destroyed    */ TRUE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 58,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ TRUE,
            /* dirty_flag            */ TRUE,
            /* flags                 */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* flush_flags           */ H5C2__FLUSH_INVALIDATE_FLAG |
                                        H5C2__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_deserialized */ FALSE,
            /* expected_cleared      */ TRUE,
            /* expected_serialized   */ FALSE,
            /* expected_destroyed    */ TRUE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 59,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ TRUE,
            /* dirty_flag            */ FALSE,
            /* flags                 */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* flush_flags           */ H5C2__FLUSH_MARKED_ENTRIES_FLAG |
                                        H5C2__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_deserialized */ FALSE,
            /* expected_cleared      */ TRUE,
            /* expected_serialized   */ FALSE,
            /* expected_destroyed    */ FALSE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 60,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ TRUE,
            /* dirty_flag            */ TRUE,
            /* flags                 */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* flush_flags           */ H5C2__FLUSH_MARKED_ENTRIES_FLAG |
                                        H5C2__FLUSH_CLEAR_ONLY_FLAG,
            /* expected_deserialized */ FALSE,
            /* expected_cleared      */ TRUE,
            /* expected_serialized   */ FALSE,
            /* expected_destroyed    */ FALSE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 61,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ TRUE,
            /* dirty_flag            */ FALSE,
            /* flags                 */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* flush_flags           */ H5C2__FLUSH_MARKED_ENTRIES_FLAG |
                                        H5C2__FLUSH_INVALIDATE_FLAG,
            /* expected_deserialized */ FALSE,
            /* expected_cleared      */ FALSE,
            /* expected_serialized   */ TRUE,
            /* expected_destroyed    */ TRUE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 62,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ TRUE,
            /* dirty_flag            */ TRUE,
            /* flags                 */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* flush_flags           */ H5C2__FLUSH_MARKED_ENTRIES_FLAG |
                                        H5C2__FLUSH_INVALIDATE_FLAG,
            /* expected_deserialized */ FALSE,
            /* expected_cleared      */ FALSE,
            /* expected_serialized   */ TRUE,
            /* expected_destroyed    */ TRUE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 63,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ TRUE,
            /* dirty_flag            */ FALSE,
            /* flags                 */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* flush_flags           */ H5C2__FLUSH_INVALIDATE_FLAG |
                                        H5C2__FLUSH_CLEAR_ONLY_FLAG |
                                        H5C2__FLUSH_MARKED_ENTRIES_FLAG,
            /* expected_deserialized */ FALSE,
            /* expected_cleared      */ TRUE,
            /* expected_serialized   */ FALSE,
            /* expected_destroyed    */ TRUE
        );
    }

    if ( pass2 ) {

        check_flush_cache__single_entry_test
        (
            /* cache_ptr             */ cache_ptr,
            /* test_num              */ 64,
            /* entry_type            */ PICO_ENTRY_TYPE,
            /* entry_idx             */ 0,
            /* insert_flag           */ TRUE,
            /* dirty_flag            */ TRUE,
            /* flags                 */ H5C2__SET_FLUSH_MARKER_FLAG,
            /* flush_flags           */ H5C2__FLUSH_INVALIDATE_FLAG |
                                        H5C2__FLUSH_CLEAR_ONLY_FLAG |
                                        H5C2__FLUSH_MARKED_ENTRIES_FLAG,
            /* expected_deserialized */ FALSE,
            /* expected_cleared      */ TRUE,
            /* expected_serialized   */ FALSE,
            /* expected_destroyed    */ TRUE
        );
    }


    /* Now run single entry tests for pinned entries.  Test all combinations
     * of:
     *
     * 	1) Unpin by unprotect vs. unpin by call to H5C2_unpin_entry().
     *
     * 	2) Marked dirty by unprotect or not.
     *
     * 	3) Marked dirty by call to H5C2_mark_pinned_entry_dirty() or not.
     *
     *  4) Marked dirty by call to H5C2_mark_pinned_or_protected_entry_dirty()
     *     while protected or not.
     *
     *  5) Marked dirty by call to H5C2_mark_pinned_or_protected_entry_dirty()
     *     while pinned or not.
     *
     * 	6) Entry marked for flush or not.
     *
     * 	7) Call flush with H5C2__FLUSH_MARKED_ENTRIES_FLAG or not.
     *
     * 	8) Call flush with H5C2__FLUSH_CLEAR_ONLY_FLAG or not.
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
     * flush in invoked WITHOUT the H5C2__FLUSH_CLEAR_ONLY_FLAG.  However,
     * there are exceptions:  If flush is invoked with the
     * H5C2__FLUSH_MARKED_ENTRIES_FLAG, only marked entries will be flushed.
     *
     * Further, unprotecting an entry with the H5C2__SET_FLUSH_MARKER_FLAG
     * will NOT mark the entry unless the entry has either been marked
     * dirty either before or durting the unprotect call.  This results in
     * some counterintuitive entries in the table.  It make be useful to
     * look in the test code to see the exact order of operations.
     *
     * Similarly, we expect an entry to be cleared if it is dirty, and
     * flush is invoked WITH the H5C2__FLUSH_CLEAR_ONLY_FLAG.  Again, there
     * are exceptions -- If flush is also invoked with the
     * H5C2__FLUSH_MARKED_ENTRIES_FLAG, only the marked entries will be
     * cleared.
     *
     * The above comments about applying unprotect with the
     * H5C2__SET_FLUSH_MARKER_FLAG apply here as well.
     */

    if ( pass2 ) {

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
            hbool_t		expected_serialized;
            hbool_t		expected_destroyed;
	} spec[256] =
	/*                                           pop    pop
	 *                         ent               mark   mark
	 *  test  entry            -ry dirty  mark   dirty  dirty  unprot                             flush                                                        expect  expect expect
         *  num   type             idx  flag  dirty  prot   pinned unpin  flags                       flags                                                        clear   srlzd  destroy
	 */
	{ {   1,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE, FALSE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__NO_FLAGS_SET,                                           FALSE, FALSE, FALSE },
	  {   2,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE, FALSE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__NO_FLAGS_SET,                                           FALSE, FALSE, FALSE },
	  {   3,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE,  TRUE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {   4,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE,  TRUE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {   5,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE, FALSE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {   6,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE, FALSE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {   7,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE,  TRUE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {   8,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE,  TRUE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {   9,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE, FALSE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  10,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE, FALSE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  11,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE,  TRUE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  12,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE,  TRUE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  13,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE, FALSE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  14,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE, FALSE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  15,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE,  TRUE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  16,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE,  TRUE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  17,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE, FALSE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  18,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE, FALSE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  19,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE,  TRUE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  20,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE,  TRUE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  21,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE, FALSE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  22,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE, FALSE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  23,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE,  TRUE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  24,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE,  TRUE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  25,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE, FALSE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  26,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE, FALSE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  27,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE,  TRUE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  28,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE,  TRUE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  29,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE, FALSE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  30,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE, FALSE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  31,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE,  TRUE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  32,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  33,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE, FALSE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__NO_FLAGS_SET,                                           FALSE, FALSE, FALSE },
	  {  34,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE, FALSE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__NO_FLAGS_SET,                                           FALSE, FALSE, FALSE },
	  {  35,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE,  TRUE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  36,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE,  TRUE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  37,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE, FALSE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  38,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE, FALSE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  39,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE,  TRUE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  40,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE,  TRUE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  41,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE, FALSE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  42,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE, FALSE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  43,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE,  TRUE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  44,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE,  TRUE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  45,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE, FALSE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  46,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE, FALSE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  47,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE,  TRUE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  48,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE,  TRUE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  49,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE, FALSE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  50,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE, FALSE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  51,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE,  TRUE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  52,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE,  TRUE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  53,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE, FALSE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  54,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE, FALSE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  55,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE,  TRUE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  56,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE,  TRUE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  57,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE, FALSE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  58,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE, FALSE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  59,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE,  TRUE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  60,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE,  TRUE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  61,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE, FALSE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  62,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE, FALSE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  63,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE,  TRUE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  64,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__NO_FLAGS_SET,                                           FALSE,  TRUE, FALSE },
	  {  65,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE, FALSE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  66,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE, FALSE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  67,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE,  TRUE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  68,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE,  TRUE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  69,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE, FALSE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  70,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE, FALSE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  71,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE,  TRUE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  72,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE,  TRUE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  73,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE, FALSE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  74,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE, FALSE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  75,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE,  TRUE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  76,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE,  TRUE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  77,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE, FALSE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  78,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE, FALSE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  79,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE,  TRUE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  80,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE,  TRUE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  81,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE, FALSE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  82,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE, FALSE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  83,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE,  TRUE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  84,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE,  TRUE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  85,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE, FALSE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  86,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE, FALSE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  87,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE,  TRUE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  88,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE,  TRUE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  89,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE, FALSE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  90,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE, FALSE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  91,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE,  TRUE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  92,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE,  TRUE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  93,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE, FALSE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  94,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE, FALSE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  95,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE,  TRUE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  96,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  97,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE, FALSE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  98,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE, FALSE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  {  99,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE,  TRUE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  { 100,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE,  TRUE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  { 101,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE, FALSE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE,  TRUE, FALSE },
	  { 102,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE, FALSE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE,  TRUE, FALSE },
	  { 103,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE,  TRUE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE,  TRUE, FALSE },
	  { 104,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE,  TRUE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE,  TRUE, FALSE },
	  { 105,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE, FALSE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  { 106,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE, FALSE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  { 107,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE,  TRUE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  { 108,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE,  TRUE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE, FALSE, FALSE },
	  { 109,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE, FALSE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE,  TRUE, FALSE },
	  { 110,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE, FALSE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE,  TRUE, FALSE },
	  { 111,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE,  TRUE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE,  TRUE, FALSE },
	  { 112,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE,  TRUE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE,  TRUE, FALSE },
	  { 113,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE, FALSE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE,  TRUE, FALSE },
	  { 114,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE, FALSE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE,  TRUE, FALSE },
	  { 115,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE,  TRUE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE,  TRUE, FALSE },
	  { 116,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE,  TRUE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE,  TRUE, FALSE },
	  { 117,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE, FALSE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE,  TRUE, FALSE },
	  { 118,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE, FALSE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE,  TRUE, FALSE },
	  { 119,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE,  TRUE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE,  TRUE, FALSE },
	  { 120,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE,  TRUE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE,  TRUE, FALSE },
	  { 121,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE, FALSE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE,  TRUE, FALSE },
	  { 122,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE, FALSE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE,  TRUE, FALSE },
	  { 123,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE,  TRUE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE,  TRUE, FALSE },
	  { 124,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE,  TRUE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE,  TRUE, FALSE },
	  { 125,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE, FALSE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE,  TRUE, FALSE },
	  { 126,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE, FALSE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE,  TRUE, FALSE },
	  { 127,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE,  TRUE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE,  TRUE, FALSE },
	  { 128,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG,                              FALSE,  TRUE, FALSE },
	  { 129,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE, FALSE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_CLEAR_ONLY_FLAG,                                  FALSE, FALSE, FALSE },
	  { 130,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE, FALSE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_CLEAR_ONLY_FLAG,                                  FALSE, FALSE, FALSE },
	  { 131,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE,  TRUE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 132,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE,  TRUE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 133,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE, FALSE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 134,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE, FALSE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 135,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE,  TRUE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 136,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE,  TRUE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 137,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE, FALSE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 138,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE, FALSE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 139,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE,  TRUE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 140,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE,  TRUE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 141,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE, FALSE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 142,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE, FALSE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 143,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE,  TRUE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 144,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE,  TRUE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 145,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE, FALSE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 146,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE, FALSE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 147,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE,  TRUE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 148,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE,  TRUE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 149,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE, FALSE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 150,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE, FALSE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 151,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE,  TRUE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 152,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE,  TRUE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 153,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE, FALSE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 154,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE, FALSE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 155,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE,  TRUE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 156,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE,  TRUE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 157,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE, FALSE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 158,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE, FALSE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 159,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE,  TRUE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 160,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 161,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE, FALSE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_CLEAR_ONLY_FLAG,                                  FALSE, FALSE, FALSE },
	  { 162,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE, FALSE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_CLEAR_ONLY_FLAG,                                  FALSE, FALSE, FALSE },
	  { 163,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE,  TRUE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 164,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE,  TRUE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 165,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE, FALSE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 166,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE, FALSE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 167,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE,  TRUE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 168,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE,  TRUE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 169,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE, FALSE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 170,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE, FALSE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 171,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE,  TRUE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 172,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE,  TRUE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 173,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE, FALSE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 174,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE, FALSE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 175,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE,  TRUE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 176,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE,  TRUE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 177,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE, FALSE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 178,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE, FALSE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 179,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE,  TRUE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 180,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE,  TRUE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 181,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE, FALSE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 182,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE, FALSE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 183,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE,  TRUE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 184,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE,  TRUE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 185,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE, FALSE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 186,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE, FALSE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 187,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE,  TRUE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 188,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE,  TRUE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 189,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE, FALSE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 190,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE, FALSE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 191,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE,  TRUE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 192,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_CLEAR_ONLY_FLAG,                                   TRUE, FALSE, FALSE },
	  { 193,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE, FALSE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 194,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE, FALSE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 195,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE,  TRUE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 196,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE,  TRUE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 197,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE, FALSE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 198,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE, FALSE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 199,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE,  TRUE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 200,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE,  TRUE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 201,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE, FALSE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 202,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE, FALSE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 203,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE,  TRUE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 204,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE,  TRUE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 205,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE, FALSE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 206,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE, FALSE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 207,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE,  TRUE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 208,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE,  TRUE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 209,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE, FALSE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 210,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE, FALSE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 211,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE,  TRUE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 212,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE,  TRUE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 213,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE, FALSE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 214,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE, FALSE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 215,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE,  TRUE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 216,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE,  TRUE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 217,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE, FALSE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 218,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE, FALSE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 219,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE,  TRUE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 220,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE,  TRUE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 221,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE, FALSE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 222,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE, FALSE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 223,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE,  TRUE, FALSE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 224,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE, H5C2__NO_FLAGS_SET,          H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 225,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE, FALSE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 226,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE, FALSE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 227,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE,  TRUE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 228,  PICO_ENTRY_TYPE,  0, FALSE, FALSE, FALSE,  TRUE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 229,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE, FALSE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG,  TRUE, FALSE, FALSE },
	  { 230,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE, FALSE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG,  TRUE, FALSE, FALSE },
	  { 231,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE,  TRUE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG,  TRUE, FALSE, FALSE },
	  { 232,  PICO_ENTRY_TYPE,  0, FALSE, FALSE,  TRUE,  TRUE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG,  TRUE, FALSE, FALSE },
	  { 233,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE, FALSE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 234,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE, FALSE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 235,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE,  TRUE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 236,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE, FALSE,  TRUE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG, FALSE, FALSE, FALSE },
	  { 237,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE, FALSE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG,  TRUE, FALSE, FALSE },
	  { 238,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE, FALSE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG,  TRUE, FALSE, FALSE },
	  { 239,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE,  TRUE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG,  TRUE, FALSE, FALSE },
	  { 240,  PICO_ENTRY_TYPE,  0, FALSE,  TRUE,  TRUE,  TRUE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG,  TRUE, FALSE, FALSE },
	  { 241,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE, FALSE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG,  TRUE, FALSE, FALSE },
	  { 242,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE, FALSE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG,  TRUE, FALSE, FALSE },
	  { 243,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE,  TRUE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG,  TRUE, FALSE, FALSE },
	  { 244,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE, FALSE,  TRUE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG,  TRUE, FALSE, FALSE },
	  { 245,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE, FALSE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG,  TRUE, FALSE, FALSE },
	  { 246,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE, FALSE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG,  TRUE, FALSE, FALSE },
	  { 247,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE,  TRUE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG,  TRUE, FALSE, FALSE },
	  { 248,  PICO_ENTRY_TYPE,  0,  TRUE, FALSE,  TRUE,  TRUE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG,  TRUE, FALSE, FALSE },
	  { 249,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE, FALSE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG,  TRUE, FALSE, FALSE },
	  { 250,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE, FALSE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG,  TRUE, FALSE, FALSE },
	  { 251,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE,  TRUE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG,  TRUE, FALSE, FALSE },
	  { 252,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE, FALSE,  TRUE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG,  TRUE, FALSE, FALSE },
	  { 253,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE, FALSE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG,  TRUE, FALSE, FALSE },
	  { 254,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE, FALSE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG,  TRUE, FALSE, FALSE },
	  { 255,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE,  TRUE, FALSE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG,  TRUE, FALSE, FALSE },
	  { 256,  PICO_ENTRY_TYPE,  0,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE, H5C2__SET_FLUSH_MARKER_FLAG, H5C2__FLUSH_MARKED_ENTRIES_FLAG | H5C2__FLUSH_CLEAR_ONLY_FLAG,  TRUE, FALSE, FALSE } };

	i = 0;
	while ( ( pass2 ) && ( i < 256 ) )
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
                /* expected_serialized   */ spec[i].expected_serialized,
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
check_flush_cache__single_entry_test(H5C2_t * cache_ptr,
                                     int test_num,
                                     int entry_type,
                                     int entry_idx,
                                     hbool_t insert_flag,
                                     hbool_t dirty_flag,
                                     unsigned int flags,
                                     unsigned int flush_flags,
                                     hbool_t expected_deserialized,
                                     hbool_t expected_cleared,
                                     hbool_t expected_serialized,
                                     hbool_t expected_destroyed)
{
    /* const char *   fcn_name = "check_flush_cache__single_entry_test"; */
    static char    msg[128];
    herr_t	   result;
    test_entry_t * base_addr;
    test_entry_t * entry_ptr;

    if ( cache_ptr == NULL ) {

        pass2 = FALSE;
        HDsnprintf(msg, (size_t)128,
                   "cache_ptr NULL on entry to single entry test #%d.",
                   test_num);
        failure_mssg2 = msg;
    }
    else if ( ( cache_ptr->index_len != 0 ) ||
              ( cache_ptr->index_size != 0 ) ) {

        pass2 = FALSE;
        HDsnprintf(msg, (size_t)128,
                   "cache not empty at beginning of single entry test #%d.",
                   test_num);
        failure_mssg2 = msg;
    }
    else if ( ( entry_type < 0 ) || ( entry_type >= NUMBER_OF_ENTRY_TYPES ) ||
              ( entry_idx < 0 ) || ( entry_idx > max_indices2[entry_type] ) ) {

        pass2 = FALSE;
        HDsnprintf(msg, (size_t)128,
                   "Bad parameters on entry to single entry test #%d.",
                   test_num);
        failure_mssg2 = msg;
    }

    if ( pass2 ) {

        base_addr = entries2[entry_type];
        entry_ptr = &(base_addr[entry_idx]);

        if ( insert_flag ) {

            insert_entry2(cache_ptr, entry_type, entry_idx, dirty_flag, flags);

        } else {

            protect_entry2(cache_ptr, entry_type, entry_idx);

            unprotect_entry2(cache_ptr, entry_type, entry_idx,
                            (int)dirty_flag, flags);
        }
    }

    if ( pass2 ) {

        result = H5C2_flush_cache(cache_ptr, H5P_DATASET_XFER_DEFAULT, 
			          flush_flags);

        if ( result < 0 ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "flush with flags 0x%x failed in single entry test #%d.",
                       flush_flags, test_num);
            failure_mssg2 = msg;
        }
#ifndef NDEBUG
	/* The clear_dirty_bits() callback is only called in debug mode --
 	 * thus we can only do our full test on the expected entry history
	 * when debug is enabled.
	 */
        else if ( ( entry_ptr->deserialized != expected_deserialized ) ||
                  ( entry_ptr->cleared != expected_cleared ) ||
                  ( entry_ptr->serialized != expected_serialized ) ||
                  ( entry_ptr->destroyed != expected_destroyed ) ) {
#else
	/* When in procduction mode, the clear_dirty_bits() callback is
	 * not called, so entry_ptr->cleared should never be set.
         */
        else if ( ( entry_ptr->deserialized != expected_deserialized ) ||
                  ( entry_ptr->cleared ) ||
                  ( entry_ptr->serialized != expected_serialized ) ||
                  ( entry_ptr->destroyed != expected_destroyed ) ) {
#endif /* NDEBUG */
#if 1 /* This is useful debugging code -- lets keep it for a while */

            HDfprintf(stdout,
             "desrlzd = %d(%d), clrd = %d(%d), srlzd = %d(%d), dest = %d(%d)\n",
             (int)(entry_ptr->deserialized),
             (int)expected_deserialized,
             (int)(entry_ptr->cleared),
             (int)expected_cleared,
             (int)(entry_ptr->serialized),
             (int)expected_serialized,
             (int)(entry_ptr->destroyed),
             (int)expected_destroyed);
#endif 
            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
                "Unexpected entry status after flush in single entry test #%d.",
                test_num);
            failure_mssg2 = msg;
        }
        else if ( ( ( (flush_flags & H5C2__FLUSH_INVALIDATE_FLAG) == 0 )
                    &&
                    ( ( cache_ptr->index_len != 1 )
                      ||
                      ( cache_ptr->index_size != entry_sizes2[entry_type] )
                    )
                  )
                  ||
                  ( ( (flush_flags & H5C2__FLUSH_INVALIDATE_FLAG) != 0 )
                    &&
                    ( ( cache_ptr->index_len != 0 )
                      ||
                      ( cache_ptr->index_size != 0 )
                    )
                  )
                ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
              "Unexpected cache len/size after flush in single entry test #%d.",
              test_num);
            failure_mssg2 = msg;
        }
    }


    /* clean up the cache to prep for the next test */
    if ( pass2 ) {

        result = H5C2_flush_cache(cache_ptr, H5P_DATASET_XFER_DEFAULT,
                                  H5C2__FLUSH_INVALIDATE_FLAG);

        if ( result < 0 ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "Flush failed on cleanup in single entry test #%d.",
                       test_num);
            failure_mssg2 = msg;
        }
        else if ( ( cache_ptr->index_len != 0 ) ||
                  ( cache_ptr->index_size != 0 ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
            "Unexpected cache len/size after cleanup in single entry test #%d.",
            test_num);
            failure_mssg2 = msg;

        } else {

            entry_ptr->deserialized = FALSE;
            entry_ptr->cleared      = FALSE;
            entry_ptr->serialized   = FALSE;
            entry_ptr->destroyed    = FALSE;
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
 * 		H5C2_mark_pinned_or_protected_entry_dirty() call.  Use the
 * 		call to mark the entry dirty while the entry is protected
 * 		if pop_mark_dirty_prot is TRUE, and to mark the entry
 * 		dirty while it is pinned if pop_mark_dirty_pinned is TRUE.
 *
 *-------------------------------------------------------------------------
 */

static void
check_flush_cache__pinned_single_entry_test(H5C2_t * cache_ptr,
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
                                            hbool_t expected_serialized,
                                            hbool_t expected_destroyed)
{
    /* const char *fcn_name = "check_flush_cache__pinned_single_entry_test"; */
    static char    msg[128];
    hbool_t        expected_deserialized = TRUE;
    herr_t	   result;
    test_entry_t * base_addr;
    test_entry_t * entry_ptr;

    if ( cache_ptr == NULL ) {

        pass2 = FALSE;
        HDsnprintf(msg, (size_t)128,
                   "cache_ptr NULL on entry to pinned single entry test #%d.",
                   test_num);
        failure_mssg2 = msg;
    }
    else if ( ( cache_ptr->index_len != 0 ) ||
              ( cache_ptr->index_size != 0 ) ) {

        pass2 = FALSE;
        HDsnprintf(msg, (size_t)128,
               "cache not empty at beginning of pinned single entry test #%d.",
               test_num);
        failure_mssg2 = msg;
    }
    else if ( ( entry_type < 0 ) || ( entry_type >= NUMBER_OF_ENTRY_TYPES ) ||
              ( entry_idx < 0 ) || ( entry_idx > max_indices2[entry_type] ) ) {

        pass2 = FALSE;
        HDsnprintf(msg, (size_t)128,
                   "Bad parameters on entry to pinned single entry test #%d.",
                   test_num);
        failure_mssg2 = msg;
    }

    if ( pass2 ) {

        base_addr = entries2[entry_type];
        entry_ptr = &(base_addr[entry_idx]);

        protect_entry2(cache_ptr, entry_type, entry_idx);

	if ( pop_mark_dirty_prot ) {

	    mark_pinned_or_protected_entry_dirty2(cache_ptr,
			                          entry_type,
			                          entry_idx);
	}

        unprotect_entry2(cache_ptr, entry_type, entry_idx,
                        (int)dirty_flag, (flags | H5C2__PIN_ENTRY_FLAG));

	if ( mark_dirty ) {

            mark_pinned_entry_dirty2(cache_ptr, entry_type, entry_idx,
			            FALSE, (size_t)0);
	}

	if ( pop_mark_dirty_pinned ) {

	    mark_pinned_or_protected_entry_dirty2(cache_ptr,
			                          entry_type,
			                          entry_idx);
	}
    }

    if ( pass2 ) {

        result = H5C2_flush_cache(cache_ptr, H5P_DATASET_XFER_DEFAULT, 
			          flush_flags);

        if ( result < 0 ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
               "flush with flags 0x%x failed in pinned single entry test #%d.",
               flush_flags, test_num);
            failure_mssg2 = msg;
        }
#ifndef NDEBUG
        /* The clear_dirty_bits() callback is only called in debug mode --
         * thus we can only do our full test on the expected entry history
         * when debug is enabled.
         */
        else if ( ( entry_ptr->deserialized != expected_deserialized ) ||
                  ( entry_ptr->cleared != expected_cleared ) ||
                  ( entry_ptr->serialized != expected_serialized ) ||
                  ( entry_ptr->destroyed != expected_destroyed ) ) {
#else
        /* When in procduction mode, the clear_dirty_bits() callback is
         * not called, so entry_ptr->cleared should never be set.
         */
        else if ( ( entry_ptr->deserialized != expected_deserialized ) ||
                  ( entry_ptr->cleared ) ||
                  ( entry_ptr->serialized != expected_serialized ) ||
                  ( entry_ptr->destroyed != expected_destroyed ) ) {
#endif
#if 0 /* this is useful debugging code -- keep it around */
            HDfprintf(stdout,
             "desrlzd = %d(%d), clrd = %d(%d), srlzd = %d(%d), dest = %d(%d)\n",
             (int)(entry_ptr->deserialized),
             (int)expected_deserialized,
             (int)(entry_ptr->cleared),
             (int)expected_cleared,
             (int)(entry_ptr->serialized),
             (int)expected_serialized,
             (int)(entry_ptr->destroyed),
             (int)expected_destroyed);
#endif 
            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
                "Unexpected entry status after flush in pinned single entry test #%d.",
                test_num);
            failure_mssg2 = msg;
        }
        else if ( ( ( (flush_flags & H5C2__FLUSH_INVALIDATE_FLAG) == 0 )
                    &&
                    ( ( cache_ptr->index_len != 1 )
                      ||
                      ( cache_ptr->index_size != entry_sizes2[entry_type] )
                    )
                  )
                  ||
                  ( ( (flush_flags & H5C2__FLUSH_INVALIDATE_FLAG) != 0 )
                    &&
                    ( ( cache_ptr->index_len != 0 )
                      ||
                      ( cache_ptr->index_size != 0 )
                    )
                  )
                ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
              "Unexpected cache len/size after flush in pinned single entry test #%d.",
              test_num);
            failure_mssg2 = msg;
        }
    }


    /* clean up the cache to prep for the next test */
    if ( pass2 ) {

        if ( unprotect_unpin ) {

            protect_entry2(cache_ptr, entry_type, entry_idx);

            unprotect_entry2(cache_ptr, entry_type, entry_idx,
                            (int)dirty_flag, H5C2__UNPIN_ENTRY_FLAG);

        } else {

            unpin_entry2(cache_ptr, entry_type, entry_idx);

        }
    }

    if ( pass2 ) {

        result = H5C2_flush_cache(cache_ptr, H5P_DATASET_XFER_DEFAULT,
                                  H5C2__FLUSH_INVALIDATE_FLAG);

        if ( result < 0 ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
                    "Flush failed on cleanup in pinned single entry test #%d.",
                    test_num);
            failure_mssg2 = msg;
        }
        else if ( ( cache_ptr->index_len != 0 ) ||
                  ( cache_ptr->index_size != 0 ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
            "Unexpected cache len/size after cleanup in pinned single entry test #%d.",
            test_num);
            failure_mssg2 = msg;

        } else {

            entry_ptr->deserialized = FALSE;
            entry_ptr->cleared      = FALSE;
            entry_ptr->serialized   = FALSE;
            entry_ptr->destroyed    = FALSE;
        }
    }

    return;

} /* check_flush_cache__pinned_single_entry_test() */


/*-------------------------------------------------------------------------
 * Function:	check_get_entry_status()
 *
 * Purpose:	Verify that H5C2_get_entry_status() behaves as expected.
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
    H5C2_t *       cache_ptr = NULL;
    test_entry_t * base_addr;
    test_entry_t * entry_ptr;

    TESTING("H5C2_get_entry_status() functionality");

    pass2 = TRUE;

    if ( pass2 ) {

        reset_entries2();

        cache_ptr = setup_cache2((size_t)(2 * 1024 * 1024),
                                (size_t)(1 * 1024 * 1024));

        base_addr = entries2[0];
        entry_ptr = &(base_addr[0]);
    }

    if ( pass2 ) {

        /* entry not in cache -- only in_cache should be touched by
         * the call.  Thus, only check that boolean.
         */

        result = H5C2_get_entry_status(cache_ptr, entry_ptr->addr, &entry_size,
                &in_cache, &is_dirty, &is_protected, &is_pinned);

	if ( result < 0 ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "H5AC_get_entry_status() reports failure 1.");
            failure_mssg2 = msg;

	} else if ( in_cache ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected status 1.");
            failure_mssg2 = msg;
        }
    }

    protect_entry2(cache_ptr, 0, 0);

    unprotect_entry2(cache_ptr, 0, 0, FALSE, H5C2__NO_FLAGS_SET);

    if ( pass2 ) {

        result = H5C2_get_entry_status(cache_ptr, entry_ptr->addr, &entry_size,
                &in_cache, &is_dirty, &is_protected, &is_pinned);

	if ( result < 0 ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "H5AC_get_entry_status() reports failure 2.");
            failure_mssg2 = msg;

	} else if ( !in_cache || is_dirty || is_protected || is_pinned ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected status 2.");
            failure_mssg2 = msg;
        }
    }

    protect_entry2(cache_ptr, 0, 0);

    if ( pass2 ) {

        result = H5C2_get_entry_status(cache_ptr, entry_ptr->addr, &entry_size,
                &in_cache, &is_dirty, &is_protected, &is_pinned);

	if ( result < 0 ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "H5AC_get_entry_status() reports failure 3.");
            failure_mssg2 = msg;

	} else if ( !in_cache || is_dirty || !is_protected || is_pinned ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected status 3.");
            failure_mssg2 = msg;
        }
    }

    unprotect_entry2(cache_ptr, 0, 0, FALSE, H5C2__PIN_ENTRY_FLAG);

    if ( pass2 ) {

        result = H5C2_get_entry_status(cache_ptr, entry_ptr->addr, &entry_size,
                &in_cache, &is_dirty, &is_protected, &is_pinned);

	if ( result < 0 ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "H5AC_get_entry_status() reports failure 4.");
            failure_mssg2 = msg;

	} else if ( !in_cache || is_dirty || is_protected || !is_pinned ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected status 4.");
            failure_mssg2 = msg;
        }
    }

    mark_pinned_entry_dirty2(cache_ptr, 0, 0, FALSE, (size_t)0);

    if ( pass2 ) {

        result = H5C2_get_entry_status(cache_ptr, entry_ptr->addr, &entry_size,
                &in_cache, &is_dirty, &is_protected, &is_pinned);

	if ( result < 0 ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "H5AC_get_entry_status() reports failure 5.");
            failure_mssg2 = msg;

	} else if ( !in_cache || !is_dirty || is_protected || !is_pinned ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected status 5.");
            failure_mssg2 = msg;
        }
    }

    unpin_entry2(cache_ptr, 0, 0);

    if ( pass2 ) {

        result = H5C2_get_entry_status(cache_ptr, entry_ptr->addr, &entry_size,
                &in_cache, &is_dirty, &is_protected, &is_pinned);

	if ( result < 0 ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "H5AC_get_entry_status() reports failure 6.");
            failure_mssg2 = msg;

	} else if ( !in_cache || !is_dirty || is_protected || is_pinned ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected status 6.");
            failure_mssg2 = msg;
        }
    }

    if ( pass2 ) {

        takedown_cache2(cache_ptr, FALSE, FALSE);
    }

    if ( pass2 ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass2 ) {

        HDfprintf(stdout, "%s(): failure_mssg2 = \"%s\".\n",
                  fcn_name, failure_mssg2);
    }

    return;

} /* check_get_entry_status() */


/*-------------------------------------------------------------------------
 * Function:	check_expunge_entry()
 *
 * Purpose:	Verify that H5C2_expunge_entry() behaves as expected.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              7/5/06
 *
 * Modifications:
 *
 * 		JRM -- 10/15/07
 * 		Minor updates to conform to new cache API.
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
    H5C2_t *       cache_ptr = NULL;
    test_entry_t * base_addr;
    test_entry_t * entry_ptr;

    TESTING("H5C2_expunge_entry() functionality");

    pass2 = TRUE;

    if ( pass2 ) {

        reset_entries2();

        cache_ptr = setup_cache2((size_t)(2 * 1024 * 1024),
                                (size_t)(1 * 1024 * 1024));

        base_addr = entries2[0];
        entry_ptr = &(base_addr[0]);
    }

    if ( pass2 ) {

        /* entry not in cache -- only in_cache should be touched by
         * the status call.  Thus, only check that boolean.
         */

        result = H5C2_get_entry_status(cache_ptr, entry_ptr->addr, &entry_size,
                                      &in_cache, &is_dirty, &is_protected, 
				      &is_pinned);

	if ( result < 0 ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "H5AC_get_entry_status() reports failure 1.");
            failure_mssg2 = msg;

	} else if ( in_cache ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected status 1.");
            failure_mssg2 = msg;

        } else if ( ( entry_ptr->deserialized ) ||
                    ( entry_ptr->cleared ) ||
		    ( entry_ptr->serialized ) ||
		    ( entry_ptr->destroyed ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected entry history 1.");
            failure_mssg2 = msg;

	}
    }

    /* protect an entry to force the cache to load it, and then unprotect
     * it without marking it dirty.
     */

    protect_entry2(cache_ptr, 0, 0);

    unprotect_entry2(cache_ptr, 0, 0, FALSE, H5C2__NO_FLAGS_SET);

    if ( pass2 ) {

        result = H5C2_get_entry_status(cache_ptr, entry_ptr->addr, &entry_size,
                &in_cache, &is_dirty, &is_protected, &is_pinned);

	if ( result < 0 ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "H5AC_get_entry_status() reports failure 2.");
            failure_mssg2 = msg;

	} else if ( !in_cache || is_dirty || is_protected || is_pinned ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected status 2.");
            failure_mssg2 = msg;

        } else if ( ( ! entry_ptr->deserialized ) ||
                    ( entry_ptr->cleared ) ||
		    ( entry_ptr->serialized ) ||
		    ( entry_ptr->destroyed ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected entry history 2.");
            failure_mssg2 = msg;

	}
    }

    /* Expunge the entry and then verify that it is no longer in the cache.
     * Also verify that the entry was loaded, cleared, and destroyed, but 
     * not flushed.
     *
     * JRM -- 10/15/07
     * With the advent of the new cache API, the old clear() callback has
     * been replaced with the new clear_dirty_bits() callback.  This 
     * callback is only called if the entry is dirty to begin with.
     * Thus, the entry will no longer be marked as cleared.
     */
    expunge_entry2(cache_ptr, 0, 0);

    if ( pass2 ) {

        /* entry shouldn't be in cache -- only in_cache should be touched 
	 * by the status call.  Thus, only check that boolean.
         */

        result = H5C2_get_entry_status(cache_ptr, entry_ptr->addr, &entry_size,
                &in_cache, &is_dirty, &is_protected, &is_pinned);

	if ( result < 0 ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "H5AC_get_entry_status() reports failure 3.");
            failure_mssg2 = msg;

	} else if ( in_cache ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected status 3.");
            failure_mssg2 = msg;

        } else if ( ( ! entry_ptr->deserialized ) ||
                    ( entry_ptr->cleared ) ||
		    ( entry_ptr->serialized ) ||
		    ( ! entry_ptr->destroyed ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected entry history 3.");
            failure_mssg2 = msg;

	}
    }

    /* now repeat the process with a different entry.  On unprotect
     * mark the entry as dirty.  Verify that it is not flushed.
     */
    
    base_addr = entries2[0];
    entry_ptr = &(base_addr[1]);

    if ( pass2 ) {

        /* entry not in cache -- only in_cache should be touched by
         * the status call.  Thus, only check that boolean.
         */

        result = H5C2_get_entry_status(cache_ptr, entry_ptr->addr, &entry_size,
                                      &in_cache, &is_dirty, &is_protected, 
				      &is_pinned);

	if ( result < 0 ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "H5AC_get_entry_status() reports failure 4.");
            failure_mssg2 = msg;

	} else if ( in_cache ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected status 4.");
            failure_mssg2 = msg;

        } else if ( ( entry_ptr->deserialized ) ||
                    ( entry_ptr->cleared ) ||
		    ( entry_ptr->serialized ) ||
		    ( entry_ptr->destroyed ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected entry history 4.");
            failure_mssg2 = msg;

	}
    }

    /* protect the entry to force the cache to load it, and then unprotect
     * it with the dirty flag set.
     */

    protect_entry2(cache_ptr, 0, 1);

    unprotect_entry2(cache_ptr, 0, 1, TRUE, H5C2__NO_FLAGS_SET);

    if ( pass2 ) {

        result = H5C2_get_entry_status(cache_ptr, entry_ptr->addr, &entry_size,
                                      &in_cache, &is_dirty, &is_protected, 
				      &is_pinned);

	if ( result < 0 ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "H5AC_get_entry_status() reports failure 5.");
            failure_mssg2 = msg;

	} else if ( !in_cache || !is_dirty || is_protected || is_pinned ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected status 5.");
            failure_mssg2 = msg;

        } else if ( ( ! entry_ptr->deserialized ) ||
                    ( entry_ptr->cleared ) ||
		    ( entry_ptr->serialized ) ||
		    ( entry_ptr->destroyed ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected entry history 5.");
            failure_mssg2 = msg;

	}
    }

    /* Expunge the entry and then verify that it is no longer in the cache.
     * Also verify that the entry was loaded, cleared and destroyed, but not
     * flushed.
     */
    expunge_entry2(cache_ptr, 0, 1);

    if ( pass2 ) {

        /* entry shouldn't be in cache -- only in_cache should be touched 
	 * by the status call.  Thus, only check that boolean.
         */

        result = H5C2_get_entry_status(cache_ptr, entry_ptr->addr, &entry_size,
                                       &in_cache, &is_dirty, &is_protected, 
				       &is_pinned);

	if ( result < 0 ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "H5AC_get_entry_status() reports failure 6.");
            failure_mssg2 = msg;

	} else if ( in_cache ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected status 6.");
            failure_mssg2 = msg;

#ifndef NDEBUG
        /* The clear_dirty_bits() callback is only called in debug mode --
         * thus we can only do our full test on the expected entry history
         * when debug is enabled.
         */
        } else if ( ( ! entry_ptr->deserialized ) ||
                    ( ! entry_ptr->cleared ) ||
		    ( entry_ptr->serialized ) ||
		    ( ! entry_ptr->destroyed ) ) {
#else
        /* When in procduction mode, the clear_dirty_bits() callback is
         * not called, so entry_ptr->cleared should never be set.
         */
        } else if ( ( ! entry_ptr->deserialized ) ||
                    ( entry_ptr->cleared ) ||
		    ( entry_ptr->serialized ) ||
		    ( ! entry_ptr->destroyed ) ) {
#endif /* NDEBUG */


            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected entry history 6.");
            failure_mssg2 = msg;

	}
    }

    if ( pass2 ) {

        takedown_cache2(cache_ptr, FALSE, FALSE);
    }

    if ( pass2 ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass2 ) {

        HDfprintf(stdout, "%s(): failure_mssg2 = \"%s\".\n",
                  fcn_name, failure_mssg2);
    }

    return;

} /* check_expunge_entry() */


/*-------------------------------------------------------------------------
 * Function:	check_multiple_read_protect()
 *
 * Purpose:	Verify that multiple, simultaneous read protects of a 
 * 		single entry perform as expectd.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              4/1/07
 *
 * Modifications:
 *
 * 		None.
 *
 *-------------------------------------------------------------------------
 */


static void
check_multiple_read_protect(void)
{
    const char * fcn_name = "check_multiple_read_protect()";
    H5C2_t * cache_ptr = NULL;
    test_entry_t * entry_ptr;

    TESTING("multiple read only protects on a single entry");

    pass2 = TRUE;

    /* allocate a cache.  Should succeed.
     *
     * Then to start with, proceed as follows:
     *
     * Read protect an entry.
     *
     * Then read protect the entry again.  Should succeed.
     *
     * Read protect yet again.  Should succeed.
     *
     * Unprotect with no changes, and then read protect twice again.
     * Should succeed.
     *
     * Now unprotect three times.  Should succeed.
     *
     * If stats are enabled, verify that correct stats are collected at
     * every step.
     *
     * Also, verify internal state of read protects at every step.
     */

    if ( pass2 ) {

        reset_entries2();

        cache_ptr = setup_cache2((size_t)(2 * 1024),
                                (size_t)(1 * 1024));

        entry_ptr = &((entries2[0])[0]);

        if ( ( entry_ptr->header.is_protected ) || 
 	     ( entry_ptr->header.is_read_only ) ||
	     ( entry_ptr->header.ro_ref_count != 0 ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected ro protected status 1.\n";
        }
    }

#if H5C2_COLLECT_CACHE_STATS
    if ( ( cache_ptr->write_protects[0] != 0 ) ||
	 ( cache_ptr->read_protects[0] != 0 ) ||
	 ( cache_ptr->max_read_protects[0] != 0 ) ) {

	pass2 = FALSE;
	failure_mssg2 = "Unexpected protect stats 1.\n";
    }
#endif /* H5C2_COLLECT_CACHE_STATS */

    if ( pass2 )
    {
        protect_entry_ro2(cache_ptr, 0, 0);

        if ( ( ! ( entry_ptr->header.is_protected ) ) || 
 	     ( ! ( entry_ptr->header.is_read_only ) ) ||
	     ( entry_ptr->header.ro_ref_count != 1 ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected ro protected status 2.\n";
        }
    }

#if H5C2_COLLECT_CACHE_STATS
    if ( ( cache_ptr->write_protects[0] != 0 ) ||
	 ( cache_ptr->read_protects[0] != 1 ) ||
	 ( cache_ptr->max_read_protects[0] != 1 ) ) {

	pass2 = FALSE;
	failure_mssg2 = "Unexpected protect stats 2.\n";
    }
#endif /* H5C2_COLLECT_CACHE_STATS */

    if ( pass2 )
    {
        protect_entry_ro2(cache_ptr, 0, 0);

        if ( ( ! ( entry_ptr->header.is_protected ) ) || 
 	     ( ! ( entry_ptr->header.is_read_only ) ) ||
	     ( entry_ptr->header.ro_ref_count != 2 ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected ro protected status 3.\n";
        }
    }

#if H5C2_COLLECT_CACHE_STATS
    if ( ( cache_ptr->write_protects[0] != 0 ) ||
	 ( cache_ptr->read_protects[0] != 2 ) ||
	 ( cache_ptr->max_read_protects[0] != 2 ) ) {

	pass2 = FALSE;
	failure_mssg2 = "Unexpected protect stats 3.\n";
    }
#endif /* H5C2_COLLECT_CACHE_STATS */

    if ( pass2 )
    {
        unprotect_entry2(cache_ptr, 0, 0, FALSE, H5C2__NO_FLAGS_SET);

        if ( ( ! ( entry_ptr->header.is_protected ) ) || 
 	     ( ! ( entry_ptr->header.is_read_only ) ) ||
	     ( entry_ptr->header.ro_ref_count != 1 ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected ro protected status 4.\n";
        }
    }

#if H5C2_COLLECT_CACHE_STATS
    if ( ( cache_ptr->write_protects[0] != 0 ) ||
	 ( cache_ptr->read_protects[0] != 2 ) ||
	 ( cache_ptr->max_read_protects[0] != 2 ) ) {

	pass2 = FALSE;
	failure_mssg2 = "Unexpected protect stats 4.\n";
    }
#endif /* H5C2_COLLECT_CACHE_STATS */

    if ( pass2 )
    {
        protect_entry_ro2(cache_ptr, 0, 0);

        if ( ( ! ( entry_ptr->header.is_protected ) ) || 
 	     ( ! ( entry_ptr->header.is_read_only ) ) ||
	     ( entry_ptr->header.ro_ref_count != 2 ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected ro protected status 5.\n";
        }
    }

#if H5C2_COLLECT_CACHE_STATS
    if ( ( cache_ptr->write_protects[0] != 0 ) ||
	 ( cache_ptr->read_protects[0] != 3 ) ||
	 ( cache_ptr->max_read_protects[0] != 2 ) ) {

	pass2 = FALSE;
	failure_mssg2 = "Unexpected protect stats 5.\n";
    }
#endif /* H5C2_COLLECT_CACHE_STATS */

    if ( pass2 )
    {
        protect_entry_ro2(cache_ptr, 0, 0);

        if ( ( ! ( entry_ptr->header.is_protected ) ) || 
 	     ( ! ( entry_ptr->header.is_read_only ) ) ||
	     ( entry_ptr->header.ro_ref_count != 3 ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected ro protected status 6.\n";
        }
    }

#if H5C2_COLLECT_CACHE_STATS
    if ( ( cache_ptr->write_protects[0] != 0 ) ||
	 ( cache_ptr->read_protects[0] != 4 ) ||
	 ( cache_ptr->max_read_protects[0] != 3 ) ) {

	pass2 = FALSE;
	failure_mssg2 = "Unexpected protect stats 6.\n";
    }
#endif /* H5C2_COLLECT_CACHE_STATS */

    if ( pass2 )
    {
        unprotect_entry2(cache_ptr, 0, 0, FALSE, H5C2__NO_FLAGS_SET);

        if ( ( ! ( entry_ptr->header.is_protected ) ) || 
 	     ( ! ( entry_ptr->header.is_read_only ) ) ||
	     ( entry_ptr->header.ro_ref_count != 2 ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected ro protected status 7.\n";
        }
    }

#if H5C2_COLLECT_CACHE_STATS
    if ( ( cache_ptr->write_protects[0] != 0 ) ||
	 ( cache_ptr->read_protects[0] != 4 ) ||
	 ( cache_ptr->max_read_protects[0] != 3 ) ) {

	pass2 = FALSE;
	failure_mssg2 = "Unexpected protect stats 7.\n";
    }
#endif /* H5C2_COLLECT_CACHE_STATS */

    if ( pass2 )
    {
        unprotect_entry2(cache_ptr, 0, 0, FALSE, H5C2__NO_FLAGS_SET);

        if ( ( ! ( entry_ptr->header.is_protected ) ) || 
 	     ( ! ( entry_ptr->header.is_read_only ) ) ||
	     ( entry_ptr->header.ro_ref_count != 1 ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected ro protected status 8.\n";
        }
    }

#if H5C2_COLLECT_CACHE_STATS
    if ( ( cache_ptr->write_protects[0] != 0 ) ||
	 ( cache_ptr->read_protects[0] != 4 ) ||
	 ( cache_ptr->max_read_protects[0] != 3 ) ) {

	pass2 = FALSE;
	failure_mssg2 = "Unexpected protect stats 8.\n";
    }
#endif /* H5C2_COLLECT_CACHE_STATS */

    if ( pass2 )
    {
        unprotect_entry2(cache_ptr, 0, 0, FALSE, H5C2__NO_FLAGS_SET);

        if ( ( entry_ptr->header.is_protected ) || 
 	     ( entry_ptr->header.is_read_only ) ||
	     ( entry_ptr->header.ro_ref_count != 0 ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected ro protected status 9.\n";
        }
    }

#if H5C2_COLLECT_CACHE_STATS
    if ( ( cache_ptr->write_protects[0] != 0 ) ||
	 ( cache_ptr->read_protects[0] != 4 ) ||
	 ( cache_ptr->max_read_protects[0] != 3 ) ) {

	pass2 = FALSE;
	failure_mssg2 = "Unexpected protect stats 9.\n";
    }
#endif /* H5C2_COLLECT_CACHE_STATS */


    /* If we get this far, do a write protect and unprotect to verify 
     * that the stats are getting collected properly here as well.
     */

    if ( pass2 )
    {
        protect_entry2(cache_ptr, 0, 0);

        if ( ( ! ( entry_ptr->header.is_protected ) ) || 
 	     ( entry_ptr->header.is_read_only ) ||
	     ( entry_ptr->header.ro_ref_count != 0 ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected ro protected status 10.\n";
        }
    }

#if H5C2_COLLECT_CACHE_STATS
    if ( ( cache_ptr->write_protects[0] != 1 ) ||
	 ( cache_ptr->read_protects[0] != 4 ) ||
	 ( cache_ptr->max_read_protects[0] != 3 ) ) {

	pass2 = FALSE;
	failure_mssg2 = "Unexpected protect stats 10.\n";
    }
#endif /* H5C2_COLLECT_CACHE_STATS */

    if ( pass2 )
    {
        unprotect_entry2(cache_ptr, 0, 0, FALSE, H5C2__NO_FLAGS_SET);

        if ( ( entry_ptr->header.is_protected ) || 
 	     ( entry_ptr->header.is_read_only ) ||
	     ( entry_ptr->header.ro_ref_count != 0 ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected ro protected status 11.\n";
        }
    }

#if H5C2_COLLECT_CACHE_STATS
    if ( ( cache_ptr->write_protects[0] != 1 ) ||
	 ( cache_ptr->read_protects[0] != 4 ) ||
	 ( cache_ptr->max_read_protects[0] != 3 ) ) {

	pass2 = FALSE;
	failure_mssg2 = "Unexpected protect stats 11.\n";
    }
#endif /* H5C2_COLLECT_CACHE_STATS */


    /* Finally, mix things up a little, using a mix of reads and 
     * and writes on different entries.  Also include a pin to verify
     * that it works as well.
     *
     * Stats are looking OK, so we will only test them one more time
     * at the end to ensure that all is at it should be.
     */

    if ( pass2 ) {

        protect_entry2(cache_ptr, 0, 2);		/* (0,2) write */
        protect_entry_ro2(cache_ptr, 0, 4);      /* (0,4) read only (1) */
	protect_entry2(cache_ptr, 0, 6);		/* (0,6) write */

        unprotect_entry2(cache_ptr, 0, 2, FALSE, /* (0,2) unprotect */
			H5C2__NO_FLAGS_SET);

        protect_entry_ro2(cache_ptr, 0, 2);	/* (0,2) read only (1) */
	protect_entry2(cache_ptr, 0, 1);         /* (0,1) write */
        protect_entry_ro2(cache_ptr, 0, 4);	/* (0,4) read only (2) */
	protect_entry2(cache_ptr, 0, 0);		/* (0,0) write */
        protect_entry_ro2(cache_ptr, 0, 2);	/* (0,2) read only (2) */

        unprotect_entry2(cache_ptr, 0, 2, FALSE, /* (0,2) read only (1) pin */
			H5C2__PIN_ENTRY_FLAG);
        unprotect_entry2(cache_ptr, 0, 6, FALSE, /* (0,6) unprotect */
			H5C2__NO_FLAGS_SET);

        protect_entry_ro2(cache_ptr, 0, 4);	/* (0,4) read only (3) */

        unprotect_entry2(cache_ptr, 0, 2, FALSE, /* (0,2) unprotect */
			H5C2__NO_FLAGS_SET);
        unprotect_entry2(cache_ptr, 0, 1, FALSE, /* (0,1) unprotect */
			H5C2__NO_FLAGS_SET);

	if ( pass2 ) {

	    entry_ptr = &((entries2[0])[4]);

	    if ( H5C2_pin_protected_entry(cache_ptr, (void *)entry_ptr) < 0 ) {

	        pass2 = FALSE;
	        failure_mssg2 = "H5C2_pin_protected_entry() failed.\n";

	    } else if ( ! (entry_ptr->header.is_pinned) ) {

	        pass2 = FALSE;
	        failure_mssg2 = "entry (0,4) not pinned.\n";

	    } else {

		/* keep test bed sanity checks happy */
		entry_ptr->is_pinned = TRUE;

	    }
	}

        unprotect_entry2(cache_ptr, 0, 4, FALSE, /* (0,4) read only (2) */
			H5C2__NO_FLAGS_SET);
        unprotect_entry2(cache_ptr, 0, 4, FALSE, /* (0,4) read only (1) */
			H5C2__UNPIN_ENTRY_FLAG);

        if ( ( pass2 ) && ( entry_ptr->header.is_pinned ) ) {

            pass2 = FALSE;
            failure_mssg2 = "enty (0,4) still pinned.\n";

	}

        unprotect_entry2(cache_ptr, 0, 4, FALSE, /* (0,4) unprotect */
			H5C2__NO_FLAGS_SET);
        unprotect_entry2(cache_ptr, 0, 0, FALSE, /* (0,0) unprotect */
			H5C2__NO_FLAGS_SET);

	unpin_entry2(cache_ptr, 0, 2);
    }

#if H5C2_COLLECT_CACHE_STATS
    if ( ( cache_ptr->write_protects[0] != 5 ) ||
	 ( cache_ptr->read_protects[0] != 9 ) ||
	 ( cache_ptr->max_read_protects[0] != 3 ) ) {

	pass2 = FALSE;
	failure_mssg2 = "Unexpected protect stats 11.\n";
    }
#endif /* H5C2_COLLECT_CACHE_STATS */


    if ( pass2 ) {

        takedown_cache2(cache_ptr, FALSE, FALSE);
    }

    if ( pass2 ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass2 ) {

        HDfprintf(stdout, "%s: failure_mssg2 = \"%s\".\n",
                  fcn_name, failure_mssg2);
    }

    return;

} /* check_multiple_read_protect() */


/*-------------------------------------------------------------------------
 * Function:	check_rename_entry()
 *
 * Purpose:	Verify that H5C2_rename_entry behaves as expected.  In
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
    H5C2_t *      cache_ptr = NULL;
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

    TESTING("H5C2_rename_entry() functionality");

    pass2 = TRUE;

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

    if ( pass2 ) {

        reset_entries2();

        cache_ptr = setup_cache2((size_t)(2 * 1024 * 1024),
                                (size_t)(1 * 1024 * 1024));
    }

    i = 0;
    while ( ( pass2 ) && ( i < 4 ) )
    {
        check_rename_entry__run_test(cache_ptr, i, &(test_specs[i]));
	i++;
    }

    if ( pass2 ) {

        takedown_cache2(cache_ptr, FALSE, FALSE);
    }

    if ( pass2 ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass2 ) {

        HDfprintf(stdout, "%s(): failure_mssg2 = \"%s\".\n",
                  fcn_name, failure_mssg2);
    }

    return;

} /* check_rename_entry() */


/*-------------------------------------------------------------------------
 * Function:    check_rename_entry__run_test()
 *
 * Purpose:     Run a rename entry test.
 *
 *		Do nothing if pass2 is FALSE on entry.
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
check_rename_entry__run_test(H5C2_t * cache_ptr,
                             int test_num,
                             struct rename_entry_test_spec * spec_ptr)
{
    /* const char *   fcn_name = "check_rename_entry__run_test"; */
    static char    msg[128];
    unsigned int   flags = H5C2__NO_FLAGS_SET;
    test_entry_t * base_addr;
    test_entry_t * entry_ptr = NULL;
    H5C2_cache_entry_t * test_ptr = NULL;

    if ( cache_ptr == NULL ) {

        pass2 = FALSE;
        HDsnprintf(msg, (size_t)128,
                   "cache_ptr NULL on entry to rename test #%d.",
                   test_num);
        failure_mssg2 = msg;

    } else if ( spec_ptr == NULL ) {

        pass2 = FALSE;
        HDsnprintf(msg, (size_t)128,
                   "spec_ptr NULL on entry to rename test #%d.",
                   test_num);
        failure_mssg2 = msg;

    }

    if ( pass2 ) {

        base_addr = entries2[spec_ptr->entry_type];
        entry_ptr = &(base_addr[spec_ptr->entry_index]);

        if ( ( entry_ptr->self != entry_ptr ) ||
             ( ( entry_ptr->cache_ptr != cache_ptr ) &&
               ( entry_ptr->cache_ptr != NULL ) ) ||
             ( ! ( entry_ptr->at_main_addr ) ) ||
             ( entry_ptr->addr != entry_ptr->main_addr ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "bad entry_ptr in rename test #%d.",
                       test_num);
            failure_mssg2 = msg;

        } else if ( spec_ptr->is_pinned ) {

            flags |= H5C2__PIN_ENTRY_FLAG;
        }
    }

    protect_entry2(cache_ptr, spec_ptr->entry_type, spec_ptr->entry_index);

    unprotect_entry2(cache_ptr, spec_ptr->entry_type, spec_ptr->entry_index,
                    (int)(spec_ptr->is_dirty), flags);

    rename_entry2(cache_ptr, spec_ptr->entry_type, spec_ptr->entry_index, FALSE);

    if ( pass2 ) {

        /* verify that the rename took place, and that the cache's internal
         * structures are as expected.  Note that some sanity checking is
         * done by rename_entry2(), so we don't have to repeat it here.
         */

        if ( spec_ptr->is_pinned ) {

            if ( ! ( entry_ptr->header.is_pinned ) ) {

                pass2 = FALSE;
                HDsnprintf(msg, (size_t)128,
                           "Pinned entry not pinned after rename in test #%d.",
                           test_num);
                failure_mssg2 = msg;
            }

            if ( pass2 ) {

                test_ptr = cache_ptr->pel_head_ptr;

                while ( ( test_ptr != NULL ) &&
                        ( test_ptr != (H5C2_cache_entry_t *)entry_ptr ) )
                {
                    test_ptr = test_ptr->next;
                }

                if ( test_ptr == NULL ) {

                    pass2 = FALSE;
                    HDsnprintf(msg, (size_t)128,
                           "Pinned entry not in pel after rename in test #%d.",
                           test_num);
                    failure_mssg2 = msg;
                }
            }

            unpin_entry2(cache_ptr, spec_ptr->entry_type, 
			 spec_ptr->entry_index);

        } else {

            if ( entry_ptr->header.is_pinned ) {

                pass2 = FALSE;
                HDsnprintf(msg, (size_t)128,
                           "Unpinned entry pinned after rename in test #%d.",
                           test_num);
                failure_mssg2 = msg;
            }

            if ( ( entry_ptr->header.prev != NULL ) ||
                 ( cache_ptr->LRU_head_ptr != (H5C2_cache_entry_t *)entry_ptr ) )
            {
                pass2 = FALSE;
                HDsnprintf(msg, (size_t)128,
                           "Entry not at head of LRU after rename in test #%d.",
                           test_num);
                failure_mssg2 = msg;
            }
        }
    }

    /* put the entry back where it started from */
    rename_entry2(cache_ptr, spec_ptr->entry_type, spec_ptr->entry_index, TRUE);

    return;

} /* check_rename_entry__run_test() */


/*-------------------------------------------------------------------------
 * Function:	check_pin_protected_entry()
 *
 * Purpose:	Verify that H5C2_pin_protected_entry behaves as expected.
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
    H5C2_t *       cache_ptr = NULL;
    test_entry_t * base_addr;
    test_entry_t * entry_ptr;

    TESTING("H5C2_pin_protected_entry() functionality");

    pass2 = TRUE;

    /* Create a cache, protect an entry, and then use H5C2_pin_protected_entry()
     * to pin it.  Verify that the entry is in fact pined.  Unprotect the entry
     * to unpin it, and then destroy the cache.
     */

    if ( pass2 ) {

        reset_entries2();

        cache_ptr = setup_cache2((size_t)(2 * 1024 * 1024),
                                (size_t)(1 * 1024 * 1024));
    }

    protect_entry2(cache_ptr, 0, 0);

    if ( pass2 ) {

        base_addr = entries2[0];
        entry_ptr = &(base_addr[0]);

	result = H5C2_pin_protected_entry(cache_ptr, (void *)entry_ptr);

	if ( result < 0 ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "H5C2_pin_protected_entry() reports failure.");
            failure_mssg2 = msg;

	} else if ( ! ( entry_ptr->header.is_pinned ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "entry not pinned when it should be.");
            failure_mssg2 = msg;

	} else {

	    entry_ptr->is_pinned = TRUE;
	}
    }

    unprotect_entry2(cache_ptr, 0, 0, FALSE, H5C2__UNPIN_ENTRY_FLAG);

    if ( pass2 ) {

        takedown_cache2(cache_ptr, FALSE, FALSE);
    }

    if ( pass2 ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass2 ) {

        HDfprintf(stdout, "%s(): failure_mssg2 = \"%s\".\n",
                  fcn_name, failure_mssg2);
    }

    return;

} /* check_pin_protected_entry() */


/*-------------------------------------------------------------------------
 * Function:	check_resize_entry()
 *
 * Purpose:	Verify that H5C2_resize_entry() and H5C2_unprotect() resize
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
    H5C2_t *        cache_ptr = NULL;
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
     * H5C2_resize_entry to reduce its size.  Verify that the entry
     * and cache have the expected sizes.
     *
     * Use H5C2_resize_entry again to restore the entry to its original
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

    pass2 = TRUE;

    /* tests with only one entry in the cache: */

    if ( pass2 ) {

        reset_entries2();

        cache_ptr = setup_cache2((size_t)(2 * 1024 * 1024),
                                (size_t)(1 * 1024 * 1024));

        base_addr = entries2[LARGE_ENTRY_TYPE];
        entry_ptr = &(base_addr[0]);
	entry_size = LARGE_ENTRY_SIZE;
    }

    if ( pass2 ) {

	if ( ( cache_ptr->index_len != 0 ) ||
	     ( cache_ptr->index_size != 0 ) ||
	     ( cache_ptr->slist_len != 0 ) ||
	     ( cache_ptr->slist_size != 0 ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected cache status 1.");
            failure_mssg2 = msg;

	}
    }

    protect_entry2(cache_ptr, LARGE_ENTRY_TYPE, 0);

    if ( pass2 ) {

	if ( ( cache_ptr->index_len != 1 ) ||
	     ( cache_ptr->index_size != LARGE_ENTRY_SIZE ) ||
	     ( cache_ptr->slist_len != 0 ) ||
	     ( cache_ptr->slist_size != 0 ) ) {


            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected cache status 2.");
            failure_mssg2 = msg;

	}
    }

    if ( pass2 ) {

        result = H5C2_get_entry_status(cache_ptr, entry_ptr->addr, 
			              &reported_entry_size, &in_cache, 
				      &is_dirty, &is_protected, &is_pinned);

	if ( result < 0 ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "H5AC_get_entry_status() reports failure 1.");
            failure_mssg2 = msg;

	} else if ( !in_cache || is_dirty || !is_protected || is_pinned ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected status 1.");
            failure_mssg2 = msg;

        } else if ( ( ! entry_ptr->deserialized ) ||
                    ( entry_ptr->cleared ) ||
		    ( entry_ptr->serialized ) ||
		    ( entry_ptr->destroyed ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected entry history 1.");
            failure_mssg2 = msg;

	}
    }

    if ( pass2 ) {

        result = H5C2_unprotect(cache_ptr, H5P_DATASET_XFER_DEFAULT,
			       &(types2[LARGE_ENTRY_TYPE]), entry_ptr->addr,
			       (void *)entry_ptr, 
			       H5C2__SIZE_CHANGED_FLAG | H5C2__DIRTIED_FLAG,
			       (LARGE_ENTRY_SIZE / 2));

	if ( result < 0 ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "H5C2_unprotect() reports failure 1.");
            failure_mssg2 = msg;

	} else {

	    /* tidy up so we play nice with the standard protect / unprotect
	     * calls.
	     */
	    entry_ptr->is_protected = FALSE;
	    entry_ptr->is_dirty = TRUE;
	    entry_ptr->size = LARGE_ENTRY_SIZE / 2;
	}
    }

    if ( pass2 ) {

	if ( ( cache_ptr->index_len != 1 ) ||
	     ( cache_ptr->index_size != (LARGE_ENTRY_SIZE / 2) ) ||
	     ( cache_ptr->slist_len != 1 ) ||
	     ( cache_ptr->slist_size != (LARGE_ENTRY_SIZE / 2) ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected cache status 3.");
            failure_mssg2 = msg;

	}
    }

    if ( pass2 ) {

        result = H5C2_get_entry_status(cache_ptr, entry_ptr->addr, 
			              &reported_entry_size, &in_cache, 
				      &is_dirty, &is_protected, &is_pinned);

	if ( result < 0 ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "H5AC_get_entry_status() reports failure 2.");
            failure_mssg2 = msg;

	} else if ( !in_cache || !is_dirty || is_protected || is_pinned ||
		    ( reported_entry_size != (LARGE_ENTRY_SIZE / 2) ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected status 2.");
            failure_mssg2 = msg;

        } else if ( ( ! entry_ptr->deserialized ) ||
                    ( entry_ptr->cleared ) ||
		    ( entry_ptr->serialized ) ||
		    ( entry_ptr->destroyed ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected entry history 2.");
            failure_mssg2 = msg;

	}
    }

    protect_entry2(cache_ptr, LARGE_ENTRY_TYPE, 0);

    if ( pass2 ) {

        result = H5C2_unprotect(cache_ptr, H5P_DATASET_XFER_DEFAULT,
			       &(types2[LARGE_ENTRY_TYPE]), entry_ptr->addr,
			       (void *)entry_ptr, 
			       (H5C2__DIRTIED_FLAG | H5C2__SIZE_CHANGED_FLAG),
			       LARGE_ENTRY_SIZE);

	if ( result < 0 ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "H5C2_unprotect() reports failure 2.");
            failure_mssg2 = msg;

	} else {

	    /* tidy up so we play nice with the standard protect / unprotect
	     * calls.
	     */
	    entry_ptr->is_protected = FALSE;
	    entry_ptr->is_dirty = TRUE;
	    entry_ptr->size = LARGE_ENTRY_SIZE;
	}
    }

    if ( pass2 ) {

	if ( ( cache_ptr->index_len != 1 ) ||
	     ( cache_ptr->index_size != LARGE_ENTRY_SIZE ) ||
	     ( cache_ptr->slist_len != 1 ) ||
	     ( cache_ptr->slist_size != LARGE_ENTRY_SIZE ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected cache status 4.");
            failure_mssg2 = msg;

	}
    }

    if ( pass2 ) {

        result = H5C2_get_entry_status(cache_ptr, entry_ptr->addr, 
			              &reported_entry_size, &in_cache, 
				      &is_dirty, &is_protected, &is_pinned);

	if ( result < 0 ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "H5AC_get_entry_status() reports failure 3.");
            failure_mssg2 = msg;

	} else if ( !in_cache || !is_dirty || is_protected || is_pinned ||
		    ( reported_entry_size != LARGE_ENTRY_SIZE ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected status 3.");
            failure_mssg2 = msg;

        } else if ( ( ! entry_ptr->deserialized ) ||
                    ( entry_ptr->cleared ) ||
		    ( entry_ptr->serialized ) ||
		    ( entry_ptr->destroyed ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected entry history 3.");
            failure_mssg2 = msg;

	}
    }

    protect_entry2(cache_ptr, LARGE_ENTRY_TYPE, 0);

    unprotect_entry2(cache_ptr, LARGE_ENTRY_TYPE, 0, FALSE, H5C2__PIN_ENTRY_FLAG);

    if ( pass2 ) {

        result = H5C2_resize_pinned_entry(cache_ptr, (void *)entry_ptr, 
			                 (LARGE_ENTRY_SIZE / 4));

	if ( result < 0 ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, 
		       "H5C2_resize_pinned_entry() reports failure 1.");
            failure_mssg2 = msg;

	} 
    }

    if ( pass2 ) {

	if ( ( cache_ptr->index_len != 1 ) ||
	     ( cache_ptr->index_size != (LARGE_ENTRY_SIZE / 4) ) ||
	     ( cache_ptr->slist_len != 1 ) ||
	     ( cache_ptr->slist_size != (LARGE_ENTRY_SIZE / 4) ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected cache status 5.");
            failure_mssg2 = msg;

	}
    }

    if ( pass2 ) {

        result = H5C2_get_entry_status(cache_ptr, entry_ptr->addr, 
			              &reported_entry_size, &in_cache, 
				      &is_dirty, &is_protected, &is_pinned);

	if ( result < 0 ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "H5AC_get_entry_status() reports failure 4.");
            failure_mssg2 = msg;

	} else if ( !in_cache || !is_dirty || is_protected || ! is_pinned ||
		    ( reported_entry_size != (LARGE_ENTRY_SIZE / 4) ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected status 4.");
            failure_mssg2 = msg;

        } else if ( ( ! entry_ptr->deserialized ) ||
                    ( entry_ptr->cleared ) ||
		    ( entry_ptr->serialized ) ||
		    ( entry_ptr->destroyed ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected entry history 4.");
            failure_mssg2 = msg;

	}
    }

    if ( pass2 ) {

        result = H5C2_resize_pinned_entry(cache_ptr, (void *)entry_ptr, 
			                 LARGE_ENTRY_SIZE);

	if ( result < 0 ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, 
		       "H5C2_resize_pinned_entry() reports failure 2.");
            failure_mssg2 = msg;

	} 
    }

    if ( pass2 ) {

	if ( ( cache_ptr->index_len != 1 ) ||
	     ( cache_ptr->index_size != LARGE_ENTRY_SIZE ) ||
	     ( cache_ptr->slist_len != 1 ) ||
	     ( cache_ptr->slist_size != LARGE_ENTRY_SIZE ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected cache status 6.");
            failure_mssg2 = msg;

	}
    }

    if ( pass2 ) {

        result = H5C2_get_entry_status(cache_ptr, entry_ptr->addr, 
			              &reported_entry_size, &in_cache, 
				      &is_dirty, &is_protected, &is_pinned);

	if ( result < 0 ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "H5AC_get_entry_status() reports failure 5.");
            failure_mssg2 = msg;

	} else if ( !in_cache || !is_dirty || is_protected || ! is_pinned ||
		    ( reported_entry_size != LARGE_ENTRY_SIZE ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected status 5.");
            failure_mssg2 = msg;

        } else if ( ( ! entry_ptr->deserialized ) ||
                    ( entry_ptr->cleared ) ||
		    ( entry_ptr->serialized ) ||
		    ( entry_ptr->destroyed ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected entry history 5.");
            failure_mssg2 = msg;

	}
    }

    protect_entry2(cache_ptr, LARGE_ENTRY_TYPE, 0);

    unprotect_entry2(cache_ptr, LARGE_ENTRY_TYPE, 0, FALSE, 
		    H5C2__UNPIN_ENTRY_FLAG | H5C2__DELETED_FLAG);

    if ( pass2 ) {

        result = H5C2_get_entry_status(cache_ptr, entry_ptr->addr, &entry_size,
                                      &in_cache, &is_dirty, &is_protected, 
				      &is_pinned);

	if ( result < 0 ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "H5AC_get_entry_status() reports failure 6.");
            failure_mssg2 = msg;

	} else if ( in_cache ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected status 6.");
            failure_mssg2 = msg;

#ifndef NDEBUG
        /* The clear_dirty_bits() callback is only called in debug mode --
         * thus we can only do our full test on the expected entry history
         * when debug is enabled.
         */
        } else if ( ( ! entry_ptr->deserialized ) ||
                    ( ! entry_ptr->cleared ) ||
		    ( entry_ptr->serialized ) ||
		    ( ! entry_ptr->destroyed ) ) {
#else
        /* When in procduction mode, the clear_dirty_bits() callback is
         * not called, so entry_ptr->cleared should never be set.
         */
        } else if ( ( ! entry_ptr->deserialized ) ||
                    ( entry_ptr->cleared ) ||
		    ( entry_ptr->serialized ) ||
		    ( ! entry_ptr->destroyed ) ) {
#endif /* NDEBUG */


            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected entry history 6.");
            failure_mssg2 = msg;

	}
    }

    if ( pass2 ) {

	if ( ( cache_ptr->index_len != 0 ) ||
	     ( cache_ptr->index_size != 0 ) ||
	     ( cache_ptr->slist_len != 0 ) ||
	     ( cache_ptr->slist_size != 0 ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected cache status 7.");
            failure_mssg2 = msg;

	}
    }


    /* now repreat the above tests with several entries in the cache: */

    if ( pass2 ) {

	if ( ( cache_ptr->index_len != 0 ) ||
	     ( cache_ptr->index_size != 0 ) ||
	     ( cache_ptr->slist_len != 0 ) ||
	     ( cache_ptr->slist_size != 0 ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected cache status 8.");
            failure_mssg2 = msg;

	}
        base_addr = entries2[LARGE_ENTRY_TYPE];
        entry_ptr = &(base_addr[3]);
	entry_size = LARGE_ENTRY_SIZE;
    }

    protect_entry2(cache_ptr, LARGE_ENTRY_TYPE, 0);
    unprotect_entry2(cache_ptr, LARGE_ENTRY_TYPE, 0, FALSE, H5C2__NO_FLAGS_SET);

    protect_entry2(cache_ptr, LARGE_ENTRY_TYPE, 1);
    unprotect_entry2(cache_ptr, LARGE_ENTRY_TYPE, 1, TRUE, H5C2__NO_FLAGS_SET);

    protect_entry2(cache_ptr, LARGE_ENTRY_TYPE, 2);
    unprotect_entry2(cache_ptr, LARGE_ENTRY_TYPE, 2, FALSE, H5C2__NO_FLAGS_SET);

    if ( pass2 ) {

	if ( ( cache_ptr->index_len != 3 ) ||
	     ( cache_ptr->index_size != 3 * LARGE_ENTRY_SIZE ) ||
	     ( cache_ptr->slist_len != 1 ) ||
	     ( cache_ptr->slist_size != LARGE_ENTRY_SIZE ) ) {


            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected cache status 9.");
            failure_mssg2 = msg;

	}
    }

    protect_entry2(cache_ptr, LARGE_ENTRY_TYPE, 3);

    if ( pass2 ) {

	if ( ( cache_ptr->index_len != 4 ) ||
	     ( cache_ptr->index_size != 4 * LARGE_ENTRY_SIZE ) ||
	     ( cache_ptr->slist_len != 1 ) ||
	     ( cache_ptr->slist_size != LARGE_ENTRY_SIZE ) ) {


            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected cache status 10.");
            failure_mssg2 = msg;

	}
    }

    if ( pass2 ) {

        result = H5C2_get_entry_status(cache_ptr, entry_ptr->addr, 
			              &reported_entry_size, &in_cache, 
				      &is_dirty, &is_protected, &is_pinned);

	if ( result < 0 ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "H5AC_get_entry_status() reports failure 7.");
            failure_mssg2 = msg;

	} else if ( !in_cache || is_dirty || !is_protected || is_pinned ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected status 7.");
            failure_mssg2 = msg;

        } else if ( ( ! entry_ptr->deserialized ) ||
                    ( entry_ptr->cleared ) ||
		    ( entry_ptr->serialized ) ||
		    ( entry_ptr->destroyed ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected entry history 7.");
            failure_mssg2 = msg;

	}
    }

    if ( pass2 ) {

        result = H5C2_unprotect(cache_ptr, H5P_DATASET_XFER_DEFAULT,
			       &(types2[LARGE_ENTRY_TYPE]), entry_ptr->addr,
			       (void *)entry_ptr, 
			       H5C2__SIZE_CHANGED_FLAG | H5C2__DIRTIED_FLAG,
			       (LARGE_ENTRY_SIZE / 2));

	if ( result < 0 ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "H5C2_unprotect() reports failure 3.");
            failure_mssg2 = msg;

	} else {

	    /* tidy up so we play nice with the standard protect / unprotect
	     * calls.
	     */
	    entry_ptr->is_protected = FALSE;
	    entry_ptr->is_dirty = TRUE;
	    entry_ptr->size = LARGE_ENTRY_SIZE / 2;
	}
    }

    if ( pass2 ) {

	if ( ( cache_ptr->index_len != 4 ) ||
	     ( cache_ptr->index_size != 
	       ((3 * LARGE_ENTRY_SIZE) + (LARGE_ENTRY_SIZE / 2)) ) ||
	     ( cache_ptr->slist_len != 2 ) ||
	     ( cache_ptr->slist_size != 
	       (LARGE_ENTRY_SIZE + (LARGE_ENTRY_SIZE / 2)) ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected cache status 11.");
            failure_mssg2 = msg;

	}
    }

    if ( pass2 ) {

        result = H5C2_get_entry_status(cache_ptr, entry_ptr->addr, 
			              &reported_entry_size, &in_cache, 
				      &is_dirty, &is_protected, &is_pinned);

	if ( result < 0 ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "H5AC_get_entry_status() reports failure 8.");
            failure_mssg2 = msg;

	} else if ( !in_cache || !is_dirty || is_protected || is_pinned ||
		    ( reported_entry_size != (LARGE_ENTRY_SIZE / 2) ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected status 8.");
            failure_mssg2 = msg;

        } else if ( ( ! entry_ptr->deserialized ) ||
                    ( entry_ptr->cleared ) ||
		    ( entry_ptr->serialized ) ||
		    ( entry_ptr->destroyed ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected entry history 8.");
            failure_mssg2 = msg;

	}
    }

    protect_entry2(cache_ptr, LARGE_ENTRY_TYPE, 3);

    if ( pass2 ) {

        result = H5C2_unprotect(cache_ptr, H5P_DATASET_XFER_DEFAULT,
			       &(types2[LARGE_ENTRY_TYPE]), entry_ptr->addr,
			       (void *)entry_ptr, 
			       (H5C2__DIRTIED_FLAG | H5C2__SIZE_CHANGED_FLAG),
			       LARGE_ENTRY_SIZE);

	if ( result < 0 ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "H5C2_unprotect() reports failure 4.");
            failure_mssg2 = msg;

	} else {

	    /* tidy up so we play nice with the standard protect / unprotect
	     * calls.
	     */
	    entry_ptr->is_protected = FALSE;
	    entry_ptr->is_dirty = TRUE;
	    entry_ptr->size = LARGE_ENTRY_SIZE;
	}
    }

    if ( pass2 ) {

	if ( ( cache_ptr->index_len != 4 ) ||
	     ( cache_ptr->index_size != 4 * LARGE_ENTRY_SIZE ) ||
	     ( cache_ptr->slist_len != 2 ) ||
	     ( cache_ptr->slist_size != 2 * LARGE_ENTRY_SIZE ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected cache status 12.");
            failure_mssg2 = msg;

	}
    }

    if ( pass2 ) {

        result = H5C2_get_entry_status(cache_ptr, entry_ptr->addr, 
			              &reported_entry_size, &in_cache, 
				      &is_dirty, &is_protected, &is_pinned);

	if ( result < 0 ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "H5AC_get_entry_status() reports failure 9.");
            failure_mssg2 = msg;

	} else if ( !in_cache || !is_dirty || is_protected || is_pinned ||
		    ( reported_entry_size != LARGE_ENTRY_SIZE ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected status 9.");
            failure_mssg2 = msg;

        } else if ( ( ! entry_ptr->deserialized ) ||
                    ( entry_ptr->cleared ) ||
		    ( entry_ptr->serialized ) ||
		    ( entry_ptr->destroyed ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected entry history 9.");
            failure_mssg2 = msg;

	}
    }

    protect_entry2(cache_ptr, LARGE_ENTRY_TYPE, 3);

    unprotect_entry2(cache_ptr, LARGE_ENTRY_TYPE, 3, FALSE, H5C2__PIN_ENTRY_FLAG);

    if ( pass2 ) {

        result = H5C2_resize_pinned_entry(cache_ptr, (void *)entry_ptr, 
			                 (LARGE_ENTRY_SIZE / 4));

	if ( result < 0 ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, 
		       "H5C2_resize_pinned_entry() reports failure 3.");
            failure_mssg2 = msg;

	} 
    }

    if ( pass2 ) {

	if ( ( cache_ptr->index_len != 4 ) ||
	     ( cache_ptr->index_size != 
	       ((3 * LARGE_ENTRY_SIZE) + (LARGE_ENTRY_SIZE / 4)) ) ||
	     ( cache_ptr->slist_len != 2 ) ||
	     ( cache_ptr->slist_size != 
	       (LARGE_ENTRY_SIZE + (LARGE_ENTRY_SIZE / 4)) ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected cache status 13.");
            failure_mssg2 = msg;

	}
    }

    if ( pass2 ) {

        result = H5C2_get_entry_status(cache_ptr, entry_ptr->addr, 
			              &reported_entry_size, &in_cache, 
				      &is_dirty, &is_protected, &is_pinned);

	if ( result < 0 ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "H5AC_get_entry_status() reports failure 10.");
            failure_mssg2 = msg;

	} else if ( !in_cache || !is_dirty || is_protected || ! is_pinned ||
		    ( reported_entry_size != (LARGE_ENTRY_SIZE / 4) ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected status 10.");
            failure_mssg2 = msg;

        } else if ( ( ! entry_ptr->deserialized ) ||
                    ( entry_ptr->cleared ) ||
		    ( entry_ptr->serialized ) ||
		    ( entry_ptr->destroyed ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected entry history 10.");
            failure_mssg2 = msg;

	}
    }

    if ( pass2 ) {

        result = H5C2_resize_pinned_entry(cache_ptr, (void *)entry_ptr, 
			                 LARGE_ENTRY_SIZE);

	if ( result < 0 ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, 
		       "H5C2_resize_pinned_entry() reports failure 4.");
            failure_mssg2 = msg;

	} 
    }

    if ( pass2 ) {

	if ( ( cache_ptr->index_len != 4 ) ||
	     ( cache_ptr->index_size != (4 * LARGE_ENTRY_SIZE) ) ||
	     ( cache_ptr->slist_len != 2 ) ||
	     ( cache_ptr->slist_size != (2 * LARGE_ENTRY_SIZE) ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected cache status 14.");
            failure_mssg2 = msg;

	}
    }

    if ( pass2 ) {

        result = H5C2_get_entry_status(cache_ptr, entry_ptr->addr, 
			              &reported_entry_size, &in_cache, 
				      &is_dirty, &is_protected, &is_pinned);

	if ( result < 0 ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "H5AC_get_entry_status() reports failure 11.");
            failure_mssg2 = msg;

	} else if ( !in_cache || !is_dirty || is_protected || ! is_pinned ||
		    ( reported_entry_size != LARGE_ENTRY_SIZE ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected status 11.");
            failure_mssg2 = msg;

        } else if ( ( ! entry_ptr->deserialized ) ||
                    ( entry_ptr->cleared ) ||
		    ( entry_ptr->serialized ) ||
		    ( entry_ptr->destroyed ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected entry history 11.");
            failure_mssg2 = msg;

	}
    }

    protect_entry2(cache_ptr, LARGE_ENTRY_TYPE, 3);

    unprotect_entry2(cache_ptr, LARGE_ENTRY_TYPE, 3, FALSE, 
		    H5C2__UNPIN_ENTRY_FLAG | H5C2__DELETED_FLAG);

    if ( pass2 ) {

        result = H5C2_get_entry_status(cache_ptr, entry_ptr->addr, &entry_size,
                                      &in_cache, &is_dirty, &is_protected, 
				      &is_pinned);

	if ( result < 0 ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "H5AC_get_entry_status() reports failure 12.");
            failure_mssg2 = msg;

	} else if ( in_cache ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected status 12.");
            failure_mssg2 = msg;

#ifndef NDEBUG
        /* The clear_dirty_bits() callback is only called in debug mode --
         * thus we can only do our full test on the expected entry history
         * when debug is enabled.
         */
        } else if ( ( ! entry_ptr->deserialized ) ||
                    ( ! entry_ptr->cleared ) ||
		    ( entry_ptr->serialized ) ||
		    ( ! entry_ptr->destroyed ) ) {
#else
        /* When in procduction mode, the clear_dirty_bits() callback is
         * not called, so entry_ptr->cleared should never be set.
         */
        } else if ( ( ! entry_ptr->deserialized ) ||
                    ( entry_ptr->cleared ) ||
		    ( entry_ptr->serialized ) ||
		    ( ! entry_ptr->destroyed ) ) {
#endif /* NDEBUG */

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected entry history 12.");
            failure_mssg2 = msg;

	}
    }

    if ( pass2 ) {

	if ( ( cache_ptr->index_len != 3 ) ||
	     ( cache_ptr->index_size != (3 * LARGE_ENTRY_SIZE) ) ||
	     ( cache_ptr->slist_len != 1 ) ||
	     ( cache_ptr->slist_size != LARGE_ENTRY_SIZE ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected cache status 15.");
            failure_mssg2 = msg;

	}
    }

    protect_entry2(cache_ptr, LARGE_ENTRY_TYPE, 2);
    unprotect_entry2(cache_ptr, LARGE_ENTRY_TYPE, 2, FALSE, H5C2__DELETED_FLAG);

    protect_entry2(cache_ptr, LARGE_ENTRY_TYPE, 1);
    unprotect_entry2(cache_ptr, LARGE_ENTRY_TYPE, 1, FALSE, H5C2__DELETED_FLAG);

    protect_entry2(cache_ptr, LARGE_ENTRY_TYPE, 0);
    unprotect_entry2(cache_ptr, LARGE_ENTRY_TYPE, 0, FALSE, H5C2__DELETED_FLAG);


    if ( pass2 ) {

	if ( ( cache_ptr->index_len != 0 ) ||
	     ( cache_ptr->index_size != 0 ) ||
	     ( cache_ptr->slist_len != 0 ) ||
	     ( cache_ptr->slist_size != 0 ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected cache status 16.");
            failure_mssg2 = msg;

	}
    }

    if ( pass2 ) {

        takedown_cache2(cache_ptr, FALSE, FALSE);
    }

    if ( pass2 ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass2 ) {

        HDfprintf(stdout, "%s(): failure_mssg2 = \"%s\".\n",
                  fcn_name, failure_mssg2);
    }

    return;

} /* check_resize_entry() */


/*-------------------------------------------------------------------------
 * Function:	check_evictions_enabled()
 *
 * Purpose:	Verify that H5C2_get_evictions_enabled() and 
 * 		H5C2_set_evictions_enabled() functions perform as expected.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              8/2/07
 *
 * Modifications:
 *
 * 		JRM -- 10/15/07
 * 		Minor updates to adapt to cache API changes.
 *
 *-------------------------------------------------------------------------
 */

static void
check_evictions_enabled(void)
{
    const char *   fcn_name = "check_evictions_enabled";
    static char    msg[128];
    herr_t         result;
    hbool_t	   show_progress = FALSE;
    hbool_t	   evictions_enabled;
    hbool_t	   in_cache;
    int 	   i;
    int		   mile_stone = 1;
    size_t	   entry_size;
    H5C2_t *        cache_ptr = NULL;
    test_entry_t * base_addr;
    test_entry_t * entry_ptr;

    TESTING("evictions enabled/disabled functionality");

    /* Setup a cache and verify that it is empty.
     *
     * Use H5C2_get_evictions_enabled() to determine if evictions are
     * currently enabled -- they should be.
     *
     * Load entries until the cache is full.  Load one more.  Verify that
     * this caused an entry to be evicted.
     *
     * Insert an entry.  Verify that this cases and entry to be evicted.
     *
     * Used H5C2_set_evictions_enabled() to disable evictions.  Verify
     * with a call to H5C2_get_evictions_enabled().
     *
     * Load another entry -- verify that this does not cause an entry
     * to be evicted.
     *
     * Insert an entry -- verify that this does not cause an entry to 
     * be evicted.
     *
     * Use H5C2_set_evictions_enabled() to re-enable evictions.  Verify
     * with a call to H5C2_get_evictions_enabled().
     *
     * Protect and unprotect some of the entries in the cache.  Verify
     * that there are no evictions (since we only try to make space
     * when we either insert or load a new entry).
     *
     * Protect an entry not in the cache.  Verify that this causes
     * two evictions.
     *
     * Used H5C2_set_evictions_enabled() to disable evictions again.  
     * Verify with a call to H5C2_get_evictions_enabled().
     *
     * Now flush and discard the cache -- should succeed.
     */

    pass2 = TRUE;

    if ( show_progress ) /* 1 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    /* create the cache */
    if ( pass2 ) {

        reset_entries2();

        cache_ptr = setup_cache2((size_t)(1 * 1024 * 1024),
                                (size_t)(     512 * 1024));

        base_addr = entries2[MONSTER_ENTRY_TYPE];
	entry_size = MONSTER_ENTRY_SIZE;
    }

    if ( show_progress ) /* 2 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    /* verify that it is empty */
    if ( pass2 ) {

	if ( ( cache_ptr->index_len != 0 ) ||
	     ( cache_ptr->index_size != 0 ) ||
	     ( cache_ptr->slist_len != 0 ) ||
	     ( cache_ptr->slist_size != 0 ) ||
	     ( cache_ptr->evictions_enabled != TRUE ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected cache status 1.");
            failure_mssg2 = msg;

	}
    }

    if ( show_progress ) /* 3 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    /* verify that H5C2_get_evictions_enabled() returns the expected value */
    if ( pass2 ) {

        result = H5C2_get_evictions_enabled(cache_ptr, &evictions_enabled);

	if ( ( result != SUCCEED ) || ( evictions_enabled != TRUE ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected evictions enabled 1.");
            failure_mssg2 = msg;
	}
    }

    if ( show_progress ) /* 4 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    /* fill the cache */
    for ( i = 0; i < 16 ; i++ )
    {
        protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i);
        unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i, 
			FALSE, H5C2__NO_FLAGS_SET);
    }

    if ( show_progress ) /* 5 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    /* verify that the cache is full */
    if ( pass2 ) {

	if ( ( cache_ptr->index_len != 16 ) ||
	     ( cache_ptr->index_size != 16 * MONSTER_ENTRY_SIZE ) ||
	     ( cache_ptr->slist_len != 0 ) ||
	     ( cache_ptr->slist_size != 0 ) ||
	     ( cache_ptr->evictions_enabled != TRUE ) ) {


            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected cache status 2.");
            failure_mssg2 = msg;

	}
    }

    if ( show_progress ) /* 6 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    /* protect and unprotect another entry */
    protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 16);
    unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 16, 
		    FALSE, H5C2__NO_FLAGS_SET);

    if ( show_progress ) /* 7 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    /* verify that an entry has been evicted */
    if ( pass2 ) {

	if ( ( cache_ptr->index_len != 16 ) ||
	     ( cache_ptr->index_size != 16 * MONSTER_ENTRY_SIZE ) ||
	     ( cache_ptr->slist_len != 0 ) ||
	     ( cache_ptr->slist_size != 0 ) ||
	     ( cache_ptr->evictions_enabled != TRUE ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected cache status 3.");
            failure_mssg2 = msg;

	}
    }

    if ( show_progress ) /* 8 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    if ( pass2 ) {

        entry_ptr = &(base_addr[0]);

        result = H5C2_get_entry_status(cache_ptr, entry_ptr->addr, 
			              NULL, &in_cache, NULL, NULL, NULL);

	if ( result < 0 ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "H5AC_get_entry_status() reports failure 1.");
            failure_mssg2 = msg;

	} else if ( in_cache ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected status 1.");
            failure_mssg2 = msg;

        } else if ( ( ! entry_ptr->deserialized ) ||
                    ( entry_ptr->cleared ) ||
		    ( entry_ptr->serialized ) ||
		    ( ! entry_ptr->destroyed ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected entry history 1.");
            failure_mssg2 = msg;

	}
    }

    if ( show_progress ) /* 9 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    /* insert an entry */
    insert_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 17, TRUE, H5C2__NO_FLAGS_SET);

    if ( show_progress ) /* 10 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    /* verify that another entry has been evicted */
    if ( pass2 ) {

	if ( ( cache_ptr->index_len != 16 ) ||
	     ( cache_ptr->index_size != 16 * MONSTER_ENTRY_SIZE ) ||
	     ( cache_ptr->slist_len != 1 ) ||
	     ( cache_ptr->slist_size != MONSTER_ENTRY_SIZE ) ||
	     ( cache_ptr->evictions_enabled != TRUE ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected cache status 4.");
            failure_mssg2 = msg;

	}
    }

    if ( show_progress ) /* 11 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    if ( pass2 ) {

        entry_ptr = &(base_addr[1]);

        result = H5C2_get_entry_status(cache_ptr, entry_ptr->addr, 
			              NULL, &in_cache, NULL, NULL, NULL);

	if ( result < 0 ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "H5AC_get_entry_status() reports failure 2.");
            failure_mssg2 = msg;

	} else if ( in_cache ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected status 2.");
            failure_mssg2 = msg;

        } else if ( ( ! entry_ptr->deserialized ) ||
                    ( entry_ptr->cleared ) ||
		    ( entry_ptr->serialized ) ||
		    ( ! entry_ptr->destroyed ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected entry history 2.");
            failure_mssg2 = msg;

	}
    }

    if ( show_progress ) /* 12 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    /* disable evictions */
    if ( pass2 ) {

        result = H5C2_set_evictions_enabled(cache_ptr, FALSE);

	if ( result != SUCCEED ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "can't disable evictions 1.");
            failure_mssg2 = msg;
	}
    }

    if ( show_progress ) /* 13 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    /* verify that evictions are disabled */
    if ( pass2 ) {

	if ( ( cache_ptr->index_len != 16 ) ||
	     ( cache_ptr->index_size != 16 * MONSTER_ENTRY_SIZE ) ||
	     ( cache_ptr->slist_len != 1 ) ||
	     ( cache_ptr->slist_size != MONSTER_ENTRY_SIZE ) ||
	     ( cache_ptr->evictions_enabled != FALSE ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected cache status 5.");
            failure_mssg2 = msg;

	}
    }

    if ( show_progress ) /* 14 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    /* protect and unprotect another entry */
    protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 18);
    unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 18, 
		    FALSE, H5C2__NO_FLAGS_SET);

    if ( show_progress ) /* 15 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    /* verify that no entry has been evicted */
    if ( pass2 ) {

	if ( ( cache_ptr->index_len != 17 ) ||
	     ( cache_ptr->index_size != 17 * MONSTER_ENTRY_SIZE ) ||
	     ( cache_ptr->slist_len != 1 ) ||
	     ( cache_ptr->slist_size != MONSTER_ENTRY_SIZE ) ||
	     ( cache_ptr->evictions_enabled != FALSE ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected cache status 6.");
            failure_mssg2 = msg;

	}
    }

    if ( show_progress ) /* 16 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    /* insert another entry */
    insert_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 19, TRUE, H5C2__NO_FLAGS_SET);

    if ( show_progress ) /* 17 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    /* verify that no entry has been evicted */
    if ( pass2 ) {

	if ( ( cache_ptr->index_len != 18 ) ||
	     ( cache_ptr->index_size != 18 * MONSTER_ENTRY_SIZE ) ||
	     ( cache_ptr->slist_len != 2 ) ||
	     ( cache_ptr->slist_size != 2 * MONSTER_ENTRY_SIZE ) ||
	     ( cache_ptr->evictions_enabled != FALSE ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected cache status 7.");
            failure_mssg2 = msg;

	}
    }

    if ( show_progress ) /* 18 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    /* re-enable evictions */
    if ( pass2 ) {

        result = H5C2_set_evictions_enabled(cache_ptr, TRUE);

	if ( result != SUCCEED ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "can't enable evictions 1.");
            failure_mssg2 = msg;
	}
    }

    if ( show_progress ) /* 19 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    /* protect and unprotect an entry that is in the cache */
    protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 19);
    unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 19, 
		    FALSE, H5C2__NO_FLAGS_SET);

    if ( show_progress ) /* 20 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    /* verify that no entries have been evicted */
    if ( pass2 ) {

	if ( ( cache_ptr->index_len != 18 ) ||
	     ( cache_ptr->index_size != 18 * MONSTER_ENTRY_SIZE ) ||
	     ( cache_ptr->slist_len != 2 ) ||
	     ( cache_ptr->slist_size != 2 * MONSTER_ENTRY_SIZE ) ||
	     ( cache_ptr->evictions_enabled != TRUE ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected cache status 8.");
            failure_mssg2 = msg;

	}
    }

    if ( show_progress ) /* 21 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    /* protect and unprotect an entry that isn't in the cache */
    protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 20);
    unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 20, 
		    FALSE, H5C2__NO_FLAGS_SET);

    if ( show_progress ) /* 22 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    /* verify that the entries have been evicted to bring the 
     * cache back down to its normal size.
     */

    if ( pass2 ) {

	if ( ( cache_ptr->index_len != 16 ) ||
	     ( cache_ptr->index_size != 16 * MONSTER_ENTRY_SIZE ) ||
	     ( cache_ptr->slist_len != 2 ) ||
	     ( cache_ptr->slist_size != 2 * MONSTER_ENTRY_SIZE ) ||
	     ( cache_ptr->evictions_enabled != TRUE ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected cache status 9.");
            failure_mssg2 = msg;

	}
    }

    if ( show_progress ) /* 23 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    if ( pass2 ) {

        entry_ptr = &(base_addr[2]);

        result = H5C2_get_entry_status(cache_ptr, entry_ptr->addr, 
			              NULL, &in_cache, NULL, NULL, NULL);

	if ( result < 0 ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "H5AC_get_entry_status() reports failure 3.");
            failure_mssg2 = msg;

	} else if ( in_cache ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected status 3.");
            failure_mssg2 = msg;

        } else if ( ( ! entry_ptr->deserialized ) ||
                    ( entry_ptr->cleared ) ||
		    ( entry_ptr->serialized ) ||
		    ( ! entry_ptr->destroyed ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected entry history 3.");
            failure_mssg2 = msg;

	}
    }

    if ( show_progress ) /* 24 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    if ( pass2 ) {

        entry_ptr = &(base_addr[3]);

        result = H5C2_get_entry_status(cache_ptr, entry_ptr->addr, 
			              NULL, &in_cache, NULL, NULL, NULL);

	if ( result < 0 ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "H5AC_get_entry_status() reports failure 4.");
            failure_mssg2 = msg;

	} else if ( in_cache ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected status 4.");
            failure_mssg2 = msg;

        } else if ( ( ! entry_ptr->deserialized ) ||
                    ( entry_ptr->cleared ) ||
		    ( entry_ptr->serialized ) ||
		    ( ! entry_ptr->destroyed ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected entry history 4.");
            failure_mssg2 = msg;

	}
    }

    if ( show_progress ) /* 25 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    /* disable evictions again */
    if ( pass2 ) {

        result = H5C2_set_evictions_enabled(cache_ptr, FALSE);

	if ( result != SUCCEED ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "can't disable evictions 2.");
            failure_mssg2 = msg;
	}
    }

    if ( show_progress ) /* 26 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    /* protect and unprotect an entry that isn't in the cache, forcing 
     * the cache to grow.
     */
    protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 21);
    unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 21, 
		    FALSE, H5C2__NO_FLAGS_SET);


    if ( show_progress ) /* 27 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    /* verify that the cache has grown */
    if ( pass2 ) {

	if ( ( cache_ptr->index_len != 17 ) ||
	     ( cache_ptr->index_size != 17 * MONSTER_ENTRY_SIZE ) ||
	     ( cache_ptr->slist_len != 2 ) ||
	     ( cache_ptr->slist_size != 2 * MONSTER_ENTRY_SIZE ) ||
	     ( cache_ptr->evictions_enabled != FALSE ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected cache status 10.");
            failure_mssg2 = msg;

	}
    }

    if ( show_progress ) /* 28 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    /* re-enable evictions again */
    if ( pass2 ) {

        result = H5C2_set_evictions_enabled(cache_ptr, TRUE);

	if ( result != SUCCEED ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "can't enable evictions 2.");
            failure_mssg2 = msg;
	}
    }

    if ( show_progress ) /* 29 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    /* insert an entry */
    insert_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 22, TRUE, H5C2__NO_FLAGS_SET);

    if ( show_progress ) /* 30 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    /* verify that the cache has returned to its maximum size */
    if ( pass2 ) {

	if ( ( cache_ptr->index_len != 16 ) ||
	     ( cache_ptr->index_size != 16 * MONSTER_ENTRY_SIZE ) ||
	     ( cache_ptr->slist_len != 3 ) ||
	     ( cache_ptr->slist_size != 3 * MONSTER_ENTRY_SIZE ) ||
	     ( cache_ptr->evictions_enabled != TRUE ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected cache status 11.");
            failure_mssg2 = msg;

	}
    }

    if ( show_progress ) /* 31 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    if ( pass2 ) {

        entry_ptr = &(base_addr[4]);

        result = H5C2_get_entry_status(cache_ptr, entry_ptr->addr, 
			              NULL, &in_cache, NULL, NULL, NULL);

	if ( result < 0 ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "H5AC_get_entry_status() reports failure 5.");
            failure_mssg2 = msg;

	} else if ( in_cache ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected status 5.");
            failure_mssg2 = msg;

        } else if ( ( ! entry_ptr->deserialized ) ||
                    ( entry_ptr->cleared ) ||
		    (  entry_ptr->serialized ) ||
		    ( ! entry_ptr->destroyed ) ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "Unexpected entry history 5.");
            failure_mssg2 = msg;

	}
    }

    if ( show_progress ) /* 32 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    /* disable evictions one last time before we shut down */
    if ( pass2 ) {

        result = H5C2_set_evictions_enabled(cache_ptr, FALSE);

	if ( result != SUCCEED ) {

            pass2 = FALSE;
            HDsnprintf(msg, (size_t)128, "can't disable evictions 3.");
            failure_mssg2 = msg;
	}
    }

    if ( show_progress ) /* 33 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    if ( pass2 ) {

        takedown_cache2(cache_ptr, FALSE, FALSE);
    }

    if ( show_progress ) /* 34 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    if ( pass2 ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass2 ) {

        HDfprintf(stdout, "%s(): failure_mssg2 = \"%s\".\n",
                  fcn_name, failure_mssg2);
    }

    return;

} /* check_evictions_enabled() */


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
    H5C2_t * cache_ptr = NULL;

    TESTING("flush cache with protected entry error");

    pass2 = TRUE;

    /* allocate a cache, protect an entry, and try to flush.  This
     * should fail.  Unprotect the entry and flush again -- should
     * succeed.
     */

    if ( pass2 ) {

        reset_entries2();

        cache_ptr = setup_cache2((size_t)(2 * 1024),
                                (size_t)(1 * 1024));

        protect_entry2(cache_ptr, 0, 0);

        if ( H5C2_flush_cache(cache_ptr, H5P_DATASET_XFER_DEFAULT, 
			      H5C2__NO_FLAGS_SET)
             >= 0 ) {

            pass2 = FALSE;
            failure_mssg2 = "flush succeeded on cache with protected entry.\n";

        } else {

            unprotect_entry2(cache_ptr, 0, 0, TRUE, H5C2__NO_FLAGS_SET);

            if ( H5C2_flush_cache(cache_ptr, H5P_DATASET_XFER_DEFAULT, 
				  H5C2__NO_FLAGS_SET)
                 < 0 ) {

                pass2 = FALSE;
                failure_mssg2 = "flush failed after unprotect.\n";

            } else {

                takedown_cache2(cache_ptr, FALSE, FALSE);
            }
        }
    }

    if ( pass2 ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass2 ) {

        HDfprintf(stdout, "%s(): failure_mssg2 = \"%s\".\n",
                  fcn_name, failure_mssg2);
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
 * 		JRM -- 10/15/07
 * 		Minor updates to accomodate cache API mods.
 *
 *-------------------------------------------------------------------------
 */

static void
check_destroy_pinned_err(void)
{
    const char * fcn_name = "check_destroy_pinned_err()";
    H5C2_t * cache_ptr = NULL;

    TESTING("destroy cache with permanently pinned entry error");

    pass2 = TRUE;

    /* allocate a cache, pin an entry, and try to flush destroy.  This
     * should fail.  Unpin the entry and flush destroy again -- should
     * succeed.
     */

    if ( pass2 ) {

        reset_entries2();

        cache_ptr = setup_cache2((size_t)(2 * 1024),
                                (size_t)(1 * 1024));

        protect_entry2(cache_ptr, 0, 0);
	unprotect_entry2(cache_ptr, 0, 0, FALSE, H5C2__PIN_ENTRY_FLAG);

        if ( H5C2_dest(cache_ptr, H5P_DATASET_XFER_DEFAULT) >= 0 ) {

            pass2 = FALSE;
            failure_mssg2 = "destroy succeeded on cache with pinned entry.\n";

        } else {

	    unpin_entry2(cache_ptr, 0, 0);

            if ( H5C2_dest(cache_ptr, H5P_DATASET_XFER_DEFAULT) < 0 ) {

                pass2 = FALSE;
                failure_mssg2 = "destroy failed after unpin.\n";

            }
        }

	/* call takedown_cache2() with a NULL cache_ptr parameter.
	 * This causes the function to close and delete the file,
	 * while skipping the call to H5C2_dest().
	 */
	takedown_cache2(NULL, FALSE, FALSE);

    }

    if ( pass2 ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass2 ) {

        HDfprintf(stdout, "%s(): failure_mssg2 = \"%s\".\n",
                  fcn_name, failure_mssg2);
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
    H5C2_t * cache_ptr = NULL;

    TESTING("destroy cache with protected entry error");

    pass2 = TRUE;

    /* allocate a cache, protect an entry, and try to flush.  This
     * should fail.  Unprotect the entry and flush again -- should
     * succeed.
     */

    if ( pass2 ) {

        reset_entries2();

        cache_ptr = setup_cache2((size_t)(2 * 1024),
                                 (size_t)(1 * 1024));

        protect_entry2(cache_ptr, 0, 0);

        if ( H5C2_dest(cache_ptr, H5P_DATASET_XFER_DEFAULT) >= 0 ) {

            pass2 = FALSE;
            failure_mssg2 = 
		    "destroy succeeded on cache with protected entry.\n";

        } else {

            unprotect_entry2(cache_ptr, 0, 0, TRUE, H5C2__NO_FLAGS_SET);

            if ( H5C2_dest(cache_ptr, H5P_DATASET_XFER_DEFAULT) < 0 ) {

                pass2 = FALSE;
                failure_mssg2 = "destroy failed after unprotect.\n";

            }
        }

	/* call takedown_cache2() with a NULL cache_ptr parameter.
	 * This causes the function to close and delete the file,
	 * while skipping the call to H5C2_dest().
	 */
	takedown_cache2(NULL, FALSE, FALSE);
    }

    if ( pass2 ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass2 ) {

        HDfprintf(stdout, "%s(): failure_mssg2 = \"%s\".\n",
                  fcn_name, failure_mssg2);
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
    H5C2_t * cache_ptr = NULL;
    test_entry_t * base_addr;
    test_entry_t * entry_ptr;

    TESTING("duplicate entry insertion error");

    pass2 = TRUE;

    /* allocate a cache, protect an entry, and then try to insert
     * the entry again.  This should fail.  Unprotect the entry and
     * destroy the cache -- should succeed.
     */

    if ( pass2 ) {

        reset_entries2();

        cache_ptr = setup_cache2((size_t)(2 * 1024),
                                (size_t)(1 * 1024));

        protect_entry2(cache_ptr, 0, 0);

        if ( pass2 ) {

            base_addr = entries2[0];
            entry_ptr = &(base_addr[0]);

            result = H5C2_insert_entry(cache_ptr, H5P_DATASET_XFER_DEFAULT,
                                      &(types2[0]), entry_ptr->addr,
				      entry_ptr->size,
                                      (void *)entry_ptr, H5C2__NO_FLAGS_SET);

            if ( result >= 0 ) {

                pass2 = FALSE;
                failure_mssg2 = "insert of duplicate entry succeeded.\n";

            } else {

                unprotect_entry2(cache_ptr, 0, 0, TRUE, H5C2__NO_FLAGS_SET);

                takedown_cache2(cache_ptr, FALSE, FALSE);
            }
        }
    }

    if ( pass2 ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass2 ) {

        HDfprintf(stdout, "%s(): failure_mssg2 = \"%s\".\n",
                  fcn_name, failure_mssg2);
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
    H5C2_t * cache_ptr = NULL;
    test_entry_t * entry_0_0_ptr;
    test_entry_t * entry_0_1_ptr;
    test_entry_t * entry_1_0_ptr;

    TESTING("rename to existing entry errors");

    pass2 = TRUE;

    /* allocate a cache, and insert several entries.  Try to rename
     * entries to other entries resident in the cache.  This should
     * fail.  Destroy the cache -- should succeed.
     */

    if ( pass2 ) {

        reset_entries2();

        cache_ptr = setup_cache2((size_t)(2 * 1024),
                                (size_t)(1 * 1024));

        insert_entry2(cache_ptr, 0, 0, TRUE, H5C2__NO_FLAGS_SET);
        insert_entry2(cache_ptr, 0, 1, TRUE, H5C2__NO_FLAGS_SET);
        insert_entry2(cache_ptr, 1, 0, TRUE, H5C2__NO_FLAGS_SET);

        entry_0_0_ptr = &((entries2[0])[0]);
        entry_0_1_ptr = &((entries2[0])[1]);
        entry_1_0_ptr = &((entries2[1])[0]);
    }

    if ( pass2 ) {

        result = H5C2_rename_entry(cache_ptr, &(types2[0]),
                                  entry_0_0_ptr->addr, entry_0_1_ptr->addr);

        if ( result >= 0 ) {

            pass2 = FALSE;
            failure_mssg2 = "rename to addr of same type succeeded.\n";
        }
    }

    if ( pass2 ) {

        result = H5C2_rename_entry(cache_ptr, &(types2[0]),
                                  entry_0_0_ptr->addr, entry_1_0_ptr->addr);

        if ( result >= 0 ) {

            pass2 = FALSE;
            failure_mssg2 = "rename to addr of different type succeeded.\n";
        }
    }

    if ( pass2 ) {

        takedown_cache2(cache_ptr, FALSE, FALSE);
    }

    if ( pass2 ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass2 ) {

        HDfprintf(stdout, "%s: failure_mssg2 = \"%s\".\n",
                  fcn_name, failure_mssg2);
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
    H5C2_t * cache_ptr = NULL;
    test_entry_t * entry_ptr;

    TESTING("pin a pinned entry error");

    pass2 = TRUE;

    /* allocate a cache, protect an entry, unprotect it with the pin flag,
     * protect it again, and then try to unprotect it again with the pin
     * flag.  This should fail.  Unpin the entry and destroy the cache
     * -- should succeed.
     */

    if ( pass2 ) {

        reset_entries2();

        cache_ptr = setup_cache2((size_t)(2 * 1024),
                                (size_t)(1 * 1024));

        protect_entry2(cache_ptr, 0, 0);

        unprotect_entry2(cache_ptr, 0, 0, FALSE, H5C2__PIN_ENTRY_FLAG);

        protect_entry2(cache_ptr, 0, 0);

        entry_ptr = &((entries2[0])[0]);
    }

    if ( pass2 ) {

        result = H5C2_unprotect(cache_ptr, H5P_DATASET_XFER_DEFAULT, 
			        &(types2[0]), entry_ptr->addr, 
				(void *)entry_ptr, H5C2__PIN_ENTRY_FLAG, 
				(size_t)0);

        if ( result > 0 ) {

            pass2 = FALSE;
            failure_mssg2 =
                "attempt to pin a pinned entry succeeded.\n";

        } else {

	    unprotect_entry2(cache_ptr, 0, 0, FALSE, H5C2__UNPIN_ENTRY_FLAG);
	}
    }

    if ( pass2 ) {

        takedown_cache2(cache_ptr, FALSE, FALSE);
    }

    if ( pass2 ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass2 ) {

        HDfprintf(stdout, "%s: failure_mssg2 = \"%s\".\n",
                  fcn_name, failure_mssg2);
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
    H5C2_t * cache_ptr = NULL;
    test_entry_t * entry_ptr;

    TESTING("unpin an unpinned entry error");

    pass2 = TRUE;

    /* allocate a cache, protect an entry, unprotect it with the unpin flag.
     * -- This should fail.
     *
     * Try again with H5C2_unpin_entry -- this should also fail.
     *
     * Destroy the cache -- should succeed.
     */

    if ( pass2 ) {

        reset_entries2();

        cache_ptr = setup_cache2((size_t)(2 * 1024),
                                (size_t)(1 * 1024));

        protect_entry2(cache_ptr, 0, 0);

        entry_ptr = &((entries2[0])[0]);
    }

    if ( pass2 ) {

        result = H5C2_unprotect(cache_ptr, H5P_DATASET_XFER_DEFAULT,  
			        &(types2[0]), entry_ptr->addr, 
				(void *)entry_ptr, H5C2__UNPIN_ENTRY_FLAG, 
				(size_t)0);

        if ( result > 0 ) {

            pass2 = FALSE;
            failure_mssg2 =
                "attempt to unpin an unpinned entry succeeded 1.\n";

        } else {

	    unprotect_entry2(cache_ptr, 0, 0, FALSE, H5C2__NO_FLAGS_SET);
	}
    }

    if ( pass2 ) {

	result =  H5C2_unpin_entry(cache_ptr, (void *)entry_ptr);

        if ( result > 0 ) {

            pass2 = FALSE;
            failure_mssg2 =
                "attempt to unpin an unpinned entry succeeded 2.\n";

        }
    }

    if ( pass2 ) {

        takedown_cache2(cache_ptr, FALSE, FALSE);
    }

    if ( pass2 ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass2 ) {

        HDfprintf(stdout, "%s: failure_mssg2 = \"%s\".\n",
                  fcn_name, failure_mssg2);
    }

    return;

} /* check_double_unpin_err() */


/*-------------------------------------------------------------------------
 * Function:	check_pin_entry_errs()
 *
 * Purpose:	Verify that invalid calls to H5C2_pin_protected_entry()
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
    H5C2_t * cache_ptr = NULL;
    test_entry_t * entry_ptr;

    TESTING("pin entry related errors");

    pass2 = TRUE;

    /* Allocate a cache, protect an entry, unprotect it with no flags,
     * and then call H5C2_pin_protected_entry() to pin it -- This should fail.
     *
     * Protect the entry again, unprotect it with a pin flag, protect it
     * again, and then call H5C2_pin_protected_entry() to pin it -- This
     * should fail also.
     *
     * Unprotect the entry with the unpin flag.
     *
     * Destroy the cache -- should succeed.
     */

    if ( pass2 ) {

        reset_entries2();

        cache_ptr = setup_cache2((size_t)(2 * 1024),
                                (size_t)(1 * 1024));

        protect_entry2(cache_ptr, 0, 0);

	unprotect_entry2(cache_ptr, 0, 0, FALSE, H5C2__NO_FLAGS_SET);

        entry_ptr = &((entries2[0])[0]);
    }

    if ( pass2 ) {

        result = H5C2_pin_protected_entry(cache_ptr, (void *)entry_ptr);

        if ( result > 0 ) {

            pass2 = FALSE;
            failure_mssg2 =
                "attempt to pin an unprotected entry succeeded.\n";

        } else {

            protect_entry2(cache_ptr, 0, 0);

	    unprotect_entry2(cache_ptr, 0, 0, FALSE, H5C2__PIN_ENTRY_FLAG);

            protect_entry2(cache_ptr, 0, 0);
	}
    }

    if ( pass2 ) {

        result = H5C2_pin_protected_entry(cache_ptr, (void *)entry_ptr);

        if ( result > 0 ) {

            pass2 = FALSE;
            failure_mssg2 =
                "attempt to pin a pinned, protected entry succeeded.\n";

        } else {

	    unprotect_entry2(cache_ptr, 0, 0, FALSE, H5C2__UNPIN_ENTRY_FLAG);

	}
    }

    if ( pass2 ) {

        takedown_cache2(cache_ptr, FALSE, FALSE);
    }

    if ( pass2 ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass2 ) {

        HDfprintf(stdout, "%s: failure_mssg2 = \"%s\".\n",
                  fcn_name, failure_mssg2);
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
 *    - Modified call to H5C2_protect() to pass2 H5C2__NO_FLAGS_SET in the
 *      the new flags parameter.
 *
 *      					JRM -- 3/28/07
 *
 *-------------------------------------------------------------------------
 */

static void
check_double_protect_err(void)
{
    const char * fcn_name = "check_double_protect_err()";
    H5C2_t * cache_ptr = NULL;
    test_entry_t * entry_ptr;
    H5C2_cache_entry_t * cache_entry_ptr;

    TESTING("protect a protected entry error");

    pass2 = TRUE;

    /* allocate a cache, protect an entry, and then try to protect
     * the entry again.  This should fail.  Unprotect the entry and
     * destroy the cache -- should succeed.
     */

    if ( pass2 ) {

        reset_entries2();

        cache_ptr = setup_cache2((size_t)(2 * 1024),
                                (size_t)(1 * 1024));

        protect_entry2(cache_ptr, 0, 0);

        entry_ptr = &((entries2[0])[0]);
    }

    if ( pass2 ) {

        cache_entry_ptr = H5C2_protect(cache_ptr, H5P_DATASET_XFER_DEFAULT, 
			               &(types2[0]), entry_ptr->addr, 
				       entry_ptr->size, NULL, 
				       H5C2__NO_FLAGS_SET);

        if ( cache_entry_ptr != NULL ) {

            pass2 = FALSE;
            failure_mssg2 = "attempt to protect a protected entry succeeded.\n";
        }
    }

    if ( pass2 ) {

        unprotect_entry2(cache_ptr, 0, 0, FALSE, H5C2__NO_FLAGS_SET);
    }

    if ( pass2 ) {

        takedown_cache2(cache_ptr, FALSE, FALSE);
    }

    if ( pass2 ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass2 ) {

        HDfprintf(stdout, "%s: failure_mssg2 = \"%s\".\n",
                  fcn_name, failure_mssg2);
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
 *		H5C2_unprotect().
 *
 *		JRM -- 9/8/05
 *		Updated function for the new size change parameter in
 *		H5C2_unprotect().  We don't use them for now.
 *
 *-------------------------------------------------------------------------
 */

static void
check_double_unprotect_err(void)
{
    const char * fcn_name = "check_double_unprotect_err()";
    herr_t result;
    H5C2_t * cache_ptr = NULL;
    test_entry_t * entry_ptr;

    TESTING("unprotect an unprotected entry error");

    pass2 = TRUE;

    /* allocate a cache, protect an entry, unprotect it, and then try to
     * unprotect the entry again.  This should fail.  Destroy the cache
     * -- should succeed.
     */

    if ( pass2 ) {

        reset_entries2();

        cache_ptr = setup_cache2((size_t)(2 * 1024),
                                (size_t)(1 * 1024));

        protect_entry2(cache_ptr, 0, 0);

        unprotect_entry2(cache_ptr, 0, 0, FALSE, H5C2__NO_FLAGS_SET);

        entry_ptr = &((entries2[0])[0]);
    }

    if ( pass2 ) {

        result = H5C2_unprotect(cache_ptr, H5P_DATASET_XFER_DEFAULT, 
			        &(types2[0]), entry_ptr->addr, 
				(void *)entry_ptr, H5C2__NO_FLAGS_SET, 
				(size_t)0);

        if ( result > 0 ) {

            pass2 = FALSE;
            failure_mssg2 =
                "attempt to unprotect an unprotected entry succeeded 1.\n";
        }
    }

    if ( pass2 ) {

        takedown_cache2(cache_ptr, FALSE, FALSE);
    }

    if ( pass2 ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass2 ) {

        HDfprintf(stdout, "%s: failure_mssg2 = \"%s\".\n",
                  fcn_name, failure_mssg2);
    }

    return;

} /* check_double_unprotect_err() */


/*-------------------------------------------------------------------------
 * Function:	check_mark_entry_dirty_errs()
 *
 * Purpose:	Verify that:
 *
 * 		1) a call to H5C2_mark_pinned_entry_dirty with an upinned
 * 		   entry as the target will generate an error.
 *
 * 		2) a call to H5C2_mark_pinned_entry_dirty with a protected
 * 		   entry as the target will generate an error.
 *
 *		3) a call to H5C2_mark_pinned_or_protected_entry_dirty with
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
    H5C2_t * cache_ptr = NULL;
    test_entry_t * entry_ptr;

    TESTING("mark entry dirty related errors");

    pass2 = TRUE;

    /* allocate a cache, protect an entry, and then attempt to mark it dirty
     * with the H5C2_mark_pinned_entry_dirty() call -- This should fail.
     *
     * Then unprotect the entry without pinning it, and try to mark it dirty
     * again -- this should fail too.
     *
     * Try it again using H5C2_mark_pinned_or_protected_entry_dirty -- this
     * should fail as well.
     *
     * Destroy the cache -- should succeed.
     */

    if ( pass2 ) {

        reset_entries2();

        cache_ptr = setup_cache2((size_t)(2 * 1024),
                                (size_t)(1 * 1024));

        protect_entry2(cache_ptr, 0, 0);

	unprotect_entry2(cache_ptr, 0, 0, FALSE, H5C2__PIN_ENTRY_FLAG);

        protect_entry2(cache_ptr, 0, 0);

        entry_ptr = &((entries2[0])[0]);
    }

    if ( pass2 ) {

	result = H5C2_mark_pinned_entry_dirty(cache_ptr, (void *)entry_ptr,
			                     FALSE, (size_t)0);

        if ( result > 0 ) {

            pass2 = FALSE;
            failure_mssg2 =
                    "attempt dirty a pinned and protected entry succeeded.\n";

        } else {

	    unprotect_entry2(cache_ptr, 0, 0, FALSE, H5C2__UNPIN_ENTRY_FLAG);
	}
    }

    if ( pass2 ) {

	result = H5C2_mark_pinned_entry_dirty(cache_ptr, (void *)entry_ptr,
			                     FALSE, (size_t)0);


        if ( result > 0 ) {

            pass2 = FALSE;
            failure_mssg2 =
            "attempt to dirty a unpinned and unprotected entry succeeded 1.\n";
        }
    }

    if ( pass2 ) {

	result = H5C2_mark_pinned_or_protected_entry_dirty(cache_ptr,
			                                  (void *)entry_ptr);


        if ( result > 0 ) {

            pass2 = FALSE;
            failure_mssg2 =
            "attempt to dirty a unpinned and unprotected entry succeeded 2.\n";
        }
    }

    if ( pass2 ) {

        takedown_cache2(cache_ptr, FALSE, FALSE);
    }

    if ( pass2 ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass2 ) {

        HDfprintf(stdout, "%s: failure_mssg2 = \"%s\".\n",
                  fcn_name, failure_mssg2);
    }

    return;

} /* check_mark_entry_dirty_errs() */


/*-------------------------------------------------------------------------
 * Function:	check_expunge_entry_errs()
 *
 * Purpose:	Verify that invalid calls to H5C2_expunge_entry()
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
    H5C2_t * cache_ptr = NULL;
    test_entry_t * entry_ptr;

    TESTING("expunge entry related errors");

    pass2 = TRUE;

    /* Allocate a cache, protect an entry, and then call H5C2_expunge_entry()
     * to expunge it -- this should fail
     *
     * Unprotect the the entry with the pinned flag, and then call
     * H5C2_expunge_entry() again.  This should fail too.
     *
     * Finally, unpin the entry and call H5C2_expunge_entry() yet again.
     * This should succeed.
     *
     * Destroy the cache -- should succeed.
     */

    if ( pass2 ) {

        reset_entries2();

        cache_ptr = setup_cache2((size_t)(2 * 1024),
                                (size_t)(1 * 1024));

        entry_ptr = &((entries2[0])[0]);

        protect_entry2(cache_ptr, 0, 0);

    }

    if ( pass2 ) {

	result = H5C2_expunge_entry(cache_ptr, H5P_DATASET_XFER_DEFAULT,
                                    &(types2[0]), entry_ptr->addr);

        if ( result > 0 ) {

            pass2 = FALSE;
            failure_mssg2 =
                "attempt to expunge a protected entry succeeded.\n";

        } else {

	    unprotect_entry2(cache_ptr, 0, 0, FALSE, H5C2__PIN_ENTRY_FLAG);

	}
    }

    if ( pass2 ) {

	result = H5C2_expunge_entry(cache_ptr, H5P_DATASET_XFER_DEFAULT,
                                    &(types2[0]), entry_ptr->addr);

        if ( result > 0 ) {

            pass2 = FALSE;
            failure_mssg2 =
                "attempt to expunge a pinned entry succeeded.\n";

        } else {

	    unpin_entry2(cache_ptr, 0, 0);

	}
    }

    if ( pass2 ) {

	result = H5C2_expunge_entry(cache_ptr, H5P_DATASET_XFER_DEFAULT,
                                    &(types2[0]), entry_ptr->addr);

        if ( result < 0 ) {

            pass2 = FALSE;
            failure_mssg2 =
              "attempt to expunge an unpinned and unprotected entry failed.\n";

	}
    }


    if ( pass2 ) {

        takedown_cache2(cache_ptr, FALSE, FALSE);
    }

    if ( pass2 ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass2 ) {

        HDfprintf(stdout, "%s: failure_mssg2 = \"%s\".\n",
                  fcn_name, failure_mssg2);
    }

    return;

} /* check_expunge_entry_errs() */


/*-------------------------------------------------------------------------
 * Function:	check_resize_entry_errs()
 *
 * Purpose:	Verify that invalid calls to H5C2_resize_pinned_entry()
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
    H5C2_t * cache_ptr = NULL;
    test_entry_t * entry_ptr;

    TESTING("resize entry related errors");

    pass2 = TRUE;

    /* Allocate a cache, protect an entry, and then call 
     * H5C2_resize_pinned_entry() to resize it -- this should fail.
     *
     * Unprotect the the entry with the pinned flag, and then call
     * H5C2_resize_pinned_entry() again with new size of zero.  
     * This should fail too.
     *
     * Finally, unpin the entry and destroy the cache.
     * This should succeed.
     */

    if ( pass2 ) {

        reset_entries2();

        cache_ptr = setup_cache2((size_t)(2 * 1024),
                                (size_t)(1 * 1024));

        entry_ptr = &((entries2[0])[0]);

        protect_entry2(cache_ptr, 0, 0);

    }

    if ( pass2 ) {

	result = H5C2_resize_pinned_entry(cache_ptr, (void *)entry_ptr, (size_t)1);

        if ( result > 0 ) {

            pass2 = FALSE;
            failure_mssg2 =
            "Call to H5C2_resize_pinned_entry on a protected entry succeeded.\n";

        } else {

	    unprotect_entry2(cache_ptr, 0, 0, FALSE, H5C2__PIN_ENTRY_FLAG);

	}
    }

    if ( pass2 ) {

	result = H5C2_resize_pinned_entry(cache_ptr, (void *)entry_ptr, 
			                  (size_t)0);

        if ( result > 0 ) {

            pass2 = FALSE;
            failure_mssg2 =
                 "Call to H5C2_resize_pinned_entry with 0 new size succeeded.\n";

        } else {

	    unpin_entry2(cache_ptr, 0, 0);

	}
    }

    if ( pass2 ) {

        takedown_cache2(cache_ptr, FALSE, FALSE);
    }

    if ( pass2 ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass2 ) {

        HDfprintf(stdout, "%s: failure_mssg2 = \"%s\".\n",
                  fcn_name, failure_mssg2);
    }

    return;

} /* check_resize_entry_errs() */


/*-------------------------------------------------------------------------
 * Function:	check_unprotect_ro_dirty_err()
 *
 * Purpose:	If an entry is protected read only, verify that unprotecting
 * 		it dirty will generate an error.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              4/3/07
 *
 * Modifications:
 *
 *		None.
 *
 *-------------------------------------------------------------------------
 */

static void
check_unprotect_ro_dirty_err(void)
{
    const char * fcn_name = "check_unprotect_ro_dirty_err()";
    herr_t result;
    H5C2_t * cache_ptr = NULL;
    test_entry_t * entry_ptr;

    TESTING("unprotect a read only entry dirty error");

    pass2 = TRUE;

    /* allocate a cache, protect an entry read only, and then unprotect it 
     * with the dirtied flag set.  This should fail.  Destroy the cache
     * -- should succeed.
     */

    if ( pass2 ) {

        reset_entries2();

        cache_ptr = setup_cache2((size_t)(2 * 1024),
                                (size_t)(1 * 1024));

        protect_entry_ro2(cache_ptr, 0, 0);

        entry_ptr = &((entries2[0])[0]);
    }

    if ( pass2 ) {

        result = H5C2_unprotect(cache_ptr, H5P_DATASET_XFER_DEFAULT, 
			        &(types2[0]), entry_ptr->addr, 
				(void *)entry_ptr, H5C2__DIRTIED_FLAG, 
				(size_t)0);

        if ( result >= 0 ) {

            pass2 = FALSE;
            failure_mssg2 =
                "attempt to unprotect a ro entry dirty succeeded 1.\n";
        }
    }

    if ( pass2 ) {

	unprotect_entry2(cache_ptr, 0, 0, FALSE, H5C2__NO_FLAGS_SET);

    }

    if ( pass2 ) {

        takedown_cache2(cache_ptr, FALSE, FALSE);
    }

    /* allocate a another cache, protect an entry read only twice, and 
     * then unprotect it with the dirtied flag set.  This should fail.  
     * Unprotect it with no flags set twice and then destroy the cache.
     * This should succeed.
     */

    if ( pass2 ) {

        reset_entries2();

        cache_ptr = setup_cache2((size_t)(2 * 1024),
                                (size_t)(1 * 1024));

        protect_entry_ro2(cache_ptr, 0, 0);
        protect_entry_ro2(cache_ptr, 0, 0);

        entry_ptr = &((entries2[0])[0]);
    }

    if ( pass2 ) {

        result = H5C2_unprotect(cache_ptr, H5P_DATASET_XFER_DEFAULT, 
			        &(types2[0]), entry_ptr->addr, 
				(void *)entry_ptr, H5C2__DIRTIED_FLAG, 
				(size_t)0);

        if ( result > 0 ) {

            pass2 = FALSE;
            failure_mssg2 =
                "attempt to unprotect a ro entry dirty succeeded 2.\n";
        }
    }

    if ( pass2 ) {

	unprotect_entry2(cache_ptr, 0, 0, FALSE, H5C2__NO_FLAGS_SET);
	unprotect_entry2(cache_ptr, 0, 0, FALSE, H5C2__NO_FLAGS_SET);

    }

    if ( pass2 ) {

        takedown_cache2(cache_ptr, FALSE, FALSE);
    }

    if ( pass2 ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass2 ) {

        HDfprintf(stdout, "%s: failure_mssg2 = \"%s\".\n",
                  fcn_name, failure_mssg2);
    }

    return;

} /* check_unprotect_ro_dirty_err() */


/*-------------------------------------------------------------------------
 * Function:	check_protect_ro_rw_err()
 *
 * Purpose:	If an entry is protected read only, verify that protecting
 * 		it rw will generate an error.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              4/9/07
 *
 * Modifications:
 *
 *		None.
 *
 *-------------------------------------------------------------------------
 */

static void
check_protect_ro_rw_err(void)
{
    const char * fcn_name = "check_protect_ro_rw_err()";
    H5C2_t * cache_ptr = NULL;
    test_entry_t * entry_ptr;
    void * thing_ptr = NULL;

    TESTING("protect a read only entry rw error");

    pass2 = TRUE;

    /* allocate a cache, protect an entry read only, and then try to protect 
     * it again rw.  This should fail.
     * 
     * Unprotect the entry and destroy the cache -- should succeed.
     */

    if ( pass2 ) {

        reset_entries2();

        cache_ptr = setup_cache2((size_t)(2 * 1024),
                                (size_t)(1 * 1024));

        protect_entry_ro2(cache_ptr, 0, 0);

        entry_ptr = &((entries2[0])[0]);
    }

    if ( pass2 ) {

        thing_ptr = H5C2_protect(cache_ptr, H5P_DATASET_XFER_DEFAULT, 
			         &(types2[0]), entry_ptr->addr, 
				 entry_ptr->size, NULL, H5C2__NO_FLAGS_SET);

        if ( thing_ptr != NULL ) {

            pass2 = FALSE;
            failure_mssg2 = "attempt to protect a ro entry rw succeeded.\n";
        }
    }

    if ( pass2 ) {

	unprotect_entry2(cache_ptr, 0, 0, FALSE, H5C2__NO_FLAGS_SET);
    }

    if ( pass2 ) {

        takedown_cache2(cache_ptr, FALSE, FALSE);
    }

    if ( pass2 ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass2 ) {

        HDfprintf(stdout, "%s: failure_mssg2 = \"%s\".\n",
                  fcn_name, failure_mssg2);
    }

    return;

} /* check_protect_ro_rw_err() */


/*-------------------------------------------------------------------------
 * Function:	check_evictions_enabled_err()
 *
 * Purpose:	Verify that H5C2_get_evictions_enabled() and 
 *              H5C2_set_evictions_enabled() generate errors as expected.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              8/3/07
 *
 * Modifications:
 *
 *		None.
 *
 *-------------------------------------------------------------------------
 */

static void
check_check_evictions_enabled_err(void)
{
    const char * fcn_name = "check_evictions_enabled_err()";
    herr_t result;
    hbool_t evictions_enabled;
    H5C2_t * cache_ptr = NULL;

    TESTING("get/set evictions enabled errors");

    pass2 = TRUE;

    /* allocate a cache.
     *
     * Call H5C2_get_evictions_enabled(), passing it a NULL cache_ptr,
     * should fail.
     *
     * Repeat with a NULL evictions_enabled_ptr, should fail as well.
     *
     * Configure the cache to use auto cache resize.  Call 
     * H5C2_set_evictions_enabled() to disable evictions.  Should fail.
     *
     * Unprotect the entry and destroy the cache -- should succeed.
     */

    if ( pass2 ) {

        reset_entries2();

        cache_ptr = setup_cache2((size_t)(2 * 1024),
                                (size_t)(1 * 1024));
    }

    if ( pass2 ) {

	result = H5C2_get_evictions_enabled(NULL, &evictions_enabled);

	if ( result == SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_get_evictions_enabled succeeded() 1.\n";
        }
    }

    if ( pass2 ) {

	result = H5C2_get_evictions_enabled(cache_ptr, NULL);

	if ( result == SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_get_evictions_enabled succeeded() 2.\n";
        }
    }

    if ( pass2 ) {

	result = H5C2_set_evictions_enabled(cache_ptr, TRUE);

	if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_set_evictions_enabled failed().\n";

	}
    }

    if ( pass2 ) {

        (cache_ptr->resize_ctl).incr_mode = H5C2_incr__threshold;

	result = H5C2_get_evictions_enabled(cache_ptr, FALSE);

	if ( result == SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_set_evictions_enabled succeeded() 1.\n";

        } else if ( cache_ptr->evictions_enabled == TRUE ) {

	}

        (cache_ptr->resize_ctl).incr_mode = H5C2_incr__off;
    }

    if ( pass2 ) {

        (cache_ptr->resize_ctl).decr_mode = H5C2_decr__threshold;

	result = H5C2_get_evictions_enabled(cache_ptr, FALSE);

	if ( result == SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_set_evictions_enabled succeeded() 2.\n";
        }

        (cache_ptr->resize_ctl).decr_mode = H5C2_decr__off;
    }


    if ( pass2 ) {

        takedown_cache2(cache_ptr, FALSE, FALSE);
    }

    if ( pass2 ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass2 ) {

        HDfprintf(stdout, "%s: failure_mssg2 = \"%s\".\n",
                  fcn_name, failure_mssg2);
    }

    return;

} /* check_evictions_enabled_err() */


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
 *              John Mainzer 1/8/08
 *              Added a basic set of tests for the flash cache size
 *              increment code.
 * 
 *-------------------------------------------------------------------------
 */

hbool_t rpt_fcn_called = FALSE;
enum H5C2_resize_status rpt_status;

static void test_rpt_fcn(UNUSED H5C2_t * cache_ptr,
                  UNUSED int32_t version,
                  UNUSED double hit_rate,
                  enum H5C2_resize_status status,
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
    H5C2_t * cache_ptr = NULL;
    H5C2_auto_size_ctl_t auto_size_ctl =
    {
        /* int32_t     version                = */ H5C2__CURR_AUTO_SIZE_CTL_VER,
        /* H5C2_auto_resize_report_fcn rpt_fcn = */ test_rpt_fcn,

        /* hbool_t     set_initial_size       = */ TRUE,
        /* size_t      initial_size           = */ (512 * 1024),

        /* double      min_clean_fraction     = */ 0.5,

        /* size_t      max_size               = */ (14 * 1024 * 1024),
        /* size_t      min_size               = */ (512 * 1024),

        /* int64_t     epoch_length           = */ 1000,


        /* enum H5C2_cache_incr_mode incr_mode = */ H5C2_incr__threshold,

        /* double     lower_hr_threshold      = */ 0.75,

        /* double      increment              = */ 2.0,

        /* hbool_t     apply_max_increment    = */ TRUE,
        /* size_t      max_increment          = */ (4 * 1024 * 1024),

        /* enum H5C2_cache_flash_incr_mode      */
	/*                    flash_incr_mode = */ H5C2_flash_incr__off,
	/* double      flash_multiple         = */ 2.0,
	/* double      flash_threshold        = */ 0.5,


        /* enum H5C2_cache_decr_mode decr_mode = */ H5C2_decr__threshold,

        /* double      upper_hr_threshold     = */ 0.995,

        /* double      decrement              = */ 0.1,

        /* hbool_t     apply_max_decrement    = */ TRUE,
        /* size_t      max_decrement          = */ (1 * 1024 * 1024),

        /* int32_t     epochs_before_eviction = */ 3,

        /* hbool_t     apply_empty_reserve    = */ TRUE,
        /* double      empty_reserve          = */ 0.05
    };

    TESTING("automatic cache resizing");

    pass2 = TRUE;

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* allocate a cache, enable automatic cache resizing, and then force
     * the cache through all its operational modes.  Verify that all
     * performs as expected.
     */

    if ( pass2 ) {

        reset_entries2();

        cache_ptr = setup_cache2((size_t)(2 * 1024),
                                (size_t)(1 * 1024));
    }

    if ( pass2 ) {

        result = H5C2_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_set_cache_auto_resize_config failed 1.\n";
        }
    }

    if ( pass2 ) {

        if ( ( cache_ptr->max_cache_size != (512 * 1024) ) ||
             ( cache_ptr->min_clean_size != (256 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "bad cache size after initialization.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate with cache not full -- should result in not
     * full status.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, PICO_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, PICO_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != not_full ) ||
             ( cache_ptr->max_cache_size != (512 * 1024) ) ||
             ( cache_ptr->min_clean_size != (256 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 1.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate with cache full -- should result in increase
     * of cache size from .5 to 1 meg.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != increase ) ||
             ( cache_ptr->max_cache_size != (1 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (512 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 2.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate with cache not full -- should result in not
     * full status.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, PICO_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, PICO_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != not_full ) ||
             ( cache_ptr->max_cache_size != (1 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (512 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 3.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate with cache full again -- should result in increase
     * of cache size from 1 to 2 meg.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != increase ) ||
             ( cache_ptr->max_cache_size != (2 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (1 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 4.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate with cache full again -- should result in increase
     * of cache size from 2 to 4 meg.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != increase ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 5.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate with cache full again -- should result in increase
     * of cache size from 4 to 8 meg.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != increase ) ||
             ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 6.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate with cache full again -- should result in increase
     * of cache size from 8 to 12 meg.  Note that max increase reduced the
     * size of the increase.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != increase ) ||
             ( cache_ptr->max_cache_size != (12 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (6 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 7.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate with cache full again -- should result in increase
     * of cache size from 12 to 14 meg.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != increase ) ||
             ( cache_ptr->max_cache_size != (14 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (7 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 8.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate with cache full and at maximum size -- should
     * in no change in size and a result of at_max_size.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != at_max_size ) ||
             ( cache_ptr->max_cache_size != (14 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (7 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 9.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate with cache full and at maximum size -- should
     * result in a decrease from 14 to 13 Meg -- note that max decrease
     * reduced the size of the reduction
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (13 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (6 * 1024 * 1024 + 512 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 10.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* the current cache configuration is inconvenient for testing cache
     * size reduction, so lets change it some something easier to work
     * with.
     */

    if ( pass2 ) {

        auto_size_ctl.version                = H5C2__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 4 * 1000 * 1000 + 10;

        auto_size_ctl.min_clean_fraction     = 0.1;

        auto_size_ctl.max_size               = 8 * 1000 * 1000;
        auto_size_ctl.min_size               = 500 * 1000;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C2_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (4 * 1000 * 1000);

	auto_size_ctl.flash_incr_mode        = H5C2_flash_incr__off;
	auto_size_ctl.flash_multiple         = 2.0;
	auto_size_ctl.flash_threshold        = 0.5;

        auto_size_ctl.decr_mode              = H5C2_decr__threshold;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = TRUE;
        auto_size_ctl.max_decrement          = (1 * 1000 * 1000);

        auto_size_ctl.epochs_before_eviction = 3;

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C2_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_set_cache_auto_resize_config failed 2.\n";
        }
    }

    if ( pass2 ) {

        if ( ( cache_ptr->max_cache_size != (4 * 1000 * 1000 + 10) ) ||
             ( cache_ptr->min_clean_size != (400 * 1000 + 1) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "bad cache size after set resize re-config 1.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate  -- should result in a decrease from ~4 to ~3
     * M -- note that max decrease reduces the size of the reduction
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (3 * 1000 * 1000 + 10) ) ||
             ( cache_ptr->min_clean_size != (300 * 1000 + 1) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 11.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate again  -- should result in a decrease from ~3
     * to ~2 M -- again note that max decrease reduces the size of the
     * reduction.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (2 * 1000 * 1000 + 10) ) ||
             ( cache_ptr->min_clean_size != (200 * 1000 + 1) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 12.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate again  -- should result in a decrease from ~2
     * to ~1 M -- again note that max decrease reduces the size of the
     * reduction, but only by five bites.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (1 * 1000 * 1000 + 10) ) ||
             ( cache_ptr->min_clean_size != (100 * 1000 + 1) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 13.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate again  -- should result in a decrease from ~1
     * to ~0.5 M -- max decrease is no longer a factor.  New size is five
     * bytes above the minimum.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (500 * 1000 + 5) ) ||
             ( cache_ptr->min_clean_size != (50 * 1000) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 14.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate again  -- should result in a decrease of five
     * bytes to the minimum cache size.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (500 * 1000) ) ||
             ( cache_ptr->min_clean_size != (50 * 1000) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 15.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate again -- Already at minimum size so no change in
     * cache size and result should be at_min_size.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != at_min_size ) ||
             ( cache_ptr->max_cache_size != (500 * 1000) ) ||
             ( cache_ptr->min_clean_size != (50 * 1000) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 16.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force in range hit rate  -- should be no change in cache size,
     * and result should be in_spec.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 900 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i + 1000);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i + 1000,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (500 * 1000) ) ||
             ( cache_ptr->min_clean_size != (50 * 1000) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 17.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate with cache full -- should
     * increase cache size from .5 to 1 M.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != increase ) ||
             ( cache_ptr->max_cache_size != (1 * 1000 * 1000) ) ||
             ( cache_ptr->min_clean_size != (100 * 1000) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 18.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate -- should result in a decrease to the
     * minimum cache size.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (500 * 1000) ) ||
             ( cache_ptr->min_clean_size != (50 * 1000) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 19.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /******************************************************************
     * now do some tests with the maximum increase and decrease sizes
     * disabled.
     ******************************************************************/

    if ( pass2 ) {

        auto_size_ctl.version                = H5C2__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 16 * 1024 * 1024;
        auto_size_ctl.min_size               = 1 * 1024 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C2_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 4.0;

        auto_size_ctl.apply_max_increment    = FALSE;
        auto_size_ctl.max_increment          = (4 * 1024 * 1024);

	auto_size_ctl.flash_incr_mode        = H5C2_flash_incr__off;
	auto_size_ctl.flash_multiple         = 2.0;
	auto_size_ctl.flash_threshold        = 0.5;

        auto_size_ctl.decr_mode              = H5C2_decr__threshold;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 0.25;

        auto_size_ctl.apply_max_decrement    = FALSE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 3;

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C2_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_set_cache_auto_resize_config failed 3.\n";
        }
    }

    if ( pass2 ) {

        if ( ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "bad cache size after set resize re-config 2.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate -- should result in a decrease to the
     * minimum cache size.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (1 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (512 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 20.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate with cache full -- should increase cache size
     * from 1 to 4 Meg.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != increase ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 21.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate again with cache full -- should increase cache
     * size from 4 to 16 Meg.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != increase ) ||
             ( cache_ptr->max_cache_size != (16 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != ( 8 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 22.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate -- should result in a decrease cache size from
     * 16 to 4 Meg.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 23.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /******************************************************************
     * We have tested the threshold increment and decrement modes.
     * must now test the ageout decrement mode.
     *
     * Reconfigure the cache for this testing.
     ******************************************************************/

    if ( pass2 ) {

        auto_size_ctl.version                = H5C2__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 8 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 8 * 1024 * 1024;
        auto_size_ctl.min_size               = 512 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C2_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (4 * 1024 * 1024);

	auto_size_ctl.flash_incr_mode        = H5C2_flash_incr__off;
	auto_size_ctl.flash_multiple         = 2.0;
	auto_size_ctl.flash_threshold        = 0.5;


        auto_size_ctl.decr_mode              = H5C2_decr__age_out;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = FALSE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 3;

        auto_size_ctl.apply_empty_reserve    = FALSE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C2_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_set_cache_auto_resize_config failed 4.\n";
        }
    }

    if ( pass2 ) {

        if ( ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "bad cache size after set resize re-config 3.\n";
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
    if ( pass2 ) { /* first epoch */

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != at_max_size ) ||
             ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 24.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass2 ) { /* second epoch */

        rpt_fcn_called = FALSE;
        i = 1000;
        while ( ( pass2 ) && ( i < 2000 ) )
        {
            protect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != at_max_size ) ||
             ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 25.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass2 ) { /* third epoch */

        rpt_fcn_called = FALSE;
        i = 2000;
        while ( ( pass2 ) && ( i < 3000 ) )
        {
            protect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != at_max_size ) ||
             ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 26.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* fourth epoch -- If the hit rate were above the lower threshold,
     * we would see cache size reduction now.  However, nothing will
     * happen until we get the hit rate above the lower threshold.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 3000;
        while ( ( pass2 ) && ( i < 4000 ) )
        {
            protect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != at_max_size ) ||
             ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 27.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* fifth epoch -- force the hit rate to 100%.  We should see cache size
     * reduction now.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 3000;
        while ( ( pass2 ) && ( i < 4000 ) )
        {
            protect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (2001 * 1024) ) ||
             ( cache_ptr->min_clean_size != (int)(2001 * 512) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 28.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* sixth epoch -- force the hit rate to 100% again.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 3000;
        while ( ( pass2 ) && ( i < 4000 ) )
        {
            protect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (1001 * 1024) ) ||
             ( cache_ptr->min_clean_size != (int)(1001 * 512) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 29.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* seventh epoch -- force the hit rate to 100% again.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 3000;
        while ( ( pass2 ) && ( i < 4000 ) )
        {
            protect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (int)(1000 * 512) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 30.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* eigth epoch -- force the hit rate to 100% again -- should be steady
     * state.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 3000;
        while ( ( pass2 ) && ( i < 4000 ) )
        {
            protect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (int)(1000 * 512) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 31.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* now just bang on one entry -- after three epochs, this should
     * get all entries other than the one evicted, and the cache size
     * should be decreased to the minimum.
     */
    if ( pass2 ) { /* ninth epoch */

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (int)(1000 * 512) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 32.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass2 ) { /* tenth epoch */

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (int)(1000 * 512) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 33.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass2 ) { /* eleventh epoch -- cache size reduction */

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
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

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 34.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass2 ) { /* twelth epoch -- at minimum size so no more ageouts */

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
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

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 35.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);


    /* repeat the above test, but with max_decrement enabled to see
     * if that features works as it should.  Note that this will change
     * the structure of the test a bit.
     */

    if ( pass2 ) {

        auto_size_ctl.version                = H5C2__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 8 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 8 * 1024 * 1024;
        auto_size_ctl.min_size               = 512 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C2_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (4 * 1024 * 1024);

	auto_size_ctl.flash_incr_mode        = H5C2_flash_incr__off;
	auto_size_ctl.flash_multiple         = 2.0;
	auto_size_ctl.flash_threshold        = 0.5;


        auto_size_ctl.decr_mode              = H5C2_decr__age_out;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = TRUE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 3;

        auto_size_ctl.apply_empty_reserve    = FALSE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C2_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_set_cache_auto_resize_config failed 5.\n";
        }
    }

    if ( pass2 ) {

        if ( ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "bad cache size after set resize re-config 4.\n";
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
    if ( pass2 ) { /* first epoch */

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != at_max_size ) ||
             ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 36.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass2 ) { /* second epoch */

        rpt_fcn_called = FALSE;
        i = 1000;
        while ( ( pass2 ) && ( i < 2000 ) )
        {
            protect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != at_max_size ) ||
             ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 37.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass2 ) { /* third epoch */

        rpt_fcn_called = FALSE;
        i = 2000;
        while ( ( pass2 ) && ( i < 3000 ) )
        {
            protect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != at_max_size ) ||
             ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 38.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* fourth epoch -- If the hit rate were above the lower threshold,
     * we would see cache size reduction now.  However, nothing will
     * happen until we get the hit rate above the lower threshold.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 3000;
        while ( ( pass2 ) && ( i < 4000 ) )
        {
            protect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != at_max_size ) ||
             ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 39.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* fifth epoch -- force the hit rate to 100%.  We should see cache size
     * reduction now.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 3000;
        while ( ( pass2 ) && ( i < 4000 ) )
        {
            protect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (7 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (7 * 512 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 40.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* sixth epoch -- force the hit rate to 100% again.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 2000;
        while ( ( pass2 ) && ( i < 3000 ) )
        {
            protect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (6 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (6 * 512 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 41.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* seventh epoch -- keep hit rate at 100%, and keep 2K entries active.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 3000;
        while ( ( pass2 ) && ( i < 4000 ) )
        {
            protect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (5 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (5 * 512 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 42.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* eigth epoch -- still 100% hit rate
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 2000;
        while ( ( pass2 ) && ( i < 3000 ) )
        {
            protect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 512 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 43.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* ninth epoch --hit rate at 100%.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 3000;
        while ( ( pass2 ) && ( i < 4000 ) )
        {
            protect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (3 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (3 * 512 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 44.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* tenth epoch -- still 100% hit rate
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 2000;
        while ( ( pass2 ) && ( i < 3000 ) )
        {
            protect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (2 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 512 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 45.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* eleventh epoch -- hit rate at 100% -- starting to stableize
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 3000;
        while ( ( pass2 ) && ( i < 4000 ) )
        {
            protect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (2000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (int)(2000 * 512) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 46.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* twelth epoch -- force the hit rate to 100% again -- should be steady
     * state.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 2000;
        while ( ( pass2 ) && ( i < 3000 ) )
        {
            protect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (2000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (int)(2000 * 512) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 47.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* now just bang on one entry -- after three epochs, this should
     * get all entries other than the one evicted, and the cache size
     * should be decreased to the minimum.
     */
    if ( pass2 ) { /* thirteenth epoch */

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (2000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (int)(2000 * 512) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 48.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass2 ) { /* fourteenth epoch */

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size !=
               (1001 * 1024 + MONSTER_ENTRY_SIZE) ) ||
             ( cache_ptr->min_clean_size !=
               (1001 * 512 + MONSTER_ENTRY_SIZE / 2) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 49.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass2 ) { /* fifteenth epoch -- cache size reduction */

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
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

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 50.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass2 ) { /* sixteenth epoch -- at minimum size so no more ageouts */

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
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

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 51.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);


    /* repeat the test yet again, this time with empty reserve enabled.
     * Again, some structural changes in the test are necessary.
     */

    if ( pass2 ) {

        auto_size_ctl.version                = H5C2__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 8 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 8 * 1024 * 1024;
        auto_size_ctl.min_size               = 512 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C2_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (4 * 1024 * 1024);

	auto_size_ctl.flash_incr_mode        = H5C2_flash_incr__off;
	auto_size_ctl.flash_multiple         = 2.0;
	auto_size_ctl.flash_threshold        = 0.5;


        auto_size_ctl.decr_mode              = H5C2_decr__age_out;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = FALSE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 3;

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 0.5; /* for ease of testing */

        result = H5C2_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_set_cache_auto_resize_config failed 6.\n";
        }
    }

    if ( pass2 ) {

        if ( ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "bad cache size after set resize re-config 5.\n";
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
    if ( pass2 ) { /* first epoch */

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != at_max_size ) ||
             ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 52.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass2 ) { /* second epoch */

        rpt_fcn_called = FALSE;
        i = 1000;
        while ( ( pass2 ) && ( i < 2000 ) )
        {
            protect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != at_max_size ) ||
             ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 53.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass2 ) { /* third epoch */

        rpt_fcn_called = FALSE;
        i = 2000;
        while ( ( pass2 ) && ( i < 3000 ) )
        {
            protect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != at_max_size ) ||
             ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 54.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* fourth epoch -- If the hit rate were above the lower threshold,
     * we would see cache size reduction now.  However, nothing will
     * happen until we get the hit rate above the lower threshold.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 3000;
        while ( ( pass2 ) && ( i < 4000 ) )
        {
            protect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != at_max_size ) ||
             ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 55.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* fifth epoch -- force the hit rate to 100%.  We should see cache size
     * reduction now.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 3000;
        while ( ( pass2 ) && ( i < 4000 ) )
        {
            protect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (4002 * 1024) ) ||
             ( cache_ptr->min_clean_size != (int)(4002 * 512) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 56.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* sixth epoch -- force the hit rate to 100% again.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 3000;
        while ( ( pass2 ) && ( i < 4000 ) )
        {
            protect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (2002 * 1024) ) ||
             ( cache_ptr->min_clean_size != (int)(2002 * 512) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 57.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* seventh epoch -- force the hit rate to 100% again.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 3000;
        while ( ( pass2 ) && ( i < 4000 ) )
        {
            protect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (2000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (int)(2000 * 512) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 58.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* eigth epoch -- force the hit rate to 100% again -- should be steady
     * state.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 3000;
        while ( ( pass2 ) && ( i < 4000 ) )
        {
            protect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (2000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (int)(2000 * 512) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 59.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* now just bang on one entry -- after three epochs, this should
     * get all entries other than the one evicted, and the cache size
     * should be decreased to the minimum.
     */
    if ( pass2 ) { /* ninth epoch */

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (2000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (int)(2000 * 512) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 60.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass2 ) { /* tenth epoch */

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (2000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2000 * 512) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 61.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass2 ) { /* eleventh epoch -- cache size reduction */

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
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

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 62.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass2 ) { /* twelth epoch -- at minimum size so no more ageouts */

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
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

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 63.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);


    /* Repeat the test again, this time using the age out with threshold
     * mode.  To simplify the testing, set epochs to eviction to 1.
     *
     * Again, there are some minor structural changes in the test.
     */

    if ( pass2 ) {

        auto_size_ctl.version                = H5C2__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 8 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 8 * 1024 * 1024;
        auto_size_ctl.min_size               = 512 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C2_incr__off;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (4 * 1024 * 1024);

	auto_size_ctl.flash_incr_mode        = H5C2_flash_incr__off;
	auto_size_ctl.flash_multiple         = 2.0;
	auto_size_ctl.flash_threshold        = 0.5;


        auto_size_ctl.decr_mode              = H5C2_decr__age_out_with_threshold;

        auto_size_ctl.upper_hr_threshold     = 0.999; /* for ease of testing */

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = FALSE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 1; /* for ease of testing */

        auto_size_ctl.apply_empty_reserve    = FALSE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C2_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_set_cache_auto_resize_config failed 7.\n";
        }
    }

    if ( pass2 ) {

        if ( ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "bad cache size after set resize re-config 6.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);


    /* fill the cache with 4K byte entries -- increment mode is off,
     * so cache size reduction should kick in as soon as we get the
     * hit rate above .999.
     */
    if ( pass2 ) { /* first epoch -- hit rate 0 */

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, LARGE_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, LARGE_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 64.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass2 ) { /* second epoch -- hit rate 0 */

        rpt_fcn_called = FALSE;
        i = 1000;
        while ( ( pass2 ) && ( i < 2000 ) )
        {
            protect_entry2(cache_ptr, LARGE_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, LARGE_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 65.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass2 ) { /* third epoch -- hit rate 1.0 -- should see decrease */

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, LARGE_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, LARGE_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (1001 * LARGE_ENTRY_SIZE) ) ||
             ( cache_ptr->min_clean_size != (1001 * LARGE_ENTRY_SIZE / 2) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 66.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* fourth epoch -- load up the cache again -- hit rate 0 */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (1001 * LARGE_ENTRY_SIZE) ) ||
             ( cache_ptr->min_clean_size != (1001 * LARGE_ENTRY_SIZE / 2) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 67.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* fifth epoch -- still loading up the cache -- hit rate 0 */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 1000;
        while ( ( pass2 ) && ( i < 2000 ) )
        {
            protect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (1001 * LARGE_ENTRY_SIZE) ) ||
             ( cache_ptr->min_clean_size != (1001 * LARGE_ENTRY_SIZE / 2) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 68.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* sixth epoch -- force hit rate to .998 -- should be no reduction */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 1002;
        while ( ( pass2 ) && ( i < 2002 ) )
        {
            protect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (1001 * LARGE_ENTRY_SIZE) ) ||
             ( cache_ptr->min_clean_size != (1001 * LARGE_ENTRY_SIZE / 2) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 69.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* seventh epoch -- force hit rate to .999 -- should see reduction
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 1003;
        while ( ( pass2 ) && ( i < 2003 ) )
        {
            protect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (1000 * MEDIUM_ENTRY_SIZE) ) ||
             ( cache_ptr->min_clean_size != (1000 * MEDIUM_ENTRY_SIZE / 2) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 70.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);


    /* We have now tested all the major ageout modes individually.
     * Lets try them all together to look for unexpected interactions
     * and/or bugs.
     */

    if ( pass2 ) {

        auto_size_ctl.version                = H5C2__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 8 * 1000 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 8 * 1000 * 1024;
        auto_size_ctl.min_size               = 512 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C2_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (4 * 1024 * 1024);

	auto_size_ctl.flash_incr_mode        = H5C2_flash_incr__off;
	auto_size_ctl.flash_multiple         = 2.0;
	auto_size_ctl.flash_threshold        = 0.5;


        auto_size_ctl.decr_mode              = H5C2_decr__age_out_with_threshold;

        auto_size_ctl.upper_hr_threshold     = 0.999; /* for ease of testing */

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = TRUE;
        auto_size_ctl.max_decrement          = (1 * 1000 * 1024);

        auto_size_ctl.epochs_before_eviction = 1; /* for ease of testing */

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 0.5; /* for ease of testing */

        result = H5C2_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_set_cache_auto_resize_config failed 8.\n";
        }
    }

    if ( pass2 ) {

        if ( ( cache_ptr->max_cache_size != (8 * 1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1000 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "bad cache size after set resize re-config 7.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* fill the cache with 4K byte entries -- increment mode is threshold,
     * so the decrease code will not be executed until the hit rate exceeds
     * .75.
     */
    if ( pass2 ) { /* first epoch -- hit rate 0 */

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, LARGE_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, LARGE_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != at_max_size ) ||
             ( cache_ptr->max_cache_size != (8 * 1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1000 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 71.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass2 ) { /* second epoch -- hit rate 0 */

        rpt_fcn_called = FALSE;
        i = 1000;
        while ( ( pass2 ) && ( i < 2000 ) )
        {
            protect_entry2(cache_ptr, LARGE_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, LARGE_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != at_max_size ) ||
             ( cache_ptr->max_cache_size != (8 * 1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1000 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 72.\n";
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
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, LARGE_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, LARGE_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (8 * 1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1000 * 1024) ) ||
             ( cache_ptr->index_size != (7 * 1000 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 73.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* fourth epoch -- hit rate still 1.0.  Index size should decrease,
     * but otherwise no change expected.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, LARGE_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, LARGE_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (8 * 1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1000 * 1024) ) ||
             ( cache_ptr->index_size != (6 * 1000 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 74.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* fifth epoch -- hit rate still 1.0.  Index size should decrease,
     * but otherwise no change expected.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, LARGE_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, LARGE_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (8 * 1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1000 * 1024) ) ||
             ( cache_ptr->index_size != (5 * 1000 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 75.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* sixth epoch -- hit rate still 1.0.  Index size should decrease,
     * but otherwise no change expected.  Note that the cache size is
     * now just on the edge of meeting the clean reserve.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, LARGE_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, LARGE_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (8 * 1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1000 * 1024) ) ||
             ( cache_ptr->index_size != (4 * 1000 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 76.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* seventh epoch -- hit rate still 1.0.  No change in index size expected.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, LARGE_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, LARGE_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (8 * 1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1000 * 1024) ) ||
             ( cache_ptr->index_size != (4 * 1000 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 77.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* eighth epoch -- start loading 1 KB entries.  Hit rate 0 so
     * decrease code shouldn't be called.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != at_max_size ) ||
             ( cache_ptr->max_cache_size != (8 * 1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1000 * 1024) ) ||
             ( cache_ptr->index_size != (5 * 1000 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 78.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* ninth epoch -- access the 1 KB entries again, driving the hit rate
     * to 1.0.  Decrease code should be triggered, but the max decrease
     * should prevent the empty reserve from being met in this epoch.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (8 * 1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1000 * 1024) ) ||
             ( cache_ptr->index_size != (4 * 1000 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 79.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* tenth epoch -- access the 1 KB entries yet again, forcing hit rate
     * to 1.0.  Decrease code should be triggered, and the empty reserve
     * should finally be met.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (7 * 1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (7 * 1000 * 1024 / 2) ) ||
             ( cache_ptr->index_size != (3 * 1000 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 80.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* eleventh epoch -- access the 1 KB entries yet again, forcing hit rate
     * to 1.0.  Decrease code should be triggered, and the empty reserve
     * should be met again.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (6 * 1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (3 * 1000 * 1024) ) ||
             ( cache_ptr->index_size != (2 * 1000 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 81.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* twelth  epoch -- hit rate 1.0 -- decrease as before.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (5 * 1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (5 * 1000 * 1024 / 2) ) ||
             ( cache_ptr->index_size != (1 * 1000 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 82.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* thirteenth  epoch -- hit rate 1.0 -- decrease as before.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (4 * 1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1000 * 1024) ) ||
             ( cache_ptr->index_size != (1 * 1000 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 83.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* fourteenth  epoch -- hit rate 1.0 -- decrease as before.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (3 * 1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (3 * 1000 * 1024 / 2) ) ||
             ( cache_ptr->index_size != (1 * 1000 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 84.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* fifteenth  epoch -- hit rate 1.0 -- decrease as before.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (2 * 1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (1 * 1000 * 1024) ) ||
             ( cache_ptr->index_size != (1 * 1000 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 85.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* sixteenth  epoch -- hit rate 1.0 -- should be stable now
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (2 * 1000 * 1024) ) ||
             ( cache_ptr->min_clean_size != (1 * 1000 * 1024) ) ||
             ( cache_ptr->index_size != (1 * 1000 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 86.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);


    /* now test the flash cache size increment code.  At least at present,
     * there should be no interaction between the regular auto-resize 
     * code and the flash cache size increment code other than a reset 
     * of the counter and stats collection used by the regular auto-resize
     * code.  Thus we do only limited tests of the two pieces of code 
     * operating together.
     *
     * Start with simple test to verify that the flash cache increment
     * code increases the cache size when and as expected.
     */

    /* Place the cache in a know state via a flush destroy on the cache 
     * to clear out all entries, and then a reset on all the entries.  
     * Then configure the cache for the flash cache size increase tests, 
     * and force the flash size increase code through all its operational 
     * modes.  Verify that all perform as expected.
     */

    if ( pass2 ) {

        flush_cache2(cache_ptr, TRUE, FALSE, FALSE);

	reset_entries2();
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass2 ) {

        auto_size_ctl.version                = H5C2__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 64 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 1024 * 1024;
        auto_size_ctl.min_size               = 5 * 1024;

        auto_size_ctl.epoch_length           = 100;


        auto_size_ctl.incr_mode              = H5C2_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (32 * 1024);

	auto_size_ctl.flash_incr_mode        = H5C2_flash_incr__add_space;
	auto_size_ctl.flash_multiple         = 1.0;
	auto_size_ctl.flash_threshold        = 0.5;


        auto_size_ctl.decr_mode              = H5C2_decr__age_out_with_threshold;

        auto_size_ctl.upper_hr_threshold     = 0.999; /* for ease of testing */

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = TRUE;
        auto_size_ctl.max_decrement          = (1 * 1000 * 1024);

        auto_size_ctl.epochs_before_eviction = 1; /* for ease of testing */

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 0.5; /* for ease of testing */

        result = H5C2_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_set_cache_auto_resize_config failed 12.\n";
        }
    }

    if ( pass2 ) {

        if ( ( cache_ptr->max_cache_size != (64 * 1024) ) ||
             ( cache_ptr->min_clean_size != (32 * 1024) ) ||
	     ( cache_ptr->index_len != 0 ) ||
	     ( cache_ptr->index_size != 0 ) ||
	     ( cache_ptr->cache_accesses != 0 ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache config (0).\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Load a huge entry into the cache */
    if ( pass2 ) {

        protect_entry2(cache_ptr, HUGE_ENTRY_TYPE, 0);

        if ( pass2 ) {
            unprotect_entry2(cache_ptr, HUGE_ENTRY_TYPE, 0,
                            NO_CHANGE, H5C2__NO_FLAGS_SET);
        }

	if ( ( pass2 ) &&
	     ( ( ( cache_ptr->max_cache_size != (64 * 1024) ) ||
		 ( cache_ptr->min_clean_size != (32 * 1024) ) ||
		 ( cache_ptr->index_len != 1 ) ||
                 ( cache_ptr->index_size != HUGE_ENTRY_SIZE ) ||
		 ( cache_ptr->cache_accesses != 1 ) ) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache config (1).\n";
	}
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Now load a monster entry.  Since a monster entry is larger than
     * half the size of the cache, and there is not sufficient space
     * for a monster entry in the cache, we will add space to the 
     * cache to make room for the entry.
     */
    if ( pass2 ) {

        protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0);

        if ( pass2 ) {
            unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                            NO_CHANGE, H5C2__NO_FLAGS_SET);
        }

	if ( ( pass2 ) &&
	     ( ( ( cache_ptr->max_cache_size != (80 * 1024) ) ||
		 ( cache_ptr->min_clean_size != (40 * 1024) ) ||
		 ( cache_ptr->index_len != 2 ) ||
                 ( cache_ptr->index_size != (HUGE_ENTRY_SIZE + 
		                             MONSTER_ENTRY_SIZE) ) ||
		 ( cache_ptr->cache_accesses != 1 ) ) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache config (2).\n";
	}
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Load a second monster entry.  Since the monster entry is larger
     * than half the size of the cache yet again, and there is not
     * sufficient space for the monster entry in the cache, we again
     * add space to the cache to make space for the entry.
     */
    if ( pass2 ) {

        protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 1);

        unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 1,
                        NO_CHANGE, H5C2__NO_FLAGS_SET);

	if ( ( pass2 ) &&
	     ( ( ( cache_ptr->max_cache_size != (144 * 1024) ) ||
		 ( cache_ptr->min_clean_size != ( 72 * 1024) ) ||
		 ( cache_ptr->index_len != 3 ) ||
                 ( cache_ptr->index_size != ((2 * MONSTER_ENTRY_SIZE) +
					     HUGE_ENTRY_SIZE) ) ||
		 ( cache_ptr->cache_accesses != 1 ) ) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache config (3).\n";
	}
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Load a third moster entry.  Should be no cache size increase this
     * time.
     */
    if ( pass2 ) {

        protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 2);

        unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 2,
                        NO_CHANGE, H5C2__NO_FLAGS_SET);

	if ( ( pass2 ) &&
	     ( ( ( cache_ptr->max_cache_size != (144 * 1024) ) ||
		 ( cache_ptr->min_clean_size != ( 72 * 1024) ) ||
		 ( cache_ptr->index_len != 2 ) ||
                 ( cache_ptr->index_size != (2 * MONSTER_ENTRY_SIZE) ) ||
		 ( cache_ptr->cache_accesses != 2 ) ) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache config (4).\n";
	}
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* delete existing entries to prepare for next test, and reset 
     * the size of the cache.
     */ 
    if ( pass2 ) {

	expunge_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 1);
	expunge_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 2);

        if ( pass2 ) {

            result = H5C2_set_cache_auto_resize_config(cache_ptr, 
			                              &auto_size_ctl);

            if ( result != SUCCEED ) {

                pass2 = FALSE; 
                failure_mssg2 = 
			"H5C2_set_cache_auto_resize_config failed 13.\n";
	    }
        }

	if ( ( pass2 ) &&
	     ( ( ( cache_ptr->max_cache_size != (64 * 1024) ) ||
		 ( cache_ptr->min_clean_size != (32 * 1024) ) ||
		 ( cache_ptr->index_len != 0 ) ||
                 ( cache_ptr->index_size != 0 ) ||
		 ( cache_ptr->cache_accesses != 0 ) ) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache config (5).\n";
	}
    }

    /* repeat the above basic test, only this time, use inserts to add 
     * entries to the cache, not protects.
     */

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* insert a huge entry into the cache */
    if ( pass2 ) {

        insert_entry2(cache_ptr, HUGE_ENTRY_TYPE, 1, TRUE,
		     H5C2__NO_FLAGS_SET);
	/* protect and unprotect a couple times to increment cache_accesses */
        protect_entry2(cache_ptr, HUGE_ENTRY_TYPE, 1);
        unprotect_entry2(cache_ptr, HUGE_ENTRY_TYPE, 1,
                        NO_CHANGE, H5C2__NO_FLAGS_SET);
        protect_entry2(cache_ptr, HUGE_ENTRY_TYPE, 1);
        unprotect_entry2(cache_ptr, HUGE_ENTRY_TYPE, 1,
                        NO_CHANGE, H5C2__NO_FLAGS_SET);

	if ( ( pass2 ) &&
	     ( ( ( cache_ptr->max_cache_size != (64 * 1024) ) ||
		 ( cache_ptr->min_clean_size != (32 * 1024) ) ||
		 ( cache_ptr->index_len != 1 ) ||
                 ( cache_ptr->index_size != HUGE_ENTRY_SIZE ) ||
		 ( cache_ptr->cache_accesses != 2 ) ) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache config (6).\n";
	}
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Now insert a monster entry.  Since a monster entry is larger than
     * half the size of the cache, and there is not sufficient space
     * for a monster entry in the cache, we will add space to the 
     * cache to make room for the entry.
     */
    if ( pass2 ) {

        insert_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 4, TRUE,
		     H5C2__NO_FLAGS_SET);

	if ( ( pass2 ) &&
	     ( ( ( cache_ptr->max_cache_size != (80 * 1024) ) ||
		 ( cache_ptr->min_clean_size != (40 * 1024) ) ||
		 ( cache_ptr->index_len != 2 ) ||
                 ( cache_ptr->index_size != 
		   HUGE_ENTRY_SIZE + MONSTER_ENTRY_SIZE ) ||
		 ( cache_ptr->cache_accesses != 0 ) ) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache config (7).\n";
	}
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Insert a second monster entry.  Cache size should increase again.
     */
    if ( pass2 ) {

        insert_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 5, TRUE, 
		     H5C2__NO_FLAGS_SET);

	if ( ( pass2 ) &&
	     ( ( ( cache_ptr->max_cache_size != (144 * 1024) ) ||
		 ( cache_ptr->min_clean_size != ( 72 * 1024) ) ||
		 ( cache_ptr->index_len != 3 ) ||
                 ( cache_ptr->index_size != 
		   2 * MONSTER_ENTRY_SIZE + HUGE_ENTRY_SIZE ) ||
		 ( cache_ptr->cache_accesses != 0 ) ) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache config (8).\n";
	}
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Insert a third monster entry.  Should be no cache size increase this
     * time.
     */
    if ( pass2 ) {

        protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 6);

        unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 6,
                        NO_CHANGE, H5C2__NO_FLAGS_SET);

	if ( ( pass2 ) &&
	     ( ( ( cache_ptr->max_cache_size != (144 * 1024) ) ||
		 ( cache_ptr->min_clean_size != ( 72 * 1024) ) ||
		 ( cache_ptr->index_len != 2 ) ||
                 ( cache_ptr->index_size != (2 * MONSTER_ENTRY_SIZE) ) ||
		 ( cache_ptr->cache_accesses != 1 ) ) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache config (9).\n";
	}
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* delete existing entries to prepare for next test, and reset 
     * the size of the cache.  We must also change the size of the needed
     * variable entries before we run the test, so will protect and 
     * unprotect them now so as to get the correct initial size.
     */ 
    if ( pass2 ) {

	expunge_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 5);
	expunge_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 6);

        protect_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 10);
        unprotect_entry_with_size_change2(cache_ptr, VARIABLE_ENTRY_TYPE, 10,
                        H5C2__DIRTIED_FLAG|H5C2__SIZE_CHANGED_FLAG, 1024);

        protect_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 11);
        unprotect_entry_with_size_change2(cache_ptr, VARIABLE_ENTRY_TYPE, 11,
                        H5C2__DIRTIED_FLAG|H5C2__SIZE_CHANGED_FLAG, 1024);

        protect_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 12);
        unprotect_entry_with_size_change2(cache_ptr, VARIABLE_ENTRY_TYPE, 12,
                        H5C2__DIRTIED_FLAG|H5C2__SIZE_CHANGED_FLAG, 1024);

        protect_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 13);
        unprotect_entry_with_size_change2(cache_ptr, VARIABLE_ENTRY_TYPE, 13,
                        H5C2__DIRTIED_FLAG|H5C2__SIZE_CHANGED_FLAG, 1024);

	flush_cache2(cache_ptr, TRUE, FALSE, FALSE);


        if ( pass2 ) {

            auto_size_ctl.initial_size           = 6 * 1024;
            result = H5C2_set_cache_auto_resize_config(cache_ptr, 
			                              &auto_size_ctl);

            if ( result != SUCCEED ) {

                pass2 = FALSE; 
                failure_mssg2 = 
			"H5C2_set_cache_auto_resize_config failed 13.\n";
	    }
        }

	if ( ( pass2 ) &&
	     ( ( ( cache_ptr->max_cache_size != (6 * 1024) ) ||
		 ( cache_ptr->min_clean_size != (3 * 1024) ) ||
		 ( cache_ptr->index_len != 0 ) ||
                 ( cache_ptr->index_size != 0 ) ||
		 ( cache_ptr->cache_accesses != 0 ) ) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache config (10).\n";
	}
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Now load the variable entries into the cache */
    if ( pass2 ) {

        protect_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 10);
        unprotect_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 10, NO_CHANGE, 
			H5C2__NO_FLAGS_SET);

        protect_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 11);
        unprotect_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 11, NO_CHANGE, 
			H5C2__NO_FLAGS_SET);

        protect_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 12);
        unprotect_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 12, NO_CHANGE,
			H5C2__NO_FLAGS_SET);

        protect_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 13);
        unprotect_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 13, NO_CHANGE,
                        H5C2__NO_FLAGS_SET);

	if ( ( pass2 ) &&
	     ( ( ( cache_ptr->max_cache_size != (6 * 1024) ) ||
		 ( cache_ptr->min_clean_size != (3 * 1024) ) ||
		 ( cache_ptr->index_len != 4 ) ||
                 ( cache_ptr->index_size != 4 * 1024 ) ||
		 ( cache_ptr->cache_accesses != 4 ) ) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache config (11).\n";
	}
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* protect a variable entry, and re-size it to 3K.  Should be
     * no effect on the size of the cache.
     */
    if ( pass2 ) {

        protect_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 10);
        unprotect_entry_with_size_change2(cache_ptr, VARIABLE_ENTRY_TYPE, 10,
                        H5C2__DIRTIED_FLAG|H5C2__SIZE_CHANGED_FLAG, 3 * 1024);


	if ( ( pass2 ) &&
	     ( ( ( cache_ptr->max_cache_size != (6 * 1024) ) ||
		 ( cache_ptr->min_clean_size != (3 * 1024) ) ||
		 ( cache_ptr->index_len != 4 ) ||
                 ( cache_ptr->index_size != 6 * 1024 ) ||
		 ( cache_ptr->cache_accesses != 5 ) ) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache config (12).\n";
	}
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* protect the variable entry again, and re-size it to 10K.  Should
     * resize the cache to 13 KB.  Note that cache_accesses will be 0
     * in this case, since cache_accesses is incremented on the protect.
     */
    if ( pass2 ) {

        protect_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 10);
        unprotect_entry_with_size_change2(cache_ptr, VARIABLE_ENTRY_TYPE, 10,
                        H5C2__DIRTIED_FLAG|H5C2__SIZE_CHANGED_FLAG, 10 * 1024);


	if ( ( pass2 ) &&
	     ( ( ( cache_ptr->max_cache_size != (13 * 1024) ) ||
		 ( cache_ptr->min_clean_size != (13 * 512) ) ||
		 ( cache_ptr->index_len != 4 ) ||
                 ( cache_ptr->index_size != 13 * 1024 ) ||
		 ( cache_ptr->cache_accesses != 0 ) ) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache config (13).\n";
	}
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* protect a second variable entry, and re-size it to 10K.  Should
     * resize to 22 KB.
     */
    if ( pass2 ) {

        protect_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 11);
        unprotect_entry_with_size_change2(cache_ptr, VARIABLE_ENTRY_TYPE, 11,
                        H5C2__DIRTIED_FLAG|H5C2__SIZE_CHANGED_FLAG, 10 * 1024);


	if ( ( pass2 ) &&
	     ( ( ( cache_ptr->max_cache_size != (22 * 1024) ) ||
		 ( cache_ptr->min_clean_size != (11 * 1024) ) ||
		 ( cache_ptr->index_len != 4 ) ||
                 ( cache_ptr->index_size != 22 * 1024 ) ||
		 ( cache_ptr->cache_accesses != 0 ) ) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache config (14).\n";
	}
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* protect a third variable entry, and re-size it to 10K.  Should
     * be no change in cache size.
     */
    if ( pass2 ) {

        protect_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 12);
        unprotect_entry_with_size_change2(cache_ptr, VARIABLE_ENTRY_TYPE, 12,
                        H5C2__DIRTIED_FLAG|H5C2__SIZE_CHANGED_FLAG, 10 * 1024);


	if ( ( pass2 ) &&
	     ( ( ( cache_ptr->max_cache_size != (22 * 1024) ) ||
		 ( cache_ptr->min_clean_size != (11 * 1024) ) ||
		 ( cache_ptr->index_len != 4 ) ||
                 ( cache_ptr->index_size != 31 * 1024 ) ||
		 ( cache_ptr->cache_accesses != 1 ) ) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache config (15).\n";
	}
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* re-size the variable entries back down to their initial size, and
     * restore the cache to its initial size as well, in preparation 
     * for the next test.
     */
    if ( pass2 ) {

        protect_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 10);
        unprotect_entry_with_size_change2(cache_ptr, VARIABLE_ENTRY_TYPE, 10,
                        H5C2__DIRTIED_FLAG|H5C2__SIZE_CHANGED_FLAG, 1 * 1024);
        protect_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 11);
        unprotect_entry_with_size_change2(cache_ptr, VARIABLE_ENTRY_TYPE, 11,
                        H5C2__DIRTIED_FLAG|H5C2__SIZE_CHANGED_FLAG, 1 * 1024);
        protect_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 12);
        unprotect_entry_with_size_change2(cache_ptr, VARIABLE_ENTRY_TYPE, 12,
                        H5C2__DIRTIED_FLAG|H5C2__SIZE_CHANGED_FLAG, 1 * 1024);

        if ( pass2 ) {

            auto_size_ctl.initial_size           = 6 * 1024;
            result = H5C2_set_cache_auto_resize_config(cache_ptr, 
			                              &auto_size_ctl);

            if ( result != SUCCEED ) {

                pass2 = FALSE; 
                failure_mssg2 = 
			"H5C2_set_cache_auto_resize_config failed 14.\n";
	    }
        }

	if ( ( pass2 ) &&
	     ( ( ( cache_ptr->max_cache_size != (6 * 1024) ) ||
		 ( cache_ptr->min_clean_size != (3 * 1024) ) ||
		 ( cache_ptr->index_len != 4 ) ||
                 ( cache_ptr->index_size != 4 * 1024 ) ||
		 ( cache_ptr->cache_accesses != 0 ) ) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache config (16).\n";
	}
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Now test flash cache resizes with pinned entries...
     */
    if ( pass2 ) {

        protect_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 10);
        unprotect_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 10, NO_CHANGE, 
			H5C2__PIN_ENTRY_FLAG);
        resize_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 10, 2 * 1024, TRUE);

	if ( ( pass2 ) &&
	     ( ( ( cache_ptr->max_cache_size != (6 * 1024) ) ||
		 ( cache_ptr->min_clean_size != (6 * 512) ) ||
		 ( cache_ptr->index_len != 4 ) ||
                 ( cache_ptr->index_size != 5 * 1024 ) ||
		 ( cache_ptr->cache_accesses != 1 ) ) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache config (17).\n";
	}
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass2 ) {

        resize_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 10, 10 * 1024, TRUE);

	if ( ( pass2 ) &&
	     ( ( ( cache_ptr->max_cache_size != (13 * 1024) ) ||
		 ( cache_ptr->min_clean_size != (13 * 512) ) ||
		 ( cache_ptr->index_len != 4 ) ||
                 ( cache_ptr->index_size != 13 * 1024 ) ||
		 ( cache_ptr->cache_accesses != 0 ) ) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache config (18).\n";
	}
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass2 ) {

        protect_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 11);
        unprotect_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 11, NO_CHANGE, 
			H5C2__PIN_ENTRY_FLAG);
        resize_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 11, 10 * 1024, TRUE);

	if ( ( pass2 ) &&
	     ( ( ( cache_ptr->max_cache_size != (22 * 1024) ) ||
		 ( cache_ptr->min_clean_size != (11 * 1024) ) ||
		 ( cache_ptr->index_len != 4 ) ||
                 ( cache_ptr->index_size != 22 * 1024 ) ||
		 ( cache_ptr->cache_accesses != 0 ) ) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache config (19).\n";
	}
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass2 ) {

        protect_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 12);
        unprotect_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 12, NO_CHANGE, 
			H5C2__PIN_ENTRY_FLAG);
        resize_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 12, 10 * 1024, TRUE);

	if ( ( pass2 ) &&
	     ( ( ( cache_ptr->max_cache_size != (22 * 1024) ) ||
		 ( cache_ptr->min_clean_size != (11 * 1024) ) ||
		 ( cache_ptr->index_len != 4 ) ||
                 ( cache_ptr->index_size != 31 * 1024 ) ||
		 ( cache_ptr->cache_accesses != 1 ) ) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache config (20).\n";
	}
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Unpin the entries.  Note that no entries are evicted as we don't
     * load any entries.
     */
    if ( pass2 ) {

        protect_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 10);
        unprotect_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 10, NO_CHANGE, 
			H5C2__UNPIN_ENTRY_FLAG);

        protect_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 11);
        unprotect_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 11, NO_CHANGE, 
			H5C2__UNPIN_ENTRY_FLAG);

        protect_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 12);
        unprotect_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 12, NO_CHANGE, 
			H5C2__UNPIN_ENTRY_FLAG);

	if ( ( pass2 ) &&
	     ( ( ( cache_ptr->max_cache_size != (22 * 1024) ) ||
		 ( cache_ptr->min_clean_size != (11 * 1024) ) ||
		 ( cache_ptr->index_len != 4 ) ||
                 ( cache_ptr->index_size != 31 * 1024 ) ||
		 ( cache_ptr->cache_accesses != 4 ) ) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache config (21).\n";
	}
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* re-size the variable entries back down to their initial size, and
     * restore the cache to its initial size as well, in preparation 
     * for the next test.
     */
    if ( pass2 ) {

        protect_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 10);
        unprotect_entry_with_size_change2(cache_ptr, VARIABLE_ENTRY_TYPE, 10,
                        H5C2__DIRTIED_FLAG|H5C2__SIZE_CHANGED_FLAG, 1 * 1024);
        protect_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 11);
        unprotect_entry_with_size_change2(cache_ptr, VARIABLE_ENTRY_TYPE, 11,
                        H5C2__DIRTIED_FLAG|H5C2__SIZE_CHANGED_FLAG, 1 * 1024);
        protect_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 12);
        unprotect_entry_with_size_change2(cache_ptr, VARIABLE_ENTRY_TYPE, 12,
                        H5C2__DIRTIED_FLAG|H5C2__SIZE_CHANGED_FLAG, 1 * 1024);

        if ( pass2 ) {

            auto_size_ctl.initial_size           = 6 * 1024;
            result = H5C2_set_cache_auto_resize_config(cache_ptr, 
			                              &auto_size_ctl);

            if ( result != SUCCEED ) {

                pass2 = FALSE; 
                failure_mssg2 = 
			"H5C2_set_cache_auto_resize_config failed 15.\n";
	    }
        }

	if ( ( pass2 ) &&
	     ( ( ( cache_ptr->max_cache_size != (6 * 1024) ) ||
		 ( cache_ptr->min_clean_size != (3 * 1024) ) ||
		 ( cache_ptr->index_len != 4 ) ||
                 ( cache_ptr->index_size != 4 * 1024 ) ||
		 ( cache_ptr->cache_accesses != 0 ) ) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache config (22).\n";
	}
    }

    if ( pass2 ) {

        protect_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 10);
        unprotect_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 10, NO_CHANGE, 
			H5C2__PIN_ENTRY_FLAG);
        resize_pinned_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 10, 2 * 1024);

	if ( ( pass2 ) &&
	     ( ( ( cache_ptr->max_cache_size != (6 * 1024) ) ||
		 ( cache_ptr->min_clean_size != (6 * 512) ) ||
		 ( cache_ptr->index_len != 4 ) ||
                 ( cache_ptr->index_size != 5 * 1024 ) ||
		 ( cache_ptr->cache_accesses != 1 ) ) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache config (23).\n";
	}
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass2 ) {

        resize_pinned_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 10, 10 * 1024);

	if ( ( pass2 ) &&
	     ( ( ( cache_ptr->max_cache_size != (13 * 1024) ) ||
		 ( cache_ptr->min_clean_size != (13 * 512) ) ||
		 ( cache_ptr->index_len != 4 ) ||
                 ( cache_ptr->index_size != 13 * 1024 ) ||
		 ( cache_ptr->cache_accesses != 0 ) ) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache config (24).\n";
	}
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass2 ) {

        protect_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 11);
        unprotect_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 11, NO_CHANGE, 
			H5C2__PIN_ENTRY_FLAG);
        resize_pinned_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 11, 10 * 1024);

	if ( ( pass2 ) &&
	     ( ( ( cache_ptr->max_cache_size != (22 * 1024) ) ||
		 ( cache_ptr->min_clean_size != (11 * 1024) ) ||
		 ( cache_ptr->index_len != 4 ) ||
                 ( cache_ptr->index_size != 22 * 1024 ) ||
		 ( cache_ptr->cache_accesses != 0 ) ) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache config (25).\n";
	}
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass2 ) {

        protect_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 12);
        unprotect_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 12, NO_CHANGE, 
			H5C2__PIN_ENTRY_FLAG);
        resize_pinned_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 12, 10 * 1024);

	if ( ( pass2 ) &&
	     ( ( ( cache_ptr->max_cache_size != (22 * 1024) ) ||
		 ( cache_ptr->min_clean_size != (11 * 1024) ) ||
		 ( cache_ptr->index_len != 4 ) ||
                 ( cache_ptr->index_size != 31 * 1024 ) ||
		 ( cache_ptr->cache_accesses != 1 ) ) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache config (26).\n";
	}
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Unpin the entries.  Note that no entries are evicted as we don't
     * load any entries.
     */
    if ( pass2 ) {

        protect_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 10);
        unprotect_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 10, NO_CHANGE, 
			H5C2__UNPIN_ENTRY_FLAG);

        protect_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 11);
        unprotect_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 11, NO_CHANGE, 
			H5C2__UNPIN_ENTRY_FLAG);

        protect_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 12);
        unprotect_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 12, NO_CHANGE, 
			H5C2__UNPIN_ENTRY_FLAG);

	if ( ( pass2 ) &&
	     ( ( ( cache_ptr->max_cache_size != (22 * 1024) ) ||
		 ( cache_ptr->min_clean_size != (11 * 1024) ) ||
		 ( cache_ptr->index_len != 4 ) ||
                 ( cache_ptr->index_size != 31 * 1024 ) ||
		 ( cache_ptr->cache_accesses != 4 ) ) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache config (27).\n";
	}
    }

    /* We have finished a basic check of the flash cache size increment
     * code.  Tidy up for a more extensive test...
     */
    if ( pass2 ) {

	expunge_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 10);
	expunge_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 11);
	expunge_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 12);
	expunge_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 13);

	if ( ( pass2 ) &&
	     ( ( ( cache_ptr->max_cache_size != (22 * 1024) ) ||
		 ( cache_ptr->min_clean_size != (11 * 1024) ) ||
		 ( cache_ptr->index_len != 0 ) ||
                 ( cache_ptr->index_size != 0 ) ||
		 ( cache_ptr->cache_accesses != 4 ) ) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache config (28).\n";
	}
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* ...and then reconfigure.   Note that we change the flash_multiple
     * and flash_threshold just to make sure that such changed perform
     * as expected.
     */
    if ( pass2 ) {

        auto_size_ctl.version                = H5C2__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 4 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 20 * 1024;
        auto_size_ctl.min_size               =  4 * 1024;

        auto_size_ctl.epoch_length           = 100;


        auto_size_ctl.incr_mode              = H5C2_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (4 * 1024);

	auto_size_ctl.flash_incr_mode        = H5C2_flash_incr__add_space;
	auto_size_ctl.flash_multiple         = 2.0;
	auto_size_ctl.flash_threshold        = 0.4;


        auto_size_ctl.decr_mode              = H5C2_decr__age_out_with_threshold;

        auto_size_ctl.upper_hr_threshold     = 0.999; /* for ease of testing */

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = TRUE;
        auto_size_ctl.max_decrement          = (2 * 1024);

        auto_size_ctl.epochs_before_eviction = 1; /* for ease of testing */

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 0.5; /* for ease of testing */

        result = H5C2_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_set_cache_auto_resize_config failed 15.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass2 ) {

        if ( ( cache_ptr->max_cache_size != (4 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 512) ) ||
	     ( cache_ptr->index_len != 0 ) ||
	     ( cache_ptr->index_size != 0 ) ||
	     ( cache_ptr->cache_accesses != 0 ) ) {

            pass2 = FALSE;
            failure_mssg2 = "bad cache after initialization 15.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* protect and unprotect a large entry -- no change in cache size since
     * a large entry will just fill the available space in the cache.  
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;

        protect_entry2(cache_ptr, LARGE_ENTRY_TYPE, 0);
        unprotect_entry2(cache_ptr, LARGE_ENTRY_TYPE, 0, NO_CHANGE, 
			H5C2__NO_FLAGS_SET);

	if ( ( pass2 ) &&
	     ( ( ( cache_ptr->max_cache_size != (4 * 1024) ) ||
		 ( cache_ptr->min_clean_size != (4 * 512) ) ||
		 ( cache_ptr->index_len != 1 ) ||
                 ( cache_ptr->index_size != LARGE_ENTRY_SIZE ) ||
		 ( cache_ptr->cache_accesses != 1 )  ||
		 ( rpt_fcn_called == TRUE ) ) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache config (29).\n";
	}
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* protect and unprotect another a large entry -- should trigger a 
     * flash cache size increase to 12 KB (remember that flash_multiple is
     * set to 2.0).
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;

        protect_entry2(cache_ptr, LARGE_ENTRY_TYPE, 1);
        unprotect_entry2(cache_ptr, LARGE_ENTRY_TYPE, 1, NO_CHANGE, 
			H5C2__NO_FLAGS_SET);

	if ( ( pass2 ) &&
	     ( ( ( cache_ptr->max_cache_size != (12 * 1024) ) ||
		 ( cache_ptr->min_clean_size != (12 * 512) ) ||
		 ( cache_ptr->index_len != 2 ) ||
                 ( cache_ptr->index_size != 2 * LARGE_ENTRY_SIZE ) ||
		 ( cache_ptr->cache_accesses != 1 )  ||
		 ( rpt_fcn_called != TRUE ) ) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache config (30).\n";
	}
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* protect and unprotect two more large entries -- shouldnt trigger a 
     * flash cache size increase.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;

        protect_entry2(cache_ptr, LARGE_ENTRY_TYPE, 2);
        unprotect_entry2(cache_ptr, LARGE_ENTRY_TYPE, 2, NO_CHANGE, 
			H5C2__NO_FLAGS_SET);
        protect_entry2(cache_ptr, LARGE_ENTRY_TYPE, 3);
        unprotect_entry2(cache_ptr, LARGE_ENTRY_TYPE, 3, NO_CHANGE, 
			H5C2__NO_FLAGS_SET);

	if ( ( pass2 ) &&
	     ( ( ( cache_ptr->max_cache_size != (12 * 1024) ) ||
		 ( cache_ptr->min_clean_size != (12 * 512) ) ||
		 ( cache_ptr->index_len != 3 ) ||
                 ( cache_ptr->index_size != 3 * LARGE_ENTRY_SIZE ) ||
		 ( cache_ptr->cache_accesses != 3 )  ||
		 ( rpt_fcn_called != FALSE ) ) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache config (31).\n";
	}
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* do many accesses of a single entry to talk the cache into reducing 
     * its size to the minimum.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, TINY_ENTRY_TYPE, 0);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, TINY_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->max_cache_size != (4 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024) ) ||
             ( cache_ptr->index_size != (1 * TINY_ENTRY_SIZE) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache config (32).\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Force another flash increase */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;

        protect_entry2(cache_ptr, LARGE_ENTRY_TYPE, 0);
        unprotect_entry2(cache_ptr, LARGE_ENTRY_TYPE, 0, NO_CHANGE, 
			H5C2__NO_FLAGS_SET);

	if ( ( pass2 ) &&
	     ( ( ( cache_ptr->max_cache_size != (4 * 1024 + 128) ) ||
		 ( cache_ptr->min_clean_size != (2 * 1024 + 64) ) ||
		 ( cache_ptr->index_len != 2 ) ||
                 ( cache_ptr->index_size != 
		   LARGE_ENTRY_SIZE + TINY_ENTRY_SIZE ) ||
		 ( cache_ptr->cache_accesses != 1 )  ||
		 ( rpt_fcn_called == FALSE ) || 
                 ( rpt_status != flash_increase ) ) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache config (33).\n";
	}
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force regular size increase up to maximum */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 500 ) )
        {
            protect_entry2(cache_ptr, TINY_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, TINY_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( cache_ptr->max_cache_size != (20 * 1024) ) ||
             ( cache_ptr->min_clean_size != (10 * 1024) ) ||
	     ( rpt_fcn_called == FALSE ) || 
             ( rpt_status != at_max_size ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache config (34).\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);


    if ( pass2 ) {

        takedown_cache2(cache_ptr, FALSE, FALSE);
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass2 ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass2 ) {

        HDfprintf(stdout, "%s: failure_mssg2 = \"%s\".\n",
                  fcn_name, failure_mssg2);
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
 *              Added code to include the flash cache size increment
 *              code in this test.
 *                                                      JRM -- 1/10/08
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
    H5C2_t * cache_ptr = NULL;
    H5C2_auto_size_ctl_t auto_size_ctl =
    {
        /* int32_t     version                = */ H5C2__CURR_AUTO_SIZE_CTL_VER,
        /* H5C2_auto_resize_report_fcn rpt_fcn = */ test_rpt_fcn,

        /* hbool_t     set_initial_size       = */ TRUE,
        /* size_t      initial_size           = */ (512 * 1024),

        /* double      min_clean_fraction     = */ 0.5,

        /* size_t      max_size               = */ (14 * 1024 * 1024),
        /* size_t      min_size               = */ (512 * 1024),

        /* int64_t     epoch_length           = */ 1000,


        /* enum H5C2_cache_incr_mode incr_mode = */ H5C2_incr__threshold,

        /* double     lower_hr_threshold      = */ 0.75,

        /* double      increment              = */ 2.0,

        /* hbool_t     apply_max_increment    = */ TRUE,
        /* size_t      max_increment          = */ (4 * 1024 * 1024),

        /* enum H5C2_cache_flash_incr_mode      */
	/*                    flash_incr_mode = */ H5C2_flash_incr__off,
	/* double      flash_multiple         = */ 2.0,
	/* double      flash_threshold        = */ 0.5,


        /* enum H5C2_cache_decr_mode decr_mode = */ H5C2_decr__threshold,

        /* double      upper_hr_threshold     = */ 0.995,

        /* double      decrement              = */ 0.1,

        /* hbool_t     apply_max_decrement    = */ TRUE,
        /* size_t      max_decrement          = */ (1 * 1024 * 1024),

        /* int32_t     epochs_before_eviction = */ 3,

        /* hbool_t     apply_empty_reserve    = */ TRUE,
        /* double      empty_reserve          = */ 0.05
    };

    TESTING("automatic cache resize disable");

    pass2 = TRUE;

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* allocate a cache, enable automatic cache resizing, and then force
     * the cache through all its operational modes.  Verify that all
     * performs as expected.
     */

    if ( pass2 ) {

        reset_entries2();

        cache_ptr = setup_cache2((size_t)(2 * 1024),
                                (size_t)(1 * 1024));
    }

    if ( pass2 ) {

        result = H5C2_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_set_cache_auto_resize_config failed 1.\n";
        }
    }

    if ( pass2 ) {

        if ( ( cache_ptr->max_cache_size != (512 * 1024) ) ||
             ( cache_ptr->min_clean_size != (256 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "bad cache size after initialization.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);


    /******************************************************************
     * So far, we have forced the auto cache resize through all modes
     * other than increase_disabled and decrease_disabled.  Force these
     * modes now.  Note that there are several ways we can reach these
     * modes.
     ******************************************************************/

    if ( pass2 ) {

        auto_size_ctl.version                = H5C2__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 16 * 1024 * 1024;
        auto_size_ctl.min_size               = 1 * 1024 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C2_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 1.0; /* disable size increases */

        auto_size_ctl.apply_max_increment    = FALSE;
        auto_size_ctl.max_increment          = (4 * 1024 * 1024);

	auto_size_ctl.flash_incr_mode        = H5C2_flash_incr__off;
	auto_size_ctl.flash_multiple         = 2.0;
	auto_size_ctl.flash_threshold        = 0.5;


        auto_size_ctl.decr_mode              = H5C2_decr__threshold;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = FALSE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 3;

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C2_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_set_cache_auto_resize_config failed 2.\n";
        }
    }

    if ( pass2 ) {

        if ( ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "bad cache size after set resize re-config 1.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate with cache full -- increase disabled so should
     * be no change in cache size, and result should be increase_disabled.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_increase_possible ) ||
             ( rpt_status != increase_disabled ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 1.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate -- make sure that we haven't disabled decreases.
     * should result in a decrease cache size from 4 to 2 Meg.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (2 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (1 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 2.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate again -- increase disabled so should
     * be no change in cache size, and result should be increase_disabled.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_increase_possible ) ||
             ( rpt_status != increase_disabled ) ||
             ( cache_ptr->max_cache_size != (2 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (1 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 3.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Repeat the above tests, disabling increase through the lower
     * threshold instead of the increment.
     */

    if ( pass2 ) {

        auto_size_ctl.version                = H5C2__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 16 * 1024 * 1024;
        auto_size_ctl.min_size               = 1 * 1024 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C2_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.0; /* disable size increases */

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = FALSE;
        auto_size_ctl.max_increment          = (4 * 1024 * 1024);

	auto_size_ctl.flash_incr_mode        = H5C2_flash_incr__off;
	auto_size_ctl.flash_multiple         = 2.0;
	auto_size_ctl.flash_threshold        = 0.5;


        auto_size_ctl.decr_mode              = H5C2_decr__threshold;

        auto_size_ctl.upper_hr_threshold     = 0.995;
        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = FALSE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 3;

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C2_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_set_cache_auto_resize_config failed 3.\n";
        }
    }

    if ( pass2 ) {

        if ( ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "bad cache size after set resize re-config 2.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate with cache full -- increase disabled so should
     * be no change in cache size, and result should be in_spec.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_increase_possible ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 4.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate -- make sure that we haven't disabled decreases.
     * should result in a decrease cache size from 4 to 2 Meg.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (2 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (1 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 5.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate again -- increase disabled so should
     * be no change in cache size, and result should be increase_disabled.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_increase_possible ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (2 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (1 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 6.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Repeat the above tests yet again, disabling increase through the
     * incr_mode.
     */

    if ( pass2 ) {

        auto_size_ctl.version                = H5C2__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 16 * 1024 * 1024;
        auto_size_ctl.min_size               = 1 * 1024 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C2_incr__off;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = FALSE;
        auto_size_ctl.max_increment          = (4 * 1024 * 1024);

	auto_size_ctl.flash_incr_mode        = H5C2_flash_incr__off;
	auto_size_ctl.flash_multiple         = 2.0;
	auto_size_ctl.flash_threshold        = 0.5;


        auto_size_ctl.decr_mode              = H5C2_decr__threshold;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = FALSE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 3;

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C2_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_set_cache_auto_resize_config failed 4.\n";
        }
    }

    if ( pass2 ) {

        if ( ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "bad cache size after set resize re-config 3.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate with cache full -- increase disabled so should
     * be no change in cache size, and result should be in_spec.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_increase_possible ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 7.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate -- make sure that we haven't disabled decreases.
     * should result in a decrease cache size from 4 to 2 Meg.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (2 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (1 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 8.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate again -- increase disabled so should
     * be no change in cache size, and result should be increase_disabled.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_increase_possible ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (2 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (1 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 9.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Now, disable size decreases, and repeat the above tests.
     */

    if ( pass2 ) {

        auto_size_ctl.version                = H5C2__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 16 * 1024 * 1024;
        auto_size_ctl.min_size               = 1 * 1024 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C2_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (2 * 1024 * 1024);

	auto_size_ctl.flash_incr_mode        = H5C2_flash_incr__off;
	auto_size_ctl.flash_multiple         = 2.0;
	auto_size_ctl.flash_threshold        = 0.5;


        auto_size_ctl.decr_mode              = H5C2_decr__threshold;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 1.0; /* disable size decreases */

        auto_size_ctl.apply_max_decrement    = TRUE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 3;

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C2_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_set_cache_auto_resize_config failed 5.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass2 ) {

        if ( ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "bad cache size after set resize re-config 4.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate -- should be no change in cache size,
     * and result should be decrease_disabled.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease_disabled ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 10.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate -- cache size should increase from 4 to 6 Meg.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != increase ) ||
             ( cache_ptr->max_cache_size != (6 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (3 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 11.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate again -- should be no change in cache size,
     * and result should be decrease_disabled.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease_disabled ) ||
             ( cache_ptr->max_cache_size != (6 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (3 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 12.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Repeat the above tests, disabling decrease through the upper
     * threshold instead of the decrement.
     */

    if ( pass2 ) {

        auto_size_ctl.version                = H5C2__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 16 * 1024 * 1024;
        auto_size_ctl.min_size               = 1 * 1024 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C2_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (2 * 1024 * 1024);

	auto_size_ctl.flash_incr_mode        = H5C2_flash_incr__off;
	auto_size_ctl.flash_multiple         = 2.0;
	auto_size_ctl.flash_threshold        = 0.5;


        auto_size_ctl.decr_mode              = H5C2_decr__threshold;

        auto_size_ctl.upper_hr_threshold     = 1.0; /* disable size decreases */

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = TRUE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 3;

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C2_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_set_cache_auto_resize_config failed 6.\n";
        }
    }

    if ( pass2 ) {

        if ( ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "bad cache size after set resize re-config 5.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate -- should be no change in cache size,
     * and result should be in_spec.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 13.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate -- cache size should increase from 4 to 6 Meg.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != increase ) ||
             ( cache_ptr->max_cache_size != (6 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (3 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 14.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate again -- should be no change in cache size,
     * and result should be in_spec.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (6 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (3 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 15.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Repeat the above tests, disabling decrease through the decr_mode.
     */

    if ( pass2 ) {

        auto_size_ctl.version                = H5C2__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 16 * 1024 * 1024;
        auto_size_ctl.min_size               = 1 * 1024 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C2_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (2 * 1024 * 1024);

	auto_size_ctl.flash_incr_mode        = H5C2_flash_incr__off;
	auto_size_ctl.flash_multiple         = 2.0;
	auto_size_ctl.flash_threshold        = 0.5;


        auto_size_ctl.decr_mode              = H5C2_decr__off;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = TRUE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 3;

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C2_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_set_cache_auto_resize_config failed 7.\n";
        }
    }

    if ( pass2 ) {

        if ( ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "bad cache size after set resize re-config 6.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate -- should be no change in cache size,
     * and result should be in_spec.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 16.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate -- cache size should increase from 4 to 6 Meg.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != increase ) ||
             ( cache_ptr->max_cache_size != (6 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (3 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 17.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate again -- should be no change in cache size,
     * and result should be in_spec.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (6 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (3 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 18.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Now do tests disabling size decrement in age out mode.
     *
     * Start by disabling size decrement by setting max_decrement to zero.
     */

    if ( pass2 ) {

        auto_size_ctl.version                = H5C2__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 16 * 1024 * 1024;
        auto_size_ctl.min_size               = 1 * 1024 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C2_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (2 * 1024 * 1024);

	auto_size_ctl.flash_incr_mode        = H5C2_flash_incr__off;
	auto_size_ctl.flash_multiple         = 2.0;
	auto_size_ctl.flash_threshold        = 0.5;


        auto_size_ctl.decr_mode              = H5C2_decr__age_out;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = TRUE;
        auto_size_ctl.max_decrement          = 0; /* disable decrement */

        auto_size_ctl.epochs_before_eviction = 1;

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C2_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_set_cache_auto_resize_config failed 8.\n";
        }
    }

    if ( pass2 ) {

        if ( ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "bad cache size after set resize re-config 7.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* flush the cache and destroy all entries so we start from a known point */
    flush_cache2(cache_ptr, TRUE, FALSE, FALSE);

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* load up the cache with small entries.  Note that it will take an
     * epoch for the ageout code to initialize itself if it is enabled.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, SMALL_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, SMALL_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( rpt_status != not_full ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 19.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Load up some more small entries.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 1000;
        while ( ( pass2 ) && ( i < 2000 ) )
        {
            protect_entry2(cache_ptr, SMALL_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, SMALL_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( rpt_status != not_full ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 20.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Now force a high hit rate so that the size increase code is
     * is satisfied.  We would see a decrease here if decrease were
     * possible.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, SMALL_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, SMALL_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( rpt_status != decrease_disabled ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 21.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate -- cache size should increase from 4 to 6 Meg.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != increase ) ||
             ( cache_ptr->max_cache_size != (6 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (3 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 22.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* just bang on a single entry.  This will see to it that there are
     * many entries that could be aged out were decreases enabled.
     * Should be no change in cache size, and result should be
     * decrease_disabled.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( rpt_status != decrease_disabled ) ||
             ( cache_ptr->max_cache_size != (6 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (3 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 23.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Now disable size decrement in age out mode via the empty reserve.
     */

    if ( pass2 ) {

        auto_size_ctl.version                = H5C2__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 16 * 1024 * 1024;
        auto_size_ctl.min_size               = 1 * 1024 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C2_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (2 * 1024 * 1024);

	auto_size_ctl.flash_incr_mode        = H5C2_flash_incr__off;
	auto_size_ctl.flash_multiple         = 2.0;
	auto_size_ctl.flash_threshold        = 0.5;


        auto_size_ctl.decr_mode              = H5C2_decr__age_out;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = TRUE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 1;

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 1.0; /* disable decrement */

        result = H5C2_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_set_cache_auto_resize_config failed 9.\n";
        }
    }

    if ( pass2 ) {

        if ( ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "bad cache size after set resize re-config 8.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* flush the cache and destroy all entries so we start from a known point */
    flush_cache2(cache_ptr, TRUE, FALSE, FALSE);

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* load up the cache with small entries.  Note that it will take an
     * epoch for the ageout code to initialize itself if it is enabled.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, SMALL_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, SMALL_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( rpt_status != not_full ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 24.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Load up some more small entries.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 1000;
        while ( ( pass2 ) && ( i < 2000 ) )
        {
            protect_entry2(cache_ptr, SMALL_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, SMALL_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( rpt_status != not_full ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 25.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Now force a high hit rate so that the size increase code is
     * is satisfied.  We would see a decrease here if decrease were
     * possible.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, SMALL_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, SMALL_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( rpt_status != decrease_disabled ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 26.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate -- cache size should increase from 4 to 6 Meg.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != increase ) ||
             ( cache_ptr->max_cache_size != (6 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (3 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 27.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* just bang on a single entry.  This will see to it that there are
     * many entries that could be aged out were decreases enabled.
     * Should be no change in cache size, and result should be
     * decrease_disabled.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( rpt_status != decrease_disabled ) ||
             ( cache_ptr->max_cache_size != (6 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (3 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 28.\n";
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

    if ( pass2 ) {

        auto_size_ctl.version                = H5C2__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 16 * 1024 * 1024;
        auto_size_ctl.min_size               = 1 * 1024 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C2_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (2 * 1024 * 1024);

	auto_size_ctl.flash_incr_mode        = H5C2_flash_incr__off;
	auto_size_ctl.flash_multiple         = 2.0;
	auto_size_ctl.flash_threshold        = 0.5;


        auto_size_ctl.decr_mode              = H5C2_decr__age_out_with_threshold;

        auto_size_ctl.upper_hr_threshold     = 1.0;

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = TRUE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 1;

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C2_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_set_cache_auto_resize_config failed 10.\n";
        }
    }

    if ( pass2 ) {

        if ( ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "bad cache size after set resize re-config 9.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* flush the cache and destroy all entries so we start from a known point */
    flush_cache2(cache_ptr, TRUE, FALSE, FALSE);

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* load up the cache with small entries.  Note that it will take an
     * epoch for the ageout code to initialize itself if it is enabled.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, SMALL_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, SMALL_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( rpt_status != not_full ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 29.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Load up some more small entries.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 1000;
        while ( ( pass2 ) && ( i < 2000 ) )
        {
            protect_entry2(cache_ptr, SMALL_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, SMALL_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( rpt_status != not_full ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 30.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Now force a high hit rate so that the size increase code is
     * is satisfied.  We would see a decrease here if decrease were
     * possible, but the upper threshold cannot be met, so no decrease.
     *
     * rpt_status should be decrease_disabled.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, SMALL_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, SMALL_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
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

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 31.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate -- cache size should increase from 4 to 6 Meg.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != increase ) ||
             ( cache_ptr->max_cache_size != (6 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (3 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 32.\n";
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
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 999);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 999,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( rpt_status != decrease_disabled ) ||
             ( cache_ptr->max_cache_size != (6 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (3 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 33.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);


    /*********************************************************************
     * Finally, use the auto cache resize code to set the size of the
     * cache and keep it there.  Again, due to the complexity of the
     * interface, there are lots of ways of doing this.  We have to
     * check them all.
     *********************************************************************/

    if ( pass2 ) {

        auto_size_ctl.version                = H5C2__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 2 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 16 * 1024 * 1024;
        auto_size_ctl.min_size               =  1 * 1024 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C2_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.0; /* disable size increases */

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (2 * 1024 * 1024);

	auto_size_ctl.flash_incr_mode        = H5C2_flash_incr__off;
	auto_size_ctl.flash_multiple         = 2.0;
	auto_size_ctl.flash_threshold        = 0.5;


        auto_size_ctl.decr_mode              = H5C2_decr__threshold;

        auto_size_ctl.upper_hr_threshold     = 1.0; /* disable size decreases */

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = TRUE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 3;

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C2_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_set_cache_auto_resize_config failed 11.\n";
        }
    }

    if ( pass2 ) {

        if ( ( cache_ptr->max_cache_size != (2 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (1 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "bad cache size after set resize re-config 10.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate -- should be no response as the auto-resize
     * code should be disabled.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( rpt_fcn_called ) ||
             ( cache_ptr->resize_enabled ) ||
             ( cache_ptr->size_increase_possible ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( cache_ptr->max_cache_size != (2 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (1 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 34.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate -- should be no response as the auto-resize
     * code should be disabled.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( rpt_fcn_called ) ||
             ( cache_ptr->resize_enabled ) ||
             ( cache_ptr->size_increase_possible ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( cache_ptr->max_cache_size != (2 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (1 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 35.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass2 ) {

        auto_size_ctl.version                = H5C2__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.25;

        auto_size_ctl.max_size               = 16 * 1024 * 1024;
        auto_size_ctl.min_size               =  1 * 1024 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C2_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 1.0; /* disable size increment */

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (2 * 1024 * 1024);

	auto_size_ctl.flash_incr_mode        = H5C2_flash_incr__off;
	auto_size_ctl.flash_multiple         = 2.0;
	auto_size_ctl.flash_threshold        = 0.5;


        auto_size_ctl.decr_mode              = H5C2_decr__threshold;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 1.0; /* disable size decrement */

        auto_size_ctl.apply_max_decrement    = TRUE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 3;

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C2_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_set_cache_auto_resize_config failed 12.\n";
        }
    }

    if ( pass2 ) {

        if ( ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (1 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "bad cache size after set resize re-config 11.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate -- should be no response as the auto-resize
     * code should be disabled.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( rpt_fcn_called ) ||
             ( cache_ptr->resize_enabled ) ||
             ( cache_ptr->size_increase_possible ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (1 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 36.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate -- should be no response as the auto-resize
     * code should be disabled.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( rpt_fcn_called ) ||
             ( cache_ptr->resize_enabled ) ||
             ( cache_ptr->size_increase_possible ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (1 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 37.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass2 ) {

        auto_size_ctl.version                = H5C2__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = FALSE;
        auto_size_ctl.initial_size           = 2 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 6 * 1024 * 1024; /* no resize */
        auto_size_ctl.min_size               = 6 * 1024 * 1024; /* no resize */

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C2_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (2 * 1024 * 1024);

	auto_size_ctl.flash_incr_mode        = H5C2_flash_incr__off;
	auto_size_ctl.flash_multiple         = 2.0;
	auto_size_ctl.flash_threshold        = 0.5;


        auto_size_ctl.decr_mode              = H5C2_decr__threshold;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = TRUE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 3;

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C2_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_set_cache_auto_resize_config failed 13.\n";
        }
    }

    if ( pass2 ) {

        if ( ( cache_ptr->max_cache_size != (6 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (3 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "bad cache size after set resize re-config 12.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate -- should be no response as the auto-resize
     * code should be disabled.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( rpt_fcn_called ) ||
             ( cache_ptr->resize_enabled ) ||
             ( cache_ptr->size_increase_possible ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( cache_ptr->max_cache_size != (6 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (3 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 38.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate -- should be no response as the auto-resize
     * code should be disabled.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( rpt_fcn_called ) ||
             ( cache_ptr->resize_enabled ) ||
             ( cache_ptr->size_increase_possible ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( cache_ptr->max_cache_size != (6 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (3 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 39.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass2 ) {

        auto_size_ctl.version                = H5C2__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.25;

        auto_size_ctl.max_size               = 16 * 1024 * 1024;
        auto_size_ctl.min_size               =  1 * 1024 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C2_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 1.0; /* disable size increment */

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (2 * 1024 * 1024);

	auto_size_ctl.flash_incr_mode        = H5C2_flash_incr__off;
	auto_size_ctl.flash_multiple         = 2.0;
	auto_size_ctl.flash_threshold        = 0.5;


        auto_size_ctl.decr_mode              = H5C2_decr__threshold;

        auto_size_ctl.upper_hr_threshold     = 1.0; /* disable size decrement */

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = TRUE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 3;

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C2_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_set_cache_auto_resize_config failed 14.\n";
        }
    }

    if ( pass2 ) {

        if ( ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (1 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "bad cache size after set resize re-config 13.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate -- should be no response as the auto-resize
     * code should be disabled.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( rpt_fcn_called ) ||
             ( cache_ptr->resize_enabled ) ||
             ( cache_ptr->size_increase_possible ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (1 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 40.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate -- should be no response as the auto-resize
     * code should be disabled.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( rpt_fcn_called ) ||
             ( cache_ptr->resize_enabled ) ||
             ( cache_ptr->size_increase_possible ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (1 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 41.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass2 ) {

        auto_size_ctl.version                = H5C2__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 16 * 1024 * 1024;
        auto_size_ctl.min_size               =  1 * 1024 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C2_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.0; /* disable size increment */

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (2 * 1024 * 1024);

	auto_size_ctl.flash_incr_mode        = H5C2_flash_incr__off;
	auto_size_ctl.flash_multiple         = 2.0;
	auto_size_ctl.flash_threshold        = 0.5;


        auto_size_ctl.decr_mode              = H5C2_decr__threshold;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 1.0; /* disable size decrement */

        auto_size_ctl.apply_max_decrement    = TRUE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 3;

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 0.05;


        result = H5C2_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_set_cache_auto_resize_config failed 15.\n";
        }
    }

    if ( pass2 ) {

        if ( ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "bad cache size after set resize re-config 14.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate -- should be no response as the auto-resize
     * code should be disabled.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( rpt_fcn_called ) ||
             ( cache_ptr->resize_enabled ) ||
             ( cache_ptr->size_increase_possible ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 42.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate -- should be no response as the auto-resize
     * code should be disabled.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( rpt_fcn_called ) ||
             ( cache_ptr->resize_enabled ) ||
             ( cache_ptr->size_increase_possible ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 43.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass2 ) {

        auto_size_ctl.version                = H5C2__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 16 * 1024 * 1024;
        auto_size_ctl.min_size               =  1 * 1024 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C2_incr__off;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (2 * 1024 * 1024);

	auto_size_ctl.flash_incr_mode        = H5C2_flash_incr__off;
	auto_size_ctl.flash_multiple         = 2.0;
	auto_size_ctl.flash_threshold        = 0.5;


        auto_size_ctl.decr_mode              = H5C2_decr__off;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = TRUE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 3;

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 0.05;


        result = H5C2_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_set_cache_auto_resize_config failed 16.\n";
        }
    }

    if ( pass2 ) {

        if ( ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "bad cache size after set resize re-config 15.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force low hit rate -- should be no response as the auto-resize
     * code should be disabled.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( rpt_fcn_called ) ||
             ( cache_ptr->resize_enabled ) ||
             ( cache_ptr->size_increase_possible ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 44.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* force high hit rate -- should be no response as the auto-resize
     * code should be disabled.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( rpt_fcn_called ) ||
             ( cache_ptr->resize_enabled ) ||
             ( cache_ptr->size_increase_possible ) ||
             ( cache_ptr->size_decrease_possible ) ||
             ( cache_ptr->max_cache_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (2 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 45.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);


    /* Now test the flash cache size increment code to verify that it
     * is disabled when it should be.  
     *
     * Since the flash size increase code doesn't look at hit rate, or 
     * use epochs (other than to start a new epoch if a flash cache size
     * increase is triggered), we go about these tests somewhat differently
     * than the rest of the tests in this function.
     *
     * As of this writing, there is only one flash cache size increment 
     * mode (add space), which is triggered whenever the size of a newly
     * loaded or inserted entry, or the delta between old and new entry
     * sizes exceeds some fraction of the current maximum cache size, and
     * the cache doesn't have enough free space to accomodate the new/
     * resize entry without performing evictions.  The range of permissible
     * values for the flash_threshold (0.1 to 1.0 as of this writing), and
     * for the flash_multiple (0.1 to 10.0) do not permit the facility to 
     * be turned off by configuration.  Thus, flash cache size increases
     * can be disabled only via the flash_incr_mode, and by setting the 
     * current max_cache_size equal to max_size.
     *
     * We have already tested the latter in check_auto_cache_resize(), so
     * we need only thest the former here.  Do this by disabling flash 
     * cache size increments via the flash_incr_mode, and then creating
     * situations that would trigger flash cache size increases were that
     * code enabled.
     */

    if ( pass2 ) {

        auto_size_ctl.version                = H5C2__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 64 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 256 * 1024;
        auto_size_ctl.min_size               =  32 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C2_incr__threshold;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (2 * 1024);

	auto_size_ctl.flash_incr_mode        = H5C2_flash_incr__off;
	auto_size_ctl.flash_multiple         = 1.0;
	auto_size_ctl.flash_threshold        = 0.25;


        auto_size_ctl.decr_mode              = H5C2_decr__age_out_with_threshold;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = TRUE;
        auto_size_ctl.max_decrement          = (1 * 1024);

        auto_size_ctl.epochs_before_eviction = 3;

        auto_size_ctl.apply_empty_reserve    = TRUE;
        auto_size_ctl.empty_reserve          = 0.05;


        result = H5C2_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_set_cache_auto_resize_config failed 17.\n";
        }
    }

    if ( pass2 ) {

        if ( ( cache_ptr->max_cache_size != (64 * 1024) ) ||
             ( cache_ptr->min_clean_size != (32 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "bad cache size after set resize re-config 16.\n";
        }
    }

    /* We have reduced the max cache size to well below the current index
     * size.  Protect and unprotect an entry to allow the cache to evict
     * entries and get within bounds
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;

        protect_entry2(cache_ptr, LARGE_ENTRY_TYPE, 0);
        unprotect_entry2(cache_ptr, LARGE_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);

        if ( ( pass2 ) &&
             ( ( ( cache_ptr->max_cache_size != (64 * 1024) ) ||
                 ( cache_ptr->min_clean_size != (32 * 1024) ) ||
                 ( cache_ptr->index_len != 1 ) ||
                 ( cache_ptr->index_size != LARGE_ENTRY_SIZE ) ||
                 ( rpt_fcn_called != FALSE ) ) ) ) {

	    HDfprintf(stdout, "\nmax_cache_size = %ld.\n",
		      (long)(cache_ptr->max_cache_size));
	    HDfprintf(stdout, "min_clean_size = %ld.\n",
		      (long)(cache_ptr->min_clean_size));
	    HDfprintf(stdout, "index_len = %ld.\n",
		      (long)(cache_ptr->index_len));
	    HDfprintf(stdout, "index_size = %ld.\n",
		      (long)(cache_ptr->index_size));
	    HDfprintf(stdout, "rpt_fcn_called = %ld.\n",
		      (long)(rpt_fcn_called));

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 46.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Now protect and unprotect a monster entry.  If the flash cache
     * size increment code was active, this would trigger an increase.
     * Verify that it doesn't.
     *
     * This finishes the additional tests needed for the flash cache 
     * size increase code.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;

        protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0);
        unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);

        if ( ( pass2 ) &&
             ( ( ( cache_ptr->max_cache_size != (64 * 1024) ) ||
                 ( cache_ptr->min_clean_size != (32 * 1024) ) ||
                 ( cache_ptr->index_len != 1 ) ||
                 ( cache_ptr->index_size != MONSTER_ENTRY_SIZE ) ||
                 ( rpt_fcn_called != FALSE ) ) ) ) {

	    HDfprintf(stdout, "\nmax_cache_size = %ld.\n",
		      (long)(cache_ptr->max_cache_size));
	    HDfprintf(stdout, "min_clean_size = %ld.\n",
		      (long)(cache_ptr->min_clean_size));
	    HDfprintf(stdout, "index_len = %ld.\n",
		      (long)(cache_ptr->index_len));
	    HDfprintf(stdout, "index_size = %ld.\n",
		      (long)(cache_ptr->index_size));
	    HDfprintf(stdout, "rpt_fcn_called = %ld.\n",
		      (long)(rpt_fcn_called));

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 47.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass2 ) {

        takedown_cache2(cache_ptr, FALSE, FALSE);
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass2 ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass2 ) {

        HDfprintf(stdout, "%s: failure_mssg2 = \"%s\".\n",
                  fcn_name, failure_mssg2);
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
    H5C2_t * cache_ptr = NULL;
    H5C2_auto_size_ctl_t auto_size_ctl =
    {
        /* int32_t     version                = */ H5C2__CURR_AUTO_SIZE_CTL_VER,
        /* H5C2_auto_resize_report_fcn rpt_fcn = */ test_rpt_fcn,

        /* hbool_t     set_initial_size       = */ TRUE,
        /* size_t      initial_size           = */ (512 * 1024),

        /* double      min_clean_fraction     = */ 0.5,

        /* size_t      max_size               = */ (14 * 1024 * 1024),
        /* size_t      min_size               = */ (512 * 1024),

        /* int64_t     epoch_length           = */ 1000,


        /* enum H5C2_cache_incr_mode incr_mode = */ H5C2_incr__threshold,

        /* double     lower_hr_threshold      = */ 0.75,

        /* double      increment              = */ 2.0,

        /* hbool_t     apply_max_increment    = */ TRUE,
        /* size_t      max_increment          = */ (4 * 1024 * 1024),

        /* enum H5C2_cache_flash_incr_mode      */
	/*                    flash_incr_mode = */ H5C2_flash_incr__off,
	/* double      flash_multiple         = */ 2.0,
	/* double      flash_threshold        = */ 0.5,


        /* enum H5C2_cache_decr_mode decr_mode = */ H5C2_decr__threshold,

        /* double      upper_hr_threshold     = */ 0.995,

        /* double      decrement              = */ 0.1,

        /* hbool_t     apply_max_decrement    = */ TRUE,
        /* size_t      max_decrement          = */ (1 * 1024 * 1024),

        /* int32_t     epochs_before_eviction = */ 3,

        /* hbool_t     apply_empty_reserve    = */ TRUE,
        /* double      empty_reserve          = */ 0.05
    };

    TESTING("automatic cache resize epoch marker management");

    pass2 = TRUE;

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass2 ) {

        reset_entries2();

        cache_ptr = setup_cache2((size_t)(2 * 1024),
                                (size_t)(1 * 1024));
    }

    if ( pass2 ) {

        result = H5C2_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_set_cache_auto_resize_config failed 1.\n";
        }
    }

    if ( pass2 ) {

        if ( ( cache_ptr->max_cache_size != (512 * 1024) ) ||
             ( cache_ptr->min_clean_size != (256 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "bad cache size after initialization.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);


    /* Now make sure that we are managing the epoch markers correctly.
     */

    if ( pass2 ) {

        auto_size_ctl.version                = H5C2__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 8 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 8 * 1024 * 1024;
        auto_size_ctl.min_size               = 512 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C2_incr__off;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (4 * 1024 * 1024);

	auto_size_ctl.flash_incr_mode        = H5C2_flash_incr__off;
	auto_size_ctl.flash_multiple         = 2.0;
	auto_size_ctl.flash_threshold        = 0.5;


        auto_size_ctl.decr_mode              = H5C2_decr__age_out;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = FALSE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 10;

        auto_size_ctl.apply_empty_reserve    = FALSE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C2_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_set_cache_auto_resize_config failed 2.\n";
        }
    }

    if ( pass2 ) {

        if ( ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "bad cache size after set resize re-config 1.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Since we just created the cache, there should be no epoch markers
     * active.  Verify that this is true.
     */

    if ( pass2 ) {

        if ( cache_ptr->epoch_markers_active != 0 ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected # of epoch markers 1.\n";
        }
    }

    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, MEDIUM_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != in_spec ) ||
             ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ||
             ( cache_ptr->index_size != (1 * 1000 * MEDIUM_ENTRY_SIZE) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 0.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);


    if ( pass2 ) {

        j = 2;
        while ( ( pass2 ) && ( j <= 10 ) )
        {

            rpt_fcn_called = FALSE;
            i = (j - 2) * 1000;
            while ( ( pass2 ) && ( i < (j - 1) * 1000 ) )
            {
                protect_entry2(cache_ptr, SMALL_ENTRY_TYPE, i);

                if ( pass2 ) {
                    unprotect_entry2(cache_ptr, SMALL_ENTRY_TYPE, i,
                                    NO_CHANGE, H5C2__NO_FLAGS_SET);
                }
                i++;
            }

            if ( ( ! rpt_fcn_called ) ||
                 ( rpt_status != in_spec ) ||
                 ( cache_ptr->epoch_markers_active != j ) ) {

                pass2 = FALSE;
                failure_mssg2 = "Unexpected # of epoch markers 2.\n";
            }

            j++;
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* we now have a full complement of epoch markers -- see if
     * we get the expected reduction.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 9000;
        while ( ( pass2 ) && ( i < 10000 ) )
        {
            protect_entry2(cache_ptr, SMALL_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, SMALL_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
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

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 1.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* now reduce the epochs before eviction, and see if the cache
     * deletes the extra markers
     */
    if ( pass2 ) {

        auto_size_ctl.version                = H5C2__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 8 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 8 * 1024 * 1024;
        auto_size_ctl.min_size               = 512 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C2_incr__off;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (4 * 1024 * 1024);

	auto_size_ctl.flash_incr_mode        = H5C2_flash_incr__off;
	auto_size_ctl.flash_multiple         = 2.0;
	auto_size_ctl.flash_threshold        = 0.5;


        auto_size_ctl.decr_mode              = H5C2_decr__age_out;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = FALSE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 1;

        auto_size_ctl.apply_empty_reserve    = FALSE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C2_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_set_cache_auto_resize_config failed 3.\n";
        }
    }

    if ( pass2 ) {

        if ( ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "bad cache size after set resize re-config 2.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* There should be exactly one active epoch marker at present.
     */
    if ( pass2 ) {

        if ( cache_ptr->epoch_markers_active != 1 ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected # of epoch markers 3.\n";
        }
    }

    /* Now do an epochs worth of accesses, and verify that everything
     * not accessed in this epoch gets evicted, and the cache size
     * is reduced.
     */
    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 9000;
        while ( ( pass2 ) && ( i < 10000 ) )
        {
            protect_entry2(cache_ptr, SMALL_ENTRY_TYPE, i);

            if ( pass2 ) {
                unprotect_entry2(cache_ptr, SMALL_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }

        if ( ( ! rpt_fcn_called ) ||
             ( rpt_status != decrease ) ||
             ( cache_ptr->max_cache_size != (512 * 1024) ) ||
             ( cache_ptr->min_clean_size != (256 * 1024) ) ||
             ( cache_ptr->index_size != (1 * 1000 * SMALL_ENTRY_SIZE) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 2.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* There should be exactly one active epoch marker at present...
     */
    if ( pass2 ) {

        if ( cache_ptr->epoch_markers_active != 1 ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected # of epoch markers 4.\n";
        }
    }

    /* shift the decrement mode to threshold, and verify that we remove
     * all epoch markers.
     */
    if ( pass2 ) {

        auto_size_ctl.version                = H5C2__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 8 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 8 * 1024 * 1024;
        auto_size_ctl.min_size               = 512 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C2_incr__off;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (4 * 1024 * 1024);

	auto_size_ctl.flash_incr_mode        = H5C2_flash_incr__off;
	auto_size_ctl.flash_multiple         = 2.0;
	auto_size_ctl.flash_threshold        = 0.5;


        auto_size_ctl.decr_mode              = H5C2_decr__threshold;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = FALSE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 1;

        auto_size_ctl.apply_empty_reserve    = FALSE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C2_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_set_cache_auto_resize_config failed 4.\n";
        }
    }

    if ( pass2 ) {

        if ( ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "bad cache size after set resize re-config 3.\n";
        }
    }

    /* ... and now there should be none.
     */
    if ( pass2 ) {

        if ( cache_ptr->epoch_markers_active != 0 ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected # of epoch markers 5.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* shift the decrement mode to age out with threshold.  Set epochs
     * before eviction to 10 again.
     */
    if ( pass2 ) {

        auto_size_ctl.version                = H5C2__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 8 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 8 * 1024 * 1024;
        auto_size_ctl.min_size               = 512 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C2_incr__off;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (4 * 1024 * 1024);

	auto_size_ctl.flash_incr_mode        = H5C2_flash_incr__off;
	auto_size_ctl.flash_multiple         = 2.0;
	auto_size_ctl.flash_threshold        = 0.5;


        auto_size_ctl.decr_mode              = H5C2_decr__age_out_with_threshold;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = FALSE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 10;

        auto_size_ctl.apply_empty_reserve    = FALSE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C2_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_set_cache_auto_resize_config failed 5.\n";
        }
    }

    /* Verify that there are no active epoch markers.
     */
    if ( pass2 ) {

        if ( cache_ptr->epoch_markers_active != 0 ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected # of epoch markers 6.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* build up a full set of epoch markers. */
    if ( pass2 ) {

        j = 1;
        while ( ( pass2 ) && ( j <= 10 ) )
        {

            rpt_fcn_called = FALSE;
            i = (j - 1) * 1000;
            while ( ( pass2 ) && ( i < j * 1000 ) )
            {
                protect_entry2(cache_ptr, SMALL_ENTRY_TYPE, i);

                if ( pass2 ) {
                    unprotect_entry2(cache_ptr, SMALL_ENTRY_TYPE, i,
                                    NO_CHANGE, H5C2__NO_FLAGS_SET);
                }
                i++;
            }

            if ( ( ! rpt_fcn_called ) ||
                 ( rpt_status != in_spec ) ||
                 ( cache_ptr->epoch_markers_active != j ) ) {

                pass2 = FALSE;
                failure_mssg2 = "Unexpected # of epoch markers 7.\n";
            }

            j++;
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* Verify that there are now 10 active epoch markers.
     */
    if ( pass2 ) {

        if ( cache_ptr->epoch_markers_active != 10 ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected # of epoch markers 8.\n";
        }
    }

    /* shift the decrement mode to off.  This should cause all epoch
     * markers to be removed.
     */
    if ( pass2 ) {

        auto_size_ctl.version                = H5C2__CURR_AUTO_SIZE_CTL_VER;
        auto_size_ctl.rpt_fcn                = test_rpt_fcn;

        auto_size_ctl.set_initial_size       = TRUE;
        auto_size_ctl.initial_size           = 8 * 1024 * 1024;

        auto_size_ctl.min_clean_fraction     = 0.5;

        auto_size_ctl.max_size               = 8 * 1024 * 1024;
        auto_size_ctl.min_size               = 512 * 1024;

        auto_size_ctl.epoch_length           = 1000;


        auto_size_ctl.incr_mode              = H5C2_incr__off;

        auto_size_ctl.lower_hr_threshold     = 0.75;

        auto_size_ctl.increment              = 2.0;

        auto_size_ctl.apply_max_increment    = TRUE;
        auto_size_ctl.max_increment          = (4 * 1024 * 1024);

	auto_size_ctl.flash_incr_mode        = H5C2_flash_incr__off;
	auto_size_ctl.flash_multiple         = 2.0;
	auto_size_ctl.flash_threshold        = 0.5;


        auto_size_ctl.decr_mode              = H5C2_decr__off;

        auto_size_ctl.upper_hr_threshold     = 0.995;

        auto_size_ctl.decrement              = 0.5;

        auto_size_ctl.apply_max_decrement    = FALSE;
        auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        auto_size_ctl.epochs_before_eviction = 10;

        auto_size_ctl.apply_empty_reserve    = FALSE;
        auto_size_ctl.empty_reserve          = 0.05;

        result = H5C2_set_cache_auto_resize_config(cache_ptr, &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_set_cache_auto_resize_config failed 6.\n";
        }
    }

    /* Verify that there are now no active epoch markers.
     */
    if ( pass2 ) {

        if ( cache_ptr->epoch_markers_active != 0 ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected # of epoch markers 9.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    /* verify that we still have the expected number of entries in the cache,
     * and that the cache is of the expected size.
     */
    if ( pass2 ) {

        if ( ( cache_ptr->max_cache_size != (8 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (4 * 1024 * 1024) )||
             ( cache_ptr->index_size != (10 * 1000 * SMALL_ENTRY_SIZE) ) ||
             ( cache_ptr->index_len != 10000 ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache size change results 3.\n";
        }
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass2 ) {

        takedown_cache2(cache_ptr, FALSE, FALSE);
    }

    if ( show_progress ) HDfprintf(stderr, "check point %d\n", checkpoint++);

    if ( pass2 ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass2 ) {

        HDfprintf(stdout, "%s: failure_mssg2 = \"%s\".\n",
                  fcn_name, failure_mssg2);
    }

    return;

} /* check_auto_cache_resize_epoch_markers() */


/*-------------------------------------------------------------------------
 * Function:	check_auto_cache_resize_input_errs()
 *
 * Purpose:	Verify that H5C2_set_cache_auto_resize_config() detects
 *		and rejects invalid input.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              10/29/04
 *
 * Modifications:
 *
 * 		Added code to verify that errors in the flash cache size
 *              increment related fields are caught as well.
 *
 *                                              JRM -- 1/17/08
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
  ( (a).flash_incr_mode        == (b).flash_incr_mode ) &&        \
  ( (a).flash_multiple         == (b).flash_multiple ) &&         \
  ( (a).flash_threshold        == (b).flash_threshold ) &&        \
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
    H5C2_t * cache_ptr = NULL;
    H5C2_auto_size_ctl_t ref_auto_size_ctl =
    {
        /* int32_t     version                = */ H5C2__CURR_AUTO_SIZE_CTL_VER,
        /* H5C2_auto_resize_report_fcn rpt_fcn = */ test_rpt_fcn,

        /* hbool_t     set_initial_size       = */ TRUE,
        /* size_t      initial_size           = */ (512 * 1024),

        /* double      min_clean_fraction     = */ 0.5,

        /* size_t      max_size               = */ (16 * 1024 * 1024),
        /* size_t      min_size               = */ (512 * 1024),

        /* int64_t     epoch_length           = */ 1000,


        /* enum H5C2_cache_incr_mode incr_mode = */ H5C2_incr__threshold,

        /* double     lower_hr_threshold      = */ 0.75,

        /* double      increment              = */ 2.0,

        /* hbool_t     apply_max_increment    = */ TRUE,
        /* size_t      max_increment          = */ (4 * 1024 * 1024),

        /* enum H5C2_cache_flash_incr_mode      */
	/*                    flash_incr_mode = */ H5C2_flash_incr__off,
	/* double      flash_multiple         = */ 2.0,
	/* double      flash_threshold        = */ 0.5,


        /* enum H5C2_cache_decr_mode decr_mode = */ H5C2_decr__threshold,

        /* double      upper_hr_threshold     = */ 0.995,

        /* double      decrement              = */ 0.1,

        /* hbool_t     apply_max_decrement    = */ TRUE,
        /* size_t      max_decrement          = */ (1 * 1024 * 1024),

        /* int32_t     epochs_before_eviction = */ 3,

        /* hbool_t     apply_empty_reserve    = */ TRUE,
        /* double      empty_reserve          = */ 0.05
    };

    H5C2_auto_size_ctl_t invalid_auto_size_ctl;
    H5C2_auto_size_ctl_t test_auto_size_ctl;

    TESTING("automatic cache resize input errors");

    pass2 = TRUE;

    /* allocate a cache, and set a reference automatic cache control
     * configuration.  Then feed H5C2_set_cache_auto_resize_config()
     * invalid input, and verify that the correct error is returned,
     * and that the configuration is not modified.
     */

    if ( pass2 ) {

        reset_entries2();

        cache_ptr = setup_cache2((size_t)(2 * 1024),
                                (size_t)(1 * 1024));
    }

    if ( pass2 ) {

        result = H5C2_set_cache_auto_resize_config(cache_ptr,
                                                  &ref_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_set_cache_auto_resize_config failed 1.\n";
        }
    }

    if ( pass2 ) {

        if ( ( cache_ptr->max_cache_size != (512 * 1024) ) ||
             ( cache_ptr->min_clean_size != (256 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "bad cache size after initialization.\n";
        }
    }

    if ( pass2 ) {

        result = H5C2_get_cache_auto_resize_config(cache_ptr,
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_get_cache_auto_resize_config failed 1.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected auto resize config 1.";
        }
    }

    if ( pass2 ) {

        invalid_auto_size_ctl.version            = H5C2__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.5;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;

        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C2_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.7;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);

	invalid_auto_size_ctl.flash_incr_mode        = H5C2_flash_incr__off;
	invalid_auto_size_ctl.flash_multiple         = 2.0;
	invalid_auto_size_ctl.flash_threshold        = 0.5;


        invalid_auto_size_ctl.decr_mode              = H5C2_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.5;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C2_set_cache_auto_resize_config(NULL,
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass2 = FALSE;
            failure_mssg2 =
                "H5C2_set_cache_auto_resize_config accepted NULL cache_ptr.\n";
        }
    }

    if ( pass2 ) {

        result = H5C2_get_cache_auto_resize_config(cache_ptr,
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_get_cache_auto_resize_config failed 2.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected auto resize config 2.";
        }
    }


    /* check bad version rejection. */

    if ( pass2 ) {

        invalid_auto_size_ctl.version                = -1; /* INVALID */
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.5;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;

        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C2_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.7;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);

	invalid_auto_size_ctl.flash_incr_mode        = H5C2_flash_incr__off;
	invalid_auto_size_ctl.flash_multiple         = 2.0;
	invalid_auto_size_ctl.flash_threshold        = 0.5;


        invalid_auto_size_ctl.decr_mode              = H5C2_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.5;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C2_set_cache_auto_resize_config(cache_ptr,
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass2 = FALSE;
            failure_mssg2 =
                "H5C2_set_cache_auto_resize_config accepted bad version.\n";
        }
    }

    if ( pass2 ) {

        result = H5C2_get_cache_auto_resize_config(cache_ptr,
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_get_cache_auto_resize_config failed 3.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected auto resize config 3.";
        }
    }


    /* check bad initial size rejection */

    if ( pass2 ) {

        invalid_auto_size_ctl.version            = H5C2__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 16 * 1024 * 1024 + 1;
                                                       /* INVALID */

        invalid_auto_size_ctl.min_clean_fraction     = 0.5;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;

        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C2_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);

	invalid_auto_size_ctl.flash_incr_mode        = H5C2_flash_incr__off;
	invalid_auto_size_ctl.flash_multiple         = 2.0;
	invalid_auto_size_ctl.flash_threshold        = 0.5;


        invalid_auto_size_ctl.decr_mode              = H5C2_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.5;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C2_set_cache_auto_resize_config(cache_ptr,
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass2 = FALSE;
            failure_mssg2 =
                "H5C2_set_cache_auto_resize_config accepted bad init size 1.\n";
        }
    }

    if ( pass2 ) {

        result = H5C2_get_cache_auto_resize_config(cache_ptr,
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_get_cache_auto_resize_config failed 4.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected auto resize config 4.";
        }
    }

    if ( pass2 ) {

        invalid_auto_size_ctl.version            = H5C2__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 1 * 1024 * 1024 - 1;
                                                       /* INVALID */

        invalid_auto_size_ctl.min_clean_fraction     = 0.5;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;

        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C2_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);

	invalid_auto_size_ctl.flash_incr_mode        = H5C2_flash_incr__off;
	invalid_auto_size_ctl.flash_multiple         = 2.0;
	invalid_auto_size_ctl.flash_threshold        = 0.5;


        invalid_auto_size_ctl.decr_mode              = H5C2_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.5;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C2_set_cache_auto_resize_config(cache_ptr,
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass2 = FALSE;
            failure_mssg2 =
                "H5C2_set_cache_auto_resize_config accepted bad init size 2.\n";
        }
    }

    if ( pass2 ) {

        result = H5C2_get_cache_auto_resize_config(cache_ptr,
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_get_cache_auto_resize_config failed 5.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected auto resize config 5.";
        }
    }


    /* test for invalid min clean fraction rejection. */

    if ( pass2 ) {

        invalid_auto_size_ctl.version            = H5C2__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 1.00001; /* INVALID */

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;

        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C2_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);

	invalid_auto_size_ctl.flash_incr_mode        = H5C2_flash_incr__off;
	invalid_auto_size_ctl.flash_multiple         = 2.0;
	invalid_auto_size_ctl.flash_threshold        = 0.5;


        invalid_auto_size_ctl.decr_mode              = H5C2_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.5;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C2_set_cache_auto_resize_config(cache_ptr,
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass2 = FALSE;
            failure_mssg2 =
            "H5C2_set_cache_auto_resize_config accepted bad min clean frac 1.\n";
        }
    }

    if ( pass2 ) {

        result = H5C2_get_cache_auto_resize_config(cache_ptr,
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_get_cache_auto_resize_config failed 6.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected auto resize config 6.";
        }
    }

    if ( pass2 ) {

        invalid_auto_size_ctl.version            = H5C2__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = -0.00001; /* INVALID */

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;

        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C2_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);

	invalid_auto_size_ctl.flash_incr_mode        = H5C2_flash_incr__off;
	invalid_auto_size_ctl.flash_multiple         = 2.0;
	invalid_auto_size_ctl.flash_threshold        = 0.5;


        invalid_auto_size_ctl.decr_mode              = H5C2_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.5;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C2_set_cache_auto_resize_config(cache_ptr,
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass2 = FALSE;
            failure_mssg2 =
            "H5C2_set_cache_auto_resize_config accepted bad min clean frac 2.\n";
        }
    }

    if ( pass2 ) {

        result = H5C2_get_cache_auto_resize_config(cache_ptr,
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_get_cache_auto_resize_config failed 7.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected auto resize config 7.";
        }
    }


    /* test for invalid max_size and/or min_size rejection */

    if ( pass2 ) {

        invalid_auto_size_ctl.version            = H5C2__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.5;

        invalid_auto_size_ctl.max_size            = H5C2__MAX_MAX_CACHE_SIZE + 1;
                                                    /* INVALID */
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;

        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C2_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);

	invalid_auto_size_ctl.flash_incr_mode        = H5C2_flash_incr__off;
	invalid_auto_size_ctl.flash_multiple         = 2.0;
	invalid_auto_size_ctl.flash_threshold        = 0.5;


        invalid_auto_size_ctl.decr_mode              = H5C2_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.5;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C2_set_cache_auto_resize_config(cache_ptr,
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass2 = FALSE;
            failure_mssg2 =
                "H5C2_set_cache_auto_resize_config accepted bad max_size.\n";
        }
    }

    if ( pass2 ) {

        result = H5C2_get_cache_auto_resize_config(cache_ptr,
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_get_cache_auto_resize_config failed 8.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected auto resize config 8.";
        }
    }

    if ( pass2 ) {

        invalid_auto_size_ctl.version            = H5C2__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.5;

        invalid_auto_size_ctl.max_size           = 1 * 1024 * 1024;/* INVALID */
        invalid_auto_size_ctl.min_size           = 1 * 1024 * 1024 + 1;/*PAIR */

        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C2_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);

	invalid_auto_size_ctl.flash_incr_mode        = H5C2_flash_incr__off;
	invalid_auto_size_ctl.flash_multiple         = 2.0;
	invalid_auto_size_ctl.flash_threshold        = 0.5;


        invalid_auto_size_ctl.decr_mode              = H5C2_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.5;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C2_set_cache_auto_resize_config(cache_ptr,
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass2 = FALSE;
            failure_mssg2 =
                "H5C2_set_cache_auto_resize_config accepted bad size pair.\n";
        }
    }

    if ( pass2 ) {

        result = H5C2_get_cache_auto_resize_config(cache_ptr,
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_get_cache_auto_resize_config failed 9.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected auto resize config 9.";
        }
    }

    if ( pass2 ) {

        invalid_auto_size_ctl.version            = H5C2__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.5;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size            = H5C2__MIN_MAX_CACHE_SIZE - 1;
                                                    /* INVALID */
        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C2_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);

	invalid_auto_size_ctl.flash_incr_mode        = H5C2_flash_incr__off;
	invalid_auto_size_ctl.flash_multiple         = 2.0;
	invalid_auto_size_ctl.flash_threshold        = 0.5;


        invalid_auto_size_ctl.decr_mode              = H5C2_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.5;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C2_set_cache_auto_resize_config(cache_ptr,
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass2 = FALSE;
            failure_mssg2 =
                "H5C2_set_cache_auto_resize_config accepted bad min_size.\n";
        }
    }

    if ( pass2 ) {

        result = H5C2_get_cache_auto_resize_config(cache_ptr,
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_get_cache_auto_resize_config failed 10.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected auto resize config 10.";
        }
    }


    /* test for invalid epoch_length rejection */

    if ( pass2 ) {

        invalid_auto_size_ctl.version            = H5C2__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.1;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;

        invalid_auto_size_ctl.epoch_length       = H5C2__MAX_AR_EPOCH_LENGTH + 1;
                                                   /* INVALID */

        invalid_auto_size_ctl.incr_mode              = H5C2_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);

	invalid_auto_size_ctl.flash_incr_mode        = H5C2_flash_incr__off;
	invalid_auto_size_ctl.flash_multiple         = 2.0;
	invalid_auto_size_ctl.flash_threshold        = 0.5;


        invalid_auto_size_ctl.decr_mode              = H5C2_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.9;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C2_set_cache_auto_resize_config(cache_ptr,
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass2 = FALSE;
            failure_mssg2 =
                "H5C2_set_cache_auto_resize_config accepted bad epoch len 1.\n";
        }
    }

    if ( pass2 ) {

        result = H5C2_get_cache_auto_resize_config(cache_ptr,
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_get_cache_auto_resize_config failed 11.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected auto resize config 11.";
        }
    }

    if ( pass2 ) {

        invalid_auto_size_ctl.version            = H5C2__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.1;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;

        invalid_auto_size_ctl.epoch_length       = H5C2__MIN_AR_EPOCH_LENGTH - 1;
                                                   /* INVALID */

        invalid_auto_size_ctl.incr_mode              = H5C2_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);

	invalid_auto_size_ctl.flash_incr_mode        = H5C2_flash_incr__off;
	invalid_auto_size_ctl.flash_multiple         = 2.0;
	invalid_auto_size_ctl.flash_threshold        = 0.5;


        invalid_auto_size_ctl.decr_mode              = H5C2_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.9;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C2_set_cache_auto_resize_config(cache_ptr,
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass2 = FALSE;
            failure_mssg2 =
                "H5C2_set_cache_auto_resize_config accepted bad epoch len 2.\n";
        }
    }

    if ( pass2 ) {

        result = H5C2_get_cache_auto_resize_config(cache_ptr,
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_get_cache_auto_resize_config failed 12.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected auto resize config 12.";
        }
    }


    /* test for bad incr_mode rejection */

    if ( pass2 ) {

        invalid_auto_size_ctl.version            = H5C2__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.1;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;

        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              =
        			(enum H5C2_cache_incr_mode) -1; /* INVALID */

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);

	invalid_auto_size_ctl.flash_incr_mode        = H5C2_flash_incr__off;
	invalid_auto_size_ctl.flash_multiple         = 2.0;
	invalid_auto_size_ctl.flash_threshold        = 0.5;


        invalid_auto_size_ctl.decr_mode              = H5C2_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.9;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C2_set_cache_auto_resize_config(cache_ptr,
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass2 = FALSE;
            failure_mssg2 =
                "H5C2_set_cache_auto_resize_config accepted bad incr_mode 1.\n";
        }
    }

    if ( pass2 ) {

        result = H5C2_get_cache_auto_resize_config(cache_ptr,
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_get_cache_auto_resize_config failed 13.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected auto resize config 13.";
        }
    }

    if ( pass2 ) {

        invalid_auto_size_ctl.version            = H5C2__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.1;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;

        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              =
        			(enum H5C2_cache_incr_mode) 2; /* INVALID */

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);

	invalid_auto_size_ctl.flash_incr_mode        = H5C2_flash_incr__off;
	invalid_auto_size_ctl.flash_multiple         = 2.0;
	invalid_auto_size_ctl.flash_threshold        = 0.5;


        invalid_auto_size_ctl.decr_mode              = H5C2_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.9;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C2_set_cache_auto_resize_config(cache_ptr,
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass2 = FALSE;
            failure_mssg2 =
                "H5C2_set_cache_auto_resize_config accepted bad incr_mode 2.\n";
        }
    }

    if ( pass2 ) {

        result = H5C2_get_cache_auto_resize_config(cache_ptr,
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_get_cache_auto_resize_config failed 14.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected auto resize config 14.";
        }
    }


    /* check for bad upper and/or lower threshold rejection */

    if ( pass2 ) {

        invalid_auto_size_ctl.version            = H5C2__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.5;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;

        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C2_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.7;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);

	invalid_auto_size_ctl.flash_incr_mode        = H5C2_flash_incr__off;
	invalid_auto_size_ctl.flash_multiple         = 2.0;
	invalid_auto_size_ctl.flash_threshold        = 0.5;


        invalid_auto_size_ctl.decr_mode              = H5C2_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 1.01; /* INVALID */

        invalid_auto_size_ctl.decrement              = 0.5;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C2_set_cache_auto_resize_config(cache_ptr,
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass2 = FALSE;
            failure_mssg2 =
             "H5C2_set_cache_auto_resize_config accepted bad upper threshold.\n";
        }
    }

    if ( pass2 ) {

        result = H5C2_get_cache_auto_resize_config(cache_ptr,
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_get_cache_auto_resize_config failed 15.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected auto resize config 15.";
        }
    }

    if ( pass2 ) {

        invalid_auto_size_ctl.version            = H5C2__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.5;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;

        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C2_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.8; /* INVALID */

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);

	invalid_auto_size_ctl.flash_incr_mode        = H5C2_flash_incr__off;
	invalid_auto_size_ctl.flash_multiple         = 2.0;
	invalid_auto_size_ctl.flash_threshold        = 0.5;


        invalid_auto_size_ctl.decr_mode              = H5C2_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.7; /* INVALID */

        invalid_auto_size_ctl.decrement              = 0.5;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C2_set_cache_auto_resize_config(cache_ptr,
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass2 = FALSE;
            failure_mssg2 =
              "H5C2_set_cache_auto_resize_config accepted bad threshold pair.\n";
        }
    }

    if ( pass2 ) {

        result = H5C2_get_cache_auto_resize_config(cache_ptr,
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_get_cache_auto_resize_config failed 16.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected auto resize config 16.";
        }
    }

    if ( pass2 ) {

        invalid_auto_size_ctl.version            = H5C2__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.5;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;

        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C2_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = -0.0001; /* INVALID */

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);

	invalid_auto_size_ctl.flash_incr_mode        = H5C2_flash_incr__off;
	invalid_auto_size_ctl.flash_multiple         = 2.0;
	invalid_auto_size_ctl.flash_threshold        = 0.5;


        invalid_auto_size_ctl.decr_mode              = H5C2_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.5;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C2_set_cache_auto_resize_config(cache_ptr,
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass2 = FALSE;
            failure_mssg2 =
             "H5C2_set_cache_auto_resize_config accepted bad lower threshold.\n";
        }
    }

    if ( pass2 ) {

        result = H5C2_get_cache_auto_resize_config(cache_ptr,
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_get_cache_auto_resize_config failed 17.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected auto resize config 17.";
        }
    }


    /* test for bad increment rejection */

    if ( pass2 ) {

        invalid_auto_size_ctl.version            = H5C2__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.1;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;

        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C2_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 0.99999; /* INVALID */

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);

	invalid_auto_size_ctl.flash_incr_mode        = H5C2_flash_incr__off;
	invalid_auto_size_ctl.flash_multiple         = 2.0;
	invalid_auto_size_ctl.flash_threshold        = 0.5;


        invalid_auto_size_ctl.decr_mode              = H5C2_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.5;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;


        result = H5C2_set_cache_auto_resize_config(cache_ptr,
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass2 = FALSE;
            failure_mssg2 =
                "H5C2_set_cache_auto_resize_config accepted bad increment.\n";
        }
    }

    if ( pass2 ) {

        result = H5C2_get_cache_auto_resize_config(cache_ptr,
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_get_cache_auto_resize_config failed 18.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected auto resize config 18.";
        }
    }

    /* test for bad flash_incr_mode rejection */

    if ( pass2 ) {

        invalid_auto_size_ctl.version           = H5C2__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.1;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;

        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C2_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);

	invalid_auto_size_ctl.flash_incr_mode        = 
       			(enum H5C2_cache_flash_incr_mode) -1; /* INVALID */
	invalid_auto_size_ctl.flash_multiple         = 2.0;
	invalid_auto_size_ctl.flash_threshold        = 0.5;


        invalid_auto_size_ctl.decr_mode              = H5C2_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.9;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C2_set_cache_auto_resize_config(cache_ptr,
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass2 = FALSE;
            failure_mssg2 =
           "H5C2_set_cache_auto_resize_config accepted bad flash_incr_mode.\n";
        }
    }

    if ( pass2 ) {

        result = H5C2_get_cache_auto_resize_config(cache_ptr,
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_get_cache_auto_resize_config failed 19.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected auto resize config 19.";
        }
    }

    /* test for bad flash_multiple rejection */

    if ( pass2 ) {

        invalid_auto_size_ctl.version           = H5C2__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.1;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;

        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C2_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);

	invalid_auto_size_ctl.flash_incr_mode        = 
						H5C2_flash_incr__add_space;
	invalid_auto_size_ctl.flash_multiple         = 0.09; /* INVALID */
	invalid_auto_size_ctl.flash_threshold        = 0.5;


        invalid_auto_size_ctl.decr_mode              = H5C2_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.9;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C2_set_cache_auto_resize_config(cache_ptr,
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass2 = FALSE;
            failure_mssg2 =
         "H5C2_set_cache_auto_resize_config accepted bad flash_multiple(1).\n";
        }
    }

    if ( pass2 ) {

        result = H5C2_get_cache_auto_resize_config(cache_ptr,
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_get_cache_auto_resize_config failed 20.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected auto resize config 20.";
        }
    }

    if ( pass2 ) {

        invalid_auto_size_ctl.version           = H5C2__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.1;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;

        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C2_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);

	invalid_auto_size_ctl.flash_incr_mode        = 
						H5C2_flash_incr__add_space;
	invalid_auto_size_ctl.flash_multiple         = 10.01; /* INVALID */
	invalid_auto_size_ctl.flash_threshold        = 0.5;


        invalid_auto_size_ctl.decr_mode              = H5C2_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.9;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C2_set_cache_auto_resize_config(cache_ptr,
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass2 = FALSE;
            failure_mssg2 =
         "H5C2_set_cache_auto_resize_config accepted bad flash_multiple(2).\n";
        }
    }

    if ( pass2 ) {

        result = H5C2_get_cache_auto_resize_config(cache_ptr,
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_get_cache_auto_resize_config failed 21.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected auto resize config 21.";
        }
    }

    /* test for bad flash_threshold rejection */

    if ( pass2 ) {

        invalid_auto_size_ctl.version           = H5C2__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.1;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;

        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C2_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);

	invalid_auto_size_ctl.flash_incr_mode        = 
						H5C2_flash_incr__add_space;
	invalid_auto_size_ctl.flash_multiple         = 1.0;
	invalid_auto_size_ctl.flash_threshold        = 0.09; /* INVALID */


        invalid_auto_size_ctl.decr_mode              = H5C2_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.9;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C2_set_cache_auto_resize_config(cache_ptr,
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass2 = FALSE;
            failure_mssg2 =
        "H5C2_set_cache_auto_resize_config accepted bad flash_threshold(1).\n";
        }
    }

    if ( pass2 ) {

        result = H5C2_get_cache_auto_resize_config(cache_ptr,
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_get_cache_auto_resize_config failed 22.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected auto resize config 22.";
        }
    }

    if ( pass2 ) {

        invalid_auto_size_ctl.version           = H5C2__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.1;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;

        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C2_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);

	invalid_auto_size_ctl.flash_incr_mode        = 
						H5C2_flash_incr__add_space;
	invalid_auto_size_ctl.flash_multiple         = 1.0;
	invalid_auto_size_ctl.flash_threshold        = 1.001; /* INVALID */


        invalid_auto_size_ctl.decr_mode              = H5C2_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.9;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C2_set_cache_auto_resize_config(cache_ptr,
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass2 = FALSE;
            failure_mssg2 =
        "H5C2_set_cache_auto_resize_config accepted bad flash_threshold(2).\n";
        }
    }

    if ( pass2 ) {

        result = H5C2_get_cache_auto_resize_config(cache_ptr,
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_get_cache_auto_resize_config failed 23.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected auto resize config 23.";
        }
    }


    /* test for bad decr_mode rejection */

    if ( pass2 ) {

        invalid_auto_size_ctl.version            = H5C2__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.1;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;

        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C2_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);

	invalid_auto_size_ctl.flash_incr_mode        = H5C2_flash_incr__off;
	invalid_auto_size_ctl.flash_multiple         = 2.0;
	invalid_auto_size_ctl.flash_threshold        = 0.5;


        invalid_auto_size_ctl.decr_mode              =
        			(enum H5C2_cache_decr_mode) -1; /* INVALID */

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.9;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C2_set_cache_auto_resize_config(cache_ptr,
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass2 = FALSE;
            failure_mssg2 =
                "H5C2_set_cache_auto_resize_config accepted bad decr_mode 1.\n";
        }
    }

    if ( pass2 ) {

        result = H5C2_get_cache_auto_resize_config(cache_ptr,
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_get_cache_auto_resize_config failed 24.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected auto resize config 24.";
        }
    }

    if ( pass2 ) {

        invalid_auto_size_ctl.version            = H5C2__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.1;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;

        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C2_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);

	invalid_auto_size_ctl.flash_incr_mode        = H5C2_flash_incr__off;
	invalid_auto_size_ctl.flash_multiple         = 2.0;
	invalid_auto_size_ctl.flash_threshold        = 0.5;


        invalid_auto_size_ctl.decr_mode              =
        			(enum H5C2_cache_decr_mode) 4; /* INVALID */

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.9;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C2_set_cache_auto_resize_config(cache_ptr,
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass2 = FALSE;
            failure_mssg2 =
                "H5C2_set_cache_auto_resize_config accepted bad decr_mode 2.\n";
        }
    }

    if ( pass2 ) {

        result = H5C2_get_cache_auto_resize_config(cache_ptr,
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_get_cache_auto_resize_config failed 25.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected auto resize config 25.";
        }
    }


    /* check for bad decrement rejection */

    if ( pass2 ) {

        invalid_auto_size_ctl.version            = H5C2__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.1;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;

        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C2_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);

	invalid_auto_size_ctl.flash_incr_mode        = H5C2_flash_incr__off;
	invalid_auto_size_ctl.flash_multiple         = 2.0;
	invalid_auto_size_ctl.flash_threshold        = 0.5;


        invalid_auto_size_ctl.decr_mode              = H5C2_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 1.000001; /* INVALID */

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C2_set_cache_auto_resize_config(cache_ptr,
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass2 = FALSE;
            failure_mssg2 =
                "H5C2_set_cache_auto_resize_config accepted bad decrement 1.\n";
        }
    }

    if ( pass2 ) {

        result = H5C2_get_cache_auto_resize_config(cache_ptr,
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_get_cache_auto_resize_config failed 26.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected auto resize config 26.";
        }
    }

    if ( pass2 ) {

        invalid_auto_size_ctl.version            = H5C2__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.1;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;

        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C2_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);

	invalid_auto_size_ctl.flash_incr_mode        = H5C2_flash_incr__off;
	invalid_auto_size_ctl.flash_multiple         = 2.0;
	invalid_auto_size_ctl.flash_threshold        = 0.5;


        invalid_auto_size_ctl.decr_mode              = H5C2_decr__threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = -0.000001; /* INVALID */

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C2_set_cache_auto_resize_config(cache_ptr,
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass2 = FALSE;
            failure_mssg2 =
                "H5C2_set_cache_auto_resize_config accepted bad decrement 2.\n";
        }
    }

    if ( pass2 ) {

        result = H5C2_get_cache_auto_resize_config(cache_ptr,
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_get_cache_auto_resize_config failed 27.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected auto resize config 27.";
        }
    }


    /* check for rejection of bad epochs_before_eviction */

    if ( pass2 ) {

        invalid_auto_size_ctl.version            = H5C2__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.1;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;

        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C2_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);

	invalid_auto_size_ctl.flash_incr_mode        = H5C2_flash_incr__off;
	invalid_auto_size_ctl.flash_multiple         = 2.0;
	invalid_auto_size_ctl.flash_threshold        = 0.5;


        invalid_auto_size_ctl.decr_mode              = H5C2_decr__age_out;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.9;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 0; /* INVALID */

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C2_set_cache_auto_resize_config(cache_ptr,
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_set_cache_auto_resize_config accepted bad epochs_before_eviction 1.\n";
        }
    }

    if ( pass2 ) {

        result = H5C2_get_cache_auto_resize_config(cache_ptr,
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_get_cache_auto_resize_config failed 28.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected auto resize config 28.";
        }
    }

    if ( pass2 ) {

        invalid_auto_size_ctl.version            = H5C2__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.1;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;

        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C2_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);

	invalid_auto_size_ctl.flash_incr_mode        = H5C2_flash_incr__off;
	invalid_auto_size_ctl.flash_multiple         = 2.0;
	invalid_auto_size_ctl.flash_threshold        = 0.5;


        invalid_auto_size_ctl.decr_mode      = H5C2_decr__age_out_with_threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.9;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction =
                                       H5C2__MAX_EPOCH_MARKERS + 1; /* INVALID */

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C2_set_cache_auto_resize_config(cache_ptr,
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_set_cache_auto_resize_config accepted bad epochs_before_eviction 2.\n";
        }
    }

    if ( pass2 ) {

        result = H5C2_get_cache_auto_resize_config(cache_ptr,
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_get_cache_auto_resize_config failed 29.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected auto resize config 29.";
        }
    }


    /* Check for bad apply_empty_reserve rejection */

    if ( pass2 ) {

        invalid_auto_size_ctl.version            = H5C2__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.1;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;

        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C2_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);

	invalid_auto_size_ctl.flash_incr_mode        = H5C2_flash_incr__off;
	invalid_auto_size_ctl.flash_multiple         = 2.0;
	invalid_auto_size_ctl.flash_threshold        = 0.5;


        invalid_auto_size_ctl.decr_mode              = H5C2_decr__age_out;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.9;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction = 3;

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = -0.0000001; /* INVALID */

        result = H5C2_set_cache_auto_resize_config(cache_ptr,
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_set_cache_auto_resize_config accepted bad empty_reserve 1.\n";
        }
    }

    if ( pass2 ) {

        result = H5C2_get_cache_auto_resize_config(cache_ptr,
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_get_cache_auto_resize_config failed 30.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected auto resize config 30.";
        }
    }

    if ( pass2 ) {

        invalid_auto_size_ctl.version            = H5C2__CURR_AUTO_SIZE_CTL_VER;
        invalid_auto_size_ctl.rpt_fcn                = NULL;

        invalid_auto_size_ctl.set_initial_size       = TRUE;
        invalid_auto_size_ctl.initial_size           = 4 * 1024 * 1024;

        invalid_auto_size_ctl.min_clean_fraction     = 0.1;

        invalid_auto_size_ctl.max_size               = 16 * 1024 * 1024;
        invalid_auto_size_ctl.min_size               =  1 * 1024 * 1024;

        invalid_auto_size_ctl.epoch_length           = 5000;


        invalid_auto_size_ctl.incr_mode              = H5C2_incr__threshold;

        invalid_auto_size_ctl.lower_hr_threshold     = 0.75;

        invalid_auto_size_ctl.increment              = 2.0;

        invalid_auto_size_ctl.apply_max_increment    = TRUE;
        invalid_auto_size_ctl.max_increment          = (2 * 1024 * 1024);

	invalid_auto_size_ctl.flash_incr_mode        = H5C2_flash_incr__off;
	invalid_auto_size_ctl.flash_multiple         = 2.0;
	invalid_auto_size_ctl.flash_threshold        = 0.5;


        invalid_auto_size_ctl.decr_mode      = H5C2_decr__age_out_with_threshold;

        invalid_auto_size_ctl.upper_hr_threshold     = 0.999;

        invalid_auto_size_ctl.decrement              = 0.9;

        invalid_auto_size_ctl.apply_max_decrement    = TRUE;
        invalid_auto_size_ctl.max_decrement          = (1 * 1024 * 1024);

        invalid_auto_size_ctl.epochs_before_eviction =
                                       H5C2__MAX_EPOCH_MARKERS + 1; /* INVALID */

        invalid_auto_size_ctl.apply_empty_reserve    = TRUE;
        invalid_auto_size_ctl.empty_reserve          = 0.05;

        result = H5C2_set_cache_auto_resize_config(cache_ptr,
                                                  &invalid_auto_size_ctl);

        if ( result != FAIL ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_set_cache_auto_resize_config accepted bad empty_reserve 2.\n";
        }
    }

    if ( pass2 ) {

        result = H5C2_get_cache_auto_resize_config(cache_ptr,
                                                  &test_auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_get_cache_auto_resize_config failed 31.";

        } else if ( ! RESIZE_CONFIGS_ARE_EQUAL(test_auto_size_ctl, \
                                               ref_auto_size_ctl, FALSE) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected auto resize config 31.";
        }
    }


    /* finally, before we finish, try feeding
     * H5C2_get_cache_auto_resize_config invalid data.
     */

    if ( pass2 ) {

        result = H5C2_get_cache_auto_resize_config(NULL, &test_auto_size_ctl);

        if ( result != FAIL ) {

            pass2 = FALSE;
            failure_mssg2 =
                "H5C2_get_cache_auto_resize_config accepted NULL cache_ptr.\n";
        }
    }

    if ( pass2 ) {

        result = H5C2_get_cache_auto_resize_config((H5C2_t *)&test_auto_size_ctl,
                                                  &test_auto_size_ctl);

        if ( result != FAIL ) {

            pass2 = FALSE;
            failure_mssg2 =
                "H5C2_get_cache_auto_resize_config accepted bad cache_ptr.\n";
        }
    }

    if ( pass2 ) {

        result = H5C2_get_cache_auto_resize_config(cache_ptr, NULL);

        if ( result != FAIL ) {

            pass2 = FALSE;
            failure_mssg2 =
                "H5C2_get_cache_auto_resize_config accepted NULL config ptr.\n";
        }
    }

    if ( pass2 ) {

        takedown_cache2(cache_ptr, FALSE, FALSE);
    }

    if ( pass2 ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass2 ) {

        HDfprintf(stdout, "%s: failure_mssg2 = \"%s\".\n",
                  fcn_name, failure_mssg2);
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
 *			H5C2_get_cache_size()
 *			H5C2_get_cache_hit_rate()
 *			H5C2_reset_cache_hit_rate_stats()
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
    H5C2_t * cache_ptr = NULL;
    double hit_rate;
    size_t max_size;
    size_t min_clean_size;
    size_t cur_size;
    int32_t cur_num_entries;
    H5C2_auto_size_ctl_t auto_size_ctl =
    {
        /* int32_t     version                = */ H5C2__CURR_AUTO_SIZE_CTL_VER,
#if 1
        /* H5C2_auto_resize_report_fcn rpt_fcn = */ NULL,
#else
        /* H5C2_auto_resize_report_fcn rpt_fcn = */ H5C2_def_auto_resize_rpt_fcn,
#endif
        /* hbool_t     set_initial_size       = */ TRUE,
        /* size_t      initial_size           = */ (1 * 1024 * 1024),

        /* double      min_clean_fraction     = */ 0.5,

        /* size_t      max_size               = */ (16 * 1024 * 1025),
        /* size_t      min_size               = */ (512 * 1024),

        /* int64_t     epoch_length           = */ 50000,


        /* enum H5C2_cache_incr_mode incr_mode = */ H5C2_incr__off,

        /* double     lower_hr_threshold      = */ 0.75,

        /* double      increment              = */ 2.0,

        /* hbool_t     apply_max_increment    = */ TRUE,
        /* size_t      max_increment          = */ (4 * 1024 * 1024),

        /* enum H5C2_cache_flash_incr_mode      */
	/*                    flash_incr_mode = */ H5C2_flash_incr__off,
	/* double      flash_multiple         = */ 2.0,
	/* double      flash_threshold        = */ 0.5,

	
        /* enum H5C2_cache_decr_mode decr_mode = */ H5C2_decr__off,

        /* double      upper_hr_threshold     = */ 0.995,

        /* double      decrement              = */ 0.9,

        /* hbool_t     apply_max_decrement    = */ TRUE,
        /* size_t      max_decrement          = */ (1 * 1024 * 1024),

        /* int32_t     epochs_before_eviction = */ 3,

        /* hbool_t     apply_empty_reserve    = */ TRUE,
        /* double      empty_reserve          = */ 0.5
    };


    TESTING("automatic cache resize auxilary functions");

    pass2 = TRUE;

    /* allocate a cache, and then test the various auxilary functions.
     */

    if ( pass2 ) {

        reset_entries2();

        cache_ptr = setup_cache2((size_t)(2 * 1024),
                                 (size_t)(1 * 1024));
    }

    if ( pass2 ) {

        result = H5C2_set_cache_auto_resize_config(cache_ptr,
                                                  &auto_size_ctl);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_set_cache_auto_resize_config failed 1.\n";
        }
    }

    if ( pass2 ) {

        if ( ( cache_ptr->max_cache_size != (1 * 1024 * 1024) ) ||
             ( cache_ptr->min_clean_size != (512 * 1024) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "bad cache size after initialization.\n";
        }
    }

    /* lets start with the H5C2_get_cache_hit_rate(),
     * H5C2_reset_cache_hit_rate_stats() pair.
     */

    if ( pass2 ) {

        if ( ( H5C2_get_cache_hit_rate(NULL, &hit_rate) != FAIL ) ||
             ( H5C2_get_cache_hit_rate(cache_ptr, NULL) != FAIL ) ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_get_cache_hit_rate accepts bad params.\n";
        }
    }

    if ( pass2 ) {

        result = H5C2_get_cache_hit_rate(cache_ptr, &hit_rate);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_get_cache_hit_rate failed.\n";

        } else if ( hit_rate != 0.0 ) {

            pass2 = FALSE;
            failure_mssg2 =
                "H5C2_get_cache_hit_rate returned unexpected hit rate 1.\n";
        }
    }

    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, PICO_ENTRY_TYPE, i);

            if ( pass2 ) {

                unprotect_entry2(cache_ptr, PICO_ENTRY_TYPE, i,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }
    }

    if ( pass2 ) {

        result = H5C2_get_cache_hit_rate(cache_ptr, &hit_rate);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_get_cache_hit_rate failed.\n";

        } else if ( hit_rate != 0.0 ) {

            pass2 = FALSE;
            failure_mssg2 =
                "H5C2_get_cache_hit_rate returned unexpected hit rate 2.\n";

        } else if ( ( cache_ptr->cache_accesses != 1000 ) ||
                    ( cache_ptr->cache_hits != 0 ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache hit rate stats.\n";

        } else if ( rpt_fcn_called ) {

            pass2 = FALSE;
            failure_mssg2 = "Report function called?.\n";

        }
    }

    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, PICO_ENTRY_TYPE, 0);

            if ( pass2 ) {

                unprotect_entry2(cache_ptr, PICO_ENTRY_TYPE, 0,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }
    }

    if ( pass2 ) {

        result = H5C2_get_cache_hit_rate(cache_ptr, &hit_rate);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_get_cache_hit_rate failed.\n";

        } else if ( hit_rate != 0.5 ) {

            pass2 = FALSE;
            failure_mssg2 =
                "H5C2_get_cache_hit_rate returned unexpected hit rate 3.\n";

        } else if ( ( cache_ptr->cache_accesses != 2000 ) ||
                    ( cache_ptr->cache_hits != 1000 ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache hit rate stats.\n";

        } else if ( rpt_fcn_called ) {

            pass2 = FALSE;
            failure_mssg2 = "Report function called?.\n";

        }
    }

    if ( pass2 ) {

        result = H5C2_reset_cache_hit_rate_stats(NULL);

        if ( result != FAIL ) {

            pass2 = FALSE;
            failure_mssg2 =
                "H5C2_reset_cache_hit_rate_stats accepted NULL cache_ptr.\n";

        } else if ( ( cache_ptr->cache_accesses != 2000 ) ||
                    ( cache_ptr->cache_hits != 1000 ) ) {

            pass2 = FALSE;
            failure_mssg2 =
              "Failed call to H5C2_reset_cache_hit_rate_stats altered stats?\n";
        }
    }

    if ( pass2 ) {

        result = H5C2_reset_cache_hit_rate_stats(cache_ptr);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_reset_cache_hit_rate_stats failed.\n";

        } else if ( ( cache_ptr->cache_accesses != 0 ) ||
                    ( cache_ptr->cache_hits != 0 ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache hit rate stats.\n";

        }
    }

    if ( pass2 ) {

        rpt_fcn_called = FALSE;
        i = 0;
        while ( ( pass2 ) && ( i < 1000 ) )
        {
            protect_entry2(cache_ptr, PICO_ENTRY_TYPE, i + 500);

            if ( pass2 ) {

                unprotect_entry2(cache_ptr, PICO_ENTRY_TYPE, i + 500,
                                NO_CHANGE, H5C2__NO_FLAGS_SET);
            }
            i++;
        }
    }


    if ( pass2 ) {

        result = H5C2_get_cache_hit_rate(cache_ptr, &hit_rate);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_get_cache_hit_rate failed.\n";

        } else if ( hit_rate != 0.5 ) {

            pass2 = FALSE;
            failure_mssg2 =
                "H5C2_get_cache_hit_rate returned unexpected hit rate 4.\n";

        } else if ( ( cache_ptr->cache_accesses != 1000 ) ||
                    ( cache_ptr->cache_hits != 500 ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Unexpected cache hit rate stats.\n";

        } else if ( rpt_fcn_called ) {

            pass2 = FALSE;
            failure_mssg2 = "Report function called?.\n";

        }
    }

    /***************************************************
     * So much for testing H5C2_get_cache_hit_rate() and
     * H5C2_reset_cache_hit_rate_stats().  Now on to
     * H5C2_get_cache_size().
     ***************************************************/

    if ( pass2 ) {

        result = H5C2_get_cache_size(NULL, &max_size, &min_clean_size,
                                    &cur_size, &cur_num_entries);

        if ( result != FAIL ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_get_cache_size accepted NULL cache_ptr.\n";
        }
    }

    if ( pass2 ) {

        max_size        = 0;
        min_clean_size  = 0;
        cur_size        = 0;
        cur_num_entries = 0;

        result = H5C2_get_cache_size(cache_ptr, &max_size, &min_clean_size,
                                    &cur_size, &cur_num_entries);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_get_cache_size failed 1.\n";

        } else if ( max_size != (1 * 1024 * 1024) ) {

            pass2 = FALSE;
            failure_mssg2 =
                "H5C2_get_cache_size reports unexpected max_size 1.\n";

        } else if ( min_clean_size != (512 * 1024) ) {

            pass2 = FALSE;
            failure_mssg2 =
                "H5C2_get_cache_size reports unexpected min_clean_size 1.\n";

        } else if ( cur_size != (1500 * PICO_ENTRY_SIZE) ) {

            pass2 = FALSE;
            failure_mssg2 =
                "H5C2_get_cache_size reports unexpected cur_size 1.\n";

        } else if ( cur_num_entries != 1500 ) {

            pass2 = FALSE;
            failure_mssg2 =
                "H5C2_get_cache_size reports unexpected cur_num_entries 1.\n";
        }
    }

    /* read a larger entry so that cur_size and cur_num_entries will be
     * different.
     */
    if ( pass2 ) {

        protect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0);
    }

    if ( pass2 ) {
        unprotect_entry2(cache_ptr, MONSTER_ENTRY_TYPE, 0, NO_CHANGE,
                        H5C2__NO_FLAGS_SET);
    }

    if ( pass2 ) {

        max_size        = 0;
        min_clean_size  = 0;
        cur_size        = 0;
        cur_num_entries = 0;

        result = H5C2_get_cache_size(cache_ptr, &max_size, &min_clean_size,
                                    &cur_size, &cur_num_entries);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_get_cache_size failed 2.\n";

        } else if ( max_size != (1 * 1024 * 1024) ) {

            pass2 = FALSE;
            failure_mssg2 =
                "H5C2_get_cache_size reports unexpected max_size 2.\n";

        } else if ( min_clean_size != (512 * 1024) ) {

            pass2 = FALSE;
            failure_mssg2 =
                "H5C2_get_cache_size reports unexpected min_clean_size 2.\n";

        } else if ( cur_size !=
                   ((1500 * PICO_ENTRY_SIZE) + MONSTER_ENTRY_SIZE) ) {

            pass2 = FALSE;
            failure_mssg2 =
                "H5C2_get_cache_size reports unexpected cur_size 2.\n";

        } else if ( cur_num_entries != 1501 ) {

            pass2 = FALSE;
            failure_mssg2 =
                "H5C2_get_cache_size reports unexpected cur_num_entries 2.\n";
        }
    }

    if ( pass2 ) {

        max_size        = 0;
        min_clean_size  = 0;
        cur_size        = 0;
        cur_num_entries = 0;

        result = H5C2_get_cache_size(cache_ptr, &max_size, NULL, NULL, NULL);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_get_cache_size failed 3.\n";

        } else if ( max_size != (1 * 1024 * 1024) ) {

            pass2 = FALSE;
            failure_mssg2 =
                "H5C2_get_cache_size reports unexpected max_size 3.\n";

        } else if ( ( min_clean_size != 0 ) ||
                    ( cur_size != 0 ) ||
                    ( cur_num_entries != 0 ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Phantom returns from H5C2_get_cache_size?\n";

        }
    }

    if ( pass2 ) {

        max_size        = 0;
        min_clean_size  = 0;
        cur_size        = 0;
        cur_num_entries = 0;

        result = H5C2_get_cache_size(cache_ptr, NULL, &min_clean_size,
                                    NULL, NULL);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_get_cache_size failed 4.\n";

        } else if ( min_clean_size != (512 * 1024) ) {

            pass2 = FALSE;
            failure_mssg2 =
                "H5C2_get_cache_size reports unexpected min_clean_size 4.\n";

        } else if ( ( max_size != 0 ) ||
                    ( cur_size != 0 ) ||
                    ( cur_num_entries != 0 ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Phantom returns from H5C2_get_cache_size?\n";

        }
    }

    if ( pass2 ) {

        max_size        = 0;
        min_clean_size  = 0;
        cur_size        = 0;
        cur_num_entries = 0;

        result = H5C2_get_cache_size(cache_ptr, NULL, NULL, &cur_size, NULL);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_get_cache_size failed 5.\n";

        } else if ( cur_size !=
                   ((1500 * PICO_ENTRY_SIZE) + MONSTER_ENTRY_SIZE) ) {

            pass2 = FALSE;
            failure_mssg2 =
                "H5C2_get_cache_size reports unexpected cur_size 5.\n";

        } else if ( ( max_size != 0 ) ||
                    ( min_clean_size != 0 ) ||
                    ( cur_num_entries != 0 ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Phantom returns from H5C2_get_cache_size?\n";

        }
    }

    if ( pass2 ) {

        max_size        = 0;
        min_clean_size  = 0;
        cur_size        = 0;
        cur_num_entries = 0;

        result = H5C2_get_cache_size(cache_ptr, NULL, NULL, NULL,
                                    &cur_num_entries);

        if ( result != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_get_cache_size failed 6.\n";

        } else if ( cur_num_entries != 1501 ) {

            pass2 = FALSE;
            failure_mssg2 =
                "H5C2_get_cache_size reports unexpected cur_num_entries 2.\n";

        } else if ( ( max_size != 0 ) ||
                    ( min_clean_size != 0 ) ||
                    ( cur_size != 0 ) ) {

            pass2 = FALSE;
            failure_mssg2 = "Phantom returns from H5C2_get_cache_size?\n";

        }
    }

    if ( pass2 ) {

        takedown_cache2(cache_ptr, FALSE, FALSE);
    }

    if ( pass2 ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass2 ) {

        HDfprintf(stdout, "%s: failure_mssg2 = \"%s\".\n",
                  fcn_name, failure_mssg2);
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

    skip_long_tests2 = FALSE;

#ifdef NDEBUG
    run_full_test2 = TRUE;
#else /* NDEBUG */
    run_full_test2 = FALSE;
#endif /* NDEBUG */

#if 1
    run_full_test2 = TRUE;
#endif

#if 1
    smoke_check_1();
    smoke_check_2();
    smoke_check_3();
    smoke_check_4();
    smoke_check_5();
    smoke_check_6();
    smoke_check_7();
    smoke_check_8();
    smoke_check_9();
    smoke_check_10();
#endif
#if 1
    write_permitted_check();
#endif
    check_insert_entry();
    check_flush_cache();
    check_get_entry_status();
    check_expunge_entry();
    check_multiple_read_protect();
    check_rename_entry();
    check_pin_protected_entry();
    check_resize_entry();
    check_evictions_enabled();
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
    check_unprotect_ro_dirty_err();
    check_protect_ro_rw_err();
    check_check_evictions_enabled_err();
    check_auto_cache_resize();
    check_auto_cache_resize_disable();
    check_auto_cache_resize_epoch_markers();
    check_auto_cache_resize_input_errs();
    check_auto_cache_resize_aux_fcns();

    return(0);

} /* main() */
