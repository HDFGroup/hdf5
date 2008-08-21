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
 *              3/08
 *
 *		This file contains tests for the metadata journaling
 *		features implemented in H5C2.c and friends.
 */

#define H5F_PACKAGE             /*suppress error about including H5Fpkg   */

#include "h5test.h"
#include "H5Eprivate.h"
#include "H5Iprivate.h"
#include "H5MMprivate.h"        /* Memory management                    */
#include "H5MFprivate.h"
#include "H5AC2private.h"
#include "cache2_common.h"
#include "H5Fpkg.h"

#define HDF5_FILE_NAME "HDF5.file"

/* global variable declarations: */

const char *FILENAMES[] = {
        "cache_test",
        "cache_journal_test",
	"cache_sb_test",
	"journal_file",
        NULL
};


/* private function declarations: */

/* utility functions */

static void begin_trans(H5C2_t * cache_ptr,
                        hbool_t verbose,
                        uint64_t expected_trans_num,
	                const char * trans_name);

static void copy_file(const char * input_file,
                      const char * output_file);

static void end_trans(H5F_t * file_ptr,
                      H5C2_t * cache_ptr,
                      hbool_t verbose,
                      uint64_t trans_num,
                      const char * trans_name);

static hbool_t file_exists(const char * file_path_ptr);

static void flush_journal(H5C2_t * cache_ptr);

static void jrnl_col_major_scan_backward2(H5F_t * file_ptr,
		                          int32_t max_index,
                                          int32_t lag, 
					  hbool_t verbose,
                                          hbool_t reset_stats,
                                          hbool_t display_stats,
                                          hbool_t display_detailed_stats,
                                          hbool_t do_inserts,
                                          hbool_t dirty_inserts,
                                          int dirty_unprotects,
			                  uint64_t trans_num);

static void jrnl_col_major_scan_forward2(H5F_t * file_ptr,
		                         int32_t max_index,
                                         int32_t lag,
                                         hbool_t verbose,
                                         hbool_t reset_stats,
                                         hbool_t display_stats,
                                         hbool_t display_detailed_stats,
                                         hbool_t do_inserts,
                                         hbool_t dirty_inserts,
                                         int dirty_unprotects,
			                 uint64_t trans_num);

static void jrnl_row_major_scan_backward2(H5F_t * file_ptr,
                                          int32_t max_index,
                                          int32_t lag,
                                          hbool_t verbose,
                                          hbool_t reset_stats,
                                          hbool_t display_stats,
                                          hbool_t display_detailed_stats,
                                          hbool_t do_inserts,
                                          hbool_t dirty_inserts,
                                          hbool_t do_renames,
                                          hbool_t rename_to_main_addr,
                                          hbool_t do_destroys,
			                  hbool_t do_mult_ro_protects,
                                          int dirty_destroys,
                                          int dirty_unprotects,
			                  uint64_t trans_num);

static void jrnl_row_major_scan_forward2(H5F_t * file_ptr,
                                         int32_t max_index,
                                         int32_t lag,
                                         hbool_t verbose,
                                         hbool_t reset_stats,
                                         hbool_t display_stats,
                                         hbool_t display_detailed_stats,
                                         hbool_t do_inserts,
                                         hbool_t dirty_inserts,
                                         hbool_t do_renames,
                                         hbool_t rename_to_main_addr,
                                         hbool_t do_destroys,
		                         hbool_t do_mult_ro_protects,
                                         int dirty_destroys,
                                         int dirty_unprotects,
			                 uint64_t trans_num);

static void open_existing_file_for_journaling(const char * hdf_file_name,
                                              const char * journal_file_name,
                                              hid_t * file_id_ptr,
                                              H5F_t ** file_ptr_ptr,
                                              H5C2_t ** cache_ptr_ptr); 

static void open_existing_file_without_journaling(const char * hdf_file_name,
                                                  hid_t * file_id_ptr,
                                                  H5F_t ** file_ptr_ptr,
                                                  H5C2_t ** cache_ptr_ptr);

static void setup_cache_for_journaling(const char * hdf_file_name,
                                       const char * journal_file_name,
                                       hid_t * file_id_ptr,
                                       H5F_t ** file_ptr_ptr,
                                       H5C2_t ** cache_ptr_ptr,
				       hbool_t use_core_driver_if_avail);

static void takedown_cache_after_journaling(hid_t file_id,
                                            const char * filename,
                                            const char * journal_filename,
			                    hbool_t use_core_driver_if_avail);

static void verify_journal_contents(const char * journal_file_path_ptr,
                                    const char * expected_file_path_ptr);

static void verify_journal_deleted(const char * journal_file_path_ptr);

static void verify_journal_empty(const char * journal_file_path_ptr);

/* test functions */

static void check_buffer_writes(void);

static void write_flush_verify(H5C2_jbrb_t * struct_ptr, 
                               int size, 
                               char * data, 
                               FILE * readback);

static void write_noflush_verify(H5C2_jbrb_t * struct_ptr, 
                                 int size, 
                                 char * data, 
                                 FILE * readback, 
                                 int repeats);

static void check_mdj_config_block_IO(void);

static void check_mdj_file_marking(void);

static void verify_mdj_file_marking_after_open(void);

static void verify_mdj_file_marking_on_create(void);

static void verify_mdj_file_marking_on_open(void);

static void verify_mdj_file_unmarking_on_file_close(void);

static void verify_mdj_file_unmarking_on_journaling_shutdown(void);

static void verify_mdj_file_unmarking_on_recovery(void);

static void test_mdj_conf_blk_read_write_discard(H5F_t * file_ptr,
		                                 const char * jrnl_file_path);

static void check_superblock_extensions(void);

static void check_message_format(void);

static void check_legal_calls(void);

static void check_transaction_tracking(void);

static void mdj_smoke_check_00(void);

static void mdj_smoke_check_01(void);

static void mdj_smoke_check_02(void);

static void write_verify_trans_num(H5C2_jbrb_t * struct_ptr, 
                                   uint64_t trans_num, 
                                   uint64_t verify_val);



/**************************************************************************/
/**************************************************************************/
/********************************* tests: *********************************/
/**************************************************************************/
/**************************************************************************/

/*** metadata journaling test utility functions ***/

/*-------------------------------------------------------------------------
 * Function:    begin_trans()
 *
 * Purpose:     If pass2 is true on entry, attempt to begin a transaction.
 * 		If the operation fails, or if it returns an unexpected
 * 		transaction number, set passw2 to FALSE, and set failure_mssg2 
 *              to point to an appropriate failure message.
 *
 *              Do nothing if pass2 is FALSE on entry.
 *
 * Return:      void
 *
 * Programmer:  John Mainzer
 *              5/15/08
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
begin_trans(H5C2_t * cache_ptr,
            hbool_t verbose,
            uint64_t expected_trans_num,
	    const char * trans_name)
{
    const char * fcn_name = "begin_trans()";
    herr_t result;
    uint64_t trans_num = 0; 

    if ( pass2 ) {

        result = H5C2_begin_transaction(cache_ptr, &trans_num, trans_name);

        if ( result < 0 ) {

            if ( verbose ) {

                HDfprintf(stdout, "%s: H5C2_begin_transaction(%s) failed.\n",
			  fcn_name, trans_name);
            }
            pass2 = FALSE;
	    failure_mssg2 = "H5C2_begin_transaction() failed.\n";

        } else if ( trans_num != expected_trans_num ) {

            if ( verbose ) {

                HDfprintf(stdout, "%s: actual/expected trans num = %lld/%lld.\n",
			  fcn_name, (long long)trans_num,
                          (long long)expected_trans_num);
            }
            pass2 = FALSE;
	    failure_mssg2 = "begin_trans() issued unexpected trans_num.\n";
        }
    }

    return;

} /* begin_trans() */


/*-------------------------------------------------------------------------
 * Function:    copy_file()
 *
 * Purpose:     If pass2 is true, copy the input file to the output file.
 *              Set pass2 to FALSE and set failure_mssg2 to point to an 
 *              appropriate error message on failure.
 *
 *              Do nothing if pass2 is false on entry.
 *
 * Return:      void
 *
 * Programmer:  John Mainzer
 *              5/15/08
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
copy_file(const char * input_file,
          const char * output_file)
{
    const char * fcn_name = "copy_file()";
    char buffer[(8 * 1024) + 1];
    hbool_t verbose = FALSE;
    size_t cur_buf_len;
    const size_t max_buf_len = (8 * 1024);
    size_t input_len;
    size_t input_remainder = 0;
    ssize_t result;
    int input_file_fd = -1;
    int output_file_fd = -1;
    h5_stat_t buf;

    if ( pass2 ) {

	if ( input_file == NULL ) {

            failure_mssg2 = "input_file NULL on entry?!?",
            pass2 = FALSE;

	} else if ( output_file == NULL ) {

            failure_mssg2 = "output_file NULL on entry?!?",
            pass2 = FALSE;

        }
    }

    /* get the length of the input file */
    if ( pass2 ) {

	if ( HDstat(input_file, &buf) != 0 ) {

	    if ( verbose ) {

	        HDfprintf(stdout, "%s: HDstat() failed with errno = %d.\n",
                          fcn_name, errno);
	    }
	    failure_mssg2 = "stat() failed on journal file.";
	    pass2 = FALSE;

	} else {

	    if ( (buf.st_size) == 0 ) {

                failure_mssg2 = "input file empty?!?";
	        pass2 = FALSE;

	    } else {
                
	        input_len = (size_t)(buf.st_size);
		input_remainder = input_len;

		if ( verbose ) {

		    HDfprintf(stdout, "%s: input_len = %d.\n", 
		              fcn_name, (int)input_len);
		}
            }
	} 
    }

    /* open the input file */
    if ( pass2 ) {

	if ( (input_file_fd = HDopen(input_file, O_RDONLY, 0777)) == -1 ) {

	    if ( verbose ) {

	        HDfprintf(stdout, "%s: HDopen(i) failed with errno = %d.\n",
                          fcn_name, errno);
	    }
            failure_mssg2 = "Can't open input file.";
	    pass2 = FALSE;
        }
    }

    /* open the output file */
    if ( pass2 ) {

	if ( (output_file_fd = HDopen(output_file, O_WRONLY|O_CREAT|O_TRUNC, 0777))
             == -1 ) {

	    if ( verbose ) {

	        HDfprintf(stdout, "%s: HDopen(i) failed with errno = %d.\n",
                          fcn_name, errno);
	    }
            failure_mssg2 = "Can't open output file.";
	    pass2 = FALSE;
        }
    }

    while ( ( pass2 ) &&
	    ( input_remainder > 0 ) ) 
    {
        if ( input_remainder > max_buf_len ) {

            cur_buf_len = max_buf_len;
            input_remainder -= max_buf_len;

        } else {

            cur_buf_len = input_remainder;
            input_remainder = 0;
        }

        result = HDread(input_file_fd, buffer, cur_buf_len);

        if ( result != (int)cur_buf_len ) {

            if ( verbose ) {

                HDfprintf(stdout, 
                          "%s: HDread() failed. result = %d, errno = %d.\n",
                          fcn_name, (int)result, errno);
            }
            failure_mssg2 = "error reading input file.";
            pass2 = FALSE;
        }

        buffer[cur_buf_len] = '\0';

        if ( pass2 ) {

            result = HDwrite(output_file_fd, buffer, cur_buf_len);

            if ( result != (int)cur_buf_len ) {

	        if ( verbose ) {

                    HDfprintf(stdout, 
                              "%s: HDwrite() failed. result = %d, errno = %d.\n",
                              fcn_name, (int)result, errno);
                }
                failure_mssg2 = "error writing output file.";
                pass2 = FALSE;
            }
        }
    }

    if ( input_file_fd != -1 ) {

        if ( HDclose(input_file_fd) != 0 ) {

	    if ( verbose ) {

	        HDfprintf(stdout, "%s: HDclose(i) failed with errno = %d.\n",
                          fcn_name, errno);
	    }

	    if ( pass2 ) {

                failure_mssg2 = "Can't close input file.";
	        pass2 = FALSE;
	    }
	}
    }

    if ( output_file_fd != -1 ) {

        if ( HDclose(output_file_fd) != 0 ) {

	    if ( verbose ) {

	        HDfprintf(stdout, "%s: HDclose(o) failed with errno = %d.\n",
                          fcn_name, errno);
	    }

	    if ( pass2 ) {

                failure_mssg2 = "Can't close output file.";
	        pass2 = FALSE;
	    }
	}
    }

    return;

} /* copy_file() */


/*-------------------------------------------------------------------------
 * Function:    end_trans()
 *
 * Purpose:     If pass2 is true on entry, attempt to end the current 
 * 		transaction.  If the operation fails, set pass2 to FALSE, 
 * 		and set failure_mssg2 to point to an appropriate failure 
 * 		message.
 *
 *              Do nothing if pass2 is FALSE on entry.
 *
 * Return:      void
 *
 * Programmer:  John Mainzer
 *              5/15/08
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
end_trans(H5F_t * file_ptr,
          H5C2_t * cache_ptr,
          hbool_t verbose,
          uint64_t trans_num,
          const char * trans_name)
{
    const char * fcn_name = "end_trans()";
    herr_t result;

    if ( pass2 ) {

        result = H5C2_end_transaction(file_ptr, cache_ptr, trans_num, trans_name);

        if ( result < 0 ) {

            if ( verbose ) {
                HDfprintf(stdout, 
			  "%s: H5C2_end_transaction(%lld, \"%s\") failed.\n",
			  fcn_name, (long long)trans_num, trans_name);
            }
            pass2 = FALSE;
	    failure_mssg2 = "H5C2_end_transaction() failed.\n";
        }
    }

    return;

} /* end_trans() */


/*-------------------------------------------------------------------------
 * Function:    file_exists()
 *
 * Purpose:     If pass2 is true on entry, stat the target file, and 
 * 		return TRUE if it exists, and FALSE if it does not.
 *
 * 		If any errors are detected in this process, set pass2 
 * 		to FALSE and set failure_mssg2 to point to an appropriate 
 * 		error message.
 *
 *              Do nothing and return FALSE if pass2 is FALSE on entry.
 *
 * Return:      void
 *
 * Programmer:  John Mainzer
 *              5//08
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static hbool_t
file_exists(const char * file_path_ptr)
{
    const char * fcn_name = "file_exists()";
    hbool_t ret_val = FALSE; /* will set to TRUE if necessary */
    hbool_t verbose = FALSE;
    h5_stat_t buf;

    if ( pass2 ) {

	if ( file_path_ptr == NULL ) {

            failure_mssg2 = "file_path_ptr NULL on entry?!?",
            pass2 = FALSE;
	}
    }

    if ( pass2 ) {

	if ( HDstat(file_path_ptr, &buf) == 0 ) {

	    if ( verbose ) {

	        HDfprintf(stdout, "%s: HDstat(%s) succeeded.\n", fcn_name,
			  file_path_ptr);
	    }

	    ret_val = TRUE;

        } else if ( errno == ENOENT ) {

	    if ( verbose ) {

	        HDfprintf(stdout, "%s: HDstat(%s) failed with ENOENT\n", 
			  fcn_name, file_path_ptr);
	    }
	    
	} else {

	    if ( verbose ) {

	        HDfprintf(stdout, 
			  "%s: HDstat() failed with unexpected errno = %d.\n",
                          fcn_name, errno);
	    }

	    failure_mssg2 = "HDstat() returned unexpected value.";
	    pass2 = FALSE;

	} 
    }

    return(ret_val);

} /* file_exists() */


/*-------------------------------------------------------------------------
 * Function:    flush_journal()
 *
 * Purpose:     If pass2 is true on entry, attempt to flush the journal.
 * 		If the operation fails, set pass2 to FALSE,  and set 
 * 		failure_mssg2 to point to an appropriate failure message.
 *
 *              Do nothing if pass2 is FALSE on entry.
 *
 * Return:      void
 *
 * Programmer:  John Mainzer
 *              5/15/08
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
flush_journal(H5C2_t * cache_ptr)
{
    if ( pass2 ) {

        if ( H5C2_jb__flush(&(cache_ptr->mdj_jbrb)) < 0 ) {
		
	    pass2 = FALSE;
	    failure_mssg2 = "H5C2_jb__flush() reports failure.";
	}
    }

    return;

} /* flush_journal() */


/*-------------------------------------------------------------------------
 * Function:	jrnl_col_major_scan_backward2()
 *
 * Purpose:	Do a sequence of inserts, protects, and unprotects
 *		broken into a sequence of transactions while scanning 
 *		backwards through the set of entries.  
 *
 *		If pass2 is false on entry, do nothing.
 *
 *		Note tht this function is an adaption of 
 *		col_major_scan_backward2()
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              5/20/08
 *
 * Modifications:
 *
 * 		None.
 *
 *-------------------------------------------------------------------------
 */

void
jrnl_col_major_scan_backward2(H5F_t * file_ptr,
		              int32_t max_index,
                              int32_t lag,
                              hbool_t verbose,
                              hbool_t reset_stats,
                              hbool_t display_stats,
                              hbool_t display_detailed_stats,
                              hbool_t do_inserts,
                              hbool_t dirty_inserts,
                              int dirty_unprotects,
			      uint64_t trans_num)
{
    const char * fcn_name = "jrnl_col_major_scan_backward2()";
    H5C2_t * cache_ptr = file_ptr->shared->cache2;
    int i;
    int mile_stone = 1;
    int32_t type;
    int32_t idx;
    int32_t local_max_index[NUMBER_OF_ENTRY_TYPES];

    if ( verbose )
        HDfprintf(stdout, "%s: entering.\n", fcn_name);

    for ( i = 0; i < NUMBER_OF_ENTRY_TYPES; i++ )
    {
        local_max_index[i] = MIN(max_index, max_indices2[i]);
    }

    HDassert( lag > 5 );

    if ( ( pass2 ) && ( reset_stats ) ) {

        H5C2_stats__reset(cache_ptr);
    }

    idx = local_max_index[NUMBER_OF_ENTRY_TYPES - 1] + lag;

    if ( verbose ) /* 1 */
        HDfprintf(stdout, "%s: point %d.\n", fcn_name, mile_stone++);


    while ( ( pass2 ) && ( (idx + lag) >= 0 ) )
    {
        type = NUMBER_OF_ENTRY_TYPES - 1;

	trans_num++;

        begin_trans(cache_ptr, verbose, trans_num, 
	            "jrnl_col_major_scan_backward outer loop");
	    
        if ( verbose ) {

            HDfprintf(stdout, "begin trans %lld, idx = %d.\n", trans_num, idx);
        }

        while ( ( pass2 ) && ( type >= 0 ) )
        {
	    if ( verbose ) {

                HDfprintf(stdout, "%d:%d: ", type, idx);
	    }
	    
            if ( ( pass2 ) && ( do_inserts) && ( (idx - lag) >= 0 ) &&
                 ( (idx - lag) <= local_max_index[type] ) &&
                 ( ((idx - lag) % 3) == 0 ) &&
                 ( ! entry_in_cache2(cache_ptr, type, (idx - lag)) ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(i, %d, %d) ", type, (idx - lag));

                insert_entry2(file_ptr, type, (idx - lag), dirty_inserts,
                              H5C2__NO_FLAGS_SET);
            }

            if ( ( pass2 ) &&
		 ( idx >= 0 ) && 
		 ( idx <= local_max_index[type] ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(p, %d, %d) ", type, idx);

                protect_entry2(file_ptr, type, idx);
            }

            if ( ( pass2 ) && ( (idx + lag) >= 0 ) &&
                 ( (idx + lag) <= local_max_index[type] ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(u, %d, %d) ", type, (idx + lag));

                unprotect_entry2(file_ptr, type, idx + lag,
                                dirty_unprotects, H5C2__NO_FLAGS_SET);
            }

            if ( verbose )
                HDfprintf(stdout, "\n");

            type--;
        }

        end_trans(file_ptr, cache_ptr, verbose, trans_num, 
	          "jrnl_col_major_scan_backward outer loop");

	if ( verbose ) {

            HDfprintf(stdout, "end trans %lld, idx = %d.\n", trans_num, idx);
        }

	if ( ( verbose ) && ( ! pass2 ) ) {

	    HDfprintf(stdout, "pass2 == FALSE, failure mssg = \"%s\".\n",
		      failure_mssg2);
	}

        idx--;
    }

    if ( verbose ) /* 2 */
        HDfprintf(stdout, "%s: point %d.\n", fcn_name, mile_stone++);

    if ( ( pass2 ) && ( display_stats ) ) {

        H5C2_stats(cache_ptr, "test cache", display_detailed_stats);
    }

    if ( verbose )
        HDfprintf(stdout, "%s: exiting.\n", fcn_name);

    return;

} /* jrnl_col_major_scan_backward2() */


/*-------------------------------------------------------------------------
 * Function:	jrnl_col_major_scan_forward2()
 *
 * Purpose:	Do a sequence of inserts, protects, and unprotects
 *		broken into a sequence of transactions while scanning 
 *		through the set of entries.  
 *
 *		Note that this function is an adaption of 
 *		col_major_scan_forward2().
 *
 *		If pass2 is false on entry, do nothing.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              5/20/08
 *
 * Modifications:
 *
 * 		None.
 *
 *-------------------------------------------------------------------------
 */

void
jrnl_col_major_scan_forward2(H5F_t * file_ptr,
		             int32_t max_index,
                             int32_t lag,
                             hbool_t verbose,
                             hbool_t reset_stats,
                             hbool_t display_stats,
                             hbool_t display_detailed_stats,
                             hbool_t do_inserts,
                             hbool_t dirty_inserts,
                             int dirty_unprotects,
			     uint64_t trans_num)
{
    const char * fcn_name = "jrnl_col_major_scan_forward2()";
    H5C2_t * cache_ptr = file_ptr->shared->cache2;
    int i;
    int32_t type;
    int32_t idx;
    int32_t local_max_index[NUMBER_OF_ENTRY_TYPES];

    if ( verbose )
        HDfprintf(stdout, "%s: entering.\n", fcn_name);

    for ( i = 0; i < NUMBER_OF_ENTRY_TYPES; i++ )
    {
        local_max_index[i] = MIN(max_index, max_indices2[i]);
    }

    HDassert( lag > 5 );

    type = 0;

    if ( ( pass2 ) && ( reset_stats ) ) {

        H5C2_stats__reset(cache_ptr);
    }

    idx = -lag;

    while ( ( pass2 ) && ( (idx - lag) <= MAX_ENTRIES ) )
    {
        type = 0;

	trans_num++;

        begin_trans(cache_ptr, verbose, trans_num, 
	            "jrnl_col_major_scan_forward outer loop");
	    
        if ( verbose ) {

            HDfprintf(stdout, "begin trans %lld, idx = %d.\n", trans_num, idx);
        }

        while ( ( pass2 ) && ( type < NUMBER_OF_ENTRY_TYPES ) )
        {
	    if ( verbose ) {

                HDfprintf(stdout, "%d:%d: ", type, idx);
	    }
	    
            if ( ( pass2 ) && ( do_inserts ) && ( (idx + lag) >= 0 ) &&
                 ( (idx + lag) <= local_max_index[type] ) &&
                 ( ((idx + lag) % 3) == 0 ) &&
                 ( ! entry_in_cache2(cache_ptr, type, (idx + lag)) ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(i, %d, %d) ", type, (idx + lag));

                insert_entry2(file_ptr, type, (idx + lag), dirty_inserts,
                              H5C2__NO_FLAGS_SET);
            }

            if ( ( pass2 ) && 
                 ( idx >= 0 ) && 
                 ( idx <= local_max_index[type] ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(p, %d, %d) ", type, idx);

                protect_entry2(file_ptr, type, idx);
            }

            if ( ( pass2 ) && ( (idx - lag) >= 0 ) &&
                 ( (idx - lag) <= local_max_index[type] ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(u, %d, %d) ", type, (idx - lag));

                unprotect_entry2(file_ptr, type, idx - lag,
                                dirty_unprotects, H5C2__NO_FLAGS_SET);
            }

            if ( verbose )
                HDfprintf(stdout, "\n");

            type++;
        }

        end_trans(file_ptr, cache_ptr, verbose, trans_num, 
	          "jrnl_col_major_scan_forward outer loop");

	if ( verbose ) {

            HDfprintf(stdout, "end trans %lld, idx = %d.\n", trans_num, idx);
        }

	if ( ( verbose ) && ( ! pass2 ) ) {

	    HDfprintf(stdout, "pass2 == FALSE, failure mssg = \"%s\".\n",
		      failure_mssg2);
	}

        idx++;
    }

    if ( ( pass2 ) && ( display_stats ) ) {

        H5C2_stats(cache_ptr, "test cache", display_detailed_stats);
    }

    return;

} /* jrnl_col_major_scan_forward2() */


/*-------------------------------------------------------------------------
 * Function:	jrnl_row_major_scan_backward2()
 *
 * Purpose:	Do a sequence of inserts, protects, unprotects, renames,
 *		destroys broken into transactions while scanning backwards 
 *		through the set of entries.  
 *
 *		If pass2 is false on entry, do nothing.
 *
 *		Note that this function is an adaption of 
 *		row_major_scan_backward2()
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              5/20/08
 *
 * Modifications:
 *
 * 		None.
 *
 *-------------------------------------------------------------------------
 */

void
jrnl_row_major_scan_backward2(H5F_t * file_ptr,
                              int32_t max_index,
                              int32_t lag,
                              hbool_t verbose,
                              hbool_t reset_stats,
                              hbool_t display_stats,
                              hbool_t display_detailed_stats,
                              hbool_t do_inserts,
                              hbool_t dirty_inserts,
                              hbool_t do_renames,
                              hbool_t rename_to_main_addr,
                              hbool_t do_destroys,
			      hbool_t do_mult_ro_protects,
                              int dirty_destroys,
                              int dirty_unprotects,
			      uint64_t trans_num)
{
    const char * fcn_name = "jrnl_row_major_scan_backward2";
    H5C2_t * cache_ptr = file_ptr->shared->cache2;
    int32_t type;
    int32_t idx;
    int32_t local_max_index;
    int32_t lower_bound;
    int32_t upper_bound;

    if ( verbose )
        HDfprintf(stdout, "%s(): Entering.\n", fcn_name);

    HDassert( lag >= 10 );

    type = NUMBER_OF_ENTRY_TYPES - 1;

    if ( ( pass2 ) && ( reset_stats ) ) {

        H5C2_stats__reset(cache_ptr);
    }

    while ( ( pass2 ) && ( type >= 0 ) )
    {
        local_max_index = MIN(max_index, max_indices2[type]);

        idx = local_max_index + lag;

	upper_bound = local_max_index;
	lower_bound = upper_bound - 8;

        while ( ( pass2 ) && ( idx >= -lag ) )
        {
	    if ( idx == ( upper_bound + lag ) ) {

	        trans_num++;

                begin_trans(cache_ptr, verbose, trans_num, 
			    "jrnl_row_major_scan_backward inner loop");

		if ( verbose )
		    HDfprintf(stdout, "begin trans %lld.\n", 
			      (long long)trans_num);

	        if ( verbose )
	            HDfprintf(stdout, "(%d, %d)\n", lower_bound, upper_bound);
	    }

	    while ( ( pass2 ) && ( idx >= lower_bound - lag ) ) 
            {
	        if ( verbose ) {

                    HDfprintf(stdout, "%lld:%d:%d: ", trans_num, type, idx);
	        }
	    
                if ( ( pass2 ) && ( do_inserts ) && 
		     ( (idx - lag) >= 0 ) &&
		     ( (idx - lag) >= lower_bound ) &&
                     ( (idx - lag) <= local_max_index ) &&
                     ( (idx - lag) <= upper_bound ) &&
                     ( ((idx - lag) % 2) == 1 ) &&
                     ( ! entry_in_cache2(cache_ptr, type, (idx - lag)) ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "(i, %d, %d) ", type, (idx - lag));

                    insert_entry2(file_ptr, type, (idx - lag), dirty_inserts,
                                  H5C2__NO_FLAGS_SET);
                }


                if ( ( pass2 ) && 
		     ( (idx - lag + 1) >= 0 ) &&
		     ( (idx - lag + 1) >= lower_bound ) &&
                     ( (idx - lag + 1) <= local_max_index ) &&
                     ( (idx - lag + 1) <= upper_bound ) &&
                     ( ( (idx - lag + 1) % 3 ) == 0 ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "(p, %d, %d) ", 
				  type, (idx - lag + 1));

                    protect_entry2(file_ptr, type, (idx - lag + 1));
                }

                if ( ( pass2 ) && 
		     ( (idx - lag + 2) >= 0 ) &&
		     ( (idx - lag + 2) >= lower_bound ) &&
                     ( (idx - lag + 2) <= local_max_index ) &&
                     ( (idx - lag + 2) <= upper_bound ) &&
                     ( ( (idx - lag + 2) % 3 ) == 0 ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "(u, %d, %d) ", 
				  type, (idx - lag + 2));

                    unprotect_entry2(file_ptr, type, idx-lag+2, NO_CHANGE,
                                     H5C2__NO_FLAGS_SET);
                }


                if ( ( pass2 ) && ( do_renames ) && 
		     ( (idx - lag + 2) >= 0 ) &&
		     ( (idx - lag + 2) >= lower_bound ) &&
                     ( (idx - lag + 2) <= local_max_index ) &&
                     ( (idx - lag + 2) <= upper_bound ) &&
                     ( ( (idx - lag + 2) % 3 ) == 0 ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "(r, %d, %d, %d) ", 
			          type, (idx - lag + 2), 
				  (int)rename_to_main_addr);

                    rename_entry2(cache_ptr, type, (idx - lag + 2),
                                  rename_to_main_addr);
                }


                if ( ( pass2 ) && 
		     ( (idx - lag + 3) >= 0 ) &&
		     ( (idx - lag + 3) >= lower_bound ) &&
                     ( (idx - lag + 3) <= local_max_index ) &&
                     ( (idx - lag + 3) <= upper_bound ) &&
                     ( ( (idx - lag + 3) % 5 ) == 0 ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "(p, %d, %d) ", 
				  type, (idx - lag + 3));

                    protect_entry2(file_ptr, type, (idx - lag + 3));
                }

                if ( ( pass2 ) && 
		     ( (idx - lag + 5) >= 0 ) &&
		     ( (idx - lag + 5) >= lower_bound ) &&
                     ( (idx - lag + 5) <= local_max_index ) &&
                     ( (idx - lag + 5) <= upper_bound ) &&
                     ( ( (idx - lag + 5) % 5 ) == 0 ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "(u, %d, %d) ", 
				  type, (idx - lag + 5));

                    unprotect_entry2(file_ptr, type, idx-lag+5, NO_CHANGE,
                                     H5C2__NO_FLAGS_SET);
                }

	        if ( do_mult_ro_protects )
	        {
		    if ( ( pass2 ) && 
		         ( (idx - lag + 5) >= 0 ) &&
		         ( (idx - lag + 5) >= lower_bound ) &&
		         ( (idx - lag + 5) < local_max_index ) &&
		         ( (idx - lag + 5) < upper_bound ) &&
		         ( (idx - lag + 5) % 9 == 0 ) ) {

                        if ( verbose )
                            HDfprintf(stdout, "(p-ro, %d, %d) ", type, 
				      (idx - lag + 5));

		        protect_entry_ro2(file_ptr, type, (idx - lag + 5));
		    }

		    if ( ( pass2 ) && 
		         ( (idx - lag + 6) >= 0 ) &&
		         ( (idx - lag + 6) >= lower_bound ) &&
		         ( (idx - lag + 6) < local_max_index ) &&
		         ( (idx - lag + 6) < upper_bound ) &&
		         ( (idx - lag + 6) % 11 == 0 ) ) {

                        if ( verbose )
                            HDfprintf(stdout, "(p-ro, %d, %d) ", type, 
				      (idx - lag + 6));

		        protect_entry_ro2(file_ptr, type, (idx - lag + 6));
		    }

		    if ( ( pass2 ) && 
		         ( (idx - lag + 7) >= 0 ) &&
		         ( (idx - lag + 7) >= lower_bound ) &&
		         ( (idx - lag + 7) < local_max_index ) &&
		         ( (idx - lag + 7) < upper_bound ) &&
		         ( (idx - lag + 7) % 13 == 0 ) ) {

                        if ( verbose )
                            HDfprintf(stdout, "(p-ro, %d, %d) ", type, 
				      (idx - lag + 7));

		        protect_entry_ro2(file_ptr, type, (idx - lag + 7));
		    }

		    if ( ( pass2 ) && 
		         ( (idx - lag + 7) >= 0 ) &&
		         ( (idx - lag + 7) >= lower_bound ) &&
		         ( (idx - lag + 7) < local_max_index ) &&
		         ( (idx - lag + 7) < upper_bound ) &&
		         ( (idx - lag + 7) % 9 == 0 ) ) {

                        if ( verbose )
                            HDfprintf(stdout, "(u-ro, %d, %d) ", type, 
				      (idx - lag + 7));

		        unprotect_entry2(file_ptr, type, (idx - lag + 7),
				         FALSE, H5C2__NO_FLAGS_SET);
		    }

		    if ( ( pass2 ) && 
		         ( (idx - lag + 8) >= 0 ) &&
		         ( (idx - lag + 8) >= lower_bound ) &&
		         ( (idx - lag + 8) < local_max_index ) &&
		         ( (idx - lag + 8) < upper_bound ) &&
		         ( (idx - lag + 8) % 11 == 0 ) ) {

                        if ( verbose )
                            HDfprintf(stdout, "(u-ro, %d, %d) ", type, 
				      (idx - lag + 8));

		        unprotect_entry2(file_ptr, type, (idx - lag + 8),
				         FALSE, H5C2__NO_FLAGS_SET);
		    }

		    if ( ( pass2 ) && 
		         ( (idx - lag + 9) >= 0 ) &&
		         ( (idx - lag + 9) >= lower_bound ) &&
		         ( (idx - lag + 9) < local_max_index ) &&
		         ( (idx - lag + 9) < upper_bound ) &&
		         ( (idx - lag + 9) % 13 == 0 ) ) {

                        if ( verbose )
                            HDfprintf(stdout, "(u-ro, %d, %d) ", type, 
				      (idx - lag + 9));

		        unprotect_entry2(file_ptr, type, (idx - lag + 9),
				         FALSE, H5C2__NO_FLAGS_SET);
		    }
	        } /* if ( do_mult_ro_protects ) */

                if ( ( pass2 ) && 
		     ( idx >= 0 ) && 
		     ( idx >= lower_bound ) && 
		     ( idx <= local_max_index ) &&
		     ( idx <= upper_bound ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "(p, %d, %d) ", type, idx);

                    protect_entry2(file_ptr, type, idx);
                }


                if ( ( pass2 ) && 
		     ( (idx + lag - 2) >= 0 ) &&
		     ( (idx + lag - 2) >= lower_bound ) &&
                     ( (idx + lag - 2) <= local_max_index ) &&
                     ( (idx + lag - 2) <= upper_bound ) &&
                     ( ( (idx + lag - 2) % 7 ) == 0 ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "(u, %d, %d) ", 
				  type, (idx + lag - 2));

                    unprotect_entry2(file_ptr, type, idx+lag-2, NO_CHANGE,
                                     H5C2__NO_FLAGS_SET);
                }

                if ( ( pass2 ) && 
		     ( (idx + lag - 1) >= 0 ) &&
		     ( (idx + lag - 1) >= lower_bound ) &&
                     ( (idx + lag - 1) <= local_max_index ) &&
                     ( (idx + lag - 1) <= upper_bound ) &&
                     ( ( (idx + lag - 1) % 7 ) == 0 ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "(p, %d, %d) ", 
				  type, (idx + lag - 1));

                    protect_entry2(file_ptr, type, (idx + lag - 1));
                }


                if ( do_destroys ) {

                    if ( ( pass2 ) && 
	                 ( (idx + lag) >= 0 ) &&
	                 ( (idx + lag) >= lower_bound ) &&
                         ( ( idx + lag) <= local_max_index ) &&
                         ( ( idx + lag) <= upper_bound ) ) {

                        switch ( (idx + lag) %4 ) {

                            case 0:
                                if ( (entries2[type])[idx+lag].is_dirty ) {

                                    unprotect_entry2(file_ptr, type, idx + lag,
                                                     NO_CHANGE, 
						     H5C2__NO_FLAGS_SET);
                                } else {

                                    unprotect_entry2(file_ptr, type, idx + lag,
                                                     dirty_unprotects,
                                                     H5C2__NO_FLAGS_SET);
                                }
                                break;

                            case 1: /* we just did an insert */
                                unprotect_entry2(file_ptr, type, idx + lag,
                                                 NO_CHANGE, 
						 H5C2__NO_FLAGS_SET);
                                break;

                            case 2:
                                if ( (entries2[type])[idx + lag].is_dirty ) {

                                    unprotect_entry2(file_ptr, type, idx + lag,
                                                     NO_CHANGE, 
						     H5C2__DELETED_FLAG);
                                } else {

                                    unprotect_entry2(file_ptr, type, idx + lag,
                                                     dirty_destroys,
                                                     H5C2__DELETED_FLAG);
                                }
                                break;

                            case 3: /* we just did an insrt */
                                unprotect_entry2(file_ptr, type, idx + lag,
                                                 NO_CHANGE, 
						 H5C2__DELETED_FLAG);
                                break;

                            default:
                                HDassert(0); /* this can't happen... */
                                break;
                        }
                    }
                } else {

                    if ( ( pass2 ) && 
		         ( (idx + lag) >= 0 ) &&
		         ( (idx + lag) >= lower_bound ) &&
                         ( ( idx + lag) <= local_max_index ) &&
                         ( ( idx + lag) <= upper_bound ) ) {

                        if ( verbose )
                            HDfprintf(stdout, 
				      "(u, %d, %d) ", type, (idx + lag));

                        unprotect_entry2(file_ptr, type, idx + lag,
                                         dirty_unprotects, 
					 H5C2__NO_FLAGS_SET);
                    }
                }

		idx--;

		if ( verbose )
		    HDfprintf(stdout, "\n");

            } /* while ( ( pass2 ) && ( idx >= lower_bound - lag ) ) */

	    end_trans(file_ptr, cache_ptr, verbose, trans_num, 
                      "jrnl_row_major_scan_backward inner loop");

	    if ( verbose )
	        HDfprintf(stdout, "end trans %lld.\n", (long long)trans_num);

	    upper_bound = lower_bound - (2 * lag) - 2;
            lower_bound = upper_bound - 8;

	    idx = upper_bound + lag;

        } /* while ( ( pass2 ) && ( idx >= -lag ) ) */

        type--;

    } /* while ( ( pass2 ) && ( type >= 0 ) ) */

    if ( ( pass2 ) && ( display_stats ) ) {

        H5C2_stats(cache_ptr, "test cache", display_detailed_stats);
    }

    return;

} /* jrnl_row_major_scan_backward2() */


/*-------------------------------------------------------------------------
 * Function:	jrnl_row_major_scan_forward2()
 *
 * Purpose:	Do a sequence of inserts, protects, unprotects, renames,
 *		and destroys broken into transactions while scanning 
 *		through the set of entries. 
 *
 *		If pass2 is false on entry, do nothing.
 *
 *		Note that this function is an adaption of 
 *		row_major_scan_forward2().
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              5/20/08
 *
 * Modifications:
 *
 * 		None.
 *
 *-------------------------------------------------------------------------
 */

void
jrnl_row_major_scan_forward2(H5F_t * file_ptr,
                             int32_t max_index,
                             int32_t lag,
                             hbool_t verbose,
                             hbool_t reset_stats,
                             hbool_t display_stats,
                             hbool_t display_detailed_stats,
                             hbool_t do_inserts,
                             hbool_t dirty_inserts,
                             hbool_t do_renames,
                             hbool_t rename_to_main_addr,
                             hbool_t do_destroys,
		             hbool_t do_mult_ro_protects,
                             int dirty_destroys,
                             int dirty_unprotects,
			     uint64_t trans_num)
{
    const char * fcn_name = "jrnl_row_major_scan_forward2";
    H5C2_t * cache_ptr = file_ptr->shared->cache2;
    int32_t type;
    int32_t idx;
    int32_t local_max_index;
    int32_t lower_bound;
    int32_t upper_bound;

    if ( verbose )
        HDfprintf(stdout, "%s(): entering.\n", fcn_name);

    HDassert( lag >= 10 );

    type = 0;

    if ( ( pass2 ) && ( reset_stats ) ) {

        H5C2_stats__reset(cache_ptr);
    }

    while ( ( pass2 ) && ( type < NUMBER_OF_ENTRY_TYPES ) )
    {
        idx = -lag;

        local_max_index = MIN(max_index, max_indices2[type]);

	lower_bound = 0;
	upper_bound = lower_bound + 8;

        while ( ( pass2 ) && ( idx <= (local_max_index + lag) ) )
        {
	    if ( idx == ( lower_bound - lag ) ) {

	        trans_num++;

                begin_trans(cache_ptr, verbose, trans_num, 
			    "jrnl_row_major_scan_forward inner loop");

		if ( verbose )
		    HDfprintf(stdout, "begin trans %lld.\n", 
			      (long long)trans_num);

	        if ( verbose )
	            HDfprintf(stdout, "(%d, %d)\n", lower_bound, upper_bound);
	    }

	    while ( ( pass2 ) && ( idx <= upper_bound + lag ) ) 
            {
	    
	        if ( verbose ) {

                    HDfprintf(stdout, "%lld:%d:%d: ", trans_num, type, idx);
	        }

                if ( ( pass2 ) && ( do_inserts ) && 
		     ( (idx + lag) >= 0 ) &&
		     ( (idx + lag) >= lower_bound ) &&
                     ( (idx + lag) <= local_max_index ) &&
                     ( (idx + lag) <= upper_bound ) &&
                     ( ((idx + lag) % 2) == 0 ) &&
                     ( ! entry_in_cache2(cache_ptr, type, (idx + lag)) ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "1(i, %d, %d) ", type, (idx + lag));

		    /*** insert entry idx + lag (if not already present *** */
                    insert_entry2(file_ptr, type, (idx + lag), dirty_inserts,
                                  H5C2__NO_FLAGS_SET);
                }


                if ( ( pass2 ) && 
		     ( (idx + lag - 1) >= 0 ) &&
		     ( (idx + lag - 1) >= lower_bound ) &&
                     ( (idx + lag - 1) <= local_max_index ) &&
                     ( (idx + lag - 1) <= upper_bound ) &&
                     ( ( (idx + lag - 1) % 3 ) == 0 ) ) {

                    if ( verbose )
                        HDfprintf(stdout, 
				  "2(p, %d, %d) ", type, (idx + lag - 1));

		    /*** protect entry idx + lag - 1 ***/
                    protect_entry2(file_ptr, type, (idx + lag - 1));
                }

                if ( ( pass2 ) && 
		     ( (idx + lag - 2) >= 0 ) &&
		     ( (idx + lag - 2) >= lower_bound ) &&
                     ( (idx + lag - 2) <= local_max_index ) &&
                     ( (idx + lag - 2) <= upper_bound ) &&
                     ( ( (idx + lag - 2) % 3 ) == 0 ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "3(u, %d, %d) ", 
				  type, (idx + lag - 2));

		    /*** unprotect entry idx + lag - 2 ***/
                    unprotect_entry2(file_ptr, type, idx+lag-2, NO_CHANGE,
                                     H5C2__NO_FLAGS_SET);
                }


                if ( ( pass2 ) && ( do_renames ) && 
		     ( (idx + lag - 2) >= 0 ) &&
		     ( (idx + lag - 2) >= lower_bound ) &&
                     ( (idx + lag - 2) <= local_max_index ) &&
                     ( (idx + lag - 2) <= upper_bound ) &&
                     ( ( (idx + lag - 2) % 3 ) == 0 ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "4(r, %d, %d, %d) ", 
			          type, (idx + lag - 2), 
				  (int)rename_to_main_addr);

		    /*** rename entry idx + lag -2 ***/
                    rename_entry2(cache_ptr, type, (idx + lag - 2),
                                  rename_to_main_addr);
                }


                if ( ( pass2 ) && 
		     ( (idx + lag - 3) >= 0 ) &&
		     ( (idx + lag - 3) >= lower_bound ) &&
                     ( (idx + lag - 3) <= local_max_index ) &&
                     ( (idx + lag - 3) <= upper_bound ) &&
                     ( ( (idx + lag - 3) % 5 ) == 0 ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "5(p, %d, %d) ", 
				  type, (idx + lag - 3));

		    /*** protect entry idx + lag - 3 ***/
                    protect_entry2(file_ptr, type, (idx + lag - 3));
                }

                if ( ( pass2 ) && 
		     ( (idx + lag - 5) >= 0 ) &&
		     ( (idx + lag - 5) >= lower_bound ) &&
                     ( (idx + lag - 5) <= local_max_index ) &&
                     ( (idx + lag - 5) <= upper_bound ) &&
                     ( ( (idx + lag - 5) % 5 ) == 0 ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "6(u, %d, %d) ", 
				  type, (idx + lag - 5));


		    /*** unprotect entry idx + lag - 5 ***/
                    unprotect_entry2(file_ptr, type, idx+lag-5, NO_CHANGE,
                                     H5C2__NO_FLAGS_SET);
                }

	        if ( do_mult_ro_protects )
	        {
		    if ( ( pass2 ) && 
		         ( (idx + lag - 5) >= 0 ) &&
		         ( (idx + lag - 5) >= lower_bound ) &&
		         ( (idx + lag - 5) < local_max_index ) &&
		         ( (idx + lag - 5) < upper_bound ) &&
		         ( (idx + lag - 5) % 9 == 0 ) ) {

                        if ( verbose )
                            HDfprintf(stdout, "7(p-ro, %d, %d) ", type, 
				      (idx + lag - 5));

		        /*** protect ro entry idx + lag - 5 ***/
		        protect_entry_ro2(file_ptr, type, (idx + lag - 5));
		    }

		    if ( ( pass2 ) && 
		         ( (idx + lag - 6) >= 0 ) &&
		         ( (idx + lag - 6) >= lower_bound ) &&
		         ( (idx + lag - 6) < local_max_index ) &&
		         ( (idx + lag - 6) < upper_bound ) &&
		         ( (idx + lag - 6) % 11 == 0 ) ) {

                        if ( verbose )
                            HDfprintf(stdout, "8(p-ro, %d, %d) ", type, 
				      (idx + lag - 6));

		        /*** protect ro entry idx + lag - 6 ***/
		        protect_entry_ro2(file_ptr, type, (idx + lag - 6));
		    }

		    if ( ( pass2 ) && 
		         ( (idx + lag - 7) >= 0 ) &&
		         ( (idx + lag - 7) >= lower_bound ) &&
		         ( (idx + lag - 7) < local_max_index ) &&
		         ( (idx + lag - 7) < upper_bound ) &&
		         ( (idx + lag - 7) % 13 == 0 ) ) {

                        if ( verbose )
                            HDfprintf(stdout, "9(p-ro, %d, %d) ", type, 
				      (idx + lag - 7));

		        /*** protect ro entry idx + lag - 7 ***/
		        protect_entry_ro2(file_ptr, type, (idx + lag - 7));
		    }

		    if ( ( pass2 ) && 
		         ( (idx + lag - 7) >= 0 ) &&
		         ( (idx + lag - 7) >= lower_bound ) &&
		         ( (idx + lag - 7) < local_max_index ) &&
		         ( (idx + lag - 7) < upper_bound ) &&
		         ( (idx + lag - 7) % 9 == 0 ) ) {

                        if ( verbose )
                            HDfprintf(stdout, "10(u-ro, %d, %d) ", type, 
				      (idx + lag - 7));

		        /*** unprotect ro entry idx + lag - 7 ***/
		        unprotect_entry2(file_ptr, type, (idx + lag - 7),
				         FALSE, H5C2__NO_FLAGS_SET);
		    }

		    if ( ( pass2 ) && 
		         ( (idx + lag - 8) >= 0 ) &&
		         ( (idx + lag - 8) >= lower_bound ) &&
		         ( (idx + lag - 8) < local_max_index ) &&
		         ( (idx + lag - 8) < upper_bound ) &&
		         ( (idx + lag - 8) % 11 == 0 ) ) {

                        if ( verbose )
                            HDfprintf(stdout, "11(u-ro, %d, %d) ", type, 
				      (idx + lag - 8));

		        /*** unprotect ro entry idx + lag - 8 ***/
		        unprotect_entry2(file_ptr, type, (idx + lag - 8),
				         FALSE, H5C2__NO_FLAGS_SET);
		    }

		    if ( ( pass2 ) && 
		         ( (idx + lag - 9) >= 0 ) &&
		         ( (idx + lag - 9) >= lower_bound ) &&
		         ( (idx + lag - 9) < local_max_index ) &&
		         ( (idx + lag - 9) < upper_bound ) &&
		         ( (idx + lag - 9) % 13 == 0 ) ) {

                        if ( verbose )
                            HDfprintf(stdout, "12(u-ro, %d, %d) ", type, 
				      (idx + lag - 9));

		        /*** unprotect ro entry idx + lag - 9 ***/
		        unprotect_entry2(file_ptr, type, (idx + lag - 9),
				         FALSE, H5C2__NO_FLAGS_SET);
		    }
	        } /* if ( do_mult_ro_protects ) */

                if ( ( pass2 ) && 
		     ( idx >= 0 ) && 
		     ( idx >= lower_bound ) && 
		     ( idx <= local_max_index ) &&
		     ( idx <= upper_bound ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "13(p, %d, %d) ", type, idx);

		    /*** protect entry idx ***/
                    protect_entry2(file_ptr, type, idx);
                }

                if ( ( pass2 ) && 
		     ( (idx - lag + 2) >= 0 ) &&
		     ( (idx - lag + 2) >= lower_bound ) &&
                     ( (idx - lag + 2) <= local_max_index ) &&
                     ( (idx - lag + 2) <= upper_bound ) &&
                     ( ( (idx - lag + 2) % 7 ) == 0 ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "14(u, %d, %d) ", 
				  type, (idx - lag + 2));

		    /*** unprotect entry idx - lag + 2 ***/
                    unprotect_entry2(file_ptr, type, idx-lag+2, NO_CHANGE,
                                     H5C2__NO_FLAGS_SET);
                }

                if ( ( pass2 ) && 
		     ( (idx - lag + 1) >= 0 ) &&
		     ( (idx - lag + 1) >= lower_bound ) &&
                     ( (idx - lag + 1) <= local_max_index ) &&
                     ( (idx - lag + 1) <= upper_bound ) &&
                     ( ( (idx - lag + 1) % 7 ) == 0 ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "15(p, %d, %d) ", 
				  type, (idx - lag + 1));

		    /*** protect entry idx - lag + 1 ***/
                    protect_entry2(file_ptr, type, (idx - lag + 1));
                }


                if ( do_destroys ) {

                    if ( ( pass2 ) && 
		         ( (idx - lag) >= 0 ) &&
		         ( (idx - lag) >= lower_bound ) &&
                         ( ( idx - lag) <= local_max_index ) &&
                         ( ( idx - lag) <= upper_bound ) ) {

                        switch ( (idx - lag) %4 ) {

                            case 0: /* we just did an insert */

                                if ( verbose )
                                    HDfprintf(stdout, "16(u, %d, %d) ", 
					      type, (idx - lag));

			        /*** unprotect entry NC idx - lag ***/
                                unprotect_entry2(file_ptr, type, idx - lag,
                                                 NO_CHANGE, H5C2__NO_FLAGS_SET);
                                break;

                            case 1:
                                if ( (entries2[type])[idx-lag].is_dirty ) {

                                    if ( verbose )
                                        HDfprintf(stdout, "17(u, %d, %d) ", 
						  type, (idx - lag));

			            /*** unprotect entry NC idx - lag ***/
                                    unprotect_entry2(file_ptr, type, idx - lag,
                                                     NO_CHANGE, 
						     H5C2__NO_FLAGS_SET);
                                } else {

                                    if ( verbose )
                                        HDfprintf(stdout, "18(u, %d, %d) ", 
						  type, (idx - lag));

			            /*** unprotect entry idx - lag ***/
                                    unprotect_entry2(file_ptr, type, idx - lag,
                                                     dirty_unprotects,
                                                     H5C2__NO_FLAGS_SET);
                                }
                                break;

                            case 2: /* we just did an insrt */

                                if ( verbose )
                                    HDfprintf(stdout, "19(u-del, %d, %d) ", 
					      type, (idx - lag));

			        /*** unprotect delete idx - lag ***/
                                unprotect_entry2(file_ptr, type, idx - lag,
                                                 NO_CHANGE, H5C2__DELETED_FLAG);
                                break;

                            case 3:
                                if ( (entries2[type])[idx-lag].is_dirty ) {

                                    if ( verbose )
                                        HDfprintf(stdout, "20(u-del, %d, %d) ", 
					          type, (idx - lag));

				    /*** unprotect delete idx - lag ***/
                                    unprotect_entry2(file_ptr, type, idx - lag,
                                                     NO_CHANGE, 
						     H5C2__DELETED_FLAG);
                                } else {

                                    if ( verbose )
                                        HDfprintf(stdout, "21(u-del, %d, %d) ", 
					          type, (idx - lag));

				    /*** unprotect delete idx - lag ***/
                                    unprotect_entry2(file_ptr, type, idx - lag,
                                                     dirty_destroys,
                                                     H5C2__DELETED_FLAG);
                                }
                                break;

                            default:
                                HDassert(0); /* this can't happen... */
                                break;
                        }
                    }

                } else {

                    if ( ( pass2 ) && 
		         ( (idx - lag) >= 0 ) &&
		         ( (idx - lag) >= lower_bound ) &&
                         ( ( idx - lag) <= local_max_index ) &&
                         ( ( idx - lag) <= upper_bound ) ) {

                        if ( verbose )
                            HDfprintf(stdout, "22(u, %d, %d) ", 
				      type, (idx - lag));

		        /*** unprotect idx - lag ***/
                        unprotect_entry2(file_ptr, type, idx - lag,
                                         dirty_unprotects, H5C2__NO_FLAGS_SET);
                    }
                }

		idx++;

		if ( verbose )
		    HDfprintf(stdout, "\n");

	    } /* while ( ( pass2 ) && ( idx <= upper_bound ) ) */

	    end_trans(file_ptr, cache_ptr, verbose, trans_num, 
                      "jrnl_row_major_scan_forward inner loop");

	    if ( verbose )
	        HDfprintf(stdout, "end trans %lld.\n", (long long)trans_num);

            lower_bound = upper_bound + (2 * lag) + 2;
	    upper_bound = lower_bound + 8;

	    idx = lower_bound - lag;

        } /* while ( ( pass2 ) && ( idx <= (local_max_index + lag) ) ) */

        type++;

    } /* while ( ( pass2 ) && ( type < NUMBER_OF_ENTRY_TYPES ) ) */

    if ( ( pass2 ) && ( display_stats ) ) {

        H5C2_stats(cache_ptr, "test cache", display_detailed_stats);
    }

    return;

} /* jrnl_row_major_scan_forward2() */


/*-------------------------------------------------------------------------
 * Function:    open_existing_file_for_journaling()
 *
 * Purpose:     If pass2 is true on entry, open the specified a HDF5 file 
 * 		with journaling enabled and journal file with the specified 
 * 		name.  Return pointers to the cache data structure and file 
 * 		data structures, and verify that it contains the expected data.
 *
 *              On failure, set pass2 to FALSE, and set failure_mssg2 
 *              to point to an appropriate failure message.
 *
 *              Do nothing if pass2 is FALSE on entry.
 *
 * Return:      void
 *
 * Programmer:  John Mainzer
 *              5/13/08
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
open_existing_file_for_journaling(const char * hdf_file_name,
                                  const char * journal_file_name,
                                  hid_t * file_id_ptr,
                                  H5F_t ** file_ptr_ptr,
                                  H5C2_t ** cache_ptr_ptr) 
{
    const char * fcn_name = "open_existing_file_for_journaling()";
    hbool_t show_progress = FALSE;
    hbool_t verbose = FALSE;
    int cp = 0;
    herr_t result;
    H5AC2_jnl_config_t jnl_config;
    hid_t fapl_id = -1;
    hid_t file_id = -1;
    H5F_t * file_ptr = NULL;
    H5C2_t * cache_ptr = NULL;

    if ( pass2 )
    {
        if ( ( hdf_file_name == NULL ) ||
             ( journal_file_name == NULL ) ||
	     ( file_id_ptr == NULL ) ||
	     ( file_ptr_ptr == NULL ) ||
	     ( cache_ptr_ptr == NULL ) ) {

            failure_mssg2 = 
               "Bad param(s) on entry to open_existing_file_for_journaling().\n";
	    pass2 = FALSE;
        }
	else if ( strlen(journal_file_name) > H5AC2__MAX_JOURNAL_FILE_NAME_LEN ) {

            failure_mssg2 = "journal file name too long.\n";
	    pass2 = FALSE;

        } else  if ( verbose ) {

            HDfprintf(stdout, "%s: HDF file name = \"%s\".\n", 
		      fcn_name, hdf_file_name);
            HDfprintf(stdout, "%s: journal file name = \"%s\".\n", 
		      fcn_name, journal_file_name);
	}
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    /* create a file access propertly list. */
    if ( pass2 ) {

        fapl_id = H5Pcreate(H5P_FILE_ACCESS);

        if ( fapl_id < 0 ) {

            pass2 = FALSE;
            failure_mssg2 = "H5Pcreate() failed.\n";
        }
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    /* call H5Pset_libver_bounds() on the fapl_id */
    if ( pass2 ) {

	if ( H5Pset_libver_bounds(fapl_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) 
		< 0 ) {

            pass2 = FALSE;
            failure_mssg2 = "H5Pset_libver_bounds() failed.\n";
        }
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    if ( pass2 ) {

        jnl_config.version = H5AC2__CURR_JNL_CONFIG_VER;

        result = H5Pget_jnl_config(fapl_id, &jnl_config);

        if ( result < 0 ) {

            pass2 = FALSE;
            failure_mssg2 = "H5Pget_jnl_config() failed.\n";
        }

	/* set journaling config fields to taste */
        jnl_config.enable_journaling       = TRUE;

        strcpy(jnl_config.journal_file_path, journal_file_name);

        jnl_config.journal_recovered       = FALSE;
        jnl_config.jbrb_buf_size           = (8 * 1024);
        jnl_config.jbrb_num_bufs           = 2;
        jnl_config.jbrb_use_aio            = FALSE;
        jnl_config.jbrb_human_readable     = TRUE;
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    if ( pass2 ) {

        result = H5Pset_jnl_config(fapl_id, &jnl_config);

        if ( result < 0 ) {

            pass2 = FALSE;
            failure_mssg2 = "H5Pset_jnl_config() failed.\n";
        }
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);
 
    /**************************************/
    /* open the file with the fapl above. */
    /**************************************/

    /* open the file using fapl_id */
    if ( pass2 ) {

        file_id = H5Fopen(hdf_file_name, H5F_ACC_RDWR, fapl_id);

        if ( file_id < 0 ) {

            pass2 = FALSE;
            failure_mssg2 = "H5Fopen() failed (1).\n";

        } else {

            file_ptr = H5I_object_verify(file_id, H5I_FILE);

            if ( file_ptr == NULL ) {

                pass2 = FALSE;
                failure_mssg2 = "Can't get file_ptr.";

                if ( verbose ) {
                    HDfprintf(stdout, "%s: Can't get file_ptr.\n", fcn_name);
                }
            }
        }
    }

    /* At least within the context of the cache2 test code, there should be
     * no need to allocate space for test entries since we are re-opening
     * the file, and any needed space allocation should have been done at 
     * file creation.
     */


    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    /* get a pointer to the files internal data structure and then 
     * to the cache structure
     */
    if ( pass2 ) {

        if ( file_ptr->shared->cache2 == NULL ) {
	
	    pass2 = FALSE;
	    failure_mssg2 = "can't get cache2 pointer(1).\n";

	} else {

	    cache_ptr = file_ptr->shared->cache2;
	}
    }


    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    if ( pass2 ) {

        *file_id_ptr = file_id;
	*file_ptr_ptr = file_ptr;
	*cache_ptr_ptr = cache_ptr;
    }

    if ( show_progress ) 
        HDfprintf(stdout, "%s: cp = %d -- exiting.\n", fcn_name, cp++);

    return;

} /* open_existing_file_for_journaling() */


/*-------------------------------------------------------------------------
 * Function:    open_existing_file_without_journaling()
 *
 * Purpose:     If pass2 is true on entry, open the specified a HDF5 file 
 * 		with journaling disabled.  Return pointers to the cache 
 * 		data structure and file data structures, and verify that 
 * 		it contains the expected data.
 *
 *              On failure, set pass2 to FALSE, and set failure_mssg2 
 *              to point to an appropriate failure message.
 *
 *              Do nothing if pass2 is FALSE on entry.
 *
 * Return:      void
 *
 * Programmer:  John Mainzer
 *              7/10/08
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
open_existing_file_without_journaling(const char * hdf_file_name,
                                      hid_t * file_id_ptr,
                                      H5F_t ** file_ptr_ptr,
                                      H5C2_t ** cache_ptr_ptr)
{
    const char * fcn_name = "open_existing_file_without_journaling()";
    hbool_t show_progress = FALSE;
    hbool_t verbose = FALSE;
    int cp = 0;
    hid_t fapl_id = -1;
    hid_t file_id = -1;
    H5F_t * file_ptr = NULL;
    H5C2_t * cache_ptr = NULL;

    if ( pass2 )
    {
        if ( ( hdf_file_name == NULL ) ||
	     ( file_id_ptr == NULL ) ||
	     ( file_ptr_ptr == NULL ) ||
	     ( cache_ptr_ptr == NULL ) ) {

            failure_mssg2 = 
           "Bad param(s) on entry to open_existing_file_without_journaling().\n";
	    pass2 = FALSE;

        } else {

            if ( verbose ) {

                HDfprintf(stdout, "%s: HDF file name = \"%s\".\n", 
			  fcn_name, hdf_file_name);
	    }
	}
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    /* create a file access propertly list. */
    if ( pass2 ) {

        fapl_id = H5Pcreate(H5P_FILE_ACCESS);

        if ( fapl_id < 0 ) {

            pass2 = FALSE;
            failure_mssg2 = "H5Pcreate() failed.\n";
        }
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    /* call H5Pset_libver_bounds() on the fapl_id */
    if ( pass2 ) {

	if ( H5Pset_libver_bounds(fapl_id, H5F_LIBVER_LATEST, 
				  H5F_LIBVER_LATEST) < 0 ) {

            pass2 = FALSE;
            failure_mssg2 = "H5Pset_libver_bounds() failed.\n";
        }
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);


    /**************************************/
    /* open the file with the fapl above. */
    /**************************************/

    /* open the file using fapl_id */
    if ( pass2 ) {

        file_id = H5Fopen(hdf_file_name, H5F_ACC_RDWR, fapl_id);

        if ( file_id < 0 ) {

            pass2 = FALSE;
            failure_mssg2 = "H5Fopen() failed (2).\n";

        } else {

            file_ptr = H5I_object_verify(file_id, H5I_FILE);

            if ( file_ptr == NULL ) {

                pass2 = FALSE;
                failure_mssg2 = "Can't get file_ptr.";

                if ( verbose ) {
                    HDfprintf(stdout, "%s: Can't get file_ptr.\n", fcn_name);
                }
            }
        }
    }

    /* At least within the context of the cache2 test code, there should be
     * no need to allocate space for test entries since we are re-opening
     * the file, and any needed space allocation should have been done at 
     * file creation.
     */


    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    /* get a pointer to the files internal data structure and then 
     * to the cache structure
     */
    if ( pass2 ) {

        if ( file_ptr->shared->cache2 == NULL ) {
	
	    pass2 = FALSE;
	    failure_mssg2 = "can't get cache2 pointer(1).\n";

	} else {

	    cache_ptr = file_ptr->shared->cache2;
	}
    }


    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    if ( pass2 ) {

        *file_id_ptr = file_id;
	*file_ptr_ptr = file_ptr;
	*cache_ptr_ptr = cache_ptr;
    }

    if ( show_progress ) 
        HDfprintf(stdout, "%s: cp = %d -- exiting.\n", fcn_name, cp++);

    return;

} /* open_existing_file_without_journaling() */


/*-------------------------------------------------------------------------
 * Function:    setup_cache_for_journaling()
 *
 * Purpose:     If pass2 is true on entry, create a HDF5 file with 
 * 		journaling enabled and journal file with the specified name.  
 * 		Return pointers to the cache data structure and file data 
 * 		structures.  and verify that it contains the expected data.
 *
 *              On failure, set pass2 to FALSE, and set failure_mssg2 
 *              to point to an appropriate failure message.
 *
 *              Do nothing if pass2 is FALSE on entry.
 *
 * Return:      void
 *
 * Programmer:  John Mainzer
 *              5/13/08
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
setup_cache_for_journaling(const char * hdf_file_name,
                           const char * journal_file_name,
                           hid_t * file_id_ptr,
                           H5F_t ** file_ptr_ptr,
                           H5C2_t ** cache_ptr_ptr,
#if USE_CORE_DRIVER
			   hbool_t use_core_driver_if_avail)
#else /* USE_CORE_DRIVER */
			   hbool_t UNUSED use_core_driver_if_avail)
#endif /* USE_CORE_DRIVER */
{
    const char * fcn_name = "setup_cache_for_journaling()";
    hbool_t show_progress = FALSE;
    hbool_t verbose = FALSE;
    int cp = 0;
    herr_t result;
    H5AC2_cache_config_t mdj_config =
    {
      /* int         version                 = */ H5C2__CURR_AUTO_SIZE_CTL_VER,
      /* hbool_t     rpt_fcn_enabled         = */ FALSE,
      /* hbool_t     open_trace_file         = */ FALSE,
      /* hbool_t     close_trace_file        = */ FALSE,
      /* char        trace_file_name[]       = */ "",
      /* hbool_t     evictions_enabled       = */ TRUE,
      /* hbool_t     set_initial_size        = */ TRUE,
      /* size_t      initial_size            = */ ( 64 * 1024 ),
      /* double      min_clean_fraction      = */ 0.5,
      /* size_t      max_size                = */ (16 * 1024 * 1024 ),
      /* size_t      min_size                = */ ( 8 * 1024 ),
      /* long int    epoch_length            = */ 50000,
      /* enum H5C2_cache_incr_mode incr_mode = */ H5C2_incr__off,
      /* double      lower_hr_threshold      = */ 0.9,
      /* double      increment               = */ 2.0,
      /* hbool_t     apply_max_increment     = */ TRUE,
      /* size_t      max_increment           = */ (4 * 1024 * 1024),
      /* enum H5C2_cache_flash_incr_mode       */
      /*                    flash_incr_mode  = */ H5C2_flash_incr__off,
      /* double      flash_multiple          = */ 1.0,
      /* double      flash_threshold         = */ 0.25,
      /* enum H5C2_cache_decr_mode decr_mode = */ H5C2_decr__off,
      /* double      upper_hr_threshold      = */ 0.999,
      /* double      decrement               = */ 0.9,
      /* hbool_t     apply_max_decrement     = */ TRUE,
      /* size_t      max_decrement           = */ (1 * 1024 * 1024),
      /* int         epochs_before_eviction  = */ 3,
      /* hbool_t     apply_empty_reserve     = */ TRUE,
      /* double      empty_reserve           = */ 0.1,
      /* int         dirty_bytes_threshold   = */ (8 * 1024)
    };
    H5AC2_jnl_config_t jnl_config =
    {
      /* int         version                 = */ H5AC2__CURR_JNL_CONFIG_VER,
      /* hbool_t     enable_journaling       = */ TRUE,
      /* char        journal_file_path[]     = */ "",
      /* hbool_t     journal_recovered       = */ FALSE,
      /* size_t      jbrb_buf_size           = */ (8 * 1024),
      /* int         jbrb_num_bufs           = */ 2,
      /* hbool_t     jbrb_use_aio            = */ FALSE,
      /* hbool_t     jbrb_human_readable     = */ TRUE
    };
    hid_t fapl_id = -1;
    hid_t file_id = -1;
    haddr_t actual_base_addr;
    H5F_t * file_ptr = NULL;
    H5C2_t * cache_ptr = NULL;

    if ( pass2 )
    {
        if ( ( hdf_file_name == NULL ) ||
             ( journal_file_name == NULL ) ||
	     ( file_id_ptr == NULL ) ||
	     ( file_ptr_ptr == NULL ) ||
	     ( cache_ptr_ptr == NULL ) ) {

            failure_mssg2 = 
                "Bad param(s) on entry to setup_cache_for_journaling().\n";
	    pass2 = FALSE;
        }
	else if ( strlen(journal_file_name) > H5AC2__MAX_JOURNAL_FILE_NAME_LEN )
	{
            failure_mssg2 = "journal file name too long.\n";
	    pass2 = FALSE;

        } else {

	    strcpy(jnl_config.journal_file_path, journal_file_name);

            if ( verbose ) {

                HDfprintf(stdout, "%s: HDF file name = \"%s\".\n", 
			  fcn_name, hdf_file_name);
                HDfprintf(stdout, "%s: journal file name = \"%s\".\n", 
			  fcn_name, journal_file_name);
	    }
	}
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    /* create a file access propertly list. */
    if ( pass2 ) {

        fapl_id = H5Pcreate(H5P_FILE_ACCESS);

        if ( fapl_id < 0 ) {

            pass2 = FALSE;
            failure_mssg2 = "H5Pcreate() failed.\n";
        }
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    /* call H5Pset_libver_bounds() on the fapl_id */
    if ( pass2 ) {

	if ( H5Pset_libver_bounds(fapl_id, H5F_LIBVER_LATEST, 
				  H5F_LIBVER_LATEST) < 0 ) {

            pass2 = FALSE;
            failure_mssg2 = "H5Pset_libver_bounds() failed.\n";
        }
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    if ( pass2 ) {

        result = H5Pset_mdc_config(fapl_id, (H5AC_cache_config_t *)&mdj_config);

        if ( result < 0 ) {

            pass2 = FALSE;
            failure_mssg2 = "H5Pset_mdc_config() failed.\n";
        }
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    if ( pass2 ) {

        result = H5Pset_jnl_config(fapl_id, &jnl_config);

        if ( result < 0 ) {

            pass2 = FALSE;
            failure_mssg2 = "H5Pset_mdc_config() failed.\n";
        }
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

#if USE_CORE_DRIVER
    if ( ( pass2 ) && ( use_core_driver_if_avail ) ) {

        if ( H5Pset_fapl_core(fapl_id, 64 * 1024 * 1024, FALSE) < 0 ) {

            pass2 = FALSE;
            failure_mssg2 = "H5P_set_fapl_core() failed.\n";
        }
    }
#endif /* USE_CORE_DRIVER */

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

 
    /**************************************/
    /* Create a file with the fapl above. */
    /**************************************/

    /* create the file using fapl_id */
    if ( pass2 ) {

        file_id = H5Fcreate(hdf_file_name, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);

        if ( file_id < 0 ) {

            pass2 = FALSE;
            failure_mssg2 = "H5Fcreate() failed.\n";

        } else {

            file_ptr = H5I_object_verify(file_id, H5I_FILE);

            if ( file_ptr == NULL ) {

                pass2 = FALSE;
                failure_mssg2 = "Can't get file_ptr.";

                if ( verbose ) {
                    HDfprintf(stdout, "%s: Can't get file_ptr.\n", fcn_name);
                }
            }
        }
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    if ( pass2 ) { /* allocate space for test entries */

        actual_base_addr = H5MF_alloc(file_ptr, H5FD_MEM_DEFAULT, H5P_DEFAULT,
                                      (hsize_t)(ADDR_SPACE_SIZE + BASE_ADDR));

        if ( actual_base_addr == HADDR_UNDEF ) {

            pass2 = FALSE;
            failure_mssg2 = "H5MF_alloc() failed.";

            if ( verbose ) {
                HDfprintf(stdout, "%s: H5MF_alloc() failed.\n", fcn_name);
            }

        } else if ( actual_base_addr > BASE_ADDR ) {

            /* If this happens, must increase BASE_ADDR so that the
             * actual_base_addr is <= BASE_ADDR.  This should only happen
             * if the size of the superblock is increase.
             */
            pass2 = FALSE;
            failure_mssg2 = "actual_base_addr > BASE_ADDR";

            if ( verbose ) {
                HDfprintf(stdout, "%s: actual_base_addr > BASE_ADDR.\n", 
			  fcn_name);
            }
        }
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    /* get a pointer to the files internal data structure and then 
     * to the cache structure
     */
    if ( pass2 ) {

        if ( file_ptr->shared->cache2 == NULL ) {
	
	    pass2 = FALSE;
	    failure_mssg2 = "can't get cache2 pointer(1).\n";

	} else {

	    cache_ptr = file_ptr->shared->cache2;
	}
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    reset_entries2();

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    /* close the fapl */
    if ( pass2 ) {

        if ( H5Pclose(fapl_id) < 0 ) {

	    pass2 = FALSE;
	    failure_mssg2 = "error closing fapl.\n";
	}
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    if ( pass2 ) {

        *file_id_ptr = file_id;
	*file_ptr_ptr = file_ptr;
	*cache_ptr_ptr = cache_ptr;
    }

    if ( show_progress ) 
        HDfprintf(stdout, "%s: cp = %d -- exiting.\n", fcn_name, cp++);

    return;

} /* setup_cache_for_journaling() */


/*-------------------------------------------------------------------------
 * Function:    takedown_cache_after_journaling()
 *
 * Purpose:     If file_id >= 0, close the associated file, and then delete
 * 		it.  Verify that they journal file has been deleted.
 *
 * Return:      void
 *
 * Programmer:  John Mainzer
 *              5/13/08
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
takedown_cache_after_journaling(hid_t file_id,
                                const char * filename,
                                const char * journal_filename,
				hbool_t 
				use_core_driver_if_avail)
{
    hbool_t verbose = FALSE;
    int error;
    
    if ( file_id >= 0 ) {

	if ( H5Fclose(file_id) < 0 ) {

	    if ( pass2 ) {

                pass2 = FALSE;
	        failure_mssg2 = "file close failed.";
	    }
	} else if ( ( ( ! USE_CORE_DRIVER ) || ( ! use_core_driver_if_avail ) ) &&
                    ( ( error = HDremove(filename) ) != 0 ) ) {

	    if ( verbose ) {
	        HDfprintf(stdout, 
		  "HDremove(\"%s\") failed, returned %d, errno = %d = %s.\n", 
		  filename, error, errno, strerror(errno));
	    }

	    if ( pass2 ) {

                pass2 = FALSE;
                failure_mssg2 = "HDremove() failed (1).\n";
            }
        }
    }

    verify_journal_deleted(journal_filename);

    return;

} /* takedown_cache_after_journaling() */


/*-------------------------------------------------------------------------
 * Function:    verify_journal_contents()
 *
 * Purpose:     If pass2 is true on entry, verify that the contents of the 
 * 		journal file matches that of the expected file.  If 
 * 		differences are detected, or if any other error is detected,
 * 		set pass2 to FALSE and set failure_mssg2 to point to an 
 * 		appropriate error message.
 *
 *              Do nothing if pass2 is FALSE on entry.
 *
 * Return:      void
 *
 * Programmer:  John Mainzer
 *              5/06/08
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
verify_journal_contents(const char * journal_file_path_ptr,
                        const char * expected_file_path_ptr)
{
    const char * fcn_name = "verify_journal_contents()";
    char ch;
    char journal_buf[(8 * 1024) + 1];
    char expected_buf[(8 * 1024) + 1];
    hbool_t verbose = FALSE;
    size_t cur_buf_len;
    const size_t max_buf_len = (8 * 1024);
    size_t journal_len = 0;
    size_t expected_len = 0;
    size_t first_line_len;
    size_t journal_remainder_len = 0;
    size_t expected_remainder_len = 0;
    ssize_t read_result;
    int journal_file_fd = -1;
    int expected_file_fd = -1;
    h5_stat_t buf;

    if ( pass2 ) {

	if ( journal_file_path_ptr == NULL ) {

            failure_mssg2 = "journal_file_path_ptr NULL on entry?!?",
            pass2 = FALSE;

	} else if ( expected_file_path_ptr == NULL ) {

            failure_mssg2 = "expected_file_path_ptr NULL on entry?!?",
            pass2 = FALSE;

        }
    }

    /* get the actual length of the journal file */
    if ( pass2 ) {

	if ( HDstat(journal_file_path_ptr, &buf) != 0 ) {

	    if ( verbose ) {

	        HDfprintf(stdout, "%s: HDstat(j) failed with errno = %d.\n",
                          fcn_name, errno);
	    }
	    failure_mssg2 = "stat() failed on journal file.";
	    pass2 = FALSE;

	} else {

	    if ( (buf.st_size) == 0 ) {

                failure_mssg2 = "Journal file empty?!?";
	        pass2 = FALSE;

	    } else {
                
	        journal_len = (size_t)(buf.st_size);

		if ( verbose ) {

		    HDfprintf(stdout, "%s: journal_len = %d.\n", 
		              fcn_name, (int)journal_len);
		}
            }
	} 
    }

    /* get the actual length of the expected file */
    if ( pass2 ) {

	if ( HDstat(expected_file_path_ptr, &buf) != 0 ) {

	    if ( verbose ) {

	        HDfprintf(stdout, "%s: HDstat(e) failed with errno = %d.\n",
                          fcn_name, errno);
	    }
	    failure_mssg2 = "stat() failed on expected file.";
	    pass2 = FALSE;

	} else {

	    if ( (buf.st_size) == 0 ) {

                failure_mssg2 = "Expected file empty?!?";
	        pass2 = FALSE;

	    } else {
                
	        expected_len = (size_t)(buf.st_size);

		if ( verbose ) {

		    HDfprintf(stdout, "%s: expected_len = %d.\n", 
		              fcn_name, (int)expected_len);
		}
            }
	} 
    }

    /* open the journal file */
    if ( pass2 ) {

	if ( (journal_file_fd = HDopen(journal_file_path_ptr, O_RDONLY, 0777)) 
	     == -1 ) {

	    if ( verbose ) {

	        HDfprintf(stdout, "%s: HDopen(j) failed with errno = %d.\n",
                          fcn_name, errno);
	    }
            failure_mssg2 = "Can't open journal file.";
	    pass2 = FALSE;
        }
    }

    /* open the expected file */
    if ( pass2 ) {

	if ( (expected_file_fd = HDopen(expected_file_path_ptr, O_RDONLY, 0777)) 
	     == -1 ) {

	    if ( verbose ) {

	        HDfprintf(stdout, "%s: HDopen(e) failed with errno = %d.\n",
                          fcn_name, errno);
	    }
            failure_mssg2 = "Can't open expected file.";
	    pass2 = FALSE;
        }
    }

    /* The first lines of the journal and expected files will usually differ
     * in creation date.  We could look at everything else on the line, but
     * for now we will just skip past it, and compute the length of the remainder
     * of the journal and expected files as we do so.
     *
     * Do this by reading the file one character at a time until we hit a newline.
     * This is very inefficient, but this is test code, and the first line
     * can't be very long.
     */
    if ( pass2 ) {

        first_line_len = 1;
	read_result = HDread(journal_file_fd, &ch, 1);

	while ( ( ch != '\n' ) && 
		( first_line_len < 128 ) &&
	        ( read_result == 1 ) ) {

	    first_line_len++;
	    read_result = HDread(journal_file_fd, &ch, 1);
	}

	if ( ch != '\n' ) {
	
	    failure_mssg2 = "error skipping first line of journal file.";
	    pass2 = FALSE;

	} else if ( first_line_len > journal_len ) {

            failure_mssg2 = "first_line_len > journal_len?!?";
	    pass2 = FALSE;

	} else {

	    journal_remainder_len = journal_len - first_line_len;
	}
    }

    if ( pass2 ) {

        first_line_len = 1;
	read_result = HDread(expected_file_fd, &ch, 1);

	while ( ( ch != '\n' ) && 
		( first_line_len < 128 ) &&
	        ( read_result == 1 ) ) {

	    first_line_len++;
	    read_result = HDread(expected_file_fd, &ch, 1);
	}

	if ( ch != '\n' ) {
	
	    failure_mssg2 = "error skipping first line of expected file.";
	    pass2 = FALSE;

	} else if ( first_line_len > expected_len ) {

            failure_mssg2 = "first_line_len > expected_len?!?";
	    pass2 = FALSE;

	} else {

	    expected_remainder_len = expected_len - first_line_len;
	}
    }

    if ( pass2 ) {

        if ( journal_remainder_len != expected_remainder_len ) {

	    failure_mssg2 = "Unexpected journal file contents(1).";
	    pass2 = FALSE;
	}
    }

    /* If we get this far without an error, the lengths of the actual 
     * and expected files (after skipping the first line) are identical.
     * Thus we have to go and compare the actual data.
     */
    while ( ( pass2 ) &&
	    ( journal_remainder_len > 0 ) ) 
    {
        HDassert( journal_remainder_len == expected_remainder_len );

        if ( journal_remainder_len > max_buf_len ) {

            cur_buf_len = max_buf_len;
            journal_remainder_len -= max_buf_len;
            expected_remainder_len -= max_buf_len;

        } else {

            cur_buf_len = journal_remainder_len;
            journal_remainder_len = 0;
            expected_remainder_len = 0;
        }

        read_result = HDread(journal_file_fd, journal_buf, cur_buf_len);

        if ( read_result != (int)cur_buf_len ) {

            if ( verbose ) {

                HDfprintf(stdout, 
                          "%s: HDread(j) failed. result = %d, errno = %d.\n",
                          fcn_name, (int)read_result, errno);
            }
            failure_mssg2 = "error reading journal file.";
            pass2 = FALSE;
        }

        journal_buf[cur_buf_len] = '\0';

        if ( pass2 ) {

            read_result = HDread(expected_file_fd, expected_buf, cur_buf_len);

            if ( read_result != (int)cur_buf_len ) {

	        if ( verbose ) {

                    HDfprintf(stdout, 
                              "%s: HDread(e) failed. result = %d, errno = %d.\n",
                              fcn_name, (int)read_result, errno);
                }
                failure_mssg2 = "error reading expected file.";
                pass2 = FALSE;
            }

            expected_buf[cur_buf_len] = '\0';
        }

        if ( pass2 ) {

            if ( HDstrcmp(journal_buf, expected_buf) != 0 ) {

		if ( verbose ) {

                    HDfprintf(stdout, "expected_buf = \"%s\"\n", expected_buf);
                    HDfprintf(stdout, "journal_buf  = \"%s\"\n", journal_buf);
                }

                failure_mssg2 = "Unexpected journal file contents(2).";
                pass2 = FALSE;
            }
	}
    }

    if ( journal_file_fd != -1 ) {

        if ( HDclose(journal_file_fd) != 0 ) {

	    if ( verbose ) {

	        HDfprintf(stdout, "%s: HDclose(j) failed with errno = %d.\n",
                          fcn_name, errno);
	    }

	    if ( pass2 ) {

                failure_mssg2 = "Can't close journal file.";
	        pass2 = FALSE;
	    }
	}
    }

    if ( expected_file_fd != -1 ) {

        if ( HDclose(expected_file_fd) != 0 ) {

	    if ( verbose ) {

	        HDfprintf(stdout, "%s: HDclose(e) failed with errno = %d.\n",
                          fcn_name, errno);
	    }

	    if ( pass2 ) {

                failure_mssg2 = "Can't close expected file.";
	        pass2 = FALSE;
	    }
	}
    }

    return;

} /* verify_journal_contents() */


/*-------------------------------------------------------------------------
 * Function:    verify_journal_deleted()
 *
 * Purpose:     If pass2 is true on entry, stat the target journal file,
 * 		and verify that it does not exist.  If it does, set
 * 		pass2 to FALSE, and set failure_mssg2 to point to an 
 * 		appropriate failure message.  Similarly, if any errors 
 * 		are detected in this process, set pass2 to FALSE and set
 * 		failure_mssg2 to point to an appropriate error message.
 *
 *              Do nothing if pass2 is FALSE on entry.
 *
 * Return:      void
 *
 * Programmer:  John Mainzer
 *              5//08
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
verify_journal_deleted(const char * journal_file_path_ptr)
{
    const char * fcn_name = "verify_journal_deleted()";
    hbool_t verbose = FALSE;
    h5_stat_t buf;

    if ( pass2 ) {

	if ( journal_file_path_ptr == NULL ) {

            failure_mssg2 = "journal_file_path_ptr NULL on entry?!?",
            pass2 = FALSE;
	}
    }

    if ( pass2 ) {

	if ( HDstat(journal_file_path_ptr, &buf) == 0 ) {

	    if ( verbose ) {

	        HDfprintf(stdout, "%s: HDstat(%s) succeeded.\n", fcn_name,
			  journal_file_path_ptr);
	    }

	    failure_mssg2 = "journal file not deleted(1).";
	    pass2 = FALSE;

        } else if ( errno != ENOENT ) {

	    if ( verbose ) {

	        HDfprintf(stdout, 
			  "%s: HDstat() failed with unexpected errno = %d.\n",
                          fcn_name, errno);
	    }
	    failure_mssg2 = "journal file not deleted(2).";
	    pass2 = FALSE;

	} 
    }

    return;

} /* verify_journal_deleted() */


/*-------------------------------------------------------------------------
 * Function:    verify_journal_empty()
 *
 * Purpose:     If pass2 is true on entry, stat the target journal file,
 * 		and verify that it has length zero.  If it is not, set
 * 		pass2 to FALSE, and set failure_mssg2 to point to an 
 * 		appropriate failure message.  Similarly, if any errors 
 * 		are detected in this process, set pass2 to FALSE and set
 * 		failure_mssg2 to point to an appropriate error message.
 *
 *              Do nothing if pass2 is FALSE on entry.
 *
 * Return:      void
 *
 * Programmer:  John Mainzer
 *              5/06/08
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
verify_journal_empty(const char * journal_file_path_ptr)
{
    const char * fcn_name = "verify_journal_empty()";
    hbool_t verbose = FALSE;
    h5_stat_t buf;

    if ( pass2 ) {

	if ( journal_file_path_ptr == NULL ) {

            failure_mssg2 = "journal_file_path_ptr NULL on entry?!?",
            pass2 = FALSE;
	}
    }

    if ( pass2 ) {

	if ( HDstat(journal_file_path_ptr, &buf) != 0 ) {

	    if ( verbose ) {

	        HDfprintf(stdout, "%s: HDstat() failed with errno = %d.\n",
                          fcn_name, errno);
	    }
	    failure_mssg2 = "stat() failed on journal file.";
	    pass2 = FALSE;

	} else {

	    if ( (buf.st_size) > 0 ) {

                failure_mssg2 = "Empty journal file expected.";
	        pass2 = FALSE;
            }
	} 
    }

    return;

} /* verify_journal_empty() */


/*** metadata journaling smoke checks ***/

/*-------------------------------------------------------------------------
 * Function:    mdj_smoke_check_00()
 *
 * Purpose:     Run a basic smoke check on the metadata journaling 
 *              facilities of the metadata cache.  Proceed as follows:
 *
 *              1) Create a file with journaling enabled.  Verify that 
 *                 journal file is created.
 *
 *              2) Using the test entries, simulate a selection of 
 *                 transactions, which exercise the full range of 
 *                 metadata cache API which can generate journal entries.  
 *                 Verify that these transactions are reflected correctly 
 *                 in the journal.
 *
 *              3) Close the hdf5 file, and verify that the journal file
 *                 is deleted.  Re-open the file with journaling, and 
 *                 do a transaction or two just to verify that the 
 *                 journaling is working.
 *
 *              4) Close the file, and verify that the journal is deleted.
 *
 *              5) Re-open the file with journaling disabled.  Do a 
 *                 transaction or two, and verify that the transactions
 *                 took place, and that there is no journal file.
 *
 *              6) Enable journaling on the open file.  Do a transaction
 *                 or two to verify that journaling is working.  
 *
 *              7) Disable journaling on the open file.  Verify that the
 *                 journal file has been deleted.
 *
 *              8) Close and delete the file.
 *
 * Return:      void
 *
 * Programmer:  John Mainzer
 *              3/11/08
 *
 * Changes:	None.
 *
 *-------------------------------------------------------------------------
 */

static void
mdj_smoke_check_00(void)
{
    const char * fcn_name = "mdj_smoke_check_00()";
    const char * testfiles[] = 
    {
        "testfiles/cache2_journal_sc00_000.jnl",
        "testfiles/cache2_journal_sc00_001.jnl",
        "testfiles/cache2_journal_sc00_002.jnl",
        "testfiles/cache2_journal_sc00_003.jnl",
        "testfiles/cache2_journal_sc00_004.jnl",
        "testfiles/cache2_journal_sc00_005.jnl",
        "testfiles/cache2_journal_sc00_006.jnl",
        "testfiles/cache2_journal_sc00_007.jnl",
        "testfiles/cache2_journal_sc00_008.jnl",
        "testfiles/cache2_journal_sc00_009.jnl",
        "testfiles/cache2_journal_sc00_010.jnl",
        "testfiles/cache2_journal_sc00_011.jnl",
        "testfiles/cache2_journal_sc00_012.jnl",
        "testfiles/cache2_journal_sc00_013.jnl",
        "testfiles/cache2_journal_sc00_014.jnl",
        "testfiles/cache2_journal_sc00_015.jnl",
        "testfiles/cache2_journal_sc00_016.jnl",
        "testfiles/cache2_journal_sc00_017.jnl",
        "testfiles/cache2_journal_sc00_018.jnl",
	NULL
    };
    char filename[512];
    char journal_filename[H5AC2__MAX_JOURNAL_FILE_NAME_LEN + 1];
    hbool_t testfile_missing = FALSE;
    hbool_t show_progress = FALSE;
    hbool_t verbose = FALSE;
    hbool_t update_architypes = FALSE;
    herr_t result;
    int cp = 0;
    hid_t file_id = -1;
    H5F_t * file_ptr = NULL;
    H5C2_t * cache_ptr = NULL;
    H5AC2_jnl_config_t jnl_config;
    
    TESTING("mdj smoke check 00 -- general coverage");

    pass2 = TRUE;

    /***********************************************************************/
    /* 1) Create a file with cache configuration set to enable journaling. */
    /***********************************************************************/

    /* setup the file name */
    if ( pass2 ) {

        if ( h5_fixname(FILENAMES[1], H5P_DEFAULT, filename, sizeof(filename))
				            == NULL ) {

            pass2 = FALSE;
            failure_mssg2 = "h5_fixname() failed (1).\n";
        }
    }

    if ( show_progress ) 
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);

    if ( verbose ) { 
        HDfprintf(stdout, "%s: filename = \"%s\".\n", fcn_name, filename); 
    }

    /* setup the journal file name */
    if ( pass2 ) {

        if ( h5_fixname(FILENAMES[3], H5P_DEFAULT, journal_filename, 
                        sizeof(journal_filename)) == NULL ) {

            pass2 = FALSE;
            failure_mssg2 = "h5_fixname() failed (2).\n";
        }
	else if ( strlen(journal_filename) >= 
			H5AC2__MAX_JOURNAL_FILE_NAME_LEN ) {

            pass2 = FALSE;
            failure_mssg2 = "journal file name too long.\n";
        }
    }

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);

    if ( verbose ) { 
        HDfprintf(stdout, "%s: journal filename = \"%s\".\n", 
		  fcn_name, journal_filename); 
    }

    /* clean out any existing journal file */
    HDremove(journal_filename);
    setup_cache_for_journaling(filename, journal_filename, &file_id,
                               &file_ptr, &cache_ptr, FALSE);

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);
 

    /********************************************************************/
    /* 2) Using the test entries, simulate a selection of transactions, */
    /*    which exercise the full range of metadata cache API calls     */
    /*    that can generate journal entries.  Verify that these         */
    /*    transactions are reflected correctly in the journal.          */
    /********************************************************************/

    /* a) First a quick check to see if we can do anything */

    begin_trans(cache_ptr, verbose, (uint64_t)1, "transaction 1.0");

    insert_entry2(file_ptr, 0, 1, FALSE, H5C2__NO_FLAGS_SET); 
    protect_entry2(file_ptr, 0, 0);
    unprotect_entry2(file_ptr, 0, 0, TRUE, H5C2__NO_FLAGS_SET);

    end_trans(file_ptr, cache_ptr, verbose, (uint64_t)1, "transaction 1.0");

    flush_journal(cache_ptr);

    if ( update_architypes ) {

        copy_file(journal_filename, testfiles[0]);
    }
    
    if ( file_exists(testfiles[0]) ) {

        verify_journal_contents(journal_filename, testfiles[0]);

    } else {

    	testfile_missing = TRUE;
    }

    flush_cache2(file_ptr, FALSE, FALSE, FALSE); /* resets transaction number */

    verify_journal_empty(journal_filename);

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);


    /* b) Verify that a sequence of cache operation that do not dirty
     *    any entry do not result in any journal activity.
     */

    begin_trans(cache_ptr, verbose, (uint64_t)1, "transaction 1.1");

    protect_entry2(file_ptr, TINY_ENTRY_TYPE, 0);
    protect_entry2(file_ptr, TINY_ENTRY_TYPE, 1);
    protect_entry2(file_ptr, TINY_ENTRY_TYPE, 2);
    unprotect_entry2(file_ptr, TINY_ENTRY_TYPE, 2, FALSE, H5C2__NO_FLAGS_SET);
    unprotect_entry2(file_ptr, TINY_ENTRY_TYPE, 1, FALSE, H5C2__NO_FLAGS_SET);
    unprotect_entry2(file_ptr, TINY_ENTRY_TYPE, 0, FALSE, H5C2__NO_FLAGS_SET);

    protect_entry_ro2(file_ptr, TINY_ENTRY_TYPE, 3);
    protect_entry_ro2(file_ptr, TINY_ENTRY_TYPE, 3);
    protect_entry_ro2(file_ptr, TINY_ENTRY_TYPE, 3);
    unprotect_entry2(file_ptr, TINY_ENTRY_TYPE, 3, FALSE, H5C2__NO_FLAGS_SET);
    unprotect_entry2(file_ptr, TINY_ENTRY_TYPE, 3, FALSE, H5C2__NO_FLAGS_SET);
    unprotect_entry2(file_ptr, TINY_ENTRY_TYPE, 3, FALSE, H5C2__NO_FLAGS_SET);

    end_trans(file_ptr, cache_ptr, verbose, (uint64_t)1, "transaction 1.1");

    flush_journal(cache_ptr);

    verify_journal_empty(journal_filename);

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);


    /* c) Verify that the most recently dirtied entry get to the head of 
     *    the transaction list (and thus appears as the last entry in the
     *    transaction).
     */

    begin_trans(cache_ptr, verbose, (uint64_t)2, "transaction 2.1");

    protect_entry2(file_ptr, TINY_ENTRY_TYPE, 0);
    unprotect_entry2(file_ptr, TINY_ENTRY_TYPE, 0, FALSE, H5C2__NO_FLAGS_SET);
    protect_entry2(file_ptr, TINY_ENTRY_TYPE, 1);
    unprotect_entry2(file_ptr, TINY_ENTRY_TYPE, 1, TRUE, H5C2__NO_FLAGS_SET);
    protect_entry2(file_ptr, TINY_ENTRY_TYPE, 2);
    unprotect_entry2(file_ptr, TINY_ENTRY_TYPE, 2, FALSE, H5C2__NO_FLAGS_SET);
    protect_entry2(file_ptr, TINY_ENTRY_TYPE, 3);
    unprotect_entry2(file_ptr, TINY_ENTRY_TYPE, 3, TRUE, H5C2__NO_FLAGS_SET);
    protect_entry2(file_ptr, TINY_ENTRY_TYPE, 4);
    unprotect_entry2(file_ptr, TINY_ENTRY_TYPE, 4, FALSE, H5C2__NO_FLAGS_SET);
    protect_entry2(file_ptr, TINY_ENTRY_TYPE, 5);
    unprotect_entry2(file_ptr, TINY_ENTRY_TYPE, 5, TRUE, H5C2__NO_FLAGS_SET);
    protect_entry2(file_ptr, TINY_ENTRY_TYPE, 3);
    unprotect_entry2(file_ptr, TINY_ENTRY_TYPE, 3, TRUE, H5C2__NO_FLAGS_SET);
    protect_entry2(file_ptr, TINY_ENTRY_TYPE, 1);
    unprotect_entry2(file_ptr, TINY_ENTRY_TYPE, 1, FALSE, H5C2__NO_FLAGS_SET);

    end_trans(file_ptr, cache_ptr, verbose, (uint64_t)2, "transaction 2.1");

    flush_journal(cache_ptr);

    if ( update_architypes ) {

        copy_file(journal_filename, testfiles[1]);
    }
    
    if ( file_exists(testfiles[1]) ) {

        verify_journal_contents(journal_filename, testfiles[1]);

    } else {

    	testfile_missing = TRUE;
    }

    flush_cache2(file_ptr, FALSE, FALSE, FALSE); /* resets transaction number */

    verify_journal_empty(journal_filename);

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);


    /* d) Mix up some protect/unprotect calls with renames.  Do this with
     *    different orders to make things interesting.
     */

    begin_trans(cache_ptr, verbose, (uint64_t)1, "transaction 1.2");

    protect_entry2(file_ptr, MICRO_ENTRY_TYPE, 0);
    unprotect_entry2(file_ptr, MICRO_ENTRY_TYPE, 0, FALSE, H5C2__NO_FLAGS_SET);
    protect_entry2(file_ptr, MICRO_ENTRY_TYPE, 1);
    unprotect_entry2(file_ptr, MICRO_ENTRY_TYPE, 1, TRUE, H5C2__NO_FLAGS_SET);
    protect_entry2(file_ptr, MICRO_ENTRY_TYPE, 2);
    unprotect_entry2(file_ptr, MICRO_ENTRY_TYPE, 2, FALSE, H5C2__NO_FLAGS_SET);
    protect_entry2(file_ptr, MICRO_ENTRY_TYPE, 2);
    unprotect_entry2(file_ptr, MICRO_ENTRY_TYPE, 2, TRUE, H5C2__NO_FLAGS_SET);
    protect_entry2(file_ptr, MICRO_ENTRY_TYPE, 3);
    unprotect_entry2(file_ptr, MICRO_ENTRY_TYPE, 3, FALSE, H5C2__NO_FLAGS_SET);
    protect_entry2(file_ptr, MICRO_ENTRY_TYPE, 4);
    unprotect_entry2(file_ptr, MICRO_ENTRY_TYPE, 4, TRUE, H5C2__NO_FLAGS_SET);

    rename_entry2(cache_ptr, MICRO_ENTRY_TYPE, 2, FALSE);
    rename_entry2(cache_ptr, MICRO_ENTRY_TYPE, 3, FALSE);

    end_trans(file_ptr, cache_ptr, verbose, (uint64_t)1, "transaction 1.2");

    flush_journal(cache_ptr);

    if ( update_architypes ) {

        copy_file(journal_filename, testfiles[2]);
    }
    
    if ( file_exists(testfiles[2]) ) {

        verify_journal_contents(journal_filename, testfiles[2]);

    } else {

    	testfile_missing = TRUE;
    }



    begin_trans(cache_ptr, verbose, (uint64_t)2, "transaction 2.2");

    rename_entry2(cache_ptr, MICRO_ENTRY_TYPE, 3, TRUE);
    rename_entry2(cache_ptr, MICRO_ENTRY_TYPE, 2, TRUE);

    protect_entry2(file_ptr, MICRO_ENTRY_TYPE, 0);
    unprotect_entry2(file_ptr, MICRO_ENTRY_TYPE, 0, FALSE, H5C2__NO_FLAGS_SET);
    protect_entry2(file_ptr, MICRO_ENTRY_TYPE, 1);
    unprotect_entry2(file_ptr, MICRO_ENTRY_TYPE, 1, TRUE, H5C2__NO_FLAGS_SET);
    protect_entry2(file_ptr, MICRO_ENTRY_TYPE, 2);
    unprotect_entry2(file_ptr, MICRO_ENTRY_TYPE, 2, FALSE, H5C2__NO_FLAGS_SET);
    protect_entry2(file_ptr, MICRO_ENTRY_TYPE, 3);
    unprotect_entry2(file_ptr, MICRO_ENTRY_TYPE, 3, TRUE, H5C2__NO_FLAGS_SET);
    protect_entry2(file_ptr, MICRO_ENTRY_TYPE, 4);
    unprotect_entry2(file_ptr, MICRO_ENTRY_TYPE, 4, FALSE, H5C2__NO_FLAGS_SET);
    protect_entry2(file_ptr, MICRO_ENTRY_TYPE, 5);
    unprotect_entry2(file_ptr, MICRO_ENTRY_TYPE, 5, TRUE, H5C2__NO_FLAGS_SET);

    end_trans(file_ptr, cache_ptr, verbose, (uint64_t)2, "transaction 2.2");

    flush_journal(cache_ptr);

    if ( update_architypes ) {

        copy_file(journal_filename, testfiles[3]);
    }
    
    if ( file_exists(testfiles[3]) ) {

        verify_journal_contents(journal_filename, testfiles[3]);

    } else {

    	testfile_missing = TRUE;
    }

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);



    /* e-1) Start by pinning a selection of entries... */

    begin_trans(cache_ptr, verbose, (uint64_t)3, "transaction 3.2");

    protect_entry2(file_ptr, MICRO_ENTRY_TYPE, 0);
    unprotect_entry2(file_ptr, MICRO_ENTRY_TYPE, 0, FALSE, H5C2__NO_FLAGS_SET);
    protect_entry2(file_ptr, MICRO_ENTRY_TYPE, 1);
    unprotect_entry2(file_ptr, MICRO_ENTRY_TYPE, 1, TRUE, H5C2__NO_FLAGS_SET);
    protect_entry2(file_ptr, MICRO_ENTRY_TYPE, 2);
    unprotect_entry2(file_ptr, MICRO_ENTRY_TYPE, 2, FALSE, H5C2__PIN_ENTRY_FLAG);
    protect_entry2(file_ptr, MICRO_ENTRY_TYPE, 3);
    unprotect_entry2(file_ptr, MICRO_ENTRY_TYPE, 3, TRUE, H5C2__PIN_ENTRY_FLAG);
    protect_entry2(file_ptr, MICRO_ENTRY_TYPE, 4);
    unprotect_entry2(file_ptr, MICRO_ENTRY_TYPE, 4, FALSE, H5C2__PIN_ENTRY_FLAG);
    protect_entry2(file_ptr, MICRO_ENTRY_TYPE, 5);
    unprotect_entry2(file_ptr, MICRO_ENTRY_TYPE, 5, TRUE, H5C2__PIN_ENTRY_FLAG);
    protect_entry2(file_ptr, MICRO_ENTRY_TYPE, 6);
    unprotect_entry2(file_ptr, MICRO_ENTRY_TYPE, 6, FALSE, H5C2__PIN_ENTRY_FLAG);
    protect_entry2(file_ptr, MICRO_ENTRY_TYPE, 7);
    unprotect_entry2(file_ptr, MICRO_ENTRY_TYPE, 7, TRUE, H5C2__PIN_ENTRY_FLAG);
    protect_entry2(file_ptr, MICRO_ENTRY_TYPE, 8);
    unprotect_entry2(file_ptr, MICRO_ENTRY_TYPE, 8, FALSE, H5C2__NO_FLAGS_SET);
    protect_entry2(file_ptr, MICRO_ENTRY_TYPE, 9);
    unprotect_entry2(file_ptr, MICRO_ENTRY_TYPE, 9, TRUE, H5C2__NO_FLAGS_SET);

    end_trans(file_ptr, cache_ptr, verbose, (uint64_t)3, "transaction 3.2");

    flush_journal(cache_ptr);

    if ( update_architypes ) {

        copy_file(journal_filename, testfiles[4]);
    }
    
    if ( file_exists(testfiles[4]) ) {

        verify_journal_contents(journal_filename, testfiles[4]);

    } else {

    	testfile_missing = TRUE;
    }

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);


    /* e-2) ... then use the H5C2_mark_pinned_or_protected_entry_dirty()
     *      call to mark a variety of protected, pinned, and pinned and 
     *      protected entries dirty.  Also rename some pinned entries.
     */

    begin_trans(cache_ptr, verbose, (uint64_t)4, "transaction 4.2");

    protect_entry2(file_ptr, MICRO_ENTRY_TYPE, 0);
    protect_entry2(file_ptr, MICRO_ENTRY_TYPE, 1);
    protect_entry2(file_ptr, MICRO_ENTRY_TYPE, 6);
    protect_entry2(file_ptr, MICRO_ENTRY_TYPE, 7);
    protect_entry2(file_ptr, MICRO_ENTRY_TYPE, 8);
    protect_entry2(file_ptr, MICRO_ENTRY_TYPE, 9);

    mark_pinned_or_protected_entry_dirty2(file_ptr, MICRO_ENTRY_TYPE, 0);
    mark_pinned_or_protected_entry_dirty2(file_ptr, MICRO_ENTRY_TYPE, 1);
    mark_pinned_or_protected_entry_dirty2(file_ptr, MICRO_ENTRY_TYPE, 2);
    mark_pinned_or_protected_entry_dirty2(file_ptr, MICRO_ENTRY_TYPE, 3);
    mark_pinned_or_protected_entry_dirty2(file_ptr, MICRO_ENTRY_TYPE, 6);
    mark_pinned_or_protected_entry_dirty2(file_ptr, MICRO_ENTRY_TYPE, 7);

    rename_entry2(cache_ptr, MICRO_ENTRY_TYPE, 4, FALSE);
    rename_entry2(cache_ptr, MICRO_ENTRY_TYPE, 5, FALSE);

    unprotect_entry2(file_ptr, MICRO_ENTRY_TYPE, 0, FALSE, H5C2__NO_FLAGS_SET);
    unprotect_entry2(file_ptr, MICRO_ENTRY_TYPE, 1, TRUE, H5C2__NO_FLAGS_SET);
    unprotect_entry2(file_ptr, MICRO_ENTRY_TYPE, 6, FALSE, H5C2__NO_FLAGS_SET);
    unprotect_entry2(file_ptr, MICRO_ENTRY_TYPE, 7, TRUE, H5C2__NO_FLAGS_SET);
    unprotect_entry2(file_ptr, MICRO_ENTRY_TYPE, 8, FALSE, H5C2__NO_FLAGS_SET);
    unprotect_entry2(file_ptr, MICRO_ENTRY_TYPE, 9, TRUE, H5C2__NO_FLAGS_SET);

    end_trans(file_ptr, cache_ptr, verbose, (uint64_t)4, "transaction 4.2");

    flush_journal(cache_ptr);

    if ( update_architypes ) {

        copy_file(journal_filename, testfiles[5]);
    }
    
    if ( file_exists(testfiles[5]) ) {

        verify_journal_contents(journal_filename, testfiles[5]);

    } else {

    	testfile_missing = TRUE;
    }

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);


    /* e-3) ...finally, upin all the pinned entries, with an undo of the
     *      previous rename in the middle.
     */

    begin_trans(cache_ptr, verbose, (uint64_t)5, "transaction 5.2");

    unpin_entry2(file_ptr, MICRO_ENTRY_TYPE, 2);
    unpin_entry2(file_ptr, MICRO_ENTRY_TYPE, 3);
    unpin_entry2(file_ptr, MICRO_ENTRY_TYPE, 4);

    rename_entry2(cache_ptr, MICRO_ENTRY_TYPE, 4, TRUE);
    rename_entry2(cache_ptr, MICRO_ENTRY_TYPE, 5, TRUE);

    unpin_entry2(file_ptr, MICRO_ENTRY_TYPE, 5);
    unpin_entry2(file_ptr, MICRO_ENTRY_TYPE, 6);
    unpin_entry2(file_ptr, MICRO_ENTRY_TYPE, 7);

    end_trans(file_ptr, cache_ptr, verbose, (uint64_t)5, "transaction 5.2");

    flush_journal(cache_ptr);

    if ( update_architypes ) {

        copy_file(journal_filename, testfiles[6]);
    }
    
    if ( file_exists(testfiles[6]) ) {

        verify_journal_contents(journal_filename, testfiles[6]);

    } else {

    	testfile_missing = TRUE;
    }

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);



    /* f-1) Pin a bunch more entries -- make them variable size, as we need
     *      to test resizing.  In passing, pin some of the entries using 
     *      the H5C2_pin_ptrotected_entry() call.
     */

    begin_trans(cache_ptr, verbose, (uint64_t)6, "transaction 6.2");

    protect_entry2(file_ptr, VARIABLE_ENTRY_TYPE, 0);
    protect_entry2(file_ptr, VARIABLE_ENTRY_TYPE, 1);
    protect_entry2(file_ptr, VARIABLE_ENTRY_TYPE, 2);
    protect_entry2(file_ptr, VARIABLE_ENTRY_TYPE, 3);
    protect_entry2(file_ptr, VARIABLE_ENTRY_TYPE, 4);
    protect_entry2(file_ptr, VARIABLE_ENTRY_TYPE, 5);
    protect_entry2(file_ptr, VARIABLE_ENTRY_TYPE, 6);
    protect_entry2(file_ptr, VARIABLE_ENTRY_TYPE, 7);

    pin_protected_entry2(file_ptr, VARIABLE_ENTRY_TYPE, 2);
    pin_protected_entry2(file_ptr, VARIABLE_ENTRY_TYPE, 3);

    unprotect_entry2(file_ptr, VARIABLE_ENTRY_TYPE, 0, FALSE, H5C2__NO_FLAGS_SET);
    unprotect_entry2(file_ptr, VARIABLE_ENTRY_TYPE, 1, TRUE, H5C2__NO_FLAGS_SET);
    unprotect_entry2(file_ptr, VARIABLE_ENTRY_TYPE, 2, FALSE, H5C2__NO_FLAGS_SET);
    unprotect_entry2(file_ptr, VARIABLE_ENTRY_TYPE, 3, TRUE, H5C2__NO_FLAGS_SET);
    unprotect_entry2(file_ptr, VARIABLE_ENTRY_TYPE, 4, FALSE, H5C2__PIN_ENTRY_FLAG);
    unprotect_entry2(file_ptr, VARIABLE_ENTRY_TYPE, 5, TRUE, H5C2__PIN_ENTRY_FLAG);
    unprotect_entry2(file_ptr, VARIABLE_ENTRY_TYPE, 6, FALSE, H5C2__PIN_ENTRY_FLAG);
    unprotect_entry2(file_ptr, VARIABLE_ENTRY_TYPE, 7, TRUE, H5C2__PIN_ENTRY_FLAG);

    end_trans(file_ptr, cache_ptr, verbose, (uint64_t)6, "transaction 6.2");

    flush_journal(cache_ptr);

    if ( update_architypes ) {

        copy_file(journal_filename, testfiles[7]);
    }
    
    if ( file_exists(testfiles[7]) ) {

        verify_journal_contents(journal_filename, testfiles[7]);

    } else {

    	testfile_missing = TRUE;
    }

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);



    /* f-2) Now resize a selection of pinned and unpinned entries via 
     *      protect/unprotect pairs, H5C2_resize_pinned_entry() and 
     *      H5C2_mark_pinned_entry_dirty().
     */


    begin_trans(cache_ptr, verbose, (uint64_t)7, "transaction 7.2");

    protect_entry2(file_ptr, VARIABLE_ENTRY_TYPE, 0);
    unprotect_entry_with_size_change2(file_ptr, VARIABLE_ENTRY_TYPE, 0,
		                      H5C2__SIZE_CHANGED_FLAG,
				      ((VARIABLE_ENTRY_SIZE / 16) * 15));

    protect_entry2(file_ptr, VARIABLE_ENTRY_TYPE, 1);
    unprotect_entry_with_size_change2(file_ptr, VARIABLE_ENTRY_TYPE, 1,
		                      H5C2__SIZE_CHANGED_FLAG|H5C2__DIRTIED_FLAG,
				      ((VARIABLE_ENTRY_SIZE / 16) * 14));

    protect_entry2(file_ptr, VARIABLE_ENTRY_TYPE, 2);
    unprotect_entry_with_size_change2(file_ptr, VARIABLE_ENTRY_TYPE, 2,
		                      H5C2__SIZE_CHANGED_FLAG,
				      ((VARIABLE_ENTRY_SIZE / 16) * 13));

    protect_entry2(file_ptr, VARIABLE_ENTRY_TYPE, 3);
    unprotect_entry_with_size_change2(file_ptr, VARIABLE_ENTRY_TYPE, 3,
		                      H5C2__SIZE_CHANGED_FLAG|H5C2__DIRTIED_FLAG,
				      ((VARIABLE_ENTRY_SIZE / 16) * 12));

    resize_pinned_entry2(file_ptr, VARIABLE_ENTRY_TYPE, 4, 
		         ((VARIABLE_ENTRY_SIZE / 16) * 11));

    resize_pinned_entry2(file_ptr, VARIABLE_ENTRY_TYPE, 5, 
		         ((VARIABLE_ENTRY_SIZE / 16) * 10));

    mark_pinned_entry_dirty2(file_ptr, VARIABLE_ENTRY_TYPE, 6, TRUE,
		             ((VARIABLE_ENTRY_SIZE / 16) * 9));

    mark_pinned_entry_dirty2(file_ptr, VARIABLE_ENTRY_TYPE, 7, TRUE,
		             ((VARIABLE_ENTRY_SIZE / 16) * 8));

    end_trans(file_ptr, cache_ptr, verbose, (uint64_t)7, "transaction 7.2");

    flush_journal(cache_ptr);

    if ( update_architypes ) {

        copy_file(journal_filename, testfiles[8]);
    }
    
    if ( file_exists(testfiles[8]) ) {

        verify_journal_contents(journal_filename, testfiles[8]);

    } else {

    	testfile_missing = TRUE;
    }

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);

    

    /* f-3) Now put all the sizes back, and also rename all the entries. */


    begin_trans(cache_ptr, verbose, (uint64_t)8, "transaction 8.2");

    rename_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 0, FALSE);
    protect_entry2(file_ptr, VARIABLE_ENTRY_TYPE, 0);
    unprotect_entry_with_size_change2(file_ptr, VARIABLE_ENTRY_TYPE, 0,
		                    H5C2__SIZE_CHANGED_FLAG, 
				    VARIABLE_ENTRY_SIZE);

    rename_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 1, FALSE);
    protect_entry2(file_ptr, VARIABLE_ENTRY_TYPE, 1);
    unprotect_entry_with_size_change2(file_ptr, VARIABLE_ENTRY_TYPE, 1,
		                      H5C2__SIZE_CHANGED_FLAG|H5C2__DIRTIED_FLAG,
				      VARIABLE_ENTRY_SIZE);

    rename_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 2, FALSE);
    protect_entry2(file_ptr, VARIABLE_ENTRY_TYPE, 2);
    unprotect_entry_with_size_change2(file_ptr, VARIABLE_ENTRY_TYPE, 2,
		                      H5C2__SIZE_CHANGED_FLAG,
				      VARIABLE_ENTRY_SIZE);

    rename_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 3, FALSE);
    protect_entry2(file_ptr, VARIABLE_ENTRY_TYPE, 3);
    unprotect_entry_with_size_change2(file_ptr, VARIABLE_ENTRY_TYPE, 3,
		                      H5C2__SIZE_CHANGED_FLAG|H5C2__DIRTIED_FLAG,
				      VARIABLE_ENTRY_SIZE);

    rename_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 4, FALSE);
    resize_pinned_entry2(file_ptr, VARIABLE_ENTRY_TYPE, 4, VARIABLE_ENTRY_SIZE);

    rename_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 5, FALSE);
    resize_pinned_entry2(file_ptr, VARIABLE_ENTRY_TYPE, 5, VARIABLE_ENTRY_SIZE);

    rename_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 6, FALSE);
    mark_pinned_entry_dirty2(file_ptr, VARIABLE_ENTRY_TYPE, 6, TRUE, 
		             VARIABLE_ENTRY_SIZE);

    rename_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 7, FALSE);
    mark_pinned_entry_dirty2(file_ptr, VARIABLE_ENTRY_TYPE, 7, TRUE,
		             VARIABLE_ENTRY_SIZE);

    end_trans(file_ptr, cache_ptr, verbose, (uint64_t)8, "transaction 8.2");

    flush_journal(cache_ptr);

    if ( update_architypes ) {

        copy_file(journal_filename, testfiles[9]);
    }
    
    if ( file_exists(testfiles[9]) ) {

        verify_journal_contents(journal_filename, testfiles[9]);

    } else {

    	testfile_missing = TRUE;
    }

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);
    
    
    
    /* f-4) Finally, rename all the entries back to their original locations,
     *      and unpin all the pinned entries.
     */

    begin_trans(cache_ptr, verbose, (uint64_t)9, "transaction 9.2");

    rename_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 0, TRUE);
    protect_entry2(file_ptr, VARIABLE_ENTRY_TYPE, 0);
    unprotect_entry_with_size_change2(file_ptr, VARIABLE_ENTRY_TYPE, 0,
		                    H5C2__SIZE_CHANGED_FLAG, VARIABLE_ENTRY_SIZE);

    rename_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 1, TRUE);
    protect_entry2(file_ptr, VARIABLE_ENTRY_TYPE, 1);
    unprotect_entry_with_size_change2(file_ptr, VARIABLE_ENTRY_TYPE, 1,
                                      H5C2__SIZE_CHANGED_FLAG|H5C2__DIRTIED_FLAG,
	                              VARIABLE_ENTRY_SIZE);

    rename_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 2, TRUE);
    protect_entry2(file_ptr, VARIABLE_ENTRY_TYPE, 2);
    unprotect_entry_with_size_change2(file_ptr, VARIABLE_ENTRY_TYPE, 2,
		                    H5C2__SIZE_CHANGED_FLAG|H5C2__UNPIN_ENTRY_FLAG, 
				    VARIABLE_ENTRY_SIZE);

    rename_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 3, TRUE);
    protect_entry2(file_ptr, VARIABLE_ENTRY_TYPE, 3);
    unprotect_entry_with_size_change2(file_ptr, VARIABLE_ENTRY_TYPE, 3,
	        H5C2__SIZE_CHANGED_FLAG|H5C2__DIRTIED_FLAG|H5C2__UNPIN_ENTRY_FLAG,
                VARIABLE_ENTRY_SIZE);

    rename_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 4, TRUE);
    resize_pinned_entry2(file_ptr, VARIABLE_ENTRY_TYPE, 4, VARIABLE_ENTRY_SIZE);
    unpin_entry2(file_ptr, VARIABLE_ENTRY_TYPE, 4);

    rename_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 5, TRUE);
    resize_pinned_entry2(file_ptr, VARIABLE_ENTRY_TYPE, 5, VARIABLE_ENTRY_SIZE);
    unpin_entry2(file_ptr, VARIABLE_ENTRY_TYPE, 5);

    rename_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 6, TRUE);
    mark_pinned_entry_dirty2(file_ptr, VARIABLE_ENTRY_TYPE, 6, TRUE, 
		             VARIABLE_ENTRY_SIZE);
    unpin_entry2(file_ptr, VARIABLE_ENTRY_TYPE, 6);

    rename_entry2(cache_ptr, VARIABLE_ENTRY_TYPE, 7, TRUE);
    mark_pinned_entry_dirty2(file_ptr, VARIABLE_ENTRY_TYPE, 7, TRUE,
		             VARIABLE_ENTRY_SIZE);
    unpin_entry2(file_ptr, VARIABLE_ENTRY_TYPE, 7);

    end_trans(file_ptr, cache_ptr, verbose, (uint64_t)9, "transaction 9.2");

    flush_journal(cache_ptr);

    if ( update_architypes ) {

        copy_file(journal_filename, testfiles[10]);
    }
    
    if ( file_exists(testfiles[10]) ) {

        verify_journal_contents(journal_filename, testfiles[10]);

    } else {

    	testfile_missing = TRUE;
    }

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);

    flush_cache2(file_ptr, FALSE, FALSE, FALSE); /* resets transaction number */

    verify_journal_empty(journal_filename);

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);
    
    
    
    /* g) verify that the journaling code handles a cascade of changes
     *    caused when the serialization of an entry causes dirties, resizes,
     *    and/or resizes of other entries.
     *
     * g-1) Load several entries of VARIABLE_ENTRY_TYPE into the cache, and
     *      set their sizes to values less than the maximum.
     */

    begin_trans(cache_ptr, verbose, (uint64_t)1, "transaction 1.3");

    protect_entry2(file_ptr, VARIABLE_ENTRY_TYPE, 10);
    unprotect_entry_with_size_change2(file_ptr, VARIABLE_ENTRY_TYPE, 10,
		                      H5C2__SIZE_CHANGED_FLAG,
				      ((VARIABLE_ENTRY_SIZE / 4) * 1));

    protect_entry2(file_ptr, VARIABLE_ENTRY_TYPE, 11);
    unprotect_entry_with_size_change2(file_ptr, VARIABLE_ENTRY_TYPE, 11,
		                      H5C2__SIZE_CHANGED_FLAG,
				      ((VARIABLE_ENTRY_SIZE / 4) * 2));

    protect_entry2(file_ptr, VARIABLE_ENTRY_TYPE, 12);
    unprotect_entry_with_size_change2(file_ptr, VARIABLE_ENTRY_TYPE, 12,
		                      H5C2__SIZE_CHANGED_FLAG,
				      ((VARIABLE_ENTRY_SIZE / 4) * 3));

    end_trans(file_ptr, cache_ptr, verbose, (uint64_t)1, "transaction 1.3");

    flush_journal(cache_ptr);

    if ( update_architypes ) {

        copy_file(journal_filename, testfiles[11]);
    }
    
    if ( file_exists(testfiles[11]) ) {

        verify_journal_contents(journal_filename, testfiles[11]);

    } else {

    	testfile_missing = TRUE;
    }


    /* g-2) Now setup flush operations on some entries to dirty, resize,
     *      and/or rename other entries.  When these entries are dirtied
     *      in a transaction, the associated flush operations should be
     *      triggered and appear in the journal.
     *
     *      In case you need a score card, in what follows, I set up the
     *      following dependencies:
     *
     *      (MICRO_ENTRY_TYPE, 20) dirties (MICRO_ENTRY_TYPE, 30)
     *
     *      (MICRO_ENTRY_TYPE, 21) renames, resizes, and dirties:
     *      				   (VARIABLE_ENTRY_TYPE, 10)
     *      				   (VARIABLE_ENTRY_TYPE, 13)
     *
     *      (MICRO_ENTRY_TYPE, 22) resizes (VARIABLE_ENTRY_TYPE, 11)
     *                                     (VARIABLE_ENTRY_TYPE, 12)
     *
     *      (MICRO_ENTRY_TYPE, 23) renames (VARIABLE_ENTRY_TYPE, 10)
     *                                     (VARIABLE_ENTRY_TYPE, 13)
     *                                     to their original locations
     *
     *      (MICRO_ENTRY_TYPE, 24) dirties (MICRO_ENTRY_TYPE, 21)
     *
     *      (MICRO_ENTRY_TYPE, 25) dirties (MICRO_ENTRY_TYPE, 22)
     *                                     (MICRO_ENTRY_TYPE, 23)
     *
     */

    add_flush_op2(MICRO_ENTRY_TYPE, 20, 
		  FLUSH_OP__DIRTY, MICRO_ENTRY_TYPE, 30, FALSE, 0);


    add_flush_op2(MICRO_ENTRY_TYPE, 21,
	  FLUSH_OP__RESIZE, VARIABLE_ENTRY_TYPE, 10, FALSE, VARIABLE_ENTRY_SIZE);
    add_flush_op2(MICRO_ENTRY_TYPE, 21,
	  FLUSH_OP__RENAME, VARIABLE_ENTRY_TYPE, 10, FALSE, 0);
    add_flush_op2(MICRO_ENTRY_TYPE, 21,
	  FLUSH_OP__DIRTY, VARIABLE_ENTRY_TYPE, 10, FALSE, 0);

    add_flush_op2(MICRO_ENTRY_TYPE, 21,
	  FLUSH_OP__RESIZE, VARIABLE_ENTRY_TYPE, 13, FALSE, VARIABLE_ENTRY_SIZE/4);
    add_flush_op2(MICRO_ENTRY_TYPE, 21,
	  FLUSH_OP__RENAME, VARIABLE_ENTRY_TYPE, 13, FALSE, 0);
    add_flush_op2(MICRO_ENTRY_TYPE, 21,
	  FLUSH_OP__DIRTY, VARIABLE_ENTRY_TYPE, 13, FALSE, 0);


    add_flush_op2(MICRO_ENTRY_TYPE, 22,
	  FLUSH_OP__RESIZE, VARIABLE_ENTRY_TYPE, 11, FALSE, VARIABLE_ENTRY_SIZE);

    add_flush_op2(MICRO_ENTRY_TYPE, 22,
	  FLUSH_OP__RESIZE, VARIABLE_ENTRY_TYPE, 12, FALSE, VARIABLE_ENTRY_SIZE);


    add_flush_op2(MICRO_ENTRY_TYPE, 23,
	  FLUSH_OP__RENAME, VARIABLE_ENTRY_TYPE, 10, TRUE, 0);

    add_flush_op2(MICRO_ENTRY_TYPE, 23,
	  FLUSH_OP__RENAME, VARIABLE_ENTRY_TYPE, 13, TRUE, 0);


    add_flush_op2(MICRO_ENTRY_TYPE, 24,
	  FLUSH_OP__DIRTY, MICRO_ENTRY_TYPE, 21, FALSE, 0);


    add_flush_op2(MICRO_ENTRY_TYPE, 25,
	  FLUSH_OP__DIRTY, MICRO_ENTRY_TYPE, 22, FALSE, 0);

    add_flush_op2(MICRO_ENTRY_TYPE, 25,
	  FLUSH_OP__DIRTY, MICRO_ENTRY_TYPE, 23, FALSE, 0);


    /* g-3) Start with a simple check -- dirty (MICRO_ENTRY_TYPE, 20),
     *      which should also dirty (MICRO_ENTRY_TYPE, 30) when 
     *      (MICRO_ENTRY_TYPE, 20) is serialized at transaction close.
     */

    begin_trans(cache_ptr, verbose, (uint64_t)2, "transaction 2.3");

    protect_entry2(file_ptr, MICRO_ENTRY_TYPE, 20);
    unprotect_entry2(file_ptr, MICRO_ENTRY_TYPE, 20, TRUE, H5C2__NO_FLAGS_SET);

    end_trans(file_ptr, cache_ptr, verbose, (uint64_t)2, "transaction 2.3");

    flush_journal(cache_ptr);

    if ( update_architypes ) {

        copy_file(journal_filename, testfiles[12]);
    }
    
    if ( file_exists(testfiles[12]) ) {

        verify_journal_contents(journal_filename, testfiles[12]);

    } else {

    	testfile_missing = TRUE;
    }


    /* g-4) Now dirty (MICRO_ENTRY_TYPE, 24), which dirties 
     *      (MICRO_ENTRY_TYPE, 21), which dirties, resizes, and 
     *      renames (VARIABLE_ENTRY_TYPE, 10) and (VARIABLE_ENTRY_TYPE, 13)
     */

    begin_trans(cache_ptr, verbose, (uint64_t)3, "transaction 3.3");

    protect_entry2(file_ptr, MICRO_ENTRY_TYPE, 24);
    unprotect_entry2(file_ptr, MICRO_ENTRY_TYPE, 24, TRUE, H5C2__NO_FLAGS_SET);

    end_trans(file_ptr, cache_ptr, verbose, (uint64_t)3, "transaction 3.3");

    flush_journal(cache_ptr);

    if ( update_architypes ) {

        copy_file(journal_filename, testfiles[13]);
    }
    
    if ( file_exists(testfiles[13]) ) {

        verify_journal_contents(journal_filename, testfiles[13]);

    } else {

    	testfile_missing = TRUE;
    }


    /* g-4) Now dirty (MICRO_ENTRY_TYPE, 25), which dirties 
     *      (MICRO_ENTRY_TYPE, 22) and (MICRO_ENTRY_TYPE, 23), which 
     *      in turn resize (VARIABLE_ENTRY_TYPE, 11) and 
     *      (VARIABLE_ENTRY_TYPE, 12), and rename (VARIABLE_ENTRY_TYPE, 10)
     *      and (VARIABLE_ENTRY_TYPE, 13) back to their original locations.
     */

    begin_trans(cache_ptr, verbose, (uint64_t)4, "transaction 4.3");

    protect_entry2(file_ptr, MICRO_ENTRY_TYPE, 25);
    unprotect_entry2(file_ptr, MICRO_ENTRY_TYPE, 25, TRUE, H5C2__NO_FLAGS_SET);

    end_trans(file_ptr, cache_ptr, verbose, (uint64_t)4, "transaction 4.3");

    flush_journal(cache_ptr);

    if ( update_architypes ) {

        copy_file(journal_filename, testfiles[14]);
    }
    
    if ( file_exists(testfiles[14]) ) {

        verify_journal_contents(journal_filename, testfiles[14]);

    } else {

    	testfile_missing = TRUE;
    }

    flush_cache2(file_ptr, FALSE, FALSE, FALSE); /* resets transaction number */

    verify_journal_empty(journal_filename);

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);
    


    /* h) Dirty an entry, and then expunge it.  Entry should not appear
     *    in the journal.  Do this twice -- first with only the expunge
     *    entry in the transaction, and a second time with other entries
     *    involved.
     *
     *    Note that no journal file will be written until the first 
     *    entry, so start with a transaction that generates some data.
     */

    begin_trans(cache_ptr, verbose, (uint64_t)1, "transaction 1.4");

    protect_entry2(file_ptr, MICRO_ENTRY_TYPE, 39);
    unprotect_entry2(file_ptr, MICRO_ENTRY_TYPE, 39, TRUE, H5C2__NO_FLAGS_SET);

    end_trans(file_ptr, cache_ptr, verbose, (uint64_t)1, "transaction 1.4");

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);


    begin_trans(cache_ptr, verbose, (uint64_t)2, "transaction 2.4");

    protect_entry2(file_ptr, MICRO_ENTRY_TYPE, 40);
    unprotect_entry2(file_ptr, MICRO_ENTRY_TYPE, 40, TRUE, H5C2__NO_FLAGS_SET);

    expunge_entry2(file_ptr, MICRO_ENTRY_TYPE, 40);

    end_trans(file_ptr, cache_ptr, verbose, (uint64_t)2, "transaction 2.4");

    flush_journal(cache_ptr);

    if ( update_architypes ) {

        copy_file(journal_filename, testfiles[15]);
    }
    
    if ( file_exists(testfiles[15]) ) {

        verify_journal_contents(journal_filename, testfiles[15]);

    } else {

    	testfile_missing = TRUE;
    }

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);
    


    begin_trans(cache_ptr, verbose, (uint64_t)3, "transaction 3.4");

    protect_entry2(file_ptr, MICRO_ENTRY_TYPE, 41);
    unprotect_entry2(file_ptr, MICRO_ENTRY_TYPE, 41, TRUE, H5C2__NO_FLAGS_SET);
    expunge_entry2(file_ptr, MICRO_ENTRY_TYPE, 41);
    protect_entry2(file_ptr, MICRO_ENTRY_TYPE, 42);
    unprotect_entry2(file_ptr, MICRO_ENTRY_TYPE, 42, TRUE, H5C2__NO_FLAGS_SET);

    end_trans(file_ptr, cache_ptr, verbose, (uint64_t)3, "transaction 3.4");

    flush_journal(cache_ptr);

    if ( update_architypes ) {

        copy_file(journal_filename, testfiles[16]);
    }
    
    if ( file_exists(testfiles[16]) ) {

        verify_journal_contents(journal_filename, testfiles[16]);

    } else {

    	testfile_missing = TRUE;
    }

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);


    /************************************************************/
    /* 3) Close the hdf5 file, and verify that the journal file */
    /*    is deleted.  Re-open the file with journaling, and    */
    /*    do a transaction or two just to verify that the       */
    /*    journaling is working.                                */
    /************************************************************/

    /* a) Close the hdf5 file. */
    if ( pass2 ) {

	if ( H5Fclose(file_id) < 0 ) {

	    pass2 = FALSE;
	    failure_mssg2 = "temporary H5Fclose() failed.\n";

	} else {
	    file_id = -1;
	    file_ptr = NULL;
	    cache_ptr = NULL;
	}
    }

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);


    /* b) Verify that the journal file has been deleted. */
    verify_journal_deleted(journal_filename);

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);


    /* c) Re-open the hdf5 file. */
    open_existing_file_for_journaling(filename, journal_filename, &file_id,
                                      &file_ptr, &cache_ptr);

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);


    /* d) do a transaction or to to verify that journaling is working. */

    begin_trans(cache_ptr, verbose, (uint64_t)1, "transaction 1.5");

    insert_entry2(file_ptr, 0, 1, FALSE, H5C2__NO_FLAGS_SET); 
    protect_entry2(file_ptr, 0, 0);
    unprotect_entry2(file_ptr, 0, 0, TRUE, H5C2__NO_FLAGS_SET);

    end_trans(file_ptr, cache_ptr, verbose, (uint64_t)1, "transaction 1.5");

    flush_journal(cache_ptr);

    if ( update_architypes ) {

        copy_file(journal_filename, testfiles[17]);
    }
    
    if ( file_exists(testfiles[17]) ) {

        verify_journal_contents(journal_filename, testfiles[17]);

    } else {

    	testfile_missing = TRUE;
    }

    flush_cache2(file_ptr, FALSE, FALSE, FALSE); /* resets transaction number */

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);

    /**************************************************************/
    /* 4) Close the file, and verify that the journal is deleted. */
    /**************************************************************/

    /* Close the hdf5 file. */
    if ( pass2 ) {

	if ( H5Fclose(file_id) < 0 ) {

	    pass2 = FALSE;
	    failure_mssg2 = "temporary H5Fclose() failed.\n";

	} else {
	    file_id = -1;
	    file_ptr = NULL;
	    cache_ptr = NULL;
	}
    }

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);


    /* b) Verify that the journal file has been deleted. */
    verify_journal_deleted(journal_filename);

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);


    /************************************************************/
    /* 5) Re-open the file with journaling disabled.  Do a      */
    /*    transaction or two, and verify that the transactions  */
    /*    took place, and that there is no journal file.        */
    /************************************************************/

    /* re-open the file without journaling enabled */

    open_existing_file_without_journaling(filename, &file_id, 
                                          &file_ptr, &cache_ptr);

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);


    /* do a transaction to verify that journaling is disabled.  
     *
     * Note that we will only get a transaction number of zero if 
     * journaling is disabled -- thus the following begin/end trans
     * calls should fail if journaling is enabled.
     */

    begin_trans(cache_ptr, verbose, (uint64_t)0, "transaction 1.6");

    insert_entry2(file_ptr, 0, 10, FALSE, H5C2__NO_FLAGS_SET); 
    protect_entry2(file_ptr, 0, 0);
    unprotect_entry2(file_ptr, 0, 0, TRUE, H5C2__NO_FLAGS_SET);

    end_trans(file_ptr, cache_ptr, verbose, (uint64_t)0, "transaction 1.6");

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);

    if ( ( pass2 ) && ( cache_ptr->mdj_enabled ) ) {

        pass2 = FALSE;
        failure_mssg2 = "journaling is enabled?!?!(1).\n";
    }

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);

    /* note that flush_journal() will throw an exception if journaling
     * is not enabled, so we don't call it here.  Instead, just call
     * verify_journal_deleted() to verify that there is no journal file.
     */

    verify_journal_deleted(journal_filename);

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);

 
    /************************************************************/
    /* 6) Enable journaling on the open file.  Do a transaction */
    /*    or two to verify that journaling is working.          */
    /************************************************************/

    /* now enable journaling */
    if ( pass2 ) {

        jnl_config.version = H5AC2__CURR_JNL_CONFIG_VER;

        result = H5Fget_jnl_config(file_id, &jnl_config);

        if ( result < 0 ) {

            pass2 = FALSE;
            failure_mssg2 = "H5Fget_jnl_config() failed.\n";
        }

        /* set journaling config fields to taste */
        jnl_config.enable_journaling       = TRUE;

        strcpy(jnl_config.journal_file_path, journal_filename);

        jnl_config.journal_recovered       = FALSE;
        jnl_config.jbrb_buf_size           = (8 * 1024);
        jnl_config.jbrb_num_bufs           = 2;
        jnl_config.jbrb_use_aio            = FALSE;
        jnl_config.jbrb_human_readable     = TRUE;
    }

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);

    if ( pass2 ) {

        result = H5Fset_jnl_config(file_id, &jnl_config);

        if ( result < 0 ) {

            pass2 = FALSE;
            failure_mssg2 = "H5Fset_jnl_config() failed.\n";
        }
    }

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);

    /* do a transaction or to to verify that journaling is working. */

    begin_trans(cache_ptr, verbose, (uint64_t)1, "transaction 1.7");

    insert_entry2(file_ptr, 0, 20, FALSE, H5C2__NO_FLAGS_SET); 
    protect_entry2(file_ptr, 0, 0);
    unprotect_entry2(file_ptr, 0, 0, TRUE, H5C2__NO_FLAGS_SET);

    end_trans(file_ptr, cache_ptr, verbose, (uint64_t)1, "transaction 1.7");

    flush_journal(cache_ptr);

    if ( update_architypes ) {

        copy_file(journal_filename, testfiles[18]);
    }
    
    if ( file_exists(testfiles[18]) ) {

        verify_journal_contents(journal_filename, testfiles[18]);

    } else {

    	testfile_missing = TRUE;
    }

    flush_cache2(file_ptr, FALSE, FALSE, FALSE); /* resets transaction number */

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);

 
    /************************************************************/
    /* 7) Disable journaling on the open file.  Verify that the */
    /*    journal file has been deleted.                        */
    /************************************************************/

    /* disable journaling */
    if ( pass2 ) {

        jnl_config.enable_journaling       = FALSE;

        result = H5Fset_jnl_config(file_id, &jnl_config);

        if ( result < 0 ) {

            pass2 = FALSE;
            failure_mssg2 = "H5Fset_jnl_config() failed.\n";
        }
    }

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);

    verify_journal_deleted(journal_filename);

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);

 
    /*********************************/
    /* 8) Close and delete the file. */
    /*********************************/
 
    if ( pass2 ) {

        if ( H5Fclose(file_id) < 0 ) {

            pass2 = FALSE;
	    failure_mssg2 = "H5Fclose(file_id) failed.\n";

	}
    }

    HDremove(journal_filename);

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);

    if ( pass2 ) { 
	    
        PASSED(); 

	if ( testfile_missing ) {

	    puts("	WARNING: One or more missing test files."); 
	    fflush(stdout);
        }
    } else { 
	    
        H5_FAILED(); 
    }

    if ( ! pass2 ) {

	failures2++;
        HDfprintf(stdout, "%s: failure_mssg2 = \"%s\".\n",
                  fcn_name, failure_mssg2);
    }

    return;

} /* mdj_smoke_check_00() */


/*-------------------------------------------------------------------------
 * Function:    mdj_smoke_check_01()
 *
 * Purpose:     Run a cut down version of smoke_check_1 in cache2.c, with
 * 		journaling enabled.  Check the journal files generated,
 * 		and verify that the journal output matches the architype
 * 		test files.  Skip the comparison and generate a warning
 * 		if an architype file is missing.
 *
 * Return:      void
 *
 * Programmer:  John Mainzer
 *              5/19/08
 *
 * Changes:	None.
 *
 *-------------------------------------------------------------------------
 */

static void
mdj_smoke_check_01(void)
{
    const char * fcn_name = "mdj_smoke_check_01()";
    const char * testfiles[] = 
    {
        "testfiles/cache2_journal_sc01_000.jnl",
        "testfiles/cache2_journal_sc01_001.jnl",
        "testfiles/cache2_journal_sc01_002.jnl",
        "testfiles/cache2_journal_sc01_003.jnl",
        "testfiles/cache2_journal_sc01_004.jnl",
	NULL
    };
    char filename[512];
    char journal_filename[H5AC2__MAX_JOURNAL_FILE_NAME_LEN + 1];
    hbool_t testfile_missing = FALSE;
    hbool_t show_progress = FALSE;
    hbool_t dirty_inserts = FALSE;
    hbool_t verbose = FALSE;
    hbool_t update_architypes = FALSE;
    int dirty_unprotects = FALSE;
    int dirty_destroys = FALSE;
    hbool_t display_stats = FALSE;
    int32_t lag = 10;
    int cp = 0;
    int32_t max_index = 128;
    uint64_t trans_num = 0;
    hid_t file_id = -1;
    H5F_t * file_ptr = NULL;
    H5C2_t * cache_ptr = NULL;
    
    TESTING("mdj smoke check 01 -- jrnl clean ins, prot, unprot, del, ren");

    pass2 = TRUE;

    /********************************************************************/
    /* Create a file with cache configuration set to enable journaling. */
    /********************************************************************/

    /* setup the file name */
    if ( pass2 ) {

        if ( h5_fixname(FILENAMES[1], H5P_DEFAULT, filename, sizeof(filename))
				            == NULL ) {

            pass2 = FALSE;
            failure_mssg2 = "h5_fixname() failed (1).\n";
        }
    }

    if ( show_progress ) 
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);

    if ( verbose ) { 
        HDfprintf(stdout, "%s: filename = \"%s\".\n", fcn_name, filename); 
    }

    /* setup the journal file name */
    if ( pass2 ) {

        if ( h5_fixname(FILENAMES[3], H5P_DEFAULT, journal_filename, 
                        sizeof(journal_filename)) == NULL ) {

            pass2 = FALSE;
            failure_mssg2 = "h5_fixname() failed (2).\n";
        }
	else if ( strlen(journal_filename) >= 
			H5AC2__MAX_JOURNAL_FILE_NAME_LEN ) {

            pass2 = FALSE;
            failure_mssg2 = "journal file name too long.\n";
        }
    }

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);

    if ( verbose ) { 
        HDfprintf(stdout, "%s: journal filename = \"%s\".\n", 
		  fcn_name, journal_filename); 
    }

    /* clean out any existing journal file */
    HDremove(journal_filename);

    /* Unfortunately, we get different journal output depending on the 
     * file driver, as at present we are including the end of address
     * space in journal entries, and the end of address space seems to 
     * be in part a function of the file driver.  
     *
     * Thus, if we want to use the core file driver when available, we 
     * will either have to remove the end of address space from the 
     * journal entries, get the different file drivers to aggree on 
     * end of address space, or maintain different sets of architype
     * files for the different file drivers.
     */
    setup_cache_for_journaling(filename, journal_filename, &file_id,
                               &file_ptr, &cache_ptr, FALSE);

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);
 

    /******************************************/
    /* Run a small, fairly simple stress test */
    /******************************************/

    trans_num = 0;

    jrnl_row_major_scan_forward2(/* file_ptr               */ file_ptr,
                                 /* max_index              */ max_index,
                                 /* lag                    */ lag,
                                 /* verbose                */ verbose,
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
                                 /* dirty_unprotects       */ dirty_unprotects,
                                 /* trans_num              */ trans_num);

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);

    flush_journal(cache_ptr);

    if ( update_architypes ) {

        copy_file(journal_filename, testfiles[0]);
    }
    
    if ( file_exists(testfiles[0]) ) {

        verify_journal_contents(journal_filename, testfiles[0]);

    } else {

    	testfile_missing = TRUE;
    }

    flush_cache2(file_ptr, FALSE, FALSE, FALSE); /* resets transaction number */

    verify_journal_empty(journal_filename);

    trans_num = 0;

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);

    jrnl_row_major_scan_backward2(/* file_ptr               */ file_ptr,
                                  /* max_index              */ max_index,
                                  /* lag                    */ lag,
                                  /* verbose                */ verbose,
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
                                  /* dirty_unprotects       */ dirty_unprotects,
                                  /* trans_num              */ trans_num);

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);

    flush_journal(cache_ptr);

    if ( update_architypes ) {

        copy_file(journal_filename, testfiles[1]);
    }
    
    if ( file_exists(testfiles[1]) ) {

        verify_journal_contents(journal_filename, testfiles[1]);

    } else {

    	testfile_missing = TRUE;
    }

    flush_cache2(file_ptr, FALSE, FALSE, FALSE); /* resets transaction number */

    verify_journal_empty(journal_filename);

    trans_num = 0;

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);

    jrnl_row_major_scan_forward2(/* file_ptr               */ file_ptr,
                                 /* max_index              */ max_index,
                                 /* lag                    */ lag,
                                 /* verbose                */ verbose,
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
                                 /* dirty_unprotects       */ dirty_unprotects,
                                 /* trans_num              */ trans_num);

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);

    flush_journal(cache_ptr);

    if ( update_architypes ) {

        copy_file(journal_filename, testfiles[2]);
    }
    
    if ( file_exists(testfiles[2]) ) {

        verify_journal_contents(journal_filename, testfiles[2]);

    } else {

    	testfile_missing = TRUE;
    }

    flush_cache2(file_ptr, FALSE, FALSE, FALSE); /* resets transaction number */

    trans_num = 0;

    verify_journal_empty(journal_filename);

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);

    jrnl_col_major_scan_forward2(/* file_ptr               */ file_ptr,
                                 /* max_index              */ max_index,
                                 /* lag                    */ lag,
                                 /* verbose                */ verbose,
                                 /* reset_stats            */ TRUE,
                                 /* display_stats          */ display_stats,
                                 /* display_detailed_stats */ TRUE,
                                 /* do_inserts             */ TRUE,
                                 /* dirty_inserts          */ dirty_inserts,
                                 /* dirty_unprotects       */ dirty_unprotects,
                                 /* trans_num              */ trans_num);

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);

    flush_journal(cache_ptr);

    if ( update_architypes ) {

        copy_file(journal_filename, testfiles[3]);
    }
    
    if ( file_exists(testfiles[3]) ) {

        verify_journal_contents(journal_filename, testfiles[3]);

    } else {

    	testfile_missing = TRUE;
    }

    flush_cache2(file_ptr, FALSE, FALSE, FALSE); /* resets transaction number */

    verify_journal_empty(journal_filename);

    trans_num = 0;

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);

    jrnl_col_major_scan_backward2(/* file_ptr               */ file_ptr,
                                  /* max_index              */ max_index,
                                  /* lag                    */ lag,
                                  /* verbose                */ verbose,
                                  /* reset_stats            */ TRUE,
                                  /* display_stats          */ display_stats,
                                  /* display_detailed_stats */ TRUE,
                                  /* do_inserts             */ TRUE,
                                  /* dirty_inserts          */ dirty_inserts,
                                  /* dirty_unprotects       */ dirty_unprotects,
                                  /* trans_num              */ trans_num);

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);

    flush_journal(cache_ptr);

    if ( update_architypes ) {

        copy_file(journal_filename, testfiles[4]);
    }
    
    if ( file_exists(testfiles[4]) ) {

        verify_journal_contents(journal_filename, testfiles[4]);

    } else {

    	testfile_missing = TRUE;
    }

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);

    /****************************************************/
    /* Close and discard the file and the journal file. */
    /****************************************************/

    takedown_cache_after_journaling(file_id, filename, journal_filename, FALSE);

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);

    verify_clean2();
    verify_unprotected2();

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);

    if ( pass2 ) { 
	    
        PASSED(); 

	if ( testfile_missing ) {

	    puts("	WARNING: One or more missing test files."); 
	    fflush(stdout);
        }
    } else { 
	    
        H5_FAILED(); 
    }

    if ( ! pass2 ) {

	failures2++;
        HDfprintf(stdout, "%s: failure_mssg2 = \"%s\".\n",
                  fcn_name, failure_mssg2);
    }

    return;

} /* mdj_smoke_check_01() */


/*-------------------------------------------------------------------------
 * Function:    mdj_smoke_check_02()
 *
 * Purpose:     Run a cut down version of smoke_check_2 in cache2.c, with
 * 		journaling enabled.  Check the journal files generated,
 * 		and verify that the journal output matches the architype
 * 		test files.  Skip the comparison and generate a warning
 * 		if an architype file is missing.
 *
 * Return:      void
 *
 * Programmer:  John Mainzer
 *              5/19/08
 *
 * Changes:	None.
 *
 *-------------------------------------------------------------------------
 */

static void
mdj_smoke_check_02(void)
{
    const char * fcn_name = "mdj_smoke_check_02()";
    const char * testfiles[] = 
    {
        "testfiles/cache2_journal_sc02_000.jnl",
        "testfiles/cache2_journal_sc02_001.jnl",
        "testfiles/cache2_journal_sc02_002.jnl",
        "testfiles/cache2_journal_sc02_003.jnl",
        "testfiles/cache2_journal_sc02_004.jnl",
	NULL
    };
    char filename[512];
    char journal_filename[H5AC2__MAX_JOURNAL_FILE_NAME_LEN + 1];
    hbool_t testfile_missing = FALSE;
    hbool_t show_progress = FALSE;
    hbool_t dirty_inserts = TRUE;
    hbool_t verbose = FALSE;
    hbool_t update_architypes = FALSE;
    int dirty_unprotects = TRUE;
    int dirty_destroys = TRUE;
    hbool_t display_stats = FALSE;
    int32_t lag = 10;
    int cp = 0;
    int32_t max_index = 128;
    uint64_t trans_num = 0;
    hid_t file_id = -1;
    H5F_t * file_ptr = NULL;
    H5C2_t * cache_ptr = NULL;
    
    TESTING("mdj smoke check 02 -- jrnl dirty ins, prot, unprot, del, ren");

    pass2 = TRUE;

    /********************************************************************/
    /* Create a file with cache configuration set to enable journaling. */
    /********************************************************************/

    /* setup the file name */
    if ( pass2 ) {

        if ( h5_fixname(FILENAMES[1], H5P_DEFAULT, filename, sizeof(filename))
				            == NULL ) {

            pass2 = FALSE;
            failure_mssg2 = "h5_fixname() failed (1).\n";
        }
    }

    if ( show_progress ) 
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);

    if ( verbose ) { 
        HDfprintf(stdout, "%s: filename = \"%s\".\n", fcn_name, filename); 
    }

    /* setup the journal file name */
    if ( pass2 ) {

        if ( h5_fixname(FILENAMES[3], H5P_DEFAULT, journal_filename, 
                        sizeof(journal_filename)) == NULL ) {

            pass2 = FALSE;
            failure_mssg2 = "h5_fixname() failed (2).\n";
        }
	else if ( strlen(journal_filename) >= 
			H5AC2__MAX_JOURNAL_FILE_NAME_LEN ) {

            pass2 = FALSE;
            failure_mssg2 = "journal file name too long.\n";
        }
    }

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);

    if ( verbose ) { 
        HDfprintf(stdout, "%s: journal filename = \"%s\".\n", 
		  fcn_name, journal_filename); 
    }

    /* clean out any existing journal file */
    HDremove(journal_filename);

    /* Unfortunately, we get different journal output depending on the 
     * file driver, as at present we are including the end of address
     * space in journal entries, and the end of address space seems to 
     * be in part a function of the file driver.  
     *
     * Thus, if we want to use the core file driver when available, we 
     * will either have to remove the end of address space from the 
     * journal entries, get the different file drivers to aggree on 
     * end of address space, or maintain different sets of architype
     * files for the different file drivers.
     */
    setup_cache_for_journaling(filename, journal_filename, &file_id,
                               &file_ptr, &cache_ptr, FALSE);

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);
 

    /******************************************/
    /* Run a small, fairly simple stress test */
    /******************************************/

    trans_num = 0;

    jrnl_row_major_scan_forward2(/* file_ptr               */ file_ptr,
                                 /* max_index              */ max_index,
                                 /* lag                    */ lag,
                                 /* verbose                */ verbose,
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
                                 /* dirty_unprotects       */ dirty_unprotects,
                                 /* trans_num              */ trans_num);

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);

    flush_journal(cache_ptr);

    if ( update_architypes ) {

        copy_file(journal_filename, testfiles[0]);
    }
    
    if ( file_exists(testfiles[0]) ) {

        verify_journal_contents(journal_filename, testfiles[0]);

    } else {

    	testfile_missing = TRUE;
    }

    flush_cache2(file_ptr, FALSE, FALSE, FALSE); /* resets transaction number */

    verify_journal_empty(journal_filename);

    trans_num = 0;

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);

    jrnl_row_major_scan_backward2(/* file_ptr               */ file_ptr,
                                  /* max_index              */ max_index,
                                  /* lag                    */ lag,
                                  /* verbose                */ verbose,
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
                                  /* dirty_unprotects       */ dirty_unprotects,
                                  /* trans_num              */ trans_num);

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);

    flush_journal(cache_ptr);

    if ( update_architypes ) {

        copy_file(journal_filename, testfiles[1]);
    }
    
    if ( file_exists(testfiles[1]) ) {

        verify_journal_contents(journal_filename, testfiles[1]);

    } else {

    	testfile_missing = TRUE;
    }

    flush_cache2(file_ptr, FALSE, FALSE, FALSE); /* resets transaction number */

    verify_journal_empty(journal_filename);

    trans_num = 0;

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);

    jrnl_row_major_scan_forward2(/* file_ptr               */ file_ptr,
                                 /* max_index              */ max_index,
                                 /* lag                    */ lag,
                                 /* verbose                */ verbose,
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
                                 /* dirty_unprotects       */ dirty_unprotects,
                                 /* trans_num              */ trans_num);

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);

    flush_journal(cache_ptr);

    if ( update_architypes ) {

        copy_file(journal_filename, testfiles[2]);
    }
    
    if ( file_exists(testfiles[2]) ) {

        verify_journal_contents(journal_filename, testfiles[2]);

    } else {

    	testfile_missing = TRUE;
    }

    flush_cache2(file_ptr, FALSE, FALSE, FALSE); /* resets transaction number */

    trans_num = 0;

    verify_journal_empty(journal_filename);

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);

    jrnl_col_major_scan_forward2(/* file_ptr               */ file_ptr,
                                 /* max_index              */ max_index,
                                 /* lag                    */ lag,
                                 /* verbose                */ verbose,
                                 /* reset_stats            */ TRUE,
                                 /* display_stats          */ display_stats,
                                 /* display_detailed_stats */ TRUE,
                                 /* do_inserts             */ TRUE,
                                 /* dirty_inserts          */ dirty_inserts,
                                 /* dirty_unprotects       */ dirty_unprotects,
                                 /* trans_num              */ trans_num);

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);

    flush_journal(cache_ptr);

    if ( update_architypes ) {

        copy_file(journal_filename, testfiles[3]);
    }
    
    if ( file_exists(testfiles[3]) ) {

        verify_journal_contents(journal_filename, testfiles[3]);

    } else {

    	testfile_missing = TRUE;
    }

    flush_cache2(file_ptr, FALSE, FALSE, FALSE); /* resets transaction number */

    verify_journal_empty(journal_filename);

    trans_num = 0;

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);

    jrnl_col_major_scan_backward2(/* file_ptr               */ file_ptr,
                                  /* max_index              */ max_index,
                                  /* lag                    */ lag,
                                  /* verbose                */ verbose,
                                  /* reset_stats            */ TRUE,
                                  /* display_stats          */ display_stats,
                                  /* display_detailed_stats */ TRUE,
                                  /* do_inserts             */ TRUE,
                                  /* dirty_inserts          */ dirty_inserts,
                                  /* dirty_unprotects       */ dirty_unprotects,
                                  /* trans_num              */ trans_num);

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);

    flush_journal(cache_ptr);

    if ( update_architypes ) {

        copy_file(journal_filename, testfiles[4]);
    }
    
    if ( file_exists(testfiles[4]) ) {

        verify_journal_contents(journal_filename, testfiles[4]);

    } else {

    	testfile_missing = TRUE;
    }

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);

    /****************************************************/
    /* Close and discard the file and the journal file. */
    /****************************************************/

    takedown_cache_after_journaling(file_id, filename, journal_filename, FALSE);

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);

    verify_clean2();
    verify_unprotected2();

    if ( show_progress )
        HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, pass2, cp++);

    if ( pass2 ) { 
	    
        PASSED(); 

	if ( testfile_missing ) {

	    puts("	WARNING: One or more missing test files."); 
	    fflush(stdout);
        }
    } else { 
	    
        H5_FAILED(); 
    }

    if ( ! pass2 ) {

	failures2++;
        HDfprintf(stdout, "%s: failure_mssg2 = \"%s\".\n",
                  fcn_name, failure_mssg2);
    }

    return;

} /* mdj_smoke_check_02() */


/*** metatada journaling config block I/O test code ***/

/*-------------------------------------------------------------------------
 * Function:    check_mdj_config_block_IO()
 *
 * Purpose:     Verify that the functions that read, write, and discard
 *              metadata journaling config blocks operate as they should.
 *
 * Return:      void
 *
 * Programmer:  John Mainzer
 *              3/11/08
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
check_mdj_config_block_IO(void)
{
    const char * fcn_name = "check_mdj_config_block_IO()";
    const char * test_path = "/a/full/path";
    char filename[512];
    hbool_t show_progress = FALSE;
    int cp = 0;
    herr_t result;
    hsize_t block_len = 0;
    haddr_t block_addr = HADDR_UNDEF;
    hid_t fapl_id = -1;
    hid_t file_id = -1;
    H5F_t * file_ptr = NULL;
    H5C2_t * cache_ptr = NULL;

    TESTING("metadata journaling config block I/O");

    pass2 = TRUE;

    /* 1) Open a file
     *
     * 2) Go through several create, read, and discard cycles.  Verify that 
     *    the correct information is read back.  Use a variety of journal 
     *    file path length to ensure that we don't have problems with blocks 
     *    with length some multiple of 4.
     *
     * 3) Create a metadata journaling configuration block.
     *
     * 4) Close the file, and reopen it.
     *
     * 5) Read the metadata journaling configuration block, and verify 
     *    that it contains the expected data.
     *
     * 6) Close and discard the file.
     *
     * Note that we don't do any tests to verify that the config block
     * code fails where expected -- We will do this in a separate test
     * function if at all.
     * 						-- JRM
     */

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    /******************/
    /* 1) Open a file */
    /******************/

    /* setup the file name */
    if ( pass2 ) {

        if ( h5_fixname(FILENAMES[2], H5P_DEFAULT, filename, sizeof(filename))
				            == NULL ) {

            pass2 = FALSE;
            failure_mssg2 = "h5_fixname() failed.\n";
        }
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    /* create a file access propertly list -- this isn't necessary in this
     * case, but it is how we will open the file when we journal, so we do
     * it regardless.
     */
    if ( pass2 ) {

        fapl_id = H5Pcreate(H5P_FILE_ACCESS);

        if ( fapl_id < 0 ) {

            pass2 = FALSE;
            failure_mssg2 = "H5Pcreate() failed.\n";
        }
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    /* call H5Pset_libver_bounds() on the fapl_id */
    if ( pass2 ) {

	if ( H5Pset_libver_bounds(fapl_id, H5F_LIBVER_LATEST, 
				  H5F_LIBVER_LATEST) < 0 ) {

            pass2 = FALSE;
            failure_mssg2 = "H5Pset_libver_bounds() failed.\n";
        }
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    /* create the file using fapl_id */
    if ( pass2 ) {

        file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);

        if ( file_id < 0 ) {

            pass2 = FALSE;
            failure_mssg2 = "H5Fcreate() failed.\n";
        }
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    /* get a pointer to the files internal data structure and then 
     * to the cache structure
     */
    if ( pass2 ) {

        file_ptr = (H5F_t *)H5I_object_verify(file_id, H5I_FILE);

        if ( file_ptr == NULL ) {

            pass2 = FALSE;
            failure_mssg2 = "Can't get file_ptr (1).\n";

        } else if ( file_ptr->shared->cache2 == NULL ) {
	
	    pass2 = FALSE;
	    failure_mssg2 = "can't get cache2 pointer(1).\n";

	} else {

	    cache_ptr = file_ptr->shared->cache2;
	}
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);


    /*************************************************************************/
    /* 2) Go through several create, read, and discard cycles.  Verify that  */
    /*    the correct information is read back.  Use a variety of journal    */
    /*    file path length to ensure that we don't have problems with blocks */
    /*    with length some multiple of 4.                                    */
    /*************************************************************************/

    test_mdj_conf_blk_read_write_discard(file_ptr, "a");

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    test_mdj_conf_blk_read_write_discard(file_ptr, "ab");

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    test_mdj_conf_blk_read_write_discard(file_ptr, "abc");

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    test_mdj_conf_blk_read_write_discard(file_ptr, "abcd");

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    test_mdj_conf_blk_read_write_discard(file_ptr, "abcde");

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    test_mdj_conf_blk_read_write_discard(file_ptr, 
		                         "abcdefghijklmnopqrstuvwxyz");

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);


    /********************************************************/
    /* 3) Create a metadata journaling configuration block. */
    /********************************************************/

    if ( pass2 ) {

	if ( ( cache_ptr->mdj_file_name_ptr != NULL ) ||
	     ( cache_ptr->mdj_conf_block_addr != HADDR_UNDEF ) ||
	     ( cache_ptr->mdj_conf_block_len != 0 ) ||
	     ( cache_ptr->mdj_conf_block_ptr != NULL ) ) {

	    pass2 = FALSE;
	    failure_mssg2 = "Bad cache config on entry.";
	}
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    if ( pass2 ) {
    
        result = H5C2_create_journal_config_block(file_ptr,
			                          H5P_DATASET_XFER_DEFAULT,
						  test_path);
	
	if ( result != SUCCEED ) {

	    pass2 = FALSE;
	    failure_mssg2 = "H5C2_create_journal_config_block() failed.";
	
	} else {

	    block_addr = cache_ptr->mdj_conf_block_addr;
            block_len = cache_ptr->mdj_conf_block_len;

	    if ( cache_ptr->mdj_conf_block_ptr == NULL ) {

		pass2 = FALSE;
		failure_mssg2 = 
			"cache_ptr->mdj_conf_block_ptr == NULL after create.";

	    } else if ( cache_ptr->mdj_file_name_ptr == NULL ) {

		pass2 = FALSE;
		failure_mssg2 = 
			"cache_ptr->mdj_file_name_ptr == NULL after create.";
	
	    } else if ( strcmp(test_path, cache_ptr->mdj_file_name_ptr) 
		        != 0 ) {

		pass2 = FALSE;
		failure_mssg2 = 
			"journal file path mismatch after create.";

	    } else if ( cache_ptr->mdj_conf_block_addr == HADDR_UNDEF ) {

		pass2 = FALSE;
		failure_mssg2 = 
                  "cache_ptr->mdj_conf_block_addr == HADDR_UNDEF after create.";

	    } else if ( cache_ptr->mdj_conf_block_len == 0 ) {

		pass2 = FALSE;
		failure_mssg2 = 
                    "cache_ptr->mdj_conf_block_len == 0 after create.";
	    }
	}
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);


    /*************************************/ 
    /* 4) Close the file, and reopen it. */
    /*************************************/

    /* close the file. */
    if ( pass2 ) {

	if ( H5Fclose(file_id) < 0 ) {

            pass2 = FALSE;
	    failure_mssg2 = "file close failed (1).";
	}
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    /* open the file r/w using the same FAPL */
    if ( pass2 ) {

        file_id = H5Fopen(filename, H5F_ACC_RDWR, fapl_id);

        if ( file_id < 0 ) {

            pass2 = FALSE;
            failure_mssg2 = "H5Fopen() failed (3).\n";
        }
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    /* get a pointer to the files internal data structure and then 
     * to the cache structure
     */
    if ( pass2 ) {

        file_ptr = (H5F_t *)H5I_object_verify(file_id, H5I_FILE);

        if ( file_ptr == NULL ) {

            pass2 = FALSE;
            failure_mssg2 = "Can't get file_ptr (1).\n";

        } else if ( file_ptr->shared->cache2 == NULL ) {
	
	    pass2 = FALSE;
	    failure_mssg2 = "can't get cache2 pointer(1).\n";

	} else {

	    cache_ptr = file_ptr->shared->cache2;
	}
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    /*******************************************************************/
    /* 5) Read the metadata journaling configuration block, and verify */
    /*    that it contains the expected data.                          */
    /*******************************************************************/

    if ( pass2 ) {

        H5MM_xfree(cache_ptr->mdj_conf_block_ptr);

        cache_ptr->mdj_conf_block_addr = HADDR_UNDEF;
        cache_ptr->mdj_conf_block_len = 0;
        cache_ptr->mdj_conf_block_ptr = NULL;
        cache_ptr->mdj_file_name_ptr = NULL;

	result = H5C2_load_journal_config_block(file_ptr,
			                        H5P_DATASET_XFER_DEFAULT,
						cache_ptr,
						block_addr,
						block_len);
	
	if ( result != SUCCEED ) {

	    pass2 = FALSE;
	    failure_mssg2 = "H5C2_load_journal_config_block() failed.";
	
	} else {

	    if ( cache_ptr->mdj_conf_block_ptr == NULL ) {

		pass2 = FALSE;
		failure_mssg2 = 
			"cache_ptr->mdj_conf_block_ptr == NULL after load.";

	    } else if ( cache_ptr->mdj_file_name_ptr == NULL ) {

		pass2 = FALSE;
		failure_mssg2 = 
			"cache_ptr->mdj_file_name_ptr == NULL after load.";
	
	    } else if ( strcmp(test_path, cache_ptr->mdj_file_name_ptr ) 
		        != 0) {

		pass2 = FALSE;
		failure_mssg2 = 
			"journal file path mismatch after load.";

	    } else if ( cache_ptr->mdj_conf_block_addr != block_addr ) {

		pass2 = FALSE;
		failure_mssg2 = 
                    "cache_ptr->mdj_conf_block_addr != block_addr after load.";

	    } else if ( cache_ptr->mdj_conf_block_len != block_len ) {

		pass2 = FALSE;
		failure_mssg2 = 
                    "cache_ptr->mdj_conf_block_len != block_len after load.";
	    }
	}
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    /**********************************/
    /* 6) Close and discard the file. */
    /**********************************/
    
    if ( pass2 ) {

	if ( H5Fclose(file_id) < 0 ) {

            pass2 = FALSE;
	    failure_mssg2 = "file close failed (5).";

        } else if ( HDremove(filename) < 0 ) {

            pass2 = FALSE;
            failure_mssg2 = "HDremove() failed.\n";
        }
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);


    if ( pass2 ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass2 ) {

	failures2++;
        HDfprintf(stdout, "%s: failure_mssg2 = \"%s\".\n",
                  fcn_name, failure_mssg2);
    }

} /* check_mdj_config_block_IO() */


/*-------------------------------------------------------------------------
 * Function:    test_mdj_conf_blk_read_write_discard()
 *
 * Purpose:     Using the supplied cache and journal file path, create 
 * 		a metadata journal configuration block, read it and 
 * 		verify the contents, and then discard it.
 *
 * 		Do nothing if pass2 is false on entry
 *
 * Return:      void
 *
 * Programmer:  John Mainzer
 *              3/13/08
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
test_mdj_conf_blk_read_write_discard(H5F_t * file_ptr,
		                     const char * jrnl_file_path)
{
    const char * fcn_name = "test_mdj_conf_blk_read_write_discard()";
    H5C2_t * cache_ptr = file_ptr->shared->cache2;
    hbool_t show_progress = FALSE;
    int cp = 0;
    herr_t result;
    hsize_t block_len = 0;
    haddr_t block_addr = HADDR_UNDEF;

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    if ( cache_ptr == NULL ) {

        pass2 = FALSE;
        failure_mssg2 = "cache_ptr NULL on entry.";
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    if ( pass2 ) {

	if ( ( cache_ptr->mdj_file_name_ptr != NULL ) ||
	     ( cache_ptr->mdj_conf_block_addr != HADDR_UNDEF ) ||
	     ( cache_ptr->mdj_conf_block_len != 0 ) ||
	     ( cache_ptr->mdj_conf_block_ptr != NULL ) ) {

	    pass2 = FALSE;
	    failure_mssg2 = "Bad cache config on entry.";
	}
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    if ( pass2 ) {
    
        result = H5C2_create_journal_config_block(file_ptr,
			                          H5P_DATASET_XFER_DEFAULT,
						  jrnl_file_path);
	
    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);
	if ( result != SUCCEED ) {

	    pass2 = FALSE;
	    failure_mssg2 = "H5C2_create_journal_config_block() failed.";
	
	} else {

	    block_addr = cache_ptr->mdj_conf_block_addr;
            block_len = cache_ptr->mdj_conf_block_len;

	    if ( cache_ptr->mdj_conf_block_ptr == NULL ) {

		pass2 = FALSE;
		failure_mssg2 = 
			"cache_ptr->mdj_conf_block_ptr == NULL after create.";

	    } else if ( cache_ptr->mdj_file_name_ptr == NULL ) {

		pass2 = FALSE;
		failure_mssg2 = 
			"cache_ptr->mdj_file_name_ptr == NULL after create.";
	
	    } else if ( strcmp(jrnl_file_path, cache_ptr->mdj_file_name_ptr) 
		        != 0 ) {

		pass2 = FALSE;
		failure_mssg2 = 
			"journal file path mismatch after create.";

	    } else if ( cache_ptr->mdj_conf_block_addr == HADDR_UNDEF ) {

		pass2 = FALSE;
		failure_mssg2 = 
                    "cache_ptr->mdj_conf_block_addr == HADDR_UNDEF after create.";

	    } else if ( cache_ptr->mdj_conf_block_len == 0 ) {

		pass2 = FALSE;
		failure_mssg2 = 
                    "cache_ptr->mdj_conf_block_len == 0 after create.";
	    }
	}
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    if ( pass2 ) {

        H5MM_xfree(cache_ptr->mdj_conf_block_ptr);

        cache_ptr->mdj_conf_block_addr = HADDR_UNDEF;
        cache_ptr->mdj_conf_block_len = 0;
        cache_ptr->mdj_conf_block_ptr = NULL;
        cache_ptr->mdj_file_name_ptr = NULL;

	result = H5C2_load_journal_config_block(file_ptr,
			                        H5P_DATASET_XFER_DEFAULT,
						cache_ptr,
						block_addr,
						block_len);
	
	if ( result != SUCCEED ) {

	    pass2 = FALSE;
	    failure_mssg2 = "H5C2_load_journal_config_block() failed.";
	
	} else {

	    if ( cache_ptr->mdj_conf_block_ptr == NULL ) {

		pass2 = FALSE;
		failure_mssg2 = 
			"cache_ptr->mdj_conf_block_ptr == NULL after load.";

	    } else if ( cache_ptr->mdj_file_name_ptr == NULL ) {

		pass2 = FALSE;
		failure_mssg2 = 
			"cache_ptr->mdj_file_name_ptr == NULL after load.";
	
	    } else if ( strcmp(jrnl_file_path, cache_ptr->mdj_file_name_ptr) 
		        != 0) {

		pass2 = FALSE;
		failure_mssg2 = 
			"journal file path mismatch after load.";

	    } else if ( cache_ptr->mdj_conf_block_addr != block_addr ) {

		pass2 = FALSE;
		failure_mssg2 = 
                    "cache_ptr->mdj_conf_block_addr != block_addr after load.";

	    } else if ( cache_ptr->mdj_conf_block_len != block_len ) {

		pass2 = FALSE;
		failure_mssg2 = 
                    "cache_ptr->mdj_conf_block_len != block_len after load.";
	    }
	}
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    if ( pass2 ) {

        result = H5C2_discard_journal_config_block(file_ptr, 
			                           H5P_DATASET_XFER_DEFAULT);
	
	if ( result != SUCCEED ) {

	    pass2 = FALSE;
	    failure_mssg2 = "H5C2_discard_journal_config_block() failed.";
	
	} else {

	    if ( cache_ptr->mdj_conf_block_ptr != NULL ) {

		pass2 = FALSE;
		failure_mssg2 = 
			"cache_ptr->mdj_conf_block_ptr != NULL after discard.";

	    } else if ( cache_ptr->mdj_file_name_ptr != NULL ) {

		pass2 = FALSE;
		failure_mssg2 = 
			"cache_ptr->mdj_file_name_ptr != NULL after discard.";
	
	    } else if ( cache_ptr->mdj_conf_block_addr != HADDR_UNDEF ) {

		pass2 = FALSE;
		failure_mssg2 = 
                    "cache_ptr->mdj_conf_block_addr != HADDR_UNDEF after discard.";

	    } else if ( cache_ptr->mdj_conf_block_len != 0 ) {

		pass2 = FALSE;
		failure_mssg2 = 
                    "cache_ptr->mdj_conf_block_len != 0 after discard.";
	    }
	}
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    return;

} /* test_mdj_conf_blk_read_write_discard() */

/*** super block extension related test code ***/

/*-------------------------------------------------------------------------
 * Function:    check_superblock_extensions()
 *
 * Purpose:     Verify that the super block extensions for tracking 
 * 		operate as they should.
 *
 *              Note that this test code will have to be re-worked
 *              once journaling is fully implemented.
 *
 * Return:      void
 *
 * Programmer:  John Mainzer
 *              2/26/08
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

extern hbool_t H5C2__check_for_journaling;

static void
check_superblock_extensions(void)
{
    const char * fcn_name = "check_superblock_extensions()";
    char filename[512];
    hbool_t show_progress = FALSE;
    haddr_t mdc_jrnl_block_loc = 0x1000;
    hsize_t mdc_jrnl_block_len = 0x100;
    int cp = 0; 
    hid_t fapl_id = -1;
    hid_t file_id = -1;
    hid_t dataset_id = -1;
    hid_t dataspace_id = -1;
    H5F_t * file_ptr = NULL;
    hsize_t dims[2];


    TESTING("superblock extensions");

    pass2 = TRUE;

    /* Verify that the journaling superblock extension performs as 
     * expected.  Note that this test will have to be re-written
     * (or possibly subsumed in another test) once the full journaling
     * code is up and running.
     *
     * For now at least, the test proceeds as follows:
     *
     *  1) create a HDF5 file, and verify that journaling is
     *     listed as being off.
     *
     *  2) create a dataset in the file, and then close the file
     * 
     *  3) Open the file again, and verifiy that journaling is still
     *     listed as being off.
     *
     *  4) Write data to the superblock marking the file as currently
     *     being journaled, and close the file again.
     *
     *  5) Open the file a third time, and verify that the superblock
     *     extension indicates that the file is being journaled.  
     *
     *  6) Reset the journaling information to indicate that the file
     *     is not being journaled, and close the file again.
     *
     *  7) Open the file a fourth time, and verify that the superblock
     *     extension indicates that the file is not being journaled.
     *
     *  8) Write data to the superblock, marking the file as being
     *     journaled.  Now write different data to the superbloc, that 
     *     still marks the file as being journaled.  Close the file. 
     *
     *  9) Re-open the file, and verify that the second write in 8 
     *     above took.
     *
     * 10) Write data to the superblock indicating that journaling is
     *     not in progress.  Close the file.
     *
     * 11) Reopen the file, and verify that journaling is not in 
     *     progress.
     *
     * 12) Close the file and delete it.
     */

    /********************************************************/
    /* 1) create a HDF5 file, and verify that journaling is */
    /*    listed as being off.                              */
    /********************************************************/

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    /* setup the file name */
    if ( pass2 ) {

        if ( h5_fixname(FILENAMES[2], H5P_DEFAULT, filename, sizeof(filename))
				            == NULL ) {

            pass2 = FALSE;
            failure_mssg2 = "h5_fixname() failed.\n";
        }
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    /* create a file access propertly list */
    if ( pass2 ) {

        fapl_id = H5Pcreate(H5P_FILE_ACCESS);

        if ( fapl_id < 0 ) {

            pass2 = FALSE;
            failure_mssg2 = "H5Pcreate() failed.\n";
        }
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    /* call H5Pset_libver_bounds() on the fapl_id */
    if ( pass2 ) {

	if ( H5Pset_libver_bounds(fapl_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0 ) {

            pass2 = FALSE;
            failure_mssg2 = "H5Pset_libver_bounds() failed.\n";
        }
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    /* create the file using fapl_id */
    if ( pass2 ) {

        file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);

        if ( file_id < 0 ) {

            pass2 = FALSE;
            failure_mssg2 = "H5Fcreate() failed.\n";
        }
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    /* get a pointer to the files internal data structure and then 
     * verify that journaling is disabled.
     */
    if ( pass2 ) {

        file_ptr = (H5F_t *)H5I_object_verify(file_id, H5I_FILE);

        if ( file_ptr == NULL ) {

            pass2 = FALSE;
            failure_mssg2 = "Can't get file_ptr (1).\n";

        } else if ( file_ptr->shared->mdc_jrnl_enabled ) {
	
	    pass2 = FALSE;
	    failure_mssg2 = "Journaling enabled on file creation.\n";
	}
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    /************************************************************/
    /* 2) create a dataset in the file, and then close the file */
    /************************************************************/

    if ( pass2 ) {

        dims[0] = 4;
        dims[1] = 6;
        dataspace_id = H5Screate_simple(2, dims, NULL);

	if ( dataspace_id < 0 ) {

	    pass2 = FALSE;
	    failure_mssg2 = "H5Screate_simple() failed.";
        }
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    if ( pass2 ) {

        /* Create the dataset. */
        dataset_id = H5Dcreate2(file_id, "/dset", H5T_STD_I32BE, dataspace_id,
                               H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

	if ( dataspace_id < 0 ) {

	    pass2 = FALSE;
	    failure_mssg2 = "H5Dcreate2() failed.";
        }
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    if ( pass2 ) {

        /* close the data set, the data space, and the file */
	if ( ( H5Dclose(dataset_id) < 0 ) ||
	     ( H5Sclose(dataspace_id) < 0 ) ||
	     ( H5Fclose(file_id) < 0 ) ) {

            pass2 = FALSE;
	    failure_mssg2 = "data set, data space, or file close failed.";
	}
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    /****************************************************************/
    /* 3) Open the file again, and verifiy that journaling is still */
    /*    listed as being off.                                      */
    /****************************************************************/

    /* open the file r/w using the default FAPL */
    if ( pass2 ) {

        file_id = H5Fopen(filename, H5F_ACC_RDWR, H5P_DEFAULT);

        if ( file_id < 0 ) {

            pass2 = FALSE;
            failure_mssg2 = "H5Fopen() failed (4).\n";
        }
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    /* get a pointer to the files internal data structure and then 
     * verify that journaling is disabled.
     */
    if ( pass2 ) {

        file_ptr = (H5F_t *)H5I_object_verify(file_id, H5I_FILE);

        if ( file_ptr == NULL ) {

            pass2 = FALSE;
            failure_mssg2 = "Can't get file_ptr (2).\n";

        } else if ( file_ptr->shared->mdc_jrnl_enabled ) {
	
	    pass2 = FALSE;
	    failure_mssg2 = "Journaling enabled on file open (1).\n";
	}
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    /*****************************************************************/
    /* 4) Write data to the superblock marking the file as currently */
    /*    being journaled, and close the file again.                 */
    /*****************************************************************/

    /* At present, we just write the super block regardless if the 
     * file is opened read/write.  This is ugly, but that is how it
     * is for now.  Thus just go in and modify the journaling fields
     * of the super block to taste.
     */

    if ( pass2 ) {

        file_ptr->shared->mdc_jrnl_enabled = TRUE;
        file_ptr->shared->mdc_jrnl_block_loc = mdc_jrnl_block_loc;
        file_ptr->shared->mdc_jrnl_block_len = mdc_jrnl_block_len;

	if ( H5F_super_write_mdj_msg(file_ptr, -1) < 0 ) {

            pass2 = FALSE;
	    failure_mssg2 = "H5F_super_write_mdj_msg failed (1).";
	}
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    /* close the file again. */
    if ( pass2 ) {

	if ( H5Fclose(file_id) < 0 ) {

            pass2 = FALSE;
	    failure_mssg2 = "file close failed (1).";
	}
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);


    /*****************************************************************/
    /* 5) Open the file a third time, and verify that the superblock */
    /*    extension indicates that the file is being journaled.      */
    /*****************************************************************/

    /* open the file r/w using the default FAPL -- turn off journaling
     * in progress check during the open.
     * */
    if ( pass2 ) {

	H5C2__check_for_journaling = FALSE;
        file_id = H5Fopen(filename, H5F_ACC_RDWR, H5P_DEFAULT);
	H5C2__check_for_journaling = TRUE;

        if ( file_id < 0 ) {

            pass2 = FALSE;
            failure_mssg2 = "H5Fopen() failed (5).\n";
        }
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    /* get a pointer to the files internal data structure and then 
     * verify that journaling is enabled.
     */
    if ( pass2 ) {

        file_ptr = (H5F_t *)H5I_object_verify(file_id, H5I_FILE);

        if ( file_ptr == NULL ) {

            pass2 = FALSE;
            failure_mssg2 = "Can't get file_ptr (3).\n";

        } else if ( ! file_ptr->shared->mdc_jrnl_enabled ) {
	
	    pass2 = FALSE;
	    failure_mssg2 = "Journaling disabled on file open (1).\n";

	} else if ( file_ptr->shared->mdc_jrnl_block_loc != 
			mdc_jrnl_block_loc ) {
	
	    pass2 = FALSE;
	    HDfprintf(stdout, "%s: block_loc = %ld (%ld).\n",
		      fcn_name, (long)(file_ptr->shared->mdc_jrnl_block_loc),
		      (long)(mdc_jrnl_block_loc));
	    failure_mssg2 = "unexpected mdc_jrnl_block_loc(1).\n";

	} else if ( file_ptr->shared->mdc_jrnl_block_len != 
		    (hsize_t)mdc_jrnl_block_len ) {
	
	    pass2 = FALSE;
	    failure_mssg2 = "unexpected mdc_jrnl_block_len (1).\n";

        }
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    /*****************************************************************/
    /* 6) Reset the journaling information to indicate that the file */
    /*    is not being journaled, and close the file again.          */
    /*****************************************************************/

    if ( pass2 ) {

	file_ptr->shared->mdc_jrnl_enabled = FALSE;

	if ( H5F_super_write_mdj_msg(file_ptr, -1) < 0 ) {

            pass2 = FALSE;
	    failure_mssg2 = "H5F_super_write_mdj_msg failed (2).";
	}
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    /* close the file again. */
    if ( pass2 ) {

	if ( H5Fclose(file_id) < 0 ) {

            pass2 = FALSE;
	    failure_mssg2 = "file close failed (2).";
	}
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);
    

    /******************************************************************/
    /* 7) Open the file a fourth time, and verify that the superblock */
    /*    extension indicates that the file is not being journaled.   */
    /*******************************************************************/

    /* open the file r/w using the default FAPL */
    if ( pass2 ) {

        file_id = H5Fopen(filename, H5F_ACC_RDWR, H5P_DEFAULT);

        if ( file_id < 0 ) {

            pass2 = FALSE;
            failure_mssg2 = "H5Fopen() failed (6).\n";
        }
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    /* get a pointer to the files internal data structure and then 
     * verify that journaling is disabled.
     */
    if ( pass2 ) {

        file_ptr = (H5F_t *)H5I_object_verify(file_id, H5I_FILE);

        if ( file_ptr == NULL ) {

            pass2 = FALSE;
            failure_mssg2 = "Can't get file_ptr (4).\n";

        } else if ( file_ptr->shared->mdc_jrnl_enabled ) {
	
	    pass2 = FALSE;
	    failure_mssg2 = "Journaling enabled on file open (2).\n";
	}
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);


    /*******************************************************************/
    /*  8) Write data to the superblock, marking the file as being     */
    /*     journaled.  Now write different data to the superbloc, that */
    /*     still marks the file as being journaled.  Close the file.   */
    /*******************************************************************/

    if ( pass2 ) {

        file_ptr->shared->mdc_jrnl_enabled = TRUE;
        file_ptr->shared->mdc_jrnl_block_loc = mdc_jrnl_block_loc * 2;
        file_ptr->shared->mdc_jrnl_block_len = mdc_jrnl_block_len * 2;

	if ( H5F_super_write_mdj_msg(file_ptr, -1) < 0 ) {

            pass2 = FALSE;
	    failure_mssg2 = "H5F_super_write_mdj_msg failed (3).";
	}
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    if ( pass2 ) {

        file_ptr->shared->mdc_jrnl_enabled = TRUE;
        file_ptr->shared->mdc_jrnl_block_loc = mdc_jrnl_block_loc / 2;
        file_ptr->shared->mdc_jrnl_block_len = mdc_jrnl_block_len / 2;

	if ( H5F_super_write_mdj_msg(file_ptr, -1) < 0 ) {

            pass2 = FALSE;
	    failure_mssg2 = "H5F_super_write_mdj_msg failed (4).";
	}
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    /* close the file again. */
    if ( pass2 ) {

	if ( H5Fclose(file_id) < 0 ) {

            pass2 = FALSE;
	    failure_mssg2 = "file close failed (3).";
	}
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    
    /***************************************************************/
    /*  9) Re-open the file, and verify that the second write in 8 */
    /*     above took.                                             */
    /***************************************************************/

    /* open the file r/w using the default FAPL -- turn off journaling
     * in progress check during the open.
     */
    if ( pass2 ) {

	H5C2__check_for_journaling = FALSE;
        file_id = H5Fopen(filename, H5F_ACC_RDWR, H5P_DEFAULT);
	H5C2__check_for_journaling = TRUE;

        if ( file_id < 0 ) {

            pass2 = FALSE;
            failure_mssg2 = "H5Fopen() failed (7).\n";
        }
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    /* get a pointer to the files internal data structure and then 
     * verify that journaling is enabled.
     */
    if ( pass2 ) {

        file_ptr = (H5F_t *)H5I_object_verify(file_id, H5I_FILE);

        if ( file_ptr == NULL ) {

            pass2 = FALSE;
            failure_mssg2 = "Can't get file_ptr (5).\n";

        } else if ( ! file_ptr->shared->mdc_jrnl_enabled ) {
	
	    pass2 = FALSE;
	    failure_mssg2 = "Journaling disabled on file open (2).\n";

	} else if ( file_ptr->shared->mdc_jrnl_block_loc != 
			mdc_jrnl_block_loc / 2 ) {
	
	    pass2 = FALSE;
	    HDfprintf(stdout, "%s: block_loc = %ld (%ld).\n",
		      fcn_name, (long)(file_ptr->shared->mdc_jrnl_block_loc),
		      (long)(mdc_jrnl_block_loc));
	    failure_mssg2 = "unexpected mdc_jrnl_block_loc(2).\n";

	} else if ( file_ptr->shared->mdc_jrnl_block_len != 
		    (hsize_t)mdc_jrnl_block_len / 2 ) {
	
	    pass2 = FALSE;
	    failure_mssg2 = "unexpected mdc_jrnl_block_len (2).\n";

        }
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);


    /******************************************************************/
    /* 10) Write data to the superblock indicating that journaling is */
    /*     not in progress.  Close the file.                          */
    /******************************************************************/

    if ( pass2 ) {

	file_ptr->shared->mdc_jrnl_enabled = FALSE;

	if ( H5F_super_write_mdj_msg(file_ptr, -1) < 0 ) {

            pass2 = FALSE;
	    failure_mssg2 = "H5F_super_write_mdj_msg failed (5).";
	}
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    /* close the file again. */
    if ( pass2 ) {

	if ( H5Fclose(file_id) < 0 ) {

            pass2 = FALSE;
	    failure_mssg2 = "file close failed (4).";
	}
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);


    /*************************************************************/
    /* 11) Reopen the file, and verify that journaling is not in */
    /*     progress.                                             */
    /*************************************************************/

    /* open the file r/w using the default FAPL */
    if ( pass2 ) {

        file_id = H5Fopen(filename, H5F_ACC_RDWR, H5P_DEFAULT);

        if ( file_id < 0 ) {

            pass2 = FALSE;
            failure_mssg2 = "H5Fopen() failed (8).\n";
        }
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    /* get a pointer to the files internal data structure and then 
     * verify that journaling is disabled.
     */
    if ( pass2 ) {

        file_ptr = (H5F_t *)H5I_object_verify(file_id, H5I_FILE);

        if ( file_ptr == NULL ) {

            pass2 = FALSE;
            failure_mssg2 = "Can't get file_ptr (6).\n";

        } else if ( file_ptr->shared->mdc_jrnl_enabled ) {
	
	    pass2 = FALSE;
	    failure_mssg2 = "Journaling enabled on file open (3).\n";
	}
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);


    /*************************************/
    /* 12) Close the file and delete it. */
    /*************************************/
    
    if ( pass2 ) {

	if ( H5Fclose(file_id) < 0 ) {

            pass2 = FALSE;
	    failure_mssg2 = "file close failed (5).";

        } else if ( HDremove(filename) < 0 ) {

            pass2 = FALSE;
            failure_mssg2 = "HDremove() failed.\n";
        }
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    if ( pass2 ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass2 ) {

	failures2++;
        HDfprintf(stdout, "%s: failure_mssg2 = \"%s\".\n",
                  fcn_name, failure_mssg2);
    }

} /* check_superblock_extensions() */


/*** Check metadata journaling file marking ***/

/***************************************************************************
 * Function: 	check_mdj_file_marking
 *
 * Purpose:  	Verify that HDF5 file is marked as having journaling 
 *              in progress when journaling is enabled, and that it is 
 *              unmarked when journaling is shut down, or when the file
 *              is closed normally.
 *
 *              Do nothing if pass2 is false on entry.
 *
 *              On failure, set pass2 to false, and failure_mssg2 to an
 *              appropriate error string.
 *
 * Return:      void
 *
 * Programmer: 	John Mainzer
 *              7/2/08
 * 
 **************************************************************************/

/* xyzzy */

static void 
check_mdj_file_marking(void)
{
    const char * fcn_name = "check_mdj_file_marking():";

    TESTING("metadata journaling file marking");

    verify_mdj_file_marking_on_create();

    verify_mdj_file_marking_on_open();

    verify_mdj_file_marking_after_open();

    verify_mdj_file_unmarking_on_file_close();

    verify_mdj_file_unmarking_on_journaling_shutdown();

    verify_mdj_file_unmarking_on_recovery();

    if ( pass2 ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass2 ) {

	failures2++;
        HDfprintf(stdout, "%s: failure_mssg2 = \"%s\".\n",
                  fcn_name, failure_mssg2);
    }

    return;

} /* check_mdj_file_marking() */


/***************************************************************************
 * Function: 	verify_mdj_file_marking_on_create
 *
 * Purpose:  	Verify that HDF5 file is marked as having journaling 
 *              in progress when journaling is enabled at file creation
 *              time.
 *
 *              Do this by forking a child process, creating a test file
 *              in the child with metadata journaling enabled, and then
 *              exiting from the child without closing the file.
 *
 *              When the child exits, try to open the child's test file.
 *              Open should fail, as the file should be marked as journaling
 *              in progress.
 *
 *              On either success or failure, clean up the test file and
 *              the associated journal file.
 *
 * Return:      void
 *
 * Programmer: 	John Mainzer
 *              7/2/08
 * 
 **************************************************************************/

static void 
verify_mdj_file_marking_on_create(void)
{
    const char * fcn_name = "verify_mdj_file_marking_on_create():";
    const char * tag = "pre-fork:";
    char filename[512];
    char journal_filename[H5AC2__MAX_JOURNAL_FILE_NAME_LEN + 1];
    hbool_t child = FALSE;
    hbool_t show_progress = FALSE;
    hbool_t verbose = FALSE;
    int cp = 0;
    int wait_status;
    uint64_t trans_num;
    pid_t child_id;
    pid_t wait_result;
    hid_t file_id = -1;
    hid_t fapl_id = -1;
    H5F_t * file_ptr = NULL;
    H5C2_t * cache_ptr = NULL;

    /* setup the file name */
    if ( pass2 ) {

        if ( h5_fixname(FILENAMES[1], H5P_DEFAULT, filename, 
	                sizeof(filename)) == NULL ) {

            pass2 = FALSE;
            failure_mssg2 = "h5_fixname() failed (1).\n";
        }
    }

    if ( show_progress ) {

        HDfprintf(stdout, "%s%s%d cp = %d.\n", fcn_name, tag,  pass2, cp++);
	HDfflush(stdout);
    }

    if ( verbose ) { 
        HDfprintf(stdout, "%s%s filename = \"%s\".\n", fcn_name, tag,filename); 
	HDfflush(stdout);
    }

    /* setup the journal file name */
    if ( pass2 ) {

        if ( h5_fixname(FILENAMES[3], H5P_DEFAULT, journal_filename, 
                        sizeof(journal_filename)) == NULL ) {

            pass2 = FALSE;
            failure_mssg2 = "h5_fixname() failed (2).\n";
        }
        else if ( strlen(journal_filename) >= 
			H5AC2__MAX_JOURNAL_FILE_NAME_LEN ) {

            pass2 = FALSE;
            failure_mssg2 = "journal file name too long.\n";
        }
    }

    if ( show_progress ) {

        HDfprintf(stdout, "%s%s%d cp = %d.\n", 
		  fcn_name, tag, (int)pass2, cp++);
	HDfflush(stdout);
    }

    if ( verbose ) { 
        HDfprintf(stdout, "%s%s journal filename = \"%s\".\n", 
	          fcn_name, tag, journal_filename); 
	HDfflush(stdout);
    }

    if ( pass2 ) {

        child_id = HDfork();

        if ( child_id == -1 ) {

            pass2 = FALSE;
	    failure_mssg2 = "H5fork() failed.";

	} else if ( child_id == 0 ) {

	    child = TRUE;
	    tag = "child:";

	} else {

            child = FALSE;
	    tag = "parent:";
	}
    }

    if ( pass2 ) {

        if ( show_progress ) {

	    HDfprintf(stdout, "%s%s%d: cp = %d fork() succeeded.\n", 
		      fcn_name, tag, (int)pass2, cp++);
	    HDfflush(stdout);
	}

	if ( child ) {

            if ( show_progress ) {

	        HDfprintf(stdout, "%s%s%d: cp = %d child starting.\n",
		          fcn_name, tag, (int)pass2, cp++);
		HDfflush(stdout);
	    }

            /* clean out any existing journal file */
            HDremove(journal_filename);
            setup_cache_for_journaling(filename, journal_filename, &file_id,
                                       &file_ptr, &cache_ptr, FALSE);

            if ( show_progress ) {

	        HDfprintf(stdout, "%s%s%d: cp = %d.\n",
		          fcn_name, tag, (int)pass2, cp++);
		HDfflush(stdout);
	    }

	    /* run a dummy transaction to fource metadata journaling
	     * initialization.
	     */
	    H5C2_begin_transaction(cache_ptr, &trans_num, "dummy");
	    H5C2_end_transaction(file_ptr, cache_ptr, trans_num, "dummy");

            if ( show_progress ) {

	        HDfprintf(stdout, "%s%s%d: cp = %d.\n",
		          fcn_name, tag, (int)pass2, cp++);
		HDfflush(stdout);
	    }

            if ( ( verbose ) && ( ! pass2 ) ) {
                HDfprintf(stdout, "%s%s%d failure_mssg = \"%s\".\n", 
			  fcn_name, tag, pass2, failure_mssg2);
		HDfflush(stdout);
            } 

            if ( show_progress ) {

	        HDfprintf(stdout, "%s%s%d cp = %d child exiting.\n", 
			  fcn_name, tag, (int)pass2, cp++);
		HDfflush(stdout);
	    }

	    abort();

	} else {

	    /* wait for the child to exit */
	    wait_result = HDwaitpid(child_id, &wait_status, 0);

	    HDsleep(3);

            if ( wait_result != child_id ) {

	        pass2 = FALSE;
		failure_mssg2 = "unexpected waitpid() result.";

		if ( show_progress ) {

	            HDfprintf(stdout, 
			      "%s%s:%d: wait_result = %ld, wait_status = %d.\n", 
			      fcn_name, tag, (int)pass2, 
			      (long)wait_result, wait_status);
		    HDfflush(stdout);
	        }
            } else {

		if ( show_progress ) {

	            HDfprintf(stdout, 
			      "%s%s:%d: cp = %d  child exited as expected.\n", 
			      fcn_name, tag, (int)pass2, cp++);
		    HDfflush(stdout);
	        }

                /* create a file access propertly list. */
                if ( pass2 ) {

                    fapl_id = H5Pcreate(H5P_FILE_ACCESS);

                    if ( fapl_id < 0 ) {

                        pass2 = FALSE;
                        failure_mssg2 = "H5Pcreate() failed.\n";
                    }
                }

                if ( show_progress ) {
		    HDfprintf(stdout, "%s%s:%d cp = %d.\n", 
			      fcn_name, tag, (int)pass2, cp++);
		    HDfflush(stdout);
	        }

                /* call H5Pset_libver_bounds() on the fapl_id */
                if ( pass2 ) {

	            if ( H5Pset_libver_bounds(fapl_id, H5F_LIBVER_LATEST, 
					      H5F_LIBVER_LATEST) < 0 ) {

                        pass2 = FALSE;
                        failure_mssg2 = "H5Pset_libver_bounds() failed.\n";
                    }
                }

                if ( show_progress ) {
		    HDfprintf(stdout, "%s%s%d cp = %d.\n", 
			      fcn_name, tag, (int)pass2, cp++);
		    HDfflush(stdout);
	        }

		/* attempt to open the file -- should fail as the child
		 * exited without closing the file properly, and thus 
		 * the file should still be marked as having journaling
		 * in progress.
		 */

		if ( pass2 ) { 

		    H5E_BEGIN_TRY {

		        file_id = H5Fopen(filename, H5F_ACC_RDWR, fapl_id);

		    } H5E_END_TRY;

		    if ( file_id >= 0 ) {

                        pass2 = FALSE;
		        failure_mssg2 = "H5Fopen() succeeded - 1.";
		    }
		}

                if ( show_progress ) {

                    HDfprintf(stdout, "%s%s%d: cp = %d.\n", 
                              fcn_name, tag, (int)pass2, cp++);
		    HDfflush(stdout);
	        }

                /* delete the HDF5 file and journal file */
#if 1
                HDremove(filename);
                HDremove(journal_filename);
#endif
		if ( show_progress ) {

	            HDfprintf(stdout, "%s%s%d cp = %d parent done.\n", 
			      fcn_name, tag, (int)pass2, cp++);
		    HDfflush(stdout);
		}
	    }
        }
    } 

    return;

} /* verify_mdj_file_marking_on_create() */


/***************************************************************************
 * Function: 	verify_mdj_file_marking_after_open
 *
 * Purpose:  	Verify that HDF5 file is marked as having journaling 
 *              in progress when journaling is enabled on an open file.
 *
 *              Do this by forking a child process and: 
 *
 *              1) creating a test file in the child, 
 *
 *              2) writing some data to it, 
 *
 *              3) enable journaling on the open file via a call to
 *                 H5Fset_mdc_config()
 *
 *              4) exiting from the child without closing the file.
 *
 *              When the child exits, try to open the child's test file.
 *              Open should fail, as the file should be marked as journaling
 *              in progress.
 *
 *              On either success or failure, clean up the test file and
 *              the associated journal file.
 *
 * Return:      void
 *
 * Programmer: 	John Mainzer
 *              7/9/08
 * 
 **************************************************************************/

static void 
verify_mdj_file_marking_after_open(void)
{
    const char * fcn_name = "verify_mdj_file_marking_after_open():";
    const char * tag = "pre-fork:";
    char filename[512];
    char journal_filename[H5AC2__MAX_JOURNAL_FILE_NAME_LEN + 1];
    hbool_t child = FALSE;
    hbool_t show_progress = FALSE;
    hbool_t verbose = FALSE;
    herr_t result;
    int cp = 0;
    int wait_status;
    pid_t child_id;
    pid_t wait_result;
    hid_t file_id = -1;
    hid_t fapl_id = -1;
    hid_t dataset_id = -1;
    hid_t dataspace_id = -1;
    hsize_t dims[2];
    H5AC2_jnl_config_t jnl_config;

    /* setup the file name */
    if ( pass2 ) {

        if ( h5_fixname(FILENAMES[1], H5P_DEFAULT, filename, 
	                sizeof(filename)) == NULL ) {

            pass2 = FALSE;
            failure_mssg2 = "h5_fixname() failed (1).\n";
        }
    }

    if ( show_progress ) {

        HDfprintf(stdout, "%s%s%d cp = %d.\n", fcn_name, tag,  pass2, cp++);
	HDfflush(stdout);
    }

    if ( verbose ) { 
        HDfprintf(stdout, "%s%s filename = \"%s\".\n", fcn_name, tag,filename); 
	HDfflush(stdout);
    }

    /* setup the journal file name */
    if ( pass2 ) {

        if ( h5_fixname(FILENAMES[3], H5P_DEFAULT, journal_filename, 
                        sizeof(journal_filename)) == NULL ) {

            pass2 = FALSE;
            failure_mssg2 = "h5_fixname() failed (2).\n";
        }
        else if ( strlen(journal_filename) >= 
			H5AC2__MAX_JOURNAL_FILE_NAME_LEN ) {

            pass2 = FALSE;
            failure_mssg2 = "journal file name too long.\n";
        }
    }

    if ( show_progress ) {

        HDfprintf(stdout, "%s%s%d cp = %d.\n", 
		  fcn_name, tag, (int)pass2, cp++);
	HDfflush(stdout);
    }

    if ( verbose ) { 
        HDfprintf(stdout, "%s%s journal filename = \"%s\".\n", 
	          fcn_name, tag, journal_filename); 
	HDfflush(stdout);
    }

    if ( pass2 ) {

        child_id = HDfork();

        if ( child_id == -1 ) {

            pass2 = FALSE;
	    failure_mssg2 = "H5fork() failed.";

	} else if ( child_id == 0 ) {

	    child = TRUE;
	    tag = "child:";

	} else {

            child = FALSE;
	    tag = "parent:";
	}
    }

    if ( pass2 ) {

	/* reset the check point */
	cp = 0;

        if ( show_progress ) {

	    HDfprintf(stdout, "%s%s%d: cp = %d fork() succeeded.\n", 
		      fcn_name, tag, (int)pass2, cp++);
	    HDfflush(stdout);
	}

	if ( child ) {

            if ( show_progress ) {

	        HDfprintf(stdout, "%s%s%d: cp = %d child starting.\n",
		          fcn_name, tag, (int)pass2, cp++);
		HDfflush(stdout);
	    }

            /* create a file access propertly list. */
            if ( pass2 ) {

                fapl_id = H5Pcreate(H5P_FILE_ACCESS);

                if ( fapl_id < 0 ) {

                    pass2 = FALSE;
                    failure_mssg2 = "H5Pcreate() failed.\n";
                }
            }

            if ( show_progress ) {

	        HDfprintf(stdout, "%s%s%d: cp = %d.\n",
		          fcn_name, tag, (int)pass2, cp++);
		HDfflush(stdout);
	    }

            /* call H5Pset_libver_bounds() on the fapl_id */
            if ( pass2 ) {

	        if ( H5Pset_libver_bounds(fapl_id, H5F_LIBVER_LATEST, 
					  H5F_LIBVER_LATEST) < 0 ) {

                    pass2 = FALSE;
                    failure_mssg2 = "H5Pset_libver_bounds() failed.\n";
                }
            }

	    /* open the file with a fapl indicating latest version of 
	     * the file format.
	     */
            if ( pass2 ) {

                file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, 
				    fapl_id);

                if ( file_id < 0 ) {

	            pass2 = FALSE;
                    failure_mssg2 = "H5Fcreate() failed.\n";
	        }
	    }

            if ( show_progress ) {

	        HDfprintf(stdout, "%s%s%d: cp = %d.\n",
		          fcn_name, tag, (int)pass2, cp++);
		HDfflush(stdout);
	    }

            if ( pass2 ) {

                dims[0] = 4;
                dims[1] = 6;
                dataspace_id = H5Screate_simple(2, dims, NULL);

	        if ( dataspace_id < 0 ) {

	            pass2 = FALSE;
	            failure_mssg2 = "H5Screate_simple() failed.";
                }
            }

            if ( show_progress ) {

	        HDfprintf(stdout, "%s%s%d: cp = %d.\n",
		          fcn_name, tag, (int)pass2, cp++);
		HDfflush(stdout);
	    }

            if ( pass2 ) {

                /* Create the dataset. */
                dataset_id = H5Dcreate2(file_id, "/dset", H5T_STD_I32BE, 
				        dataspace_id, H5P_DEFAULT, 
					H5P_DEFAULT, H5P_DEFAULT);

	        if ( dataspace_id < 0 ) {

	            pass2 = FALSE;
	            failure_mssg2 = "H5Dcreate2() failed.";
                }
            }

            if ( show_progress ) {

	        HDfprintf(stdout, "%s%s%d: cp = %d.\n",
		          fcn_name, tag, (int)pass2, cp++);
		HDfflush(stdout);
	    }

	    /* now enable journaling */
	    if ( pass2 ) {

                jnl_config.version = H5AC2__CURR_JNL_CONFIG_VER;

                result = H5Fget_jnl_config(file_id, &jnl_config);

                if ( result < 0 ) {

                    pass2 = FALSE;
                    failure_mssg2 = "H5Fget_jnl_config() failed.\n";
                }

	        /* set journaling config fields to taste */
                jnl_config.enable_journaling       = TRUE;

                strcpy(jnl_config.journal_file_path, journal_filename);

                jnl_config.journal_recovered       = FALSE;
                jnl_config.jbrb_buf_size           = (8 * 1024);
                jnl_config.jbrb_num_bufs           = 2;
                jnl_config.jbrb_use_aio            = FALSE;
                jnl_config.jbrb_human_readable     = TRUE;
            }

            if ( show_progress ) {

	        HDfprintf(stdout, "%s%s%d: cp = %d.\n",
		          fcn_name, tag, (int)pass2, cp++);
		HDfflush(stdout);
	    }

            if ( pass2 ) {

                result = H5Fset_jnl_config(file_id, &jnl_config);

                if ( result < 0 ) {

                    pass2 = FALSE;
                    failure_mssg2 = "H5Fset_jnl_config() failed.\n";
                }
            }

            if ( show_progress ) {

	        HDfprintf(stdout, "%s%s%d: cp = %d.\n",
		          fcn_name, tag, (int)pass2, cp++);
		HDfflush(stdout);
	    }

            if ( ( verbose ) && ( ! pass2 ) ) {
                HDfprintf(stdout, "%s%s%d failure_mssg = \"%s\".\n", 
			  fcn_name, tag, pass2, failure_mssg2);
		HDfflush(stdout);
            } 

            if ( show_progress ) {

	        HDfprintf(stdout, "%s%s%d cp = %d child exiting.\n", 
			  fcn_name, tag, (int)pass2, cp++);
		HDfflush(stdout);
	    }

	    abort();

	} else {

	    /* wait for the child to exit */
	    wait_result = HDwaitpid(child_id, &wait_status, 0);

	    HDsleep(3);

            if ( wait_result != child_id ) {

	        pass2 = FALSE;
		failure_mssg2 = "unexpected waitpid() result.";

		if ( show_progress ) {

	            HDfprintf(stdout, 
			      "%s%s:%d: wait_result = %ld, wait_status = %d.\n", 
			      fcn_name, tag, (int)pass2, 
			      (long)wait_result, wait_status);
		    HDfflush(stdout);
	        }
            } else {

		if ( show_progress ) {

	            HDfprintf(stdout, 
			      "%s%s:%d: cp = %d  child exited as expected.\n", 
			      fcn_name, tag, (int)pass2, cp++);
		    HDfflush(stdout);
	        }

                /* create a file access propertly list. */
                if ( pass2 ) {

                    fapl_id = H5Pcreate(H5P_FILE_ACCESS);

                    if ( fapl_id < 0 ) {

                        pass2 = FALSE;
                        failure_mssg2 = "H5Pcreate() failed.\n";
                    }
                }

                if ( show_progress ) {
		    HDfprintf(stdout, "%s%s:%d cp = %d.\n", 
			      fcn_name, tag, (int)pass2, cp++);
		    HDfflush(stdout);
	        }

                /* call H5Pset_libver_bounds() on the fapl_id */
                if ( pass2 ) {

	            if ( H5Pset_libver_bounds(fapl_id, H5F_LIBVER_LATEST, 
					      H5F_LIBVER_LATEST) < 0 ) {

                        pass2 = FALSE;
                        failure_mssg2 = "H5Pset_libver_bounds() failed.\n";
                    }
                }

                if ( show_progress ) {
		    HDfprintf(stdout, "%s%s%d cp = %d.\n", 
			      fcn_name, tag, (int)pass2, cp++);
		    HDfflush(stdout);
	        }

		/* attempt to open the file -- should fail as the child
		 * exited without closing the file properly, and thus 
		 * the file should still be marked as having journaling
		 * in progress.
		 */

		if ( pass2 ) {
  
		    H5E_BEGIN_TRY {

		        file_id = H5Fopen(filename, H5F_ACC_RDWR, fapl_id);

		    } H5E_END_TRY;

		    if ( file_id >= 0 ) {

                        pass2 = FALSE;
		        failure_mssg2 = "H5Fopen() succeeded - 2.";
		    }
		}

                if ( show_progress ) {

                    HDfprintf(stdout, "%s%s%d: cp = %d.\n", 
                              fcn_name, tag, (int)pass2, cp++);
		    HDfflush(stdout);
	        }

                /* delete the HDF5 file and journal file */
#if 1
                HDremove(filename);
                HDremove(journal_filename);
#endif
		if ( show_progress ) {

	            HDfprintf(stdout, "%s%s%d cp = %d parent done.\n", 
			      fcn_name, tag, (int)pass2, cp++);
		    HDfflush(stdout);
		}
	    }
        }
    } 

    return;

} /* verify_mdj_file_marking_after_open() */


/***************************************************************************
 * Function: 	verify_mdj_file_marking_on_open
 *
 * Purpose:  	Verify that HDF5 file is marked as having journaling 
 *              in progress when journaling is enabled at file open
 *              time.
 *
 *              Do this by forking a child process and: 
 *
 *              1) creating a test file in the child, 
 *
 *              2) writing some data to it, 
 *
 *              3) closing the test file.
 *
 *              4) re-openting the test file with metadata journaling 
 *                 enabled, and then
 *
 *              5) exiting from the child without closing the file.
 *
 *              When the child exits, try to open the child's test file.
 *              Open should fail, as the file should be marked as journaling
 *              in progress.
 *
 *              On either success or failure, clean up the test file and
 *              the associated journal file.
 *
 * Return:      void
 *
 * Programmer: 	John Mainzer
 *              7/2/08
 * 
 **************************************************************************/

static void 
verify_mdj_file_marking_on_open(void)
{
    const char * fcn_name = "verify_mdj_file_marking_on_open():";
    const char * tag = "pre-fork:";
    char filename[512];
    char journal_filename[H5AC2__MAX_JOURNAL_FILE_NAME_LEN + 1];
    hbool_t child = FALSE;
    hbool_t show_progress = FALSE;
    hbool_t verbose = FALSE;
    herr_t result;
    int cp = 0;
    int wait_status;
    pid_t child_id;
    pid_t wait_result;
    hid_t file_id = -1;
    hid_t fapl_id = -1;
    hid_t dataset_id = -1;
    hid_t dataspace_id = -1;
    hsize_t dims[2];
    H5F_t * file_ptr = NULL;
    H5AC2_jnl_config_t jnl_config;

    /* setup the file name */
    if ( pass2 ) {

        if ( h5_fixname(FILENAMES[1], H5P_DEFAULT, filename, 
	                sizeof(filename)) == NULL ) {

            pass2 = FALSE;
            failure_mssg2 = "h5_fixname() failed (1).\n";
        }
    }

    if ( show_progress ) {

        HDfprintf(stdout, "%s%s%d cp = %d.\n", fcn_name, tag,  pass2, cp++);
	HDfflush(stdout);
    }

    if ( verbose ) { 
        HDfprintf(stdout, "%s%s filename = \"%s\".\n", fcn_name, tag,filename); 
	HDfflush(stdout);
    }

    /* setup the journal file name */
    if ( pass2 ) {

        if ( h5_fixname(FILENAMES[3], H5P_DEFAULT, journal_filename, 
                        sizeof(journal_filename)) == NULL ) {

            pass2 = FALSE;
            failure_mssg2 = "h5_fixname() failed (2).\n";
        }
        else if ( strlen(journal_filename) >= 
			H5AC2__MAX_JOURNAL_FILE_NAME_LEN ) {

            pass2 = FALSE;
            failure_mssg2 = "journal file name too long.\n";
        }
    }

    if ( show_progress ) {

        HDfprintf(stdout, "%s%s%d cp = %d.\n", 
		  fcn_name, tag, (int)pass2, cp++);
	HDfflush(stdout);
    }

    if ( verbose ) { 
        HDfprintf(stdout, "%s%s journal filename = \"%s\".\n", 
	          fcn_name, tag, journal_filename); 
	HDfflush(stdout);
    }

    if ( pass2 ) {

        child_id = HDfork();

        if ( child_id == -1 ) {

            pass2 = FALSE;
	    failure_mssg2 = "H5fork() failed.";

	} else if ( child_id == 0 ) {

	    child = TRUE;
	    tag = "child:";

	} else {

            child = FALSE;
	    tag = "parent:";
	}
    }

    if ( pass2 ) {

	/* reset the check point */
	cp = 0;

        if ( show_progress ) {

	    HDfprintf(stdout, "%s%s%d: cp = %d fork() succeeded.\n", 
		      fcn_name, tag, (int)pass2, cp++);
	    HDfflush(stdout);
	}

	if ( child ) {

            if ( show_progress ) {

	        HDfprintf(stdout, "%s%s%d: cp = %d child starting.\n",
		          fcn_name, tag, (int)pass2, cp++);
		HDfflush(stdout);
	    }
#if 0 /* JRM */
	    /* Quincey:
	     *
	     * It looks like we may have a bug here -- 
	     *
	     * In the original version of this test, I:
	     *
	     * 	1) created a file using the default FAPL, 
	     *
	     * 	2) added a data set to the file
	     *
	     * 	3) closed it,
	     *
	     * 	4) tried to re-open it using a FAPL that set the 
	     * 	   latest format, and enabled journaling.
	     *
	     * I hit an assertion failure on step 4.  
	     *
	     * I then modified the above to select the latest file 
	     * format on file create, and the problem went away.
	     *
	     * Is this as it should be, or do we have a bug here?
	     *
	     *                              JRM -- 7/9/08
	     */

            if ( pass2 ) {

                file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, 
				    H5P_DEFAULT);

                if ( file_id < 0 ) {

	            pass2 = FALSE;
                    failure_mssg2 = "H5Fcreate() failed.\n";
	        }
	    }
#else /* JRM */

            /* create a file access propertly list. */
            if ( pass2 ) {

                fapl_id = H5Pcreate(H5P_FILE_ACCESS);

                if ( fapl_id < 0 ) {

                    pass2 = FALSE;
                    failure_mssg2 = "H5Pcreate() failed.\n";
                }
            }

            if ( show_progress ) {

	        HDfprintf(stdout, "%s%s%d: cp = %d.\n",
		          fcn_name, tag, (int)pass2, cp++);
		HDfflush(stdout);
	    }

            /* call H5Pset_libver_bounds() on the fapl_id */
            if ( pass2 ) {

	        if ( H5Pset_libver_bounds(fapl_id, H5F_LIBVER_LATEST, 
					  H5F_LIBVER_LATEST) < 0 ) {

                    pass2 = FALSE;
                    failure_mssg2 = "H5Pset_libver_bounds() failed.\n";
                }
            }

	    /* open the file with a fapl indicating latest version of 
	     * the file format.
	     */
            if ( pass2 ) {

                file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, 
				    fapl_id);

                if ( file_id < 0 ) {

	            pass2 = FALSE;
                    failure_mssg2 = "H5Fcreate() failed.\n";
	        }
	    }
#endif /* JRM */
            if ( show_progress ) {

	        HDfprintf(stdout, "%s%s%d: cp = %d.\n",
		          fcn_name, tag, (int)pass2, cp++);
		HDfflush(stdout);
	    }

            if ( pass2 ) {

                dims[0] = 4;
                dims[1] = 6;
                dataspace_id = H5Screate_simple(2, dims, NULL);

	        if ( dataspace_id < 0 ) {

	            pass2 = FALSE;
	            failure_mssg2 = "H5Screate_simple() failed.";
                }
            }

            if ( show_progress ) {

	        HDfprintf(stdout, "%s%s%d: cp = %d.\n",
		          fcn_name, tag, (int)pass2, cp++);
		HDfflush(stdout);
	    }

            if ( pass2 ) {

                /* Create the dataset. */
                dataset_id = H5Dcreate2(file_id, "/dset", H5T_STD_I32BE, 
				        dataspace_id, H5P_DEFAULT, 
					H5P_DEFAULT, H5P_DEFAULT);

	        if ( dataspace_id < 0 ) {

	            pass2 = FALSE;
	            failure_mssg2 = "H5Dcreate2() failed.";
                }
            }

            if ( show_progress ) {

	        HDfprintf(stdout, "%s%s%d: cp = %d.\n",
		          fcn_name, tag, (int)pass2, cp++);
		HDfflush(stdout);
	    }

            if ( pass2 ) {

                /* close the data set, the data space, and the file */
	        if ( ( H5Dclose(dataset_id) < 0 ) ||
	             ( H5Sclose(dataspace_id) < 0 ) ||
#if 1 /* JRM */
                     ( H5Pclose(fapl_id) < 0 ) ||
#endif /* JRM */
	             ( H5Fclose(file_id) < 0 ) ) {

                    pass2 = FALSE;
	            failure_mssg2 = 
			    "data set, data space, or file close failed.";
	        }
            }

            if ( show_progress ) {

	        HDfprintf(stdout, "%s%s%d: cp = %d.\n",
		          fcn_name, tag, (int)pass2, cp++);
		HDfflush(stdout);
	    }

            /* create a file access propertly list. */
            if ( pass2 ) {

                fapl_id = H5Pcreate(H5P_FILE_ACCESS);

                if ( fapl_id < 0 ) {

                    pass2 = FALSE;
                    failure_mssg2 = "H5Pcreate() failed.\n";
                }
            }

            if ( show_progress ) {

	        HDfprintf(stdout, "%s%s%d: cp = %d.\n",
		          fcn_name, tag, (int)pass2, cp++);
		HDfflush(stdout);
	    }

            /* call H5Pset_libver_bounds() on the fapl_id */
            if ( pass2 ) {

	        if ( H5Pset_libver_bounds(fapl_id, H5F_LIBVER_LATEST, 
					  H5F_LIBVER_LATEST) < 0 ) {

                    pass2 = FALSE;
                    failure_mssg2 = "H5Pset_libver_bounds() failed.\n";
                }
            }

            if ( show_progress ) {

	        HDfprintf(stdout, "%s%s%d: cp = %d.\n",
		          fcn_name, tag, (int)pass2, cp++);
		HDfflush(stdout);
	    }

            if ( pass2 ) {

                jnl_config.version = H5AC2__CURR_JNL_CONFIG_VER;

                result = H5Pget_jnl_config(fapl_id, &jnl_config);

                if ( result < 0 ) {

                    pass2 = FALSE;
                    failure_mssg2 = "H5Pget_jnl_config() failed.\n";
                }

	        /* set journaling config fields to taste */
                jnl_config.enable_journaling       = TRUE;

                strcpy(jnl_config.journal_file_path, journal_filename);

                jnl_config.journal_recovered       = FALSE;
                jnl_config.jbrb_buf_size           = (8 * 1024);
                jnl_config.jbrb_num_bufs           = 2;
                jnl_config.jbrb_use_aio            = FALSE;
                jnl_config.jbrb_human_readable     = TRUE;
            }

            if ( show_progress ) {

	        HDfprintf(stdout, "%s%s%d: cp = %d.\n",
		          fcn_name, tag, (int)pass2, cp++);
		HDfflush(stdout);
	    }

            if ( pass2 ) {

                result = H5Pset_jnl_config(fapl_id, &jnl_config);

                if ( result < 0 ) {

                    pass2 = FALSE;
                    failure_mssg2 = "H5Pset_jnl_config() failed.\n";
                }
            }

            if ( show_progress ) {

	        HDfprintf(stdout, "%s%s%d: *cp = %d.\n",
		          fcn_name, tag, (int)pass2, cp++);
		HDfflush(stdout);
	    }

            /* open the file using fapl_id */
            if ( pass2 ) {

                file_id = H5Fopen(filename, H5F_ACC_RDWR, fapl_id);

                if ( file_id < 0 ) {

                    pass2 = FALSE;
                    failure_mssg2 = "H5Fopen() failed (9).\n";

                } else {

                    file_ptr = H5I_object_verify(file_id, H5I_FILE);

                    if ( file_ptr == NULL ) {

                        pass2 = FALSE;
                        failure_mssg2 = "Can't get file_ptr.";

                        if ( verbose ) {
                            HDfprintf(stdout, "%s: Can't get file_ptr.\n", 
				      fcn_name);
                        }
                    }
                }
            }

            if ( show_progress ) {

	        HDfprintf(stdout, "%s%s%d: cp = %d.\n",
		          fcn_name, tag, (int)pass2, cp++);
		HDfflush(stdout);
	    }

            if ( ( verbose ) && ( ! pass2 ) ) {
                HDfprintf(stdout, "%s%s%d failure_mssg = \"%s\".\n", 
			  fcn_name, tag, pass2, failure_mssg2);
		HDfflush(stdout);
            } 

            if ( show_progress ) {

	        HDfprintf(stdout, "%s%s%d cp = %d child exiting.\n", 
			  fcn_name, tag, (int)pass2, cp++);
		HDfflush(stdout);
	    }

	    abort();

	} else {

	    /* wait for the child to exit */
	    wait_result = HDwaitpid(child_id, &wait_status, 0);

	    HDsleep(3);

            if ( wait_result != child_id ) {

	        pass2 = FALSE;
		failure_mssg2 = "unexpected waitpid() result.";

		if ( show_progress ) {

	            HDfprintf(stdout, 
			      "%s%s:%d: wait_result = %ld, wait_status = %d.\n", 
			      fcn_name, tag, (int)pass2, 
			      (long)wait_result, wait_status);
		    HDfflush(stdout);
	        }
            } else {

		if ( show_progress ) {

	            HDfprintf(stdout, 
			      "%s%s:%d: cp = %d  child exited as expected.\n", 
			      fcn_name, tag, (int)pass2, cp++);
		    HDfflush(stdout);
	        }

                /* create a file access propertly list. */
                if ( pass2 ) {

                    fapl_id = H5Pcreate(H5P_FILE_ACCESS);

                    if ( fapl_id < 0 ) {

                        pass2 = FALSE;
                        failure_mssg2 = "H5Pcreate() failed.\n";
                    }
                }

                if ( show_progress ) {
		    HDfprintf(stdout, "%s%s:%d cp = %d.\n", 
			      fcn_name, tag, (int)pass2, cp++);
		    HDfflush(stdout);
	        }

                /* call H5Pset_libver_bounds() on the fapl_id */
                if ( pass2 ) {

	            if ( H5Pset_libver_bounds(fapl_id, H5F_LIBVER_LATEST, 
					      H5F_LIBVER_LATEST) < 0 ) {

                        pass2 = FALSE;
                        failure_mssg2 = "H5Pset_libver_bounds() failed.\n";
                    }
                }

                if ( show_progress ) {
		    HDfprintf(stdout, "%s%s%d cp = %d.\n", 
			      fcn_name, tag, (int)pass2, cp++);
		    HDfflush(stdout);
	        }

		/* attempt to open the file -- should fail as the child
		 * exited without closing the file properly, and thus 
		 * the file should still be marked as having journaling
		 * in progress.
		 */

		if ( pass2 ) {

		    H5E_BEGIN_TRY {

		        file_id = H5Fopen(filename, H5F_ACC_RDWR, fapl_id);

                    } H5E_END_TRY;

		    if ( file_id >= 0 ) {

                        pass2 = FALSE;
		        failure_mssg2 = "H5Fopen() succeeded - 3.";
		    }
                }

                if ( show_progress ) {

                    HDfprintf(stdout, "%s%s%d: cp = %d.\n", 
                              fcn_name, tag, (int)pass2, cp++);
		    HDfflush(stdout);
	        }

                /* delete the HDF5 file and journal file */
#if 1
                HDremove(filename);
                HDremove(journal_filename);
#endif
		if ( show_progress ) {

	            HDfprintf(stdout, "%s%s%d cp = %d parent done.\n", 
			      fcn_name, tag, (int)pass2, cp++);
		    HDfflush(stdout);
		}
	    }
        }
    } 

    return;

} /* verify_mdj_file_marking_on_open() */


/***************************************************************************
 * Function: 	verify_mdj_file_unmarking_on_file_close
 *
 * Purpose:  	Verify that HDF5 file on which journaling is enabled is 
 * 		marked as having not haveing journaling in progress when 
 * 		the file is closed.
 *
 *              Do this as follows:
 *
 *                  1) create a test file with metadata journaling 
 *                     enabled, 
 *
 *                  2) perform some operation(s) that dirty metadata
 *                     and result in journal activity.
 *
 * 		    3) close the file.
 *
 * 		    4) attempt to re-open the file -- should succeed.
 *
 *              On either success or failure, clean up the test file and
 *              the associated journal file.
 *
 * Return:      void
 *
 * Programmer: 	John Mainzer
 *              7/10/08
 * 
 **************************************************************************/

static void 
verify_mdj_file_unmarking_on_file_close(void)
{
    const char * fcn_name = "verify_mdj_file_unmarking_on_file_close():";
    char filename[512];
    char journal_filename[H5AC2__MAX_JOURNAL_FILE_NAME_LEN + 1];
    hbool_t show_progress = FALSE;
    hbool_t verbose = FALSE;
    int cp = 0;
    hid_t file_id = -1;
    hid_t fapl_id = -1;
    hid_t dataset_id = -1;
    hid_t dataspace_id = -1;
    hsize_t dims[2];
    H5F_t * file_ptr = NULL;
    H5C2_t * cache_ptr = NULL;

    if ( show_progress ) {

        HDfprintf(stdout, "%s%d cp = %d -- entering.\n", fcn_name, pass2, cp++);
	HDfflush(stdout);
    }

    /* setup the file name */
    if ( pass2 ) {

        if ( h5_fixname(FILENAMES[1], H5P_DEFAULT, filename, 
	                sizeof(filename)) == NULL ) {

            pass2 = FALSE;
            failure_mssg2 = "h5_fixname() failed (1).\n";
        }
    }

    if ( show_progress ) {

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass2, cp++);
	HDfflush(stdout);
    }

    if ( verbose ) { 
        HDfprintf(stdout, "%s filename = \"%s\".\n", fcn_name, filename); 
	HDfflush(stdout);
    }

    /* setup the journal file name */
    if ( pass2 ) {

        if ( h5_fixname(FILENAMES[3], H5P_DEFAULT, journal_filename, 
                        sizeof(journal_filename)) == NULL ) {

            pass2 = FALSE;
            failure_mssg2 = "h5_fixname() failed (2).\n";
        }
        else if ( strlen(journal_filename) >= 
			H5AC2__MAX_JOURNAL_FILE_NAME_LEN ) {

            pass2 = FALSE;
            failure_mssg2 = "journal file name too long.\n";
        }
    }

    if ( show_progress ) {

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, (int)pass2, cp++);
	HDfflush(stdout);
    }

    if ( verbose ) { 
        HDfprintf(stdout, "%s journal filename = \"%s\".\n", 
		  fcn_name, journal_filename); 
	HDfflush(stdout);
    }

    if ( pass2 ) {

        /* clean out any existing journal file */
        setup_cache_for_journaling(filename, journal_filename, &file_id,
                                   &file_ptr, &cache_ptr, FALSE);
    }

    if ( show_progress ) {

        HDfprintf(stdout, "%s%d: cp = %d.\n", fcn_name, (int)pass2, cp++);
        HDfflush(stdout);
    }

    /* create a data set so as to force a bit of journaling */
    if ( pass2 ) {

        dims[0] = 4;
        dims[1] = 6;
        dataspace_id = H5Screate_simple(2, dims, NULL);

        if ( dataspace_id < 0 ) {

            pass2 = FALSE;
            failure_mssg2 = "H5Screate_simple() failed.";
        }
    }

    if ( show_progress ) {

        HDfprintf(stdout, "%s%d: cp = %d.\n", fcn_name, (int)pass2, cp++);
        HDfflush(stdout);
    }

    if ( pass2 ) {

        /* Create the dataset. */
        dataset_id = H5Dcreate2(file_id, "/dset", H5T_STD_I32BE, 
			        dataspace_id, H5P_DEFAULT, 
				H5P_DEFAULT, H5P_DEFAULT);

        if ( dataspace_id < 0 ) {

            pass2 = FALSE;
            failure_mssg2 = "H5Dcreate2() failed.";
        }
    }

    if ( show_progress ) {

        HDfprintf(stdout, "%s%d: cp = %d.\n", fcn_name, (int)pass2, cp++);
        HDfflush(stdout);
    }

    /* now close the file... */

    if ( pass2 ) {

	if ( ( H5Dclose(dataset_id) < 0 ) ||
	     ( H5Sclose(dataspace_id) < 0 ) ||
	     ( H5Fclose(file_id) < 0 ) ) {

            pass2 = FALSE;
            failure_mssg2 = "dataset, dataspace, or file close failed.";
        }
    }

    if ( show_progress ) {

        HDfprintf(stdout, "%s%d: cp = %d.\n", fcn_name, (int)pass2, cp++);
        HDfflush(stdout);
    }

    /* ... and attempt to re-open it.  Should succeed */

    /* create a file access propertly list. */
    if ( pass2 ) {

        fapl_id = H5Pcreate(H5P_FILE_ACCESS);

        if ( fapl_id < 0 ) {

            pass2 = FALSE;
            failure_mssg2 = "H5Pcreate() failed.\n";
        }
    }

    if ( show_progress ) {

        HDfprintf(stdout, "%s%d: cp = %d.\n", fcn_name, (int)pass2, cp++);
        HDfflush(stdout);
    }

    /* call H5Pset_libver_bounds() on the fapl_id */
    if ( pass2 ) {

        if ( H5Pset_libver_bounds(fapl_id, H5F_LIBVER_LATEST, 
                                  H5F_LIBVER_LATEST) < 0 ) {

            pass2 = FALSE;
            failure_mssg2 = "H5Pset_libver_bounds() failed.\n";
        }
    }

    if ( show_progress ) {

        HDfprintf(stdout, "%s%d: *cp = %d.\n", fcn_name, (int)pass2, cp++);
        HDfflush(stdout);
    }

    /* attempt to open the file -- should succeed as the close should 
     * shutdown journaling.
     */

    if ( pass2 ) {

        file_id = H5Fopen(filename, H5F_ACC_RDWR, fapl_id);

	if ( file_id < 0 ) {

            pass2 = FALSE;
	    failure_mssg2 = "H5Fopen() failed (10).";
        }
    }

    if ( show_progress ) {

        HDfprintf(stdout, "%s%d: cp = %d.\n", fcn_name, (int)pass2, cp++);
        HDfflush(stdout);
    }

    /* close the file and fapl */

    if ( pass2 ) {

        if ( ( H5Pclose(fapl_id) < 0 ) ||
             ( H5Fclose(file_id) < 0 ) ) {

                pass2 = FALSE;
                failure_mssg2 = "fapl or file close failed.";
        }
    }

    if ( show_progress ) {

        HDfprintf(stdout, "%s%d: cp = %d.\n", fcn_name, (int)pass2, cp++);
        HDfflush(stdout);
    }

    /* delete the HDF5 file and journal file */
#if 1
    HDremove(filename);
    HDremove(journal_filename);
#endif

    if ( show_progress ) {

        HDfprintf(stdout, "%s%d: cp = %d.\n", fcn_name, (int)pass2, cp++);
        HDfflush(stdout);
    }

    return;

} /* verify_mdj_file_unmarking_on_file_close() */


/***************************************************************************
 * Function: 	verify_mdj_file_unmarking_on_journaling_shutdown
 *
 * Purpose:  	Verify that HDF5 file on which journaling is enabled is 
 * 		marked as having not haveing journaling in progress when 
 * 		journaling is disabled via the H5Fset_mdc_config() API
 * 		call.
 *
 *              Do this by:
 *
 *                  1) forking a child process, 
 *
 *                  2) creating a test file in the child with metadata 
 *                     journaling enabled, 
 *
 *                  3) performing some operation(s) that dirty metadata
 *                     and result in journal activity.
 *
 *                  4) using the H5Fset_mdc_config() to disable journaling.
 *
 *                  5) exiting from the child without closing the file.
 *
 *              When the child exits, try to open the child's test file.
 *              Open should succeed, as the file should be marked as 
 *              journaling not in progress.
 *
 *              Note that the file will be synced out as part of the 
 *              journaling shutdown process, so the metadata should be 
 *              in a consistant state.  Strictly speaking, this is not
 *              necessary for this test, for as long as the the file is
 *              not marked as having journaling in progress, we should
 *              pass.  However, testing this without using the HDF5 
 *              library to open the file would be inconvenient -- hence
 *              we make use of the sync on journal shutdown to make the
 *              test easier to implement.
 *
 *              On either success or failure, clean up the test file and
 *              the associated journal file.
 *
 * Return:      void
 *
 * Programmer: 	John Mainzer
 *              7/9/08
 * 
 **************************************************************************/

static void 
verify_mdj_file_unmarking_on_journaling_shutdown(void)
{
    const char * fcn_name = 
	    "verify_mdj_file_unmarking_on_journaling_shutdown():";
    const char * tag = "pre-fork:";
    char filename[512];
    char journal_filename[H5AC2__MAX_JOURNAL_FILE_NAME_LEN + 1];
    hbool_t child = FALSE;
    hbool_t show_progress = FALSE;
    hbool_t verbose = FALSE;
    herr_t result;
    int cp = 0;
    int wait_status;
    pid_t child_id;
    pid_t wait_result;
    hid_t file_id = -1;
    hid_t fapl_id = -1;
    hid_t dataset_id = -1;
    hid_t dataspace_id = -1;
    hsize_t dims[2];
    H5F_t * file_ptr = NULL;
    H5C2_t * cache_ptr = NULL;
    H5AC2_jnl_config_t jnl_config;

    /* setup the file name */
    if ( pass2 ) {

        if ( h5_fixname(FILENAMES[1], H5P_DEFAULT, filename, 
	                sizeof(filename)) == NULL ) {

            pass2 = FALSE;
            failure_mssg2 = "h5_fixname() failed (1).\n";
        }
    }

    if ( show_progress ) {

        HDfprintf(stdout, "%s%s%d cp = %d.\n", fcn_name, tag,  pass2, cp++);
	HDfflush(stdout);
    }

    if ( verbose ) { 
        HDfprintf(stdout, "%s%s filename = \"%s\".\n", fcn_name, tag,filename); 
	HDfflush(stdout);
    }

    /* setup the journal file name */
    if ( pass2 ) {

        if ( h5_fixname(FILENAMES[3], H5P_DEFAULT, journal_filename, 
                        sizeof(journal_filename)) == NULL ) {

            pass2 = FALSE;
            failure_mssg2 = "h5_fixname() failed (2).\n";
        }
        else if ( strlen(journal_filename) >= 
			H5AC2__MAX_JOURNAL_FILE_NAME_LEN ) {

            pass2 = FALSE;
            failure_mssg2 = "journal file name too long.\n";
        }
    }

    if ( show_progress ) {

        HDfprintf(stdout, "%s%s%d cp = %d.\n", 
		  fcn_name, tag, (int)pass2, cp++);
	HDfflush(stdout);
    }

    if ( verbose ) { 
        HDfprintf(stdout, "%s%s journal filename = \"%s\".\n", 
	          fcn_name, tag, journal_filename); 
	HDfflush(stdout);
    }

    if ( pass2 ) {

        child_id = HDfork();

        if ( child_id == -1 ) {

            pass2 = FALSE;
	    failure_mssg2 = "H5fork() failed.";

	} else if ( child_id == 0 ) {

	    child = TRUE;
	    tag = "child:";

	} else {

            child = FALSE;
	    tag = "parent:";
	}
    }

    if ( pass2 ) {

        if ( show_progress ) {

	    HDfprintf(stdout, "%s%s%d: cp = %d fork() succeeded.\n", 
		      fcn_name, tag, (int)pass2, cp++);
	    HDfflush(stdout);
	}

	if ( child ) {

            if ( show_progress ) {

	        HDfprintf(stdout, "%s%s%d: cp = %d child starting.\n",
		          fcn_name, tag, (int)pass2, cp++);
		HDfflush(stdout);
	    }

            /* clean out any existing journal file */
            setup_cache_for_journaling(filename, journal_filename, &file_id,
                                       &file_ptr, &cache_ptr, FALSE);

            if ( show_progress ) {

	        HDfprintf(stdout, "%s%s%d: cp = %d.\n",
		          fcn_name, tag, (int)pass2, cp++);
		HDfflush(stdout);
	    }

	    /* create a data set so as to force a bit of journaling */
            if ( pass2 ) {

                dims[0] = 4;
                dims[1] = 6;
                dataspace_id = H5Screate_simple(2, dims, NULL);

	        if ( dataspace_id < 0 ) {

	            pass2 = FALSE;
	            failure_mssg2 = "H5Screate_simple() failed.";
                }
            }

            if ( show_progress ) {

	        HDfprintf(stdout, "%s%s%d: cp = %d.\n",
		          fcn_name, tag, (int)pass2, cp++);
		HDfflush(stdout);
	    }

            if ( pass2 ) {

                /* Create the dataset. */
                dataset_id = H5Dcreate2(file_id, "/dset", H5T_STD_I32BE, 
				        dataspace_id, H5P_DEFAULT, 
					H5P_DEFAULT, H5P_DEFAULT);

	        if ( dataspace_id < 0 ) {

	            pass2 = FALSE;
	            failure_mssg2 = "H5Dcreate2() failed.";
                }
            }

            if ( show_progress ) {

	        HDfprintf(stdout, "%s%s%d: cp = %d.\n",
		          fcn_name, tag, (int)pass2, cp++);
		HDfflush(stdout);
	    }

	    /* now dis-able journaling */
	    if ( pass2 ) {

                jnl_config.version = H5AC2__CURR_JNL_CONFIG_VER;

                result = H5Fget_jnl_config(file_id, &jnl_config);

                if ( result < 0 ) {

                    pass2 = FALSE;
                    failure_mssg2 = "H5Fget_jnl_config() failed.\n";
                }

                jnl_config.enable_journaling       = FALSE;
            }

            if ( show_progress ) {

	        HDfprintf(stdout, "%s%s%d: cp = %d.\n",
		          fcn_name, tag, (int)pass2, cp++);
		HDfflush(stdout);
	    }

            if ( pass2 ) {

                result = H5Fset_jnl_config(file_id, &jnl_config);

                if ( result < 0 ) {

                    pass2 = FALSE;
                    failure_mssg2 = "H5Fset_jnl_config() failed.\n";
                }
            }

            if ( show_progress ) {

	        HDfprintf(stdout, "%s%s%d: cp = %d.\n",
		          fcn_name, tag, (int)pass2, cp++);
		HDfflush(stdout);
	    }

            if ( ( verbose ) && ( ! pass2 ) ) {
                HDfprintf(stdout, "%s%s%d failure_mssg = \"%s\".\n", 
			  fcn_name, tag, pass2, failure_mssg2);
		HDfflush(stdout);
            } 

            if ( show_progress ) {

	        HDfprintf(stdout, "%s%s%d cp = %d child exiting.\n", 
			  fcn_name, tag, (int)pass2, cp++);
		HDfflush(stdout);
	    }

	    abort();

	} else {

	    /* wait for the child to exit */
	    wait_result = HDwaitpid(child_id, &wait_status, 0);

	    HDsleep(3);

            if ( wait_result != child_id ) {

	        pass2 = FALSE;
		failure_mssg2 = "unexpected waitpid() result.";

		if ( show_progress ) {

	            HDfprintf(stdout, 
			      "%s%s:%d: wait_result = %ld, wait_status = %d.\n", 
			      fcn_name, tag, (int)pass2, 
			      (long)wait_result, wait_status);
		    HDfflush(stdout);
	        }
            } else {

		if ( show_progress ) {

	            HDfprintf(stdout, 
			      "%s%s:%d: cp = %d  child exited as expected.\n", 
			      fcn_name, tag, (int)pass2, cp++);
		    HDfflush(stdout);
	        }

                /* create a file access propertly list. */
                if ( pass2 ) {

                    fapl_id = H5Pcreate(H5P_FILE_ACCESS);

                    if ( fapl_id < 0 ) {

                        pass2 = FALSE;
                        failure_mssg2 = "H5Pcreate() failed.\n";
                    }
                }

                if ( show_progress ) {
		    HDfprintf(stdout, "%s%s:%d cp = %d.\n", 
			      fcn_name, tag, (int)pass2, cp++);
		    HDfflush(stdout);
	        }

                /* call H5Pset_libver_bounds() on the fapl_id */
                if ( pass2 ) {

	            if ( H5Pset_libver_bounds(fapl_id, H5F_LIBVER_LATEST, 
					      H5F_LIBVER_LATEST) < 0 ) {

                        pass2 = FALSE;
                        failure_mssg2 = "H5Pset_libver_bounds() failed.\n";
                    }
                }

                if ( show_progress ) {
		    HDfprintf(stdout, "%s%s%d cp = %d.\n", 
			      fcn_name, tag, (int)pass2, cp++);
		    HDfflush(stdout);
	        }

		/* attempt to open the file -- should succeed as the child
		 * disabled journaling just before exiting, which should have
		 * had the dual effect of marking the file as not having
		 * journaling in progress, and syncing the file out to disk.
		 */

		file_id = H5Fopen(filename, H5F_ACC_RDWR, fapl_id);

		if ( file_id < 0 ) {

                    pass2 = FALSE;
		    failure_mssg2 = "H5Fopen() failed (11).";
		}

                if ( show_progress ) {

                    HDfprintf(stdout, "%s%s%d: cp = %d.\n", 
                              fcn_name, tag, (int)pass2, cp++);
		    HDfflush(stdout);
	        }

		/* close the file and fapl */

                if ( pass2 ) {

	            if ( ( H5Pclose(fapl_id) < 0 ) ||
	                 ( H5Fclose(file_id) < 0 ) ) {

                        pass2 = FALSE;
	                failure_mssg2 = "fapl or file close failed.";
	            }
                }

                if ( show_progress ) {

                    HDfprintf(stdout, "%s%s%d: cp = %d.\n", 
                              fcn_name, tag, (int)pass2, cp++);
		    HDfflush(stdout);
	        }

                /* delete the HDF5 file and journal file */
#if 1
                HDremove(filename);
                HDremove(journal_filename);
#endif
		if ( show_progress ) {

	            HDfprintf(stdout, "%s%s%d cp = %d parent done.\n", 
			      fcn_name, tag, (int)pass2, cp++);
		    HDfflush(stdout);
		}
	    }
        }
    } 

    return;

} /* verify_mdj_file_unmarking_on_journaling_shutdown() */


/***************************************************************************
 * Function: 	verify_mdj_file_unmarking_on_recovery
 *
 * Purpose:  	Verify that HDF5 file that is marked as as having journaling 
 *              in progress is unmarked when the file is opened with the 
 *              journal_recovered flag set in the cache configuration 
 *              structure in the file access property list.
 *
 *              Do this by forking a child process, creating a test file
 *              in the child with metadata journaling enabled, and then
 *              exiting from the child without closing the file.  Note that
 *              we must flush the file before exiting, as we want the file
 *              to be readable, but be marked as journaling in progress.
 *
 *              When the child exits, try to open the child's test file.
 *              Open should fail, as the file should be marked as journaling
 *              in progress.
 *
 *              Try to open again with the journal_recovered flag set.  This
 *              should succeed.  Close and open again without the 
 *              journal recovered flag set to verify that the file is 
 *              no longer marked as having journaling in progress.
 *
 *              On either success or failure, clean up the test file and
 *              the associated journal file.
 *
 * Return:      void
 *
 * Programmer: 	John Mainzer
 *              7/14/08
 * 
 **************************************************************************/

static void 
verify_mdj_file_unmarking_on_recovery(void)
{
    const char * fcn_name = "verify_mdj_file_unmarking_on_recovery():";
    const char * tag = "pre-fork:";
    char filename[512];
    char journal_filename[H5AC2__MAX_JOURNAL_FILE_NAME_LEN + 1];
    hbool_t child = FALSE;
    hbool_t show_progress = FALSE;
    hbool_t verbose = FALSE;
    herr_t result;
    int cp = 0;
    int wait_status;
    uint64_t trans_num;
    pid_t child_id;
    pid_t wait_result;
    hid_t file_id = -1;
    hid_t fapl_id = -1;
    H5F_t * file_ptr = NULL;
    H5C2_t * cache_ptr = NULL;
    H5AC2_jnl_config_t jnl_config;

    /* setup the file name */
    if ( pass2 ) {

        if ( h5_fixname(FILENAMES[1], H5P_DEFAULT, filename, 
	                sizeof(filename)) == NULL ) {

            pass2 = FALSE;
            failure_mssg2 = "h5_fixname() failed (1).\n";
        }
    }

    if ( show_progress ) {

        HDfprintf(stdout, "%s%s%d cp = %d.\n", fcn_name, tag,  pass2, cp++);
	HDfflush(stdout);
    }

    if ( verbose ) { 
        HDfprintf(stdout, "%s%s filename = \"%s\".\n", fcn_name, tag,filename); 
	HDfflush(stdout);
    }

    /* setup the journal file name */
    if ( pass2 ) {

        if ( h5_fixname(FILENAMES[3], H5P_DEFAULT, journal_filename, 
                        sizeof(journal_filename)) == NULL ) {

            pass2 = FALSE;
            failure_mssg2 = "h5_fixname() failed (2).\n";
        }
        else if ( strlen(journal_filename) >= 
			H5AC2__MAX_JOURNAL_FILE_NAME_LEN ) {

            pass2 = FALSE;
            failure_mssg2 = "journal file name too long.\n";
        }
    }

    if ( show_progress ) {

        HDfprintf(stdout, "%s%s%d cp = %d.\n", 
		  fcn_name, tag, (int)pass2, cp++);
	HDfflush(stdout);
    }

    if ( verbose ) { 
        HDfprintf(stdout, "%s%s journal filename = \"%s\".\n", 
	          fcn_name, tag, journal_filename); 
	HDfflush(stdout);
    }

    if ( pass2 ) {

        child_id = HDfork();

        if ( child_id == -1 ) {

            pass2 = FALSE;
	    failure_mssg2 = "H5fork() failed.";

	} else if ( child_id == 0 ) {

	    child = TRUE;
	    tag = "child:";

	} else {

            child = FALSE;
	    tag = "parent:";
	}
    }

    if ( pass2 ) {

        if ( show_progress ) {

	    HDfprintf(stdout, "%s%s%d: cp = %d fork() succeeded.\n", 
		      fcn_name, tag, (int)pass2, cp++);
	    HDfflush(stdout);
	}

	if ( child ) {

            if ( show_progress ) {

	        HDfprintf(stdout, "%s%s%d: cp = %d child starting.\n",
		          fcn_name, tag, (int)pass2, cp++);
		HDfflush(stdout);
	    }

            /* clean out any existing journal file */
            HDremove(journal_filename);
            setup_cache_for_journaling(filename, journal_filename, &file_id,
                                       &file_ptr, &cache_ptr, FALSE);

            if ( show_progress ) {

	        HDfprintf(stdout, "%s%s%d: cp = %d.\n",
		          fcn_name, tag, (int)pass2, cp++);
		HDfflush(stdout);
	    }

	    /* run a dummy transaction to fource metadata journaling
	     * initialization.
	     */
	    H5C2_begin_transaction(cache_ptr, &trans_num, "dummy");
	    H5C2_end_transaction(file_ptr, cache_ptr, trans_num, "dummy");

            if ( show_progress ) {

	        HDfprintf(stdout, "%s%s%d: cp = %d.\n",
		          fcn_name, tag, (int)pass2, cp++);
		HDfflush(stdout);
	    }

	    /* flush the file to ensure that it is in a readable state */
	    if ( pass2 ) {

	        if ( H5Fflush(file_id, H5F_SCOPE_GLOBAL) < 0 ) {

		    if ( verbose ) {

			HDfprintf(stdout, "%s:%s: H5Fflush() failed.\n", 
				  fcn_name, tag);
	                HDfflush(stdout);
		    }
		    pass2 = FALSE;
		    failure_mssg2 = "H5Fflush() failed.";
	        }
	    }

            if ( ( verbose ) && ( ! pass2 ) ) {
                HDfprintf(stdout, "%s%s%d failure_mssg = \"%s\".\n", 
			  fcn_name, tag, pass2, failure_mssg2);
		HDfflush(stdout);
            } 

            if ( show_progress ) {

	        HDfprintf(stdout, "%s%s%d cp = %d child exiting.\n", 
			  fcn_name, tag, (int)pass2, cp++);
		HDfflush(stdout);
	    }

	    abort();

	} else {

	    /* wait for the child to exit */
	    wait_result = HDwaitpid(child_id, &wait_status, 0);

	    HDsleep(3);

            if ( wait_result != child_id ) {

	        pass2 = FALSE;
		failure_mssg2 = "unexpected waitpid() result.";

		if ( show_progress ) {

	            HDfprintf(stdout, 
			      "%s%s:%d: wait_result = %ld, wait_status = %d.\n", 
			      fcn_name, tag, (int)pass2, 
			      (long)wait_result, wait_status);
		    HDfflush(stdout);
	        }
            } else {

		if ( show_progress ) {

	            HDfprintf(stdout, 
			      "%s%s:%d: cp = %d  child exited as expected.\n", 
			      fcn_name, tag, (int)pass2, cp++);
		    HDfflush(stdout);
	        }

                /* create a file access propertly list. */
                if ( pass2 ) {

                    fapl_id = H5Pcreate(H5P_FILE_ACCESS);

                    if ( fapl_id < 0 ) {

                        pass2 = FALSE;
                        failure_mssg2 = "H5Pcreate() failed.\n";
                    }
                }

                if ( show_progress ) {
		    HDfprintf(stdout, "%s%s:%d cp = %d.\n", 
			      fcn_name, tag, (int)pass2, cp++);
		    HDfflush(stdout);
	        }

                /* call H5Pset_libver_bounds() on the fapl_id */
                if ( pass2 ) {

	            if ( H5Pset_libver_bounds(fapl_id, H5F_LIBVER_LATEST, 
					      H5F_LIBVER_LATEST) < 0 ) {

                        pass2 = FALSE;
                        failure_mssg2 = "H5Pset_libver_bounds() failed.\n";
                    }
                }

                if ( show_progress ) {
		    HDfprintf(stdout, "%s%s%d cp = %d.\n", 
			      fcn_name, tag, (int)pass2, cp++);
		    HDfflush(stdout);
	        }

		/* attempt to open the file -- should fail as the child
		 * exited without closing the file properly, and thus 
		 * the file should still be marked as having journaling
		 * in progress.
		 */

		if ( pass2 ) { 

		    H5E_BEGIN_TRY {

		        file_id = H5Fopen(filename, H5F_ACC_RDWR, fapl_id);

		    } H5E_END_TRY;

		    if ( file_id >= 0 ) {

                        pass2 = FALSE;
		        failure_mssg2 = "H5Fopen() succeeded - 4.";
		    }
		}

                if ( show_progress ) {

                    HDfprintf(stdout, "%s%s%d: cp = %d.\n", 
                              fcn_name, tag, (int)pass2, cp++);
		    HDfflush(stdout);
	        }


		/* now set the file recovered flag in the journal config 
		 * structure in the fapl, and try to open again.  Should
		 * succeed, and the file should not be marked as having
		 * journaling in progress.
		 */
	        if ( pass2 ) {

                    jnl_config.version = H5AC2__CURR_JNL_CONFIG_VER;

                    result = H5Pget_jnl_config(fapl_id, &jnl_config);

                    if ( result < 0 ) {

                        pass2 = FALSE;
                        failure_mssg2 = "H5Pget_jnl_config() failed.\n";
                    }

                    jnl_config.journal_recovered       = TRUE;
                }

                if ( show_progress ) {

	            HDfprintf(stdout, "%s%s%d: cp = %d.\n",
		              fcn_name, tag, (int)pass2, cp++);
		    HDfflush(stdout);
	        }

                if ( pass2 ) {

                    result = H5Pset_jnl_config(fapl_id, &jnl_config);

                    if ( result < 0 ) {

                        pass2 = FALSE;
                        failure_mssg2 = "H5Pset_jnl_config() failed(1).\n";
                    }
                }

                if ( show_progress ) {

	            HDfprintf(stdout, "%s%s%d: cp = %d.\n",
		              fcn_name, tag, (int)pass2, cp++);
		    HDfflush(stdout);
	        }

		if ( pass2 ) { 

		    file_id = H5Fopen(filename, H5F_ACC_RDWR, fapl_id);

		    if ( file_id < 0 ) {

                        pass2 = FALSE;
		        failure_mssg2 = "H5Fopen() failed (12).";
		    }
		}

                if ( show_progress ) {

	            HDfprintf(stdout, "%s%s%d: cp = %d.\n",
		              fcn_name, tag, (int)pass2, cp++);
		    HDfflush(stdout);
	        }

		if ( pass2 ) {

		    if ( H5Fclose(file_id) < 0 ) {

                        pass2 = FALSE;
		        failure_mssg2 = "H5Fclose() failed(1).";
		    }
		}

                if ( show_progress ) {

	            HDfprintf(stdout, "%s%s%d: cp = %d.\n",
		              fcn_name, tag, (int)pass2, cp++);
		    HDfflush(stdout);
	        }

		/* now, turn off the journal recovered flag, and try to 
		 * open the file again.  Should succeed.
		 */

                if ( pass2 ) {

                    jnl_config.journal_recovered       = FALSE;

                    result = H5Pset_jnl_config(fapl_id, &jnl_config);

                    if ( result < 0 ) {

                        pass2 = FALSE;
                        failure_mssg2 = "H5Pset_jnl_config() failed(2).\n";
                    }
                }

                if ( show_progress ) {

	            HDfprintf(stdout, "%s%s%d: cp = %d.\n",
		              fcn_name, tag, (int)pass2, cp++);
		    HDfflush(stdout);
	        }

		if ( pass2 ) { 

		    file_id = H5Fopen(filename, H5F_ACC_RDWR, fapl_id);

		    if ( file_id < 0 ) {

                        pass2 = FALSE;
		        failure_mssg2 = "H5Fopen() failed (13).";
		    }
		}

                if ( show_progress ) {

	            HDfprintf(stdout, "%s%s%d: cp = %d.\n",
		              fcn_name, tag, (int)pass2, cp++);
		    HDfflush(stdout);
	        }

		if ( pass2 ) {

		    if ( ( H5Fclose(file_id) < 0 ) || 
			 ( H5Pclose(fapl_id) < 0 ) ) {

                        pass2 = FALSE;
		        failure_mssg2 = "H5Fclose() or H5Pclose() failed(2).";
		    }
		}

                if ( show_progress ) {

	            HDfprintf(stdout, "%s%s%d: cp = %d.\n",
		              fcn_name, tag, (int)pass2, cp++);
		    HDfflush(stdout);
	        }

                /* delete the HDF5 file and journal file */
#if 1
                HDremove(filename);
                HDremove(journal_filename);
#endif
		if ( show_progress ) {

	            HDfprintf(stdout, "%s%s%d cp = %d parent done.\n", 
			      fcn_name, tag, (int)pass2, cp++);
		    HDfflush(stdout);
		}
	    }
        }
    } 

    return;

} /* verify_mdj_file_unmarking_on_recovery() */


/***************************************************************************
 * Function: 	check_buffer_writes
 *
 * Purpose:  	Verify the function H5C_jb__write_to_buffer properly writes
 *              messages of varying sizes into the journal buffers, and 
 *              that the journal buffers properly flush out when filled.
 *
 * Return:      void
 *
 * Programmer: 	Mike McGreevy <mcgreevy@hdfgroup.org>
 *              Thursday, February 21, 2008
 * 
 **************************************************************************/
static void 
check_buffer_writes(void)
{
    const char * fcn_name = "check_buffer_writes(): ";
    char filename[512];
    int i;
    herr_t result;
    H5C2_jbrb_t jbrb_struct;
    FILE * readback;
    hbool_t show_progress = FALSE;
    int32_t checkpoint = 1;
    char filldata[12][100];
    int repeatnum[12];

    TESTING("metadata buffer & file writes");

    pass2 = TRUE;

    /* Initialize data to get written as tests */
    HDmemcpy(filldata[0], "abcdefghijklmn\n", 16);
    HDmemcpy(filldata[1], "ABCDEFGHIJKLMNO\n", 17);
    HDmemcpy(filldata[2], "AaBbCcDdEeFfGgHh\n", 18);
    HDmemcpy(filldata[3], "ZAB-ZAB-ZAB-ZAB-ZAB-ZAB-ZAB-ZA\n", 32);
    HDmemcpy(filldata[4], "ABC-ABC-ABC-ABC-ABC-ABC-ABC-ABC\n", 33);
    HDmemcpy(filldata[5], "BCD-BCD-BCD-BCD-BCD-BCD-BCD-BCD-\n", 34);
    HDmemcpy(filldata[6], "12345-12345-12345-12345-12345-12345-12345-1234\n", 
	     48);
    HDmemcpy(filldata[7], "01234-01234-01234-01234-01234-01234-01234-01234\n", 
	     49);
    HDmemcpy(filldata[8], "23456-23456-23456-23456-23456-23456-23456-23456-\n",
	     50);
    HDmemcpy(filldata[9], "aaaa-bbbb-cccc-dddd-eeee-ffff-gggg-hhhh-iiii-jjjj-kkkk-llll-mmmm-nnnn-oooo-pppp-qqqq-rrrr-ssss\n", 96);
    HDmemcpy(filldata[10], "bbbb-cccc-dddd-eeee-ffff-gggg-hhhh-iiii-jjjj-kkkk-llll-mmmm-nnnn-oooo-pppp-qqqq-rrrr-ssss-tttt-\n", 97);
    HDmemcpy(filldata[11], "cccc-dddd-eeee-ffff-gggg-hhhh-iiii-jjjj-kkkk-llll-mmmm-nnnn-oooo-pppp-qqqq-rrrr-ssss-tttt-uuuu-v\n", 98);
    
    /* Assert that size of data is as expected */
    HDassert(HDstrlen(filldata[0]) == 15);
    HDassert(HDstrlen(filldata[1]) == 16);
    HDassert(HDstrlen(filldata[2]) == 17);
    HDassert(HDstrlen(filldata[3]) == 31);
    HDassert(HDstrlen(filldata[4]) == 32);
    HDassert(HDstrlen(filldata[5]) == 33);
    HDassert(HDstrlen(filldata[6]) == 47);
    HDassert(HDstrlen(filldata[7]) == 48);
    HDassert(HDstrlen(filldata[8]) == 49);
    HDassert(HDstrlen(filldata[9]) == 95);
    HDassert(HDstrlen(filldata[10]) == 96);
    HDassert(HDstrlen(filldata[11]) == 97);

    /* Give structure its magic number */
    jbrb_struct.magic = H5C2__H5C2_JBRB_T_MAGIC;
	
    if ( show_progress ) /* 1 */ 
        HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
                  checkpoint++, (int)pass2);

    /* setup the file name */
    if ( pass2 ) {

        if ( h5_fixname(FILENAMES[1], H5P_DEFAULT, filename, sizeof(filename))
             == NULL ) {

            pass2 = FALSE;
            failure_mssg2 = "h5_fixname() failed";

        } /* end if */

    } /* end if */
	
    if ( show_progress ) /* 2 */ 
	HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
		  checkpoint++, (int)pass2);

    /* Initialize H5C2_jbrb_t structure. */
    if ( pass2 ) {

       	result = H5C2_jb__init(/* H5C2_jbrb_t */            &jbrb_struct, 
                               /* HDF5 file name */         HDF5_FILE_NAME,
                               /* journal file name */      filename, 
                               /* Buffer size */            16, 
                               /* Number of Buffers */      3, 
                               /* Use Synchronois I/O */    FALSE, 
                               /* human readable journal */ TRUE);

        if ( result != 0) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_jb_init failed, check 1";

       	} /* end if */

    } /* end if */
	
    if ( show_progress ) /* 3 */ 
        HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
                  checkpoint++, (int)pass2);

    /* Flush and truncate journal file to get rid of the header
     * message for subsequent tests. */
    if ( pass2 ) {
	
	if ( H5C2_jb__flush(&jbrb_struct) != SUCCEED ) {

            pass2 = FALSE;
	    failure_mssg2 = "H5C2_jb_flush failed";

	} /* end if */	

    } /* end if */
	
    /* Truncate journal file */
    if ( pass2 ) {

	if ( H5C2_jb__trunc(&jbrb_struct) != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_jb_trunc failed";

	} /* end if */

    } /* end if */

    if ( show_progress ) /* 4 */ 
	HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
		  checkpoint++, (int)pass2);

    /* open journal file for reading */
    readback = fopen(filename, "r");

    /* run a collection of calls to write_flush_verify(). These calls 
     * write specific lengths of data into the journal buffers and 
     * then flushes them to disk, and ensures that what makes it to 
     * disk is as expected 
     */

    for (i=0; i<12; i++) {

	write_flush_verify(&jbrb_struct, 
			   (int)HDstrlen(filldata[i]), 
			   filldata[i], 
			   readback);

	if ( show_progress )
	    HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
		      checkpoint++, (int)pass2);

    } /* end for */

    /* run a collection of calls to write_noflush_verify(). These 
     * calls write specific lengths of data into the journal buffers 
     * multiple times, but only flushes at the end of the set of writes. 
     * This tests to ensure that the automatic flush calls in 
     * H5C2_jb__write_to_buffer are working properly. The routine then 
     * ensures that what makes it it disk is as expected 
     */

    /* Initialize repeat array to specify how many times to repeat each write
       within the write_noflush_verify calls. */
    repeatnum[0] = 16;
    repeatnum[1] = 6;
    repeatnum[2] = 16;
    repeatnum[3] = 16;
    repeatnum[4] = 6;
    repeatnum[5] = 16;
    repeatnum[6] = 16;
    repeatnum[7] = 6;
    repeatnum[8] = 16;
    repeatnum[9] = 16;
    repeatnum[10] = 6;
    repeatnum[11] = 16;

    for (i=0; i<12; i++) {

        write_noflush_verify(&jbrb_struct,
                             (int)HDstrlen(filldata[i]),
                             filldata[i],
                             readback,
                             repeatnum[i]);
        
	if ( show_progress )
	    HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
		      checkpoint++, (int)pass2);

    } /* end for */
    
    /* close journal file pointer */
    fclose(readback);

    /* Truncate the journal file */
    if ( pass2 ) {

	if ( H5C2_jb__trunc(&jbrb_struct) != SUCCEED ) {

		pass2 = FALSE;
		failure_mssg2 = "H5C2_jb__trunc failed";

	} /* end if */

    } /* end if */

    /* take down the journal file */
    if ( pass2 ) {

	if (H5C2_jb__takedown(&jbrb_struct) != SUCCEED) {

	    pass2 = FALSE;
	    failure_mssg2 = "H5C2_jb__takedown failed";

	} /* end if */

    } /* end if */

    /* report pass / failure information */
    if ( pass2 ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass2 ) {

	failures2++;
        HDfprintf(stdout, "%s: failure_mssg2 = \"%s\".\n",
                  fcn_name, failure_mssg2);

    }

    return;

} /* check_buffer_writes */



/***************************************************************************
 * Function: 	write_flush_verify
 *
 * Purpose:  	Helper function for check_buffer_writes test. Writes a 
 *              piece of data of specified size into the journal buffer, then
 *              flushes the journal buffers. The data is read back and
 *              verified for correctness.
 *
 * Return:      void
 *
 * Programmer: 	Mike McGreevy <mcgreevy@hdfgroup.org>
 *              Thursday, February 21, 2008
 * 
 **************************************************************************/
static void 
write_flush_verify(H5C2_jbrb_t * struct_ptr, 
		   int size, 
		   char * data, 
		   FILE * readback)
{
    char verify[150];

    if ( pass2 ) {

	if ( H5C2_jb__write_to_buffer(struct_ptr, (size_t)size, 
				      data, 0, (uint64_t)0) != SUCCEED ) {

	    pass2 = FALSE;
	    failure_mssg2 = "H5C2_jb__write_to_buffer failed";

	} /* end if */

    } /* end if */

    if ( pass2 ) {

	if ( H5C2_jb__flush(struct_ptr) != SUCCEED ) {

	    pass2 = FALSE;
            failure_mssg2 = "H5C2_jb_flush failed";

        } /* end if */

    } /* end if */

    if ( pass2 ) {

	fgets(verify, size+10, readback);
		
	if (HDstrcmp(verify, data) != 0) {

	    pass2 = FALSE;
	    failure_mssg2 = "Journal entry not written correctly";

	} /* end if */

    } /* end if */

    return;

} /* write_flush_verify */



/***************************************************************************
 * Function: 	write_noflush_verify
 *
 * Purpose:  	Helper function for check_buffer_writes test. Writes a 
 *              piece of data of specified size into the journal buffer
 *              multiple times, without calling H5C_jb__flush in between
 *              writes. After all writes are completed, H5C_jb__flush is 
 *              called, and the data is read back from the journal file and
 *              verified for correctness.
 *
 * Return:      void
 *
 * Programmer: 	Mike McGreevy <mcgreevy@hdfgroup.org>
 *              Thursday, February 21, 2008
 * 
 **************************************************************************/
static void 
write_noflush_verify(H5C2_jbrb_t * struct_ptr, 
		     int size, 
		     char * data, 
		     FILE * readback, 
		     int repeats)
{
    int i;
    char verify[150];	

    for (i=0; i<repeats; i++) {

        if ( pass2 ) {
	
            if ( H5C2_jb__write_to_buffer(struct_ptr, (size_t)size, 
				          data, 0, (uint64_t)0) != SUCCEED ) {

                pass2 = FALSE;
                failure_mssg2 = "H5C2_jb__write_to_buffer failed";

            } /* end if */

        } /* end if */

    } /* end for */

    if ( pass2 ) {

        if ( H5C2_jb__flush(struct_ptr) != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_jb_flush failed";

	} /* end if */	

    } /* end if */

    for (i=0; i<repeats; i++) {

        if ( pass2 ) {
            fgets(verify, size+10, readback);
            if (HDstrcmp(verify, data) != 0) {

                pass2 = FALSE;
                failure_mssg2 = "Journal entry not written correctly";

            } /* end if */

	} /* end if */

    } /* end for */

    return;

} /* write_noflush_verify */



/***************************************************************************
 * Function: 	check_message_format
 *
 * Purpose:  	Verify that the functions that write messages into the journal
 *              buffers actually write the correct messages.
 *
 * Return:      void
 *
 * Programmer: 	Mike McGreevy <mcgreevy@hdfgroup.org>
 *              Tuesday, February 26, 2008
 * 
 **************************************************************************/
static void 
check_message_format(void)
{
    const char * fcn_name = "check_message_format(): ";
    char filename[512];
    int i;
    herr_t result;
    H5C2_jbrb_t jbrb_struct;
    FILE * readback;
    hbool_t show_progress = FALSE;
    int32_t checkpoint = 1;
    char verify[9][500];
    char from_journal[9][500];
    time_t current_date;

    TESTING("journal file message format");

    pass2 = TRUE;

    /* setup the file name */
    if ( pass2 ) {

        if ( h5_fixname(FILENAMES[1], H5P_DEFAULT, filename, sizeof(filename))
             == NULL ) {

            pass2 = FALSE;
            failure_mssg2 = "h5_fixname() failed";

        } /* end if */

    } /* end if */

    /* Give structure its magic number */
    jbrb_struct.magic = H5C2__H5C2_JBRB_T_MAGIC;

    /* Initialize H5C2_jbrb_t structure. */
    if ( pass2 ) {

       	result = H5C2_jb__init(/* H5C2_jbrb_t */            &jbrb_struct, 
                               /* HDF5 file name */         HDF5_FILE_NAME,
                               /* journal file name */      filename, 
                               /* Buffer size */            16, 
                               /* Number of Buffers */      3, 
                               /* Use Synchronois I/O */    FALSE, 
                               /* human readable journal */ TRUE);

        if ( result != 0) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_jb_init failed, check 2";

       	} /* end if */

    } /* end if */

    if ( show_progress ) /* 1 */ 
	HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
		  checkpoint++, (int)pass2);
 
    /* Start a transaction */
    if ( pass2 ) {

        if ( H5C2_jb__start_transaction(/* H5C2_jbrb_t  */  &jbrb_struct, 
                                        /* trans number */  (uint64_t)1) 
           != SUCCEED ) {
    
            pass2 = FALSE;
            failure_mssg2 = "H5C2_jb__start_transaction failed";

        } /* end if */

    } /* end if */

    if ( show_progress ) /* 2 */ 
	HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
		  checkpoint++, (int)pass2);
 
    /* Write a journal entry */
    if ( pass2 ) {

        if ( H5C2_jb__journal_entry(/* H5C2_jbrb_t  */  &jbrb_struct, 
                                    /* trans number */  (uint64_t)1, 
                                    /* base address */  (haddr_t)0, 
                                    /* data length  */  1, 
                                    /* data         */  (const uint8_t *)"A") 
           != SUCCEED ) {
            
            pass2 = FALSE;
            failure_mssg2 = "H5C2_jb__journal_entry failed";

        } /* end if */

    } /* end if */

    if ( show_progress ) /* 3 */ 
	HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
		  checkpoint++, (int)pass2);
 
    /* Write a journal entry */
    if ( pass2 ) {

        if ( H5C2_jb__journal_entry(/* H5C2_jbrb_t  */  &jbrb_struct, 
                                    /* trans number */  (uint64_t)1, 
                                    /* base address */  (haddr_t)1, 
                                    /* data length  */  2, 
                                    /* data         */  (const uint8_t *)"AB") 
           != SUCCEED ) {
            
            pass2 = FALSE;
            failure_mssg2 = "H5C2_jb__journal_entry failed";

        } /* end if */

    } /* end if */

    if ( show_progress ) /* 4 */ 
	HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
		  checkpoint++, (int)pass2);
 
    /* Write a journal entry */
    if ( pass2 ) {

        if ( H5C2_jb__journal_entry(/* H5C2_jbrb_t  */  &jbrb_struct, 
                                    /* trans number */  (uint64_t)1, 
                                    /* base address */  (haddr_t)3, 
                                    /* data length  */  4, 
                                    /* data         */  (const uint8_t *)"CDEF") 
           != SUCCEED ) {
            
            pass2 = FALSE;
            failure_mssg2 = "H5C2_jb__journal_entry failed";

        } /* end if */

    } /* end if */

    if ( show_progress ) /* 5 */ 
	HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
		  checkpoint++, (int)pass2);
 
    /* End transaction */
    if ( pass2 ) {
        if ( H5C2_jb__end_transaction(/* H5C2_jbrb_t  */  &jbrb_struct, 
                                      /* trans number */  (uint64_t)1) 
           != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_jb__end_transaction failed";
            
        } /* end if */

    } /* end if */

    if ( show_progress ) /* 6 */ 
	HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
		  checkpoint++, (int)pass2);

    /* Start a transaction */
    if ( pass2 ) {

        if ( H5C2_jb__start_transaction(/* H5C2_jbrb_t  */  &jbrb_struct, 
                                        /* trans number */  (uint64_t)2) 
           != SUCCEED ) {
    
            pass2 = FALSE;
            failure_mssg2 = "H5C2_jb__start_transaction failed";

        } /* end if */

    } /* end if */

    if ( show_progress ) /* 7 */ 
	HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
		  checkpoint++, (int)pass2);

    /* Write a journal entry */
    if ( pass2 ) {

        if ( H5C2_jb__journal_entry(/* H5C2_jbrb_t  */  &jbrb_struct, 
                                    /* trans number */  (uint64_t)2, 
                                    /* base address */  (haddr_t)285, 
                                    /* data length  */  11, 
                                    /* data         */  (const uint8_t *)"Test Data?!") 
           != SUCCEED ) {
            
            pass2 = FALSE;
            failure_mssg2 = "H5C2_jb__journal_entry failed";

        } /* end if */

    } /* end if */

    if ( show_progress ) /* 8 */ 
	HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
		  checkpoint++, (int)pass2);

    /* End transaction */
    if ( pass2 ) {
        if ( H5C2_jb__end_transaction(/* H5C2_jbrb_t  */  &jbrb_struct, 
                                      /* trans number */  (uint64_t)2) 
           != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_jb__end_transaction failed";
            
        } /* end if */

    } /* end if */

    if ( show_progress ) /* 9 */ 
	HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
		  checkpoint++, (int)pass2);

    /* Flush the journal buffers. */
    if ( pass2 ) {

	if ( H5C2_jb__flush(/* H5C2_jbrb_t */  &jbrb_struct) 
           != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_jb__flush failed";

	} /* end if */

    } /* end if */

    current_date = time(NULL);

    if ( show_progress ) /* 10 */ 
	HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
		  checkpoint++, (int)pass2);

    /* Fill out verify array with expected messages */
    sprintf(verify[0], "0 ver_num 1 target_file_name HDF5.file creation_date %10.10s human_readable 1\n", ctime(&current_date));
    sprintf(verify[1], "1 bgn_trans 1\n");
    sprintf(verify[2], "2 trans_num 1 length 1 base_addr 0x0 body  41 \n");
    sprintf(verify[3], "2 trans_num 1 length 2 base_addr 0x1 body  41 42 \n");
    sprintf(verify[4], "2 trans_num 1 length 4 base_addr 0x3 body  43 44 45 46 \n");
    sprintf(verify[5], "3 end_trans 1\n");
    sprintf(verify[6], "1 bgn_trans 2\n");
    sprintf(verify[7], "2 trans_num 2 length 11 base_addr 0x11d body  54 65 73 74 20 44 61 74 61 3f 21 \n");
    sprintf(verify[8], "3 end_trans 2\n");

    /* verify that messages in journal are same as expected */
    readback = fopen(filename, "r");
    for (i = 0; i < 9; i++) {

        if ( pass2) {

            fgets(from_journal[i], 300, readback);

            if ( HDstrcmp(verify[i], from_journal[i]) != 0) {

                pass2 = FALSE;
                failure_mssg2 = "journal file not written correctly";

            } /* end if */

        } /* end if */

    } /* end for */
    fclose(readback);

    /* Truncate the journal file */
    if ( pass2 ) {

	if ( H5C2_jb__trunc(&jbrb_struct) != SUCCEED ) {

		pass2 = FALSE;
		failure_mssg2 = "H5C2_jb__trunc failed";

	} /* end if */

    } /* end if */

    if ( show_progress ) /* 11 */ 
	HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
		  checkpoint++, (int)pass2);

    /* Start a transaction */
    if ( pass2 ) {

        if ( H5C2_jb__start_transaction(/* H5C2_jbrb_t  */  &jbrb_struct, 
                                        /* trans number */  (uint64_t)3) 
           != SUCCEED ) {
    
            pass2 = FALSE;
            failure_mssg2 = "H5C2_jb__start_transaction failed";

        } /* end if */

    } /* end if */

    if ( show_progress ) /* 12 */ 
	HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
		  checkpoint++, (int)pass2);

    /* Write a journal entry */
    if ( pass2 ) {

        if ( H5C2_jb__journal_entry(/* H5C2_jbrb_t  */  &jbrb_struct, 
                                    /* trans number */  (uint64_t)3, 
                                    /* base address */  (haddr_t)28591, 
                                    /* data length  */  6, 
                                    /* data         */  (const uint8_t *)"#1nN`}") 
           != SUCCEED ) {
            
            pass2 = FALSE;
            failure_mssg2 = "H5C2_jb__journal_entry failed";

        } /* end if */

    } /* end if */

    if ( show_progress ) /* 13 */ 
	HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
		  checkpoint++, (int)pass2);

    /* End transaction */
    if ( pass2 ) {
        if ( H5C2_jb__end_transaction(/* H5C2_jbrb_t  */  &jbrb_struct, 
                                      /* trans number */  (uint64_t)3) 
           != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_jb__end_transaction failed";
            
        } /* end if */

    } /* end if */

    if ( show_progress ) /* 14 */ 
	HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
		  checkpoint++, (int)pass2);

    /* Add a comment */
    if ( pass2 ) {
        if ( H5C2_jb__comment(/* H5C2_jbrb_t     */  &jbrb_struct, 
                              /* comment message */  "This is a comment!") 
           != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_jb__comment failed";
            
        } /* end if */

    } /* end if */

    if ( show_progress ) /* 14 */ 
	HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
		  checkpoint++, (int)pass2);

    /* Add a comment */
    if ( pass2 ) {
        if ( H5C2_jb__comment(/* H5C2_jbrb_t     */  &jbrb_struct, 
                              /* comment message */  "This is another comment!") 
           != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_jb__comment failed";
            
        } /* end if */

    } /* end if */

    if ( show_progress ) /* 14 */ 
	HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
		  checkpoint++, (int)pass2);

    /* Flush the journal buffers. */
    if ( pass2 ) {

	if ( H5C2_jb__flush(/* H5C2_jbrb_t */  &jbrb_struct) 
           != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_jb__flush failed";

	} /* end if */

    } /* end if */

    current_date = time(NULL);

    if ( show_progress ) /* 15 */ 
	HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
		  checkpoint++, (int)pass2);

    /* Fill out verify array with expected messages */
    sprintf(verify[0], "0 ver_num 1 target_file_name HDF5.file creation_date %10.10s human_readable 1\n", ctime(&current_date));
    sprintf(verify[1], "1 bgn_trans 3\n");
    sprintf(verify[2], "2 trans_num 3 length 6 base_addr 0x6faf body  23 31 6e 4e 60 7d \n");
    sprintf(verify[3], "3 end_trans 3\n");
    sprintf(verify[4], "C comment This is a comment!\n");
    sprintf(verify[5], "C comment This is another comment!\n");

    /* verify that messages in journal are same as expected */
    readback = fopen(filename, "r");
    for (i = 0; i < 6; i++) {

        if ( pass2) {

            fgets(from_journal[i], 300, readback);

            if ( HDstrcmp(verify[i], from_journal[i]) != 0) {
    
                pass2 = FALSE;
                failure_mssg2 = "journal file not written correctly";

            } /* end if */

        } /* end if */

    } /* end for */
    fclose(readback);

    /* Truncate the journal file */
    if ( pass2 ) {

	if ( H5C2_jb__trunc(&jbrb_struct) != SUCCEED ) {

		pass2 = FALSE;
		failure_mssg2 = "H5C2_jb__trunc failed";

	} /* end if */

    } /* end if */

    /* take down the journal file */
    if ( pass2 ) {

	if (H5C2_jb__takedown(&jbrb_struct) != SUCCEED) {

	    pass2 = FALSE;
	    failure_mssg2 = "H5C2_jb__takedown failed";

	} /* end if */

    } /* end if */

    if ( show_progress ) /* 16 */ 
	HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
		  checkpoint++, (int)pass2);

    /* report pass / failure information */
    if ( pass2 ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass2 ) {

	failures2++;
        HDfprintf(stdout, "%s: failure_mssg2 = \"%s\".\n",
                  fcn_name, failure_mssg2);

    }

    return;

} /* end check_message_format */



/***************************************************************************
 * Function: 	check_legal_calls
 *
 * Purpose:  	Verify that all H5C_jb functions prevent use when appropriate.
 *
 * Return:      void
 *
 * Programmer:  Mike McGreevy <mcgreevy@hdfgroup.org>
 *              Tuesday, February 26, 2008
 * 
 **************************************************************************/
static void 
check_legal_calls(void)
{
    const char * fcn_name = "check_legal_calls(): ";
    char filename[512];
    herr_t result;
    H5C2_jbrb_t jbrb_struct;
    hbool_t show_progress = FALSE;
    int32_t checkpoint = 1;

    TESTING("journaling routine compatibility");

    pass2 = TRUE;

    /* Give structure its magic number */
    jbrb_struct.magic = H5C2__H5C2_JBRB_T_MAGIC;

    /* setup the file name */
    if ( pass2 ) {

        if ( h5_fixname(FILENAMES[1], H5P_DEFAULT, filename, sizeof(filename))
             == NULL ) {

            pass2 = FALSE;
            failure_mssg2 = "h5_fixname() failed";

        } /* end if */

    } /* end if */

    if ( show_progress ) /* 1 */ 
        HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
                  checkpoint++, (int)pass2);

    /* Initialize H5C2_jbrb_t structure. This call should SUCCEED. */
    if ( pass2 ) {

       	result = H5C2_jb__init(/* H5C2_jbrb_t */            &jbrb_struct, 
                               /* HDF5 file name */         HDF5_FILE_NAME,
                               /* journal file name */      filename, 
                               /* Buffer size */            4000, 
                               /* Number of Buffers */      3, 
                               /* Use Synchronois I/O */    FALSE, 
                               /* human readable journal */ TRUE);

        if ( result != SUCCEED) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_jb_init failed, check 3";

       	} /* end if */

    } /* end if */
	
    if ( show_progress ) /* 2 */ 
        HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
                  checkpoint++, (int)pass2);
#if 0
    /* Start transaction 2. This should FAIL because transaction 1 has
       not occurred yet. Ensure that it fails, and flag an error if it 
       does not. */
    /* transaction numbers need not be sequential, only monitonically 
     * increasing -- thus this is not an error any more.
     *                                                    -- JRM
     */
    if ( pass2 ) {

	if ( H5C2_jb__start_transaction(/* H5C2_jbrb_t   */  &jbrb_struct, 
                                        /* Transaction # */  (uint64_t)2)
           == SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_jb__start_transaction should have failed";

	} /* end if */

    } /* end if */ 
#endif
    if ( show_progress ) /* 3 */ 
        HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
                  checkpoint++, (int)pass2);

    /* End transaction 1. This should FAIL because transaction 1 has
       not started yet. Ensure that it fails, and flag an error if it 
       does not. */
    if ( pass2 ) {

	if ( H5C2_jb__end_transaction(/* H5C2_jbrb_t   */  &jbrb_struct, 
                                      /* Transaction # */  (uint64_t)1)
           == SUCCEED ) {
        
            pass2 = FALSE;
            failure_mssg2 = "H5C2_jb__end_transaction should have failed";
 
	} /* end if */

    } /* end if */

    if ( show_progress ) /* 4 */ 
        HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
                  checkpoint++, (int)pass2);

    /* Attempt to write a journal entry before transaction has started..
       This should FAIL because transaction 1 has not started yet. Ensure 
       that it fails, and flag an error if it does not. */
    if ( pass2 ) {

	if ( H5C2_jb__journal_entry(/* H5C2_jbrb_t   */  &jbrb_struct, 
                                    /* Transaction # */  (uint64_t)1,
                                    /* Base Address  */  (haddr_t)123456789, 
                                    /* Length        */  16, 
                                    /* Body          */  (const uint8_t *)"This should fail")
           == SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_jb__journal_entry should have failed";

	} /* end if */

    } /* end if */

    if ( show_progress ) /* 5 */ 
        HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
                  checkpoint++, (int)pass2);

    /* Start transaction 1. This should SUCCEED. */
    if ( pass2 ) {

	if ( H5C2_jb__start_transaction(/* H5C2_jbrb_t   */  &jbrb_struct, 
                                        /* Transaction # */  (uint64_t)1)
           != SUCCEED ) {

	    pass2 = FALSE;
	    failure_mssg2 = "H5C2_jb__start_transaction failed";

	} /* end if */

    } /* end if */

    if ( show_progress ) /* 6 */ 
        HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
                  checkpoint++, (int)pass2);

    /* Attempt to start transaction 1 again. This should FAIL because 
       transaction 1 is already open. Ensure that it fails, and flag an
       error if it does not. */
    if ( pass2 ) {

	if ( H5C2_jb__start_transaction(/* H5C2_jbrb_t   */  &jbrb_struct, 
                                        /* Transaction # */  (uint64_t)1)
           == SUCCEED ) {

	    pass2 = FALSE;
	    failure_mssg2 = "H5C2_jb__start_transaction should have failed";

	} /* end if */

    } /* end if */

    if ( show_progress ) /* 7 */ 
        HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
                  checkpoint++, (int)pass2);

    /* Attempt to end transaction 1. This should FAIL because no 
       journal entry has been written under this transaction. */
    if ( pass2 ) {

	if ( H5C2_jb__end_transaction(/* H5C2_jbrb_t   */  &jbrb_struct, 
                                      /* Transaction # */  (uint64_t)1)
           == SUCCEED ) {

	    pass2 = FALSE;
	    failure_mssg2 = "H5C2_jb__end_transaction should have failed";

	} /* end if */

    } /* end if */

    if ( show_progress ) /* 8 */ 
        HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
                  checkpoint++, (int)pass2);

    /* Attempt to write a journal entry into the wrong transaction number.
       This should FAIL because specified transaction number isn't in 
       progress. Ensure that it fails, and flag an error if it does not. */
    if ( pass2 ) {

	if ( H5C2_jb__journal_entry(/* H5C2_jbrb_t   */  &jbrb_struct, 
                                    /* Transaction # */  (uint64_t)2,
                                    /* Base Address  */  (haddr_t)123456789, 
                                    /* Length        */  16, 
                                    /* Body          */  (const uint8_t *)"This should fail")
           == SUCCEED ) {

	    pass2 = FALSE;
	    failure_mssg2 = "H5C2_jb__journal_entry should have failed";

	} /* end if */

    } /* end if */

    if ( show_progress ) /* 9 */ 
        HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
                  checkpoint++, (int)pass2);

    /* Write a journal entry during transaction 1. This should SUCCEED. */
    if ( pass2 ) {

	if ( H5C2_jb__journal_entry(/* H5C2_jbrb_t   */  &jbrb_struct, 
                                    /* Transaction # */  (uint64_t)1,
                                    /* Base Address  */  (haddr_t)123456789, 
                                    /* Length        */  51, 
                                    /* Body          */  (const uint8_t *)"This is the first transaction during transaction 1.")
           != SUCCEED ) {

	    pass2 = FALSE;
	    failure_mssg2 = "H5C2_jb__journal_entry failed";

	} /* end if */

    } /* end if */

    if ( show_progress ) /* 10 */ 
        HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
                  checkpoint++, (int)pass2);

    /* Attempt to flush buffers. This should FAIL because a transaction
       is still in progress. Ensure that it fails, and flag an error
       if it does not. */
    if ( pass2 ) {

	if ( H5C2_jb__flush(/* H5C2_jbrb_t */  &jbrb_struct) 
             == SUCCEED ) {

	    pass2 = FALSE;
	    failure_mssg2 = "H5C2_jb__flush should have failed";

	} /* end if */

    } /* end if */

    if ( show_progress ) /* 11 */ 
        HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
                  checkpoint++, (int)pass2);

    /* End transaction 1. This should SUCCEED. */
    if ( pass2 ) {

	if ( H5C2_jb__end_transaction(/* H5C2_jbrb_t   */  &jbrb_struct, 
                                      /* Transaction # */  (uint64_t)1)
           != SUCCEED ) {

	    pass2 = FALSE;
	    failure_mssg2 = "H5C2_jb__end_transaction failed";

	} /* end if */

    } /* end if */

    if ( show_progress ) /* 12 */ 
        HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
                  checkpoint++, (int)pass2);

    /* Attempt to start transaction 1 again. This should FAIL because
       transaction 1 has already occurred. Ensure that it fails, and flag
       an error if it does not. */
    if ( pass2 ) {

	if ( H5C2_jb__start_transaction(/* H5C2_jbrb_t   */  &jbrb_struct, 
                                        /* Transaction # */  (uint64_t)1)
           == SUCCEED ) {

	    pass2 = FALSE;
	    failure_mssg2 = "H5C2_jb__start_transaction should have failed";

	} /* end if */

    } /* end if */

    if ( show_progress ) /* 13 */ 
        HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
                  checkpoint++, (int)pass2);

    /* Start transaction 2. This should SUCCEED.*/
    if ( pass2 ) {

	if ( H5C2_jb__start_transaction(/* H5C2_jbrb_t   */  &jbrb_struct, 
                                        /* Transaction # */  (uint64_t)2)
           != SUCCEED ) {

	    pass2 = FALSE;
	    failure_mssg2 = "H5C2_jb__start_transaction failed";

	} /* end if */

    } /* end if */

    if ( show_progress ) /* 14 */ 
        HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
                  checkpoint++, (int)pass2);

    /* Write a journal entry during transaction 2. This should SUCCEED. */
    if ( pass2 ) {

	if ( H5C2_jb__journal_entry(/* H5C2_jbrb_t   */  &jbrb_struct, 
                                    /* Transaction # */  (uint64_t)2,
                                    /* Base Address  */  (haddr_t)7465, 
                                    /* Length        */  51, 
                                    /* Body          */  (const uint8_t *)"This is the first transaction during transaction 2!")
           != SUCCEED ) {

	    pass2 = FALSE;
	    failure_mssg2 = "H5C2_jb__journal_entry failed";

	} /* end if */

    } /* end if */

    if ( show_progress ) /* 15 */ 
        HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
                  checkpoint++, (int)pass2);

    /* Write a journal entry during transaction 2. This should SUCCEED. */
    if ( pass2 ) {

	if ( H5C2_jb__journal_entry(/* H5C2_jbrb_t   */  &jbrb_struct, 
                                    /* Transaction # */  (uint64_t)2,
                                    /* Base Address  */  (haddr_t)123456789, 
                                    /* Length        */  60, 
                                    /* Body          */  (const uint8_t *)"... And here's your second transaction during transaction 2.")
           != SUCCEED ) {

	    pass2 = FALSE;
	    failure_mssg2 = "H5C2_jb__journal_entry failed";
 
	} /* end if */

    } /* end if */

    if ( show_progress ) /* 16 */ 
        HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
                  checkpoint++, (int)pass2);

    /* End transaction 2. This should SUCCEED. */
    if ( pass2 ) {

	if ( H5C2_jb__end_transaction(/* H5C2_jbrb_t   */  &jbrb_struct, 
                                      /* Transaction # */  (uint64_t)2)
           != SUCCEED ) {

	    pass2 = FALSE;
	    failure_mssg2 = "H5C2_jb__end_transaction failed";
  
	} /* end if */

    } /* end if */

    if ( show_progress ) /* 17 */ 
        HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
                  checkpoint++, (int)pass2);

    /* Attempt to truncate the journal file. This should FAIL because the
       journal buffers have not been flushed yet. Ensure that it fails, and
       flag and error if it does not. */
    if ( pass2 ) {

	if ( H5C2_jb__trunc(/* H5C2_jbrb_t */  &jbrb_struct) 
           == SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_jb__trunc should have failed";

	} /* end if */

    } /* end if */

    if ( show_progress ) /* 18 */ 
        HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
                  checkpoint++, (int)pass2);

    /* Attempt to take down the ring buffer. This should FAIL because the 
       journal buffers have not been flushed yet. Ensure that it fails, and
       flag and error if it does not. */
    if ( pass2 ) {

	if (H5C2_jb__takedown(/* H5C2_jbrb_t */  &jbrb_struct) 
           == SUCCEED) {

	    pass2 = FALSE;
	    failure_mssg2 = "H5C2_jb__takedown should have failed";

	} /* end if */

    } /* end if */

    if ( show_progress ) /* 19 */ 
        HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
                  checkpoint++, (int)pass2);

    /* Flush the journal buffers. This should SUCCEED. */
    if ( pass2 ) {

	if ( H5C2_jb__flush(/* H5C2_jbrb_t */  &jbrb_struct) 
           != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_jb__flush failed";

	} /* end if */

    } /* end if */

    if ( show_progress ) /* 20 */ 
        HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
                  checkpoint++, (int)pass2);

    /* Attempt to take down the ring buffer. This should FAIL because the 
       journal file has not been truncated. Ensure that it fails, and
       flag and error if it does not. */
    if ( pass2 ) {

	if (H5C2_jb__takedown(/* H5C2_jbrb_t */  &jbrb_struct) 
           == SUCCEED) {

	    pass2 = FALSE;
	    failure_mssg2 = "H5C2_jb__takedown should have failed";

	} /* end if */

    } /* end if */

    if ( show_progress ) /* 21 */ 
        HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
                  checkpoint++, (int)pass2);

    /* Truncate the journal file. This should SUCCEED. */
    if ( pass2 ) {

	if ( H5C2_jb__trunc(/* H5C2_jbrb_t */  &jbrb_struct) 
           != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_jb__trunc failed";

	} /* end if */

    } /* end if */

    if ( show_progress ) /* 22 */ 
        HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
                  checkpoint++, (int)pass2);

    /* Take down the journal file. This should SUCCEED. */
    if ( pass2 ) {

	if (H5C2_jb__takedown(/* H5C2_jbrb_t */  &jbrb_struct) 
           != SUCCEED) {

	    pass2 = FALSE;
	    failure_mssg2 = "H5C2_jb__takedown failed";
 
	} /* end if */

   } /* end if */

    if ( show_progress ) /* 23 */ 
        HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
                  checkpoint++, (int)pass2);

    /* report pass / failure information */
    if ( pass2 ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass2 ) {

	failures2++;
        HDfprintf(stdout, "%s: failure_mssg2 = \"%s\".\n",
                  fcn_name, failure_mssg2);

    }

    return;

} /* end check_legal_calls */



/***************************************************************************
 * Function: 	check_transaction_tracking
 *
 * Purpose:  	Verify that the ring buffer successfully tracks when
 *              transactions make it to disk. 
 *
 * Return:      void
 *
 * Programmer: 	Mike McGreevy <mcgreevy@hdfgroup.org>
 *              Tuesday, February 26, 2008
 * 
 **************************************************************************/
static void 
check_transaction_tracking(void)
{
    const char * fcn_name = "check_transaction_tracking(): ";
    char filename[512];
    int i;
    herr_t result;
    H5C2_jbrb_t jbrb_struct;
    hbool_t show_progress = FALSE;
    int32_t checkpoint = 1;
    int expected_tval[12];

    TESTING("journal file transaction tracking");

    pass2 = TRUE;

    /* setup the file name */
    if ( pass2 ) {

        if ( h5_fixname(FILENAMES[1], H5P_DEFAULT, filename, sizeof(filename))
             == NULL ) {

            pass2 = FALSE;
            failure_mssg2 = "h5_fixname() failed";

        } /* end if */

    } /* end if */

    if ( show_progress ) /* 1 */ 
        HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
                  checkpoint++, (int)pass2);

    /* Give structure its magic number */
    jbrb_struct.magic = H5C2__H5C2_JBRB_T_MAGIC;

    /* ===================================================
     * First ring buffer configuration.
     * 4 Buffers, each size 250.
     * Writing transactions of size 100.
     * Test cases: 
     *     - writing multiple transactions in each buffer
     *     - writing end transaction message to exact end
     *       of a journal buffer, as well as the exact end
     *       of the ring buffer.
     * =================================================== */

    /* Initialize H5C2_jbrb_t structure. */
    if ( pass2 ) {

       	result = H5C2_jb__init(/* H5C2_jbrb_t */            &jbrb_struct, 
                               /* HDF5 file name */         HDF5_FILE_NAME,
                               /* journal file name */      filename, 
                               /* Buffer size */            250, 
                               /* Number of Buffers */      4, 
                               /* Use Synchronois I/O */    FALSE, 
                               /* human readable journal */ TRUE);

        if ( result != SUCCEED) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_jb_init failed, check 4";

       	} /* end if */

    } /* end if */

    if ( show_progress ) /* 2 */ 
        HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
                  checkpoint++, (int)pass2);

    /* Flush the journal buffers. */
    if ( pass2 ) {

	if ( H5C2_jb__flush(/* H5C2_jbrb_t */  &jbrb_struct) 
           != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_jb__flush failed";

	} /* end if */

    } /* end if */

    if ( show_progress ) /* 3 */ 
        HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
                  checkpoint++, (int)pass2);

    /* Write journal entries and verify that the transactions that get to
       disk are accurately reported after each write. The following test the
       case where multiple journal entries reside in each buffer before a flush
       occurs. Also, the case when a transaction ends on a buffer boundary
       is also tested. */

    /* set up array of expected transaction values on disk */
    expected_tval[1] = 0;
    expected_tval[2] = 0;
    expected_tval[3] = 2;
    expected_tval[4] = 2;
    expected_tval[5] = 5;
    expected_tval[6] = 5;
    expected_tval[7] = 5;
    expected_tval[8] = 7;
    expected_tval[9] = 7;
    expected_tval[10] = 10;

    /* write 20 messages and verify that expected values are as indicated in
       the expected_tval array */
    for (i = 1; i < 11; i++) {

        write_verify_trans_num(/* H5C2_jbrb_t   */ &jbrb_struct, 
                           /* transaction num */ (uint64_t)i, 
                           /* expected trans */(uint64_t)expected_tval[i]);

    } /* end for */

    if ( show_progress ) /* 4 */ 
        HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
                  checkpoint++, (int)pass2);

    /* Flush the journal buffers. */
    if ( pass2 ) {

	if ( H5C2_jb__flush(/* H5C2_jbrb_t */  &jbrb_struct) 
           != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_jb__flush failed";

	} /* end if */

    } /* end if */

    /* Truncate the journal file. */
    if ( pass2 ) {

	if ( H5C2_jb__trunc(/* H5C2_jbrb_t */  &jbrb_struct) 
           != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_jb__trunc failed";

	} /* end if */

    } /* end if */

    /* Take down the journal file. */
    if ( pass2 ) {

	if (H5C2_jb__takedown(/* H5C2_jbrb_t */  &jbrb_struct) 
           != SUCCEED) {

	    pass2 = FALSE;
	    failure_mssg2 = "H5C2_jb__takedown failed";

	} /* end if */

    } /* end if */

    if ( show_progress ) /* 5 */ 
        HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
                  checkpoint++, (int)pass2);

    /* ===================================================
     * Second ring buffer configuration
     * 4 Buffers, each size 100.
     * Writing transactions of size 100.
     * Test cases: 
     *     - end transaction messages appear on buffer
     *       boundaries.
     * =================================================== */

    /* Initialize H5C2_jbrb_t structure. */
    if ( pass2 ) {

       	result = H5C2_jb__init(/* H5C2_jbrb_t */            &jbrb_struct, 
                               /* HDF5 file name */         HDF5_FILE_NAME,
                               /* journal file name */      filename, 
                               /* Buffer size */            100, 
                               /* Number of Buffers */      4, 
                               /* Use Synchronois I/O */    FALSE, 
                               /* human readable journal */ TRUE);

        if ( result != SUCCEED) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_jb_init failed, check 5";

       	} /* end if */

    } /* end if */

    /* Flush the journal buffers. */
    if ( pass2 ) {

	if ( H5C2_jb__flush(/* H5C2_jbrb_t */  &jbrb_struct) 
           != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_jb__flush failed";

	} /* end if */

    } /* end if */

    if ( show_progress ) /* 6 */ 
        HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
                  checkpoint++, (int)pass2);

    /* Write journal entries and verify that the transactions that get to
       disk are accurately reported after each write. The following tests the
       case where end transaction messages hit exactly at the end of the 
       ring buffer. */
    for (i=1; i<20; i++) {

        write_verify_trans_num(/* H5C2_ujbrb_t */&jbrb_struct, 
                               /* transaction num */(uint64_t)i, 
                               /* expected trans on disk */(uint64_t)i);

    } /* end for */

    if ( show_progress ) /* 7 */ 
        HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
                  checkpoint++, (int)pass2);

    /* Flush the journal buffers. */
    if ( pass2 ) {

	if ( H5C2_jb__flush(/* H5C2_jbrb_t */  &jbrb_struct) 
           != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_jb__flush failed";

	} /* end if */

    } /* end if */

    /* Truncate the journal file. */
    if ( pass2 ) {

	if ( H5C2_jb__trunc(/* H5C2_jbrb_t */  &jbrb_struct) 
           != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_jb__trunc failed";

	} /* end if */

    } /* end if */

    /* Take down the journal file. */
    if ( pass2 ) {

	if (H5C2_jb__takedown(/* H5C2_jbrb_t */  &jbrb_struct) 
           != SUCCEED) {

	    pass2 = FALSE;
	    failure_mssg2 = "H5C2_jb__takedown failed";

	} /* end if */

    } /* end if */

    /* ===================================================
     * Third ring buffer configuration
     * 10 Buffers, each size 30.
     * Writing transactions of size 100.
     * Test cases: 
     *     - end transaction messages start in one buffer
     *       and end in the following buffer.
     *     - end transaction messages start in the last 
     *       buffer and loop around to the first buffer.
     *     - multiple buffers are filled between end 
     *       transaction messages.
     * =================================================== */

    /* Initialize H5C2_jbrb_t structure. */
    if ( pass2 ) {

       	result = H5C2_jb__init(/* H5C2_jbrb_t */            &jbrb_struct, 
                               /* HDF5 file name */         HDF5_FILE_NAME,
                               /* journal file name */      filename, 
                               /* Buffer size */            30, 
                               /* Number of Buffers */      10, 
                               /* Use Synchronois I/O */    FALSE, 
                               /* human readable journal */ TRUE);

        if ( result != SUCCEED) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_jb_init failed, check 6";

       	} /* end if */

    } /* end if */

    if ( show_progress ) /* 8 */ 
        HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
                  checkpoint++, (int)pass2);

    /* Flush the journal buffers. */
    if ( pass2 ) {

	if ( H5C2_jb__flush(/* H5C2_jbrb_t */  &jbrb_struct) 
           != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_jb__flush failed";

	} /* end if */

    } /* end if */

    if ( show_progress ) /* 9 */ 
        HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
                  checkpoint++, (int)pass2);

    /* Write journal entries and verify that the transactions that get to
       disk are accurately reported after each write. The following tests the
       case where end transaction messages start in one buffer and end in
       another buffer. Also tests the case where one transaction ends several
       buffers ahead of the next transaction end. */
    write_verify_trans_num(&jbrb_struct, 
		           (uint64_t)1, 
			   (uint64_t)0); /* 1 in bufs, 0 on disk */
    write_verify_trans_num(&jbrb_struct, 
		           (uint64_t)2, 
			   (uint64_t)1); /* 2 in bufs, 1 on disk */
    write_verify_trans_num(&jbrb_struct, 
		           (uint64_t)3, 
			   (uint64_t)3); /* nothing in bufs, 3 on disk */
    H5C2_jb__write_to_buffer(&jbrb_struct, 10, "XXXXXXXXX\n", 0, (uint64_t)0);   
    write_verify_trans_num(&jbrb_struct, 
		           (uint64_t)4, 
			   (uint64_t)3); /* 1 in bufs, 0 on disk */
    write_verify_trans_num(&jbrb_struct, 
		           (uint64_t)5, 
			   (uint64_t)5); /* 2 in bufs, 1 on disk */
    write_verify_trans_num(&jbrb_struct, 
		           (uint64_t)6, 
			   (uint64_t)5); /* nothing in bufs, 3 on disk */
    H5C2_jb__write_to_buffer(&jbrb_struct, 10, "XXXXXXXXX\n", 0, (uint64_t)0);   
    write_verify_trans_num(&jbrb_struct, 
		           (uint64_t)7, 
			   (uint64_t)7); /* 1 in bufs, 0 on disk */
    write_verify_trans_num(&jbrb_struct, 
		           (uint64_t)8, 
			   (uint64_t)7); /* 2 in bufs, 1 on disk */
    write_verify_trans_num(&jbrb_struct, 
		           (uint64_t)9, 
			   (uint64_t)8); /* nothing in bufs, 3 on disk */
    H5C2_jb__write_to_buffer(&jbrb_struct, 10, "XXXXXXXXX\n", 0, (uint64_t)0);   
    write_verify_trans_num(&jbrb_struct, 
		           (uint64_t)10, 
			   (uint64_t)9); /* 1 in bufs, 0 on disk */
    write_verify_trans_num(&jbrb_struct, 
		           (uint64_t)11, 
			   (uint64_t)10); /* 2 in bufs, 1 on disk */
    write_verify_trans_num(&jbrb_struct, 
		           (uint64_t)12, 
			   (uint64_t)12); /* nothing in buf, 3 on disk */

    if ( show_progress ) /* 10 */ 
        HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
                  checkpoint++, (int)pass2);

    /* Flush the journal buffers. */
    if ( pass2 ) {

	if ( H5C2_jb__flush(/* H5C2_jbrb_t */  &jbrb_struct) 
           != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_jb__flush failed";

	} /* end if */

    } /* end if */

    /* Truncate the journal file. */
    if ( pass2 ) {

	if ( H5C2_jb__trunc(/* H5C2_jbrb_t */  &jbrb_struct) 
           != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_jb__trunc failed";

	} /* end if */

    } /* end if */

    /* Take down the journal file. */
    if ( pass2 ) {

	if (H5C2_jb__takedown(/* H5C2_jbrb_t */  &jbrb_struct) 
           != SUCCEED) {

	    pass2 = FALSE;
	    failure_mssg2 = "H5C2_jb__takedown failed";

	} /* end if */

    } /* end if */

    /* ===================================================
     * Fourth ring buffer configuration
     * 35 Buffers, each size 1.
     * Writing transactions of size 100.
     * Test cases: 
     *     - end transaction messages are longer than the 
     *       entire ring buffer structure. note this is an
     *       extreme corner case situation as buffer sizes
     *       should generally be much larger than an end
     *       transaction message.
     * =================================================== */

    /* Initialize H5C2_jbrb_t structure. */
    if ( pass2 ) {

       	result = H5C2_jb__init(/* H5C2_jbrb_t */            &jbrb_struct, 
                               /* HDF5 file name */         HDF5_FILE_NAME,
                               /* journal file name */      filename, 
                               /* Buffer size */            1, 
                               /* Number of Buffers */      35, 
                               /* Use Synchronois I/O */    FALSE, 
                               /* human readable journal */ TRUE);

        if ( result != SUCCEED) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_jb_init failed, check 7";

       	} /* end if */

    } /* end if */

    /* Flush the journal buffers. */
    if ( pass2 ) {

	if ( H5C2_jb__flush(/* H5C2_jbrb_t */  &jbrb_struct) 
           != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_jb__flush failed";

	} /* end if */

    } /* end if */

    if ( show_progress ) /* 11 */ 
        HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
                  checkpoint++, (int)pass2);

    /* Write journal entries and verify that the transactions that get to
       disk are accurately reported after each write. The following tests the
       case where end transaction messages take up several journal buffers, and
       ensures that the trans_tracking array is properly propogated */
    for (i=1; i<5; i++) {

        write_verify_trans_num(/* H5C2_jbrb_t */  &jbrb_struct, 
                               /* transaction num */  (uint64_t)i, 
                               /* expected returned trans */  (uint64_t)i);

    } /* end for */

    if ( show_progress ) /* 12 */ 
        HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
                  checkpoint++, (int)pass2);

    /* Flush the journal buffers. */
    if ( pass2 ) {

	if ( H5C2_jb__flush(/* H5C2_jbrb_t */  &jbrb_struct) 
           != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_jb__flush failed";

	} /* end if */

    } /* end if */

    /* Truncate the journal file. */
    if ( pass2 ) {

	if ( H5C2_jb__trunc(/* H5C2_jbrb_t */  &jbrb_struct) 
           != SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_jb__trunc failed";

	} /* end if */

    } /* end if */

    /* Take down the journal file. */
    if ( pass2 ) {

	if (H5C2_jb__takedown(/* H5C2_jbrb_t */  &jbrb_struct) 
           != SUCCEED) {

	    pass2 = FALSE;
	    failure_mssg2 = "H5C2_jb__takedown failed";

	} /* end if */

    } /* end if */

    if ( show_progress ) /* 13 */ 
        HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
                  checkpoint++, (int)pass2);

    /* report pass / failure information */
    if ( pass2 ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass2 ) {

	failures2++;
        HDfprintf(stdout, "%s: failure_mssg2 = \"%s\".\n",
                  fcn_name, failure_mssg2);

    }

    return;

} /* end check_transaction_tracking */



/***************************************************************************
 * Function: 	write_verify_trans_num
 *
 * Purpose:  	Helper function for check_transaction_tracking test. Writes a 
 *              journal entry of length 100 into the ring buffer, provided that
 *              the transaction number of the journal entry is less than 1000, 
 *              and then verifies that the recorded last transaction on disk is 
 *              as specified in verify_val. 
 *
 * Return:      void
 *
 * Programmer: 	Mike McGreevy <mcgreevy@hdfgroup.org>
 *              Thursday, February 28, 2008
 * 
 **************************************************************************/
static void
write_verify_trans_num(H5C2_jbrb_t * struct_ptr, 
                       uint64_t trans_num, 
                       uint64_t verify_val)
{
    uint64_t trans_verify;
    
    /* Write an entire transaction. (start, journal entry, end).
     * As long as the supplied transaction number is less than 1000,
     * the total length of the transaction will be 100. For cases where
     * the transaction number increases in number of digits, the amount
     * of data in the body is reduced to account for the extra trans digits,
     * so transactions remain at size 100. Note that data is converted
     * into hex, so reducing input by one character reduces journal entry 
     * by three (two hex characters and a space).
     */  
    if ( pass2 ) {
        
       	if ( H5C2_jb__start_transaction(/* H5C2_jbrb_t  */  struct_ptr, 
                                        /* trans number */  trans_num)
           != SUCCEED) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_jb__start_transaction failed";

       	} /* end if */


        if (trans_num < 10) {

	        if ( H5C2_jb__journal_entry(/* H5C2_jbrb_t   */  struct_ptr, 
                                            /* Transaction # */  trans_num,
                                            /* Base Address  */  (haddr_t)16, 
                                            /* Length        */  9, 
                                            /* Body          */  (const uint8_t *)"XXXXXXXXX")
                   != SUCCEED ) {

	            pass2 = FALSE;
	            failure_mssg2 = "H5C2_jb__journal_entry failed";

	        } /* end if */

        } /* end if */

        else if (trans_num < 100) {

	        if ( H5C2_jb__journal_entry(/* H5C2_jbrb_t   */  struct_ptr, 
                                            /* Transaction # */  trans_num,
                                            /* Base Address  */  (haddr_t)16, 
                                            /* Length        */  8, 
                                            /* Body          */  (const uint8_t *)"XXXXXXXX")
                   != SUCCEED ) {

	            pass2 = FALSE;
	            failure_mssg2 = "H5C2_jb__journal_entry failed";

	        } /* end if */

        } /* end else if */

        else {

	        if ( H5C2_jb__journal_entry(/* H5C2_jbrb_t   */  struct_ptr, 
                                            /* Transaction # */  trans_num,
                                            /* Base Address  */  (haddr_t)16, 
                                            /* Length        */  7, 
                                            /* Body          */  (const uint8_t *)"XXXXXXX")
                   != SUCCEED ) {

	            pass2 = FALSE;
	            failure_mssg2 = "H5C2_jb__journal_entry failed";

	        } /* end if */

        } /* end else */

	if ( H5C2_jb__end_transaction(/* H5C2_jbrb_t   */  struct_ptr, 
                                      /* Transaction # */  trans_num)
           != SUCCEED ) {

	    pass2 = FALSE;
	    failure_mssg2 = "H5C2_jb__end_transaction failed";

	} /* end if */

    } /* end if */

    /* Make sure the last transaction that's on disk is as expected. */
    if ( pass2 ) {

        if ( H5C2_jb__get_last_transaction_on_disk(
                                              /* H5C2_jbrb_t  */  struct_ptr,
                                              /* trans number */  &trans_verify)
           != SUCCEED ) {
            
            pass2 = FALSE;
            failure_mssg2 = "H5C2_jb__get_last_transaction_on_disk failed";

        } /* end if */

        if ( trans_verify != verify_val) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_jb__get_last_transaction_on_disk returned the wrong transaction number!";

        } /* end if */

    } /* end if */

    return;

} /* end write_verify_trans_num */


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Run tests on the cache code contained in H5C2.c
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
    int express_test;

    failures2 = 0;

    H5open();

    express_test = GetTestExpress();
    
#if 1
    mdj_smoke_check_00();
#endif
#if 1
    mdj_smoke_check_01();
#endif
#if 1
    mdj_smoke_check_02();
#endif
#if 1
    check_buffer_writes();
    check_legal_calls();
    check_message_format();
    check_transaction_tracking();
#endif
#if 1
    check_superblock_extensions();
#endif 
#if 1
    check_mdj_config_block_IO();
#endif 
#if 1
    check_mdj_file_marking();
#endif

    return(failures2);

} /* main() */

