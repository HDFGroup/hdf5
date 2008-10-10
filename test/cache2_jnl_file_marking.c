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
        "test_in_progress",
        NULL
};


/* private function declarations: */

/* utility functions */

static void check_test_in_progress(const char * str);

static hbool_t file_exists(const char * file_path_ptr);

static void mark_test_in_progress(const char * str);

static void setup_cache_for_journaling(const char * hdf_file_name,
                                       const char * journal_file_name,
                                       hid_t * file_id_ptr,
                                       H5F_t ** file_ptr_ptr,
                                       H5C2_t ** cache_ptr_ptr,
				       hbool_t use_core_driver_if_avail);

static void usage(void);

/* test functions */

static void setup_mdj_file_marking_after_open_test(hbool_t verbose);
static void check_mdj_file_marking_after_open_test(hbool_t verbose);

static void setup_mdj_file_marking_on_create_test(hbool_t verbose);
static void check_mdj_file_marking_on_create_test(hbool_t verbose);

static void setup_mdj_file_marking_on_open_test(hbool_t verbose);
static void check_mdj_file_marking_on_open_test(hbool_t verbose);

static void setup_mdj_file_unmarking_on_file_close_test(hbool_t verbose);
static void check_mdj_file_unmarking_on_file_close_test(hbool_t verbose);

static void setup_mdj_file_unmarking_on_journaling_shutdown_test(hbool_t verbose);
static void check_mdj_file_unmarking_on_journaling_shutdown_test(hbool_t verbose);

static void setup_mdj_file_unmarking_on_recovery_test(hbool_t verbose);
static void check_mdj_file_unmarking_on_recovery_test(hbool_t verbose);


/**************************************************************************/
/**************************************************************************/
/********************************* tests: *********************************/
/**************************************************************************/
/**************************************************************************/

/*** metadata journaling test utility functions ***/

/*-------------------------------------------------------------------------
 * Function:    check_test_in_progress()
 *
 * Purpose:     If pass2 is true on entry, test to see if the test in 
 *		progress file exists.  If it does not, set pass2 to FALSE
 *		and set a failure message.
 *
 *		If the test in progress file does exist, check to see if 
 *		its contents matches the supplied string.  It it does not,
 *		set pass2 to FALSE and set the appropriate failure message.
 *
 *              Do nothing if pass2 is FALSE on entry.
 *
 * Return:      void
 *
 * Programmer:  John Mainzer
 *              10/9/08
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
check_test_in_progress(const char * str)

{
    const char * fcn_name = "check_test_in_progress()";
    char buffer[512];
    char filename[512];
    hbool_t show_progress = FALSE;
    hbool_t verbose = FALSE;
    size_t input_len;
    int cp = 0;
    herr_t result;
    int fd = -1;
    h5_stat_t buf;

    if ( show_progress ) {

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass2, cp++);
	HDfflush(stdout);
    }

    if ( pass2 ) {

        if ( ( str == NULL ) ||
             ( strlen(str) <= 0 ) ||
             ( strlen(str) >= 512 ) ) {

            pass2 = FALSE;
            failure_mssg2 = "bad str on entry to check_test_in_progress().";
        }
    }

    if ( show_progress ) {

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass2, cp++);
	HDfflush(stdout);
    }

    /* setup the journal file name */
    if ( pass2 ) {

        if ( h5_fixname(FILENAMES[4], H5P_DEFAULT, filename, 
                        sizeof(filename)) == NULL ) {

            pass2 = FALSE;
            failure_mssg2 = "h5_fixname() failed.\n";
        }
        else if ( strlen(filename) >= 512 ) {

            pass2 = FALSE;
            failure_mssg2 = "test in progress file name too long.\n";
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

    if ( ( pass2 ) && ( ! file_exists(filename) ) ) {

        pass2 = FALSE;
        failure_mssg2 = "test not in progress?!?";
    }


    /* get the length of the test in progress file */
    if ( pass2 ) {

	if ( HDstat(filename, &buf) != 0 ) {

	    if ( verbose ) {

	        HDfprintf(stdout, "%s: HDstat() failed with errno = %d.\n",
                          fcn_name, errno);
	    }
	    failure_mssg2 = "stat() failed on test in progress file.";
	    pass2 = FALSE;

	} else {

	    if ( (buf.st_size) == 0 ) {

                failure_mssg2 = "test in progress file empty?!?";
	        pass2 = FALSE;

            } else if ( (buf.st_size) >= 512 ) {

                failure_mssg2 = "test in progress file too big?!?";
	        pass2 = FALSE;

	    } else {
                
	        input_len = (size_t)(buf.st_size);

		if ( verbose ) {

		    HDfprintf(stdout, "%s: input_len = %d.\n", 
		              fcn_name, (int)input_len);
		}
            }
	} 
    }

    /* open the test in progress file */
    if ( pass2 ) {

	if ( (fd = HDopen(filename, O_RDONLY, 0777)) == -1 ) {

	    if ( verbose ) {

	        HDfprintf(stdout, "%s: HDopen(i) failed with errno = %d.\n",
                          fcn_name, errno);
	    }
            failure_mssg2 = "Can't open test in progress file.";
	    pass2 = FALSE;
        }
    }

    /* read the contents of the test in progress file */
    if ( pass2 )
    {
        result = HDread(fd, buffer, input_len);

        if ( result != (int)input_len ) {

            if ( verbose ) {

                HDfprintf(stdout, 
                          "%s: HDread() failed. result = %d, errno = %d.\n",
                          fcn_name, (int)result, errno);
            }
            failure_mssg2 = "error reading test in progress file.";
            pass2 = FALSE;
        }

        buffer[input_len] = '\0';
    }

    if ( fd != -1 ) {

        if ( HDclose(fd) != 0 ) {

	    if ( verbose ) {

	        HDfprintf(stdout, "%s: HDclose() failed with errno = %d.\n",
                          fcn_name, errno);
	    }

	    if ( pass2 ) {

                failure_mssg2 = "Can't close test in progress file.";
	        pass2 = FALSE;
	    }
	}
    }

    HDremove(filename);

    if ( pass2 ) {

        if ( strcmp(str, buffer) != 0 ) {

	    if ( verbose ) {

	        HDfprintf(stdout, 
                          "%s: expected/actual test in progress = %s/%s\n",
                          fcn_name, str, buffer);
	    }

            pass2 = FALSE;
            failure_mssg2 = "Unexpected test in progress?!?";
        }
    }

    return;

} /* check_test_in_progress() */


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
 * Function:    mark_test_in_progress()
 *
 * Purpose:     If pass2 is true on entry, test to see if the test in 
 *		progress file exists.  If it does, set pass2 to FALSE
 *		and set a failure message.
 *
 *		If the test in progress file doesn't exist, create it,
 *		open it, write the supplied string to it, and then close
 *		it.  If any errors are detected, set pass2 to FALSE, and
 *		set the appropriate failure message.
 *
 *              Do nothing if pass2 is FALSE on entry.
 *
 * Return:      void
 *
 * Programmer:  John Mainzer
 *              10/9/08
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
mark_test_in_progress(const char * str)

{
    const char * fcn_name = "mark_test_in_progress()";
    char filename[512];
    hbool_t show_progress = FALSE;
    hbool_t verbose = FALSE;
    int cp = 0;
    herr_t result;
    int fd = -1;

    if ( show_progress ) {

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass2, cp++);
	HDfflush(stdout);
    }

    if ( pass2 ) {

        if ( ( str == NULL ) ||
             ( strlen(str) >= 512 ) ) {

            pass2 = FALSE;
            failure_mssg2 = "bad str on entry to mark_test_in_progress().";
        }
    }

    if ( show_progress ) {

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass2, cp++);
	HDfflush(stdout);
    }

    /* setup the journal file name */
    if ( pass2 ) {

        if ( h5_fixname(FILENAMES[4], H5P_DEFAULT, filename, 
                        sizeof(filename)) == NULL ) {

            pass2 = FALSE;
            failure_mssg2 = "h5_fixname() failed.\n";
        }
        else if ( strlen(filename) >= 512 ) {

            pass2 = FALSE;
            failure_mssg2 = "test in progress file name too long.\n";
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

    if ( ( pass2 ) && ( file_exists(filename) ) ) {

        pass2 = FALSE;
        failure_mssg2 = "test already in progress?!?";
    }

    /* open the test in progress file */
    if ( pass2 ) {

	if ( (fd = HDopen(filename, O_WRONLY|O_CREAT|O_TRUNC, 0777))
             == -1 ) {

	    if ( verbose ) {

	        HDfprintf(stdout, "%s: HDopen(i) failed with errno = %d.\n",
                          fcn_name, errno);
	    }
            failure_mssg2 = "Can't open test in progress file.";
	    pass2 = FALSE;
        }
    }

    if ( pass2 ) {
        
        result = HDwrite(fd, str, strlen(str));

        if ( result != (int)strlen(str) ) {

            if ( verbose ) {

                HDfprintf(stdout, 
                          "%s: HDwrite() failed. result = %d, errno = %d.\n",
                          fcn_name, (int)result, errno);
            }
            failure_mssg2 = "error writing test in progress file.";
            pass2 = FALSE;
        }
    }

    if ( fd != -1 ) {

        if ( HDclose(fd) != 0 ) {

	    if ( verbose ) {

	        HDfprintf(stdout, "%s: HDclose() failed with errno = %d.\n",
                          fcn_name, errno);
	    }

	    if ( pass2 ) {

                failure_mssg2 = "Can't close test in progress file.";
	        pass2 = FALSE;
	    }
	}
    }

    return;

} /* mark_test_in_progress() */


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

            file_ptr = (H5F_t *)H5I_object_verify(file_id, H5I_FILE);

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


/***************************************************************************
 * Function: 	setup_mdj_file_marking_on_create_test
 *
 * Purpose:  	Setup test to verify that HDF5 file is marked as having 
 *              journaling in progress when journaling is enabled at file 
 *              creation time.
 *
 *              Do this by creating a test file with metadata journaling 
 *              enabled, and then exiting without closing the file.
 *
 * Return:      void
 *
 * Programmer: 	John Mainzer
 *              10/8/08
 * 
 **************************************************************************/

static void 
setup_mdj_file_marking_on_create_test(hbool_t verbose)
{
    const char * fcn_name = "setup_mdj_file_marking_on_create_test():";
    char filename[512];
    char journal_filename[H5AC2__MAX_JOURNAL_FILE_NAME_LEN + 1];
    hbool_t show_progress = FALSE;
    int cp = 0;
    uint64_t trans_num;
    hid_t file_id = -1;
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

        HDfprintf(stdout, "%s%d cp = %d.\n", 
		  fcn_name, (int)pass2, cp++);
	HDfflush(stdout);
    }

    if ( verbose ) { 
        HDfprintf(stdout, "%s journal filename = \"%s\".\n", 
	          fcn_name, journal_filename); 
	HDfflush(stdout);
    }

    if ( pass2 ) {

        /* clean out any existing journal file */
        HDremove(journal_filename);
        setup_cache_for_journaling(filename, journal_filename, &file_id,
                                   &file_ptr, &cache_ptr, FALSE);

        if ( show_progress ) {

	    HDfprintf(stdout, "%s%d: cp = %d.\n", fcn_name, (int)pass2, cp++);
	    HDfflush(stdout);
        }

        /* run a dummy transaction to fource metadata journaling
         * initialization.
         */
        H5C2_begin_transaction(cache_ptr, &trans_num, "dummy");
        H5C2_end_transaction(file_ptr, H5AC2_dxpl_id, cache_ptr, 
			     trans_num, "dummy");

        if ( show_progress ) {

	    HDfprintf(stdout, "%s%d: cp = %d.\n", fcn_name, (int)pass2, cp++);
	    HDfflush(stdout);
        }

        if ( ( verbose ) && ( ! pass2 ) ) {
            HDfprintf(stdout, "%s%d failure_mssg = \"%s\".\n", 
		      fcn_name, pass2, failure_mssg2);
	    HDfflush(stdout);
        } 

        if ( show_progress ) {

	    HDfprintf(stdout, "%s%d cp = %d child exiting.\n", 
		      fcn_name, (int)pass2, cp++);
	    HDfflush(stdout);
	}

        mark_test_in_progress("mdj_file_marking_on_create_test");

	abort();

    } 

    return;

} /* setup_mdj_file_marking_on_create_test() */


/***************************************************************************
 * Function: 	check_mdj_file_marking_on_create_test
 *
 * Purpose:  	Check to see if a test to verify that a HDF5 file is marked 
 *              as having journaling in progress when journaling is enabled 
 *              at file creation time passes.
 *
 *              Do this by trying to open the test file created by the 
 *              associated setup function.  Open should fail, as the file 
 *              should be marked as journaling in progress.
 *
 *              On either success or failure, clean up the test file and
 *              the associated journal file.
 *
 * Return:      void
 *
 * Programmer: 	John Mainzer
 *              10/8/08
 * 
 **************************************************************************/

static void 
check_mdj_file_marking_on_create_test(hbool_t verbose)
{
    const char * fcn_name = "check_mdj_file_marking_on_create_test():";
    char filename[512];
    char journal_filename[H5AC2__MAX_JOURNAL_FILE_NAME_LEN + 1];
    hbool_t show_progress = FALSE;
    int cp = 0;
    hid_t file_id = -1;
    hid_t fapl_id = -1;

    check_test_in_progress("mdj_file_marking_on_create_test");

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

	if ( show_progress ) {

            HDfprintf(stdout, "%s:%d: cp = %d  child exited as expected.\n", 
		      fcn_name, (int)pass2, cp++);
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
	    HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, (int)pass2, cp++);
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
	    HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, (int)pass2, cp++);
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

            HDfprintf(stdout, "%s%d: cp = %d.\n", fcn_name, (int)pass2, cp++);
	    HDfflush(stdout);
        }

        /* delete the HDF5 file and journal file */
#if 1
        HDremove(filename);
        HDremove(journal_filename);
#endif
	if ( show_progress ) {

            HDfprintf(stdout, "%s%d cp = %d parent done.\n", 
		      fcn_name, (int)pass2, cp++);
	    HDfflush(stdout);
        }
    } 

    return;

} /* check_mdj_file_marking_on_create_test() */


/***************************************************************************
 * Function: 	setup_mdj_file_marking_after_open_test
 *
 * Purpose:  	Setup a test to verify that a HDF5 file is marked as having 
 *              journaling in progress when journaling is enabled on an 
 *              open file.
 *
 *              Do this by:
 *
 *              1) creating a test file, 
 *
 *              2) writing some data to it, 
 *
 *              3) enable journaling on the open file via a call to
 *                 H5Fset_mdc_config()
 *
 *              4) exiting without closing the file.
 *
 *              set pass2 to FALSE and set a failure message if errors
 *              are detected.
 *
 * Return:      void
 *
 * Programmer: 	John Mainzer
 *              10/8/08
 * 
 **************************************************************************/

static void 
setup_mdj_file_marking_after_open_test(hbool_t verbose)
{
    const char * fcn_name = "setup_mdj_file_marking_after_open_test():";
    char filename[512];
    char journal_filename[H5AC2__MAX_JOURNAL_FILE_NAME_LEN + 1];
    hbool_t show_progress = FALSE;
    herr_t result;
    int cp = 0;
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

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass2, cp++);
	HDfflush(stdout);
    }

    if ( verbose ) { 
        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass2, cp++);
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

        /* create a file access propertly list. */
        if ( pass2 ) {

            fapl_id = H5Pcreate(H5P_FILE_ACCESS);

            if ( fapl_id < 0 ) {

                pass2 = FALSE;
                failure_mssg2 = "H5Pcreate() failed.\n";
            }
        }

        if ( show_progress ) {

            HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass2, cp++);
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

            file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);

            if ( file_id < 0 ) {

	        pass2 = FALSE;
                failure_mssg2 = "H5Fcreate() failed.\n";
	    }
	}

        if ( show_progress ) {

	    HDfprintf(stdout, "%s%d: cp = %d.\n", fcn_name, (int)pass2, cp++);
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

	    HDfprintf(stdout, "%s%d: cp = %d.\n", fcn_name, (int)pass2, cp++);
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

	    HDfprintf(stdout, "%s%d: cp = %d.\n", fcn_name, (int)pass2, cp++);
	    HDfflush(stdout);
        }

        if ( ( verbose ) && ( ! pass2 ) ) {
            HDfprintf(stdout, "%s%d failure_mssg = \"%s\".\n", 
		      fcn_name, pass2, failure_mssg2);
	    HDfflush(stdout);
        } 

        if ( show_progress ) {

           HDfprintf(stdout, "%s%d cp = %d exiting.\n", 
	             fcn_name, (int)pass2, cp++);
	    HDfflush(stdout);
        }

        mark_test_in_progress("mdj_file_marking_after_open_test");

        abort();

    } 

    return;

} /* setup_mdj_file_marking_after_open_test() */


/***************************************************************************
 * Function: 	check_mdj_file_marking_after_open_test
 *
 * Purpose:  	Check to see if the test to rvVerify that a HDF5 file is 
 *              marked as having journaling in progress when journaling is 
 *              enabled on an open file passed.
 *
 *              Try to open the test file created by the associated setup
 *              function.  Open should fail, as the file should be marked 
 *              as journaling in progress.
 *
 *              On either success or failure, clean up the test file and
 *              the associated journal file.
 *
 * Return:      void
 *
 * Programmer: 	John Mainzer
 *              10/8/08
 * 
 **************************************************************************/

static void 
check_mdj_file_marking_after_open_test(hbool_t verbose)
{
    const char * fcn_name = "check_mdj_file_marking_after_open_test():";
    char filename[512];
    char journal_filename[H5AC2__MAX_JOURNAL_FILE_NAME_LEN + 1];
    hbool_t show_progress = FALSE;
    int cp = 0;
    hid_t file_id = -1;
    hid_t fapl_id = -1;

    check_test_in_progress("mdj_file_marking_after_open_test");

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

        HDfprintf(stdout, "%s%d cp = %d.\n", 
		  fcn_name, (int)pass2, cp++);
	HDfflush(stdout);
    }

    if ( verbose ) { 
        HDfprintf(stdout, "%s journal filename = \"%s\".\n", 
	          fcn_name, journal_filename); 
	HDfflush(stdout);
    }

    if ( pass2 ) {

        /* create a file access propertly list. */
        if ( pass2 ) {

            fapl_id = H5Pcreate(H5P_FILE_ACCESS);

            if ( fapl_id < 0 ) {

                pass2 = FALSE;
                failure_mssg2 = "H5Pcreate() failed.\n";
            }
        }

        if ( show_progress ) {
	    HDfprintf(stdout, "%s:%d cp = %d.\n", 
		      fcn_name, (int)pass2, cp++);
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
	    HDfprintf(stdout, "%s%d cp = %d.\n", 
		      fcn_name, (int)pass2, cp++);
	    HDfflush(stdout);
        }

	/* attempt to open the file -- should fail as the setup program
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

            HDfprintf(stdout, "%s%d: cp = %d.\n", 
                      fcn_name, (int)pass2, cp++);
            HDfflush(stdout);
        }

        /* delete the HDF5 file and journal file */
#if 1
        HDremove(filename);
        HDremove(journal_filename);
#endif
	if ( show_progress ) {

            HDfprintf(stdout, "%s%d cp = %d parent done.\n", 
		      fcn_name, (int)pass2, cp++);
	    HDfflush(stdout);
	}
    } 

    return;

} /* check_mdj_file_marking_after_open_test() */


/***************************************************************************
 * Function: setup_mdj_file_marking_on_open_test
 *
 * Purpose:  	Setup test to verify that a HDF5 file is marked as having 
 *		journaling in progress when journaling is enabled at file 
 *		open time.
 *
 *              Do this by: 
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
 * Return:      void
 *
 * Programmer: 	John Mainzer
 *              10/8/08
 * 
 **************************************************************************/

static void 
setup_mdj_file_marking_on_open_test(hbool_t verbose)
{
    const char * fcn_name = "setup_mdj_file_marking_on_open_test():";
    char filename[512];
    char journal_filename[H5AC2__MAX_JOURNAL_FILE_NAME_LEN + 1];
    hbool_t show_progress = FALSE;
    herr_t result;
    int cp = 0;
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

        HDfprintf(stdout, "%s%d cp = %d.\n", 
		  fcn_name, (int)pass2, cp++);
	HDfflush(stdout);
    }

    if ( verbose ) { 
        HDfprintf(stdout, "%s journal filename = \"%s\".\n", 
	          fcn_name, journal_filename); 
	HDfflush(stdout);
    }

    if ( pass2 ) {

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

        /* open the file with a fapl indicating latest version of 
         * the file format.
         */
        if ( pass2 ) {

            file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);

            if ( file_id < 0 ) {

	        pass2 = FALSE;
                failure_mssg2 = "H5Fcreate() failed.\n";
	    }
        }
#endif /* JRM */
        if ( show_progress ) {

            HDfprintf(stdout, "%s%d: cp = %d.\n", fcn_name, (int)pass2, cp++);
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

        if ( pass2 ) {

            /* close the data set, the data space, and the file */
	    if ( ( H5Dclose(dataset_id) < 0 ) ||
	         ( H5Sclose(dataspace_id) < 0 ) ||
#if 1 /* JRM */
                 ( H5Pclose(fapl_id) < 0 ) ||
#endif /* JRM */
	         ( H5Fclose(file_id) < 0 ) ) {

                pass2 = FALSE;
	        failure_mssg2 = "data set, data space, or file close failed.";
	    }
        }

        if ( show_progress ) {

            HDfprintf(stdout, "%s%d: cp = %d.\n", fcn_name, (int)pass2, cp++);
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

            HDfprintf(stdout, "%s%d: cp = %d.\n", fcn_name, (int)pass2, cp++);
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

	    HDfprintf(stdout, "%s%d: cp = %d.\n", fcn_name, (int)pass2, cp++);
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

            HDfprintf(stdout, "%s%d: cp = %d.\n", fcn_name, (int)pass2, cp++);
	    HDfflush(stdout);
        }

        /* open the file using fapl_id */
        if ( pass2 ) {

            file_id = H5Fopen(filename, H5F_ACC_RDWR, fapl_id);

            if ( file_id < 0 ) {

                pass2 = FALSE;
                failure_mssg2 = "H5Fopen() failed (9).\n";

            } else {

                file_ptr = (H5F_t *)H5I_object_verify(file_id, H5I_FILE);

                if ( file_ptr == NULL ) {

                    pass2 = FALSE;
                    failure_mssg2 = "Can't get file_ptr.";

                    if ( verbose ) {

                        HDfprintf(stdout, "%s: Can't get file_ptr.\n",fcn_name);
                    }
                }
            }
        }

        if ( show_progress ) {

            HDfprintf(stdout, "%s%d: cp = %d.\n", fcn_name, (int)pass2, cp++);
	    HDfflush(stdout);
        }

        if ( ( verbose ) && ( ! pass2 ) ) {

            HDfprintf(stdout, "%s%d failure_mssg = \"%s\".\n", 
		      fcn_name, pass2, failure_mssg2);
	    HDfflush(stdout);
        } 

        if ( show_progress ) {

            HDfprintf(stdout, "%s%d cp = %d exiting.\n", 
                      fcn_name, (int)pass2, cp++);
	    HDfflush(stdout);
        }

        mark_test_in_progress("mdj_file_marking_on_open_test");

        abort();
    } 

    return;

} /* setup_mdj_file_marking_on_open_test() */


/***************************************************************************
 * Function: 	check_mdj_file_marking_on_open_test
 *
 * Purpose:  	Check to verify that a HDF5 file is marked as having 
 *              journaling in progress when journaling is enabled at 
 *		file open time.
 *
 *              Do this by trying to open the test file created by the
 *		associated setup function.
 *
 *              Open should fail, as the file should be marked as journaling
 *              in progress.
 *
 *              On either success or failure, clean up the test file and
 *              the associated journal file.
 *
 * Return:      void
 *
 * Programmer: 	John Mainzer
 *              10/8/08
 * 
 **************************************************************************/

static void 
check_mdj_file_marking_on_open_test(hbool_t verbose)
{
    const char * fcn_name = "check_mdj_file_marking_on_open_test():";
    char filename[512];
    char journal_filename[H5AC2__MAX_JOURNAL_FILE_NAME_LEN + 1];
    hbool_t show_progress = FALSE;
    int cp = 0;
    hid_t file_id = -1;
    hid_t fapl_id = -1;

    check_test_in_progress("mdj_file_marking_on_open_test");

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

        /* create a file access propertly list. */
        if ( pass2 ) {

            fapl_id = H5Pcreate(H5P_FILE_ACCESS);

            if ( fapl_id < 0 ) {

                pass2 = FALSE;
                failure_mssg2 = "H5Pcreate() failed.\n";
            }
        }

        if ( show_progress ) {
	    HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, (int)pass2, cp++);
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
	    HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, (int)pass2, cp++);
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

            HDfprintf(stdout, "%s%d: cp = %d.\n", fcn_name, (int)pass2, cp++);
	    HDfflush(stdout);
        }

        /* delete the HDF5 file and journal file */
#if 1
        HDremove(filename);
        HDremove(journal_filename);
#endif
	if ( show_progress ) {

            HDfprintf(stdout, "%s%d cp = %d done.\n", 
                      fcn_name, (int)pass2, cp++);
	    HDfflush(stdout);
        }
    } 

    return;

} /* check_mdj_file_marking_on_open_test() */


/***************************************************************************
 * Function: 	setup_mdj_file_unmarking_on_file_close_test
 *
 * Purpose:  	Setup test to verify that a HDF5 file on which journaling 
 *		is enabled is marked as having not having journaling in 
 *		progress when the file is closed.
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
 * Return:      void
 *
 * Programmer: 	John Mainzer
 *              10/8/08
 * 
 **************************************************************************/

static void 
setup_mdj_file_unmarking_on_file_close_test(hbool_t verbose)
{
    const char * fcn_name = "setup_mdj_file_unmarking_on_file_close_test():";
    char filename[512];
    char journal_filename[H5AC2__MAX_JOURNAL_FILE_NAME_LEN + 1];
    hbool_t show_progress = FALSE;
    int cp = 0;
    hid_t file_id = -1;
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

    mark_test_in_progress("mdj_file_unmarking_on_file_close_test");

    return;

} /* setup_mdj_file_unmarking_on_file_close_test() */


/***************************************************************************
 * Function: 	check_mdj_file_unmarking_on_file_close_test
 *
 * Purpose:  	Check to verify that a HDF5 file on which journaling is 
 *		enabled is marked as having not having journaling in 
 *		progress when the file is closed.
 *
 *              To do this, attempt to re-open the file created by the
 *		associated setup function.  This should succeed.
 *
 *              On either success or failure, clean up the test file and
 *              the associated journal file.
 *
 * Return:      void
 *
 * Programmer: 	John Mainzer
 *              10/8/08
 * 
 **************************************************************************/

static void 
check_mdj_file_unmarking_on_file_close_test(hbool_t verbose)
{
    const char * fcn_name = "check_mdj_file_unmarking_on_file_close_test():";
    char filename[512];
    char journal_filename[H5AC2__MAX_JOURNAL_FILE_NAME_LEN + 1];
    hbool_t show_progress = FALSE;
    int cp = 0;
    hid_t fapl_id = -1;
    hid_t file_id = -1;

    if ( show_progress ) {

        HDfprintf(stdout, "%s%d cp = %d -- entering.\n", fcn_name, pass2, cp++);
	HDfflush(stdout);
    }

    check_test_in_progress("mdj_file_unmarking_on_file_close_test");

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

    /* attempt to re-open file created by setup.  Should succeed */

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

        HDfprintf(stdout, "%s%d: cp = %d.\n", fcn_name, (int)pass2, cp++);
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
 * Function: 	setup_mdj_file_unmarking_on_journaling_shutdown_test
 *
 * Purpose:  	Setup test to verify that a HDF5 file on which journaling 
 *		is enabled is marked as having not having journaling in 
 *		progress when journaling is disabled via the 
 *		H5Fset_mdc_config() API call.
 *
 *              Do this by:
 *
 *                  1) creating a test file in the child with metadata 
 *                     journaling enabled, 
 *
 *                  2) performing some operation(s) that dirty metadata
 *                     and result in journal activity.
 *
 *                  3) using the H5Fset_mdc_config() to disable journaling.
 *
 *                  4) exiting from the child without closing the file.
 *
 * Return:      void
 *
 * Programmer: 	John Mainzer
 *              7/9/08
 * 
 **************************************************************************/

static void 
setup_mdj_file_unmarking_on_journaling_shutdown_test(hbool_t verbose)
{
    const char * fcn_name = 
	    "setup_mdj_file_unmarking_on_journaling_shutdown_test():";
    char filename[512];
    char journal_filename[H5AC2__MAX_JOURNAL_FILE_NAME_LEN + 1];
    hbool_t show_progress = FALSE;
    herr_t result;
    int cp = 0;
    hid_t file_id = -1;
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

            HDfprintf(stdout, "%s%d: cp = %d.\n", fcn_name, (int)pass2, cp++);
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

            HDfprintf(stdout, "%s%d: cp = %d.\n", fcn_name, (int)pass2, cp++);
	    HDfflush(stdout);
        }

        if ( ( verbose ) && ( ! pass2 ) ) {
            HDfprintf(stdout, "%s%d failure_mssg = \"%s\".\n", 
                     fcn_name, pass2, failure_mssg2);
            HDfflush(stdout);
        } 

        if ( show_progress ) {

            HDfprintf(stdout, "%s%d cp = %d exiting.\n", 
                      fcn_name, (int)pass2, cp++);
            HDfflush(stdout);
        }

        mark_test_in_progress("mdj_file_unmarking_on_journaling_shutdown_test");

        abort();
    } 

    return;

} /* setup_mdj_file_unmarking_on_journaling_shutdown_test() */


/***************************************************************************
 * Function: 	check_mdj_file_unmarking_on_journaling_shutdown_test
 *
 * Purpose:  	Check to verify that a HDF5 file on which journaling is 
 *		enabled is marked as having not having journaling in 
 *		progress when journaling is disabled via the 
 *		H5Fset_mdc_config() API	call.
 *
 *              Do this by trying to open the test file created by the 
 *		associated setup function.
 *
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
 *              10/8/08
 * 
 **************************************************************************/

static void 
check_mdj_file_unmarking_on_journaling_shutdown_test(hbool_t verbose)
{
    const char * fcn_name = 
	    "check_mdj_file_unmarking_on_journaling_shutdown_test():";
    char filename[512];
    char journal_filename[H5AC2__MAX_JOURNAL_FILE_NAME_LEN + 1];
    hbool_t show_progress = FALSE;
    int cp = 0;
    hid_t file_id = -1;
    hid_t fapl_id = -1;

    check_test_in_progress("mdj_file_unmarking_on_journaling_shutdown_test");

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

        /* create a file access propertly list. */
        if ( pass2 ) {

            fapl_id = H5Pcreate(H5P_FILE_ACCESS);

            if ( fapl_id < 0 ) {

                pass2 = FALSE;
                failure_mssg2 = "H5Pcreate() failed.\n";
            }
        }

        if ( show_progress ) {
	    HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, (int)pass2, cp++);
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
	    HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, (int)pass2, cp++);
	    HDfflush(stdout);
        }

	/* attempt to open the file -- should succeed as the setup function
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

            HDfprintf(stdout, "%s%d cp = %d done.\n", 
		      fcn_name, (int)pass2, cp++);
	    HDfflush(stdout);
	}
    } 

    return;

} /* check_mdj_file_unmarking_on_journaling_shutdown_test() */


/***************************************************************************
 * Function: 	setup_mdj_file_unmarking_on_recovery_test
 *
 * Purpose:  	Setup test to verify that HDF5 file that is marked as as 
 *		having journaling in progress is unmarked when the file 
 *		is opened with the journal_recovered flag set in the 
 *		cache configuration structure in the file access 
 *		property list.
 *
 *              Do this by creating a test file metadata journaling enabled, 
 *		and then exiting without closing the file.  Note that
 *              we must flush the file before exiting, as we want the file
 *              to be readable, but be marked as journaling in progress.
 *
 * Return:      void
 *
 * Programmer: 	John Mainzer
 *              7/14/08
 * 
 **************************************************************************/

static void 
setup_mdj_file_unmarking_on_recovery_test(hbool_t verbose)
{
    const char * fcn_name = "setup_mdj_file_unmarking_on_recovery_test():";
    char filename[512];
    char journal_filename[H5AC2__MAX_JOURNAL_FILE_NAME_LEN + 1];
    hbool_t show_progress = FALSE;
    int cp = 0;
    uint64_t trans_num;
    hid_t file_id = -1;
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
        HDremove(journal_filename);
        setup_cache_for_journaling(filename, journal_filename, &file_id,
                                   &file_ptr, &cache_ptr, FALSE);

        if ( show_progress ) {

            HDfprintf(stdout, "%s%d: cp = %d.\n", fcn_name, (int)pass2, cp++);
	    HDfflush(stdout);
        }

        /* run a dummy transaction to fource metadata journaling
         * initialization.
         */
        H5C2_begin_transaction(cache_ptr, &trans_num, "dummy");
        H5C2_end_transaction(file_ptr, H5AC2_dxpl_id, cache_ptr, 
                             trans_num, "dummy");

        if ( show_progress ) {

            HDfprintf(stdout, "%s%d: cp = %d.\n", fcn_name, (int)pass2, cp++);
	    HDfflush(stdout);
        }

        /* flush the file to ensure that it is in a readable state */
        if ( pass2 ) {

            if ( H5Fflush(file_id, H5F_SCOPE_GLOBAL) < 0 ) {

	        if ( verbose ) {

		    HDfprintf(stdout, "%s: H5Fflush() failed.\n", fcn_name);
                    HDfflush(stdout);
                }
	        pass2 = FALSE;
	        failure_mssg2 = "H5Fflush() failed.";
            }
        }

        if ( ( verbose ) && ( ! pass2 ) ) {
            HDfprintf(stdout, "%s%d failure_mssg = \"%s\".\n", 
                     fcn_name, pass2, failure_mssg2);
	    HDfflush(stdout);
        } 

        if ( show_progress ) {

            HDfprintf(stdout, "%s%d cp = %d exiting.\n", 
                      fcn_name, (int)pass2, cp++);
	    HDfflush(stdout);
        }

        mark_test_in_progress("mdj_file_unmarking_on_recovery_test");

        abort();
    } 

    return;

} /* setup_mdj_file_unmarking_on_recovery_test() */


/***************************************************************************
 * Function: 	check_mdj_file_unmarking_on_recovery_test
 *
 * Purpose:  	Check to verify that a HDF5 file that is marked as as 
 *		having journaling in progress is unmarked when the file 
 *		is opened with the journal_recovered flag set in the 
 *		cache configuration structure in the file access property 
 *		list.
 *
 *              Do this by trying to open the test file created by the
 *		assocated setup function.
 *
 *              Open should fail, as the file should be marked as journaling
 *              in progress.
 *
 *              Try to open again with the journal_recovered flag set.  This
 *              should succeed.  
 *
 *		Close and open again without the journal recovered flag 
 *		set to verify that the file is no longer marked as 
 *		having journaling in progress.
 *
 *              On either success or failure, clean up the test file and
 *              the associated journal file.
 *
 * Return:      void
 *
 * Programmer: 	John Mainzer
 *              10/8/08
 * 
 **************************************************************************/

static void 
check_mdj_file_unmarking_on_recovery_test(hbool_t verbose)
{
    const char * fcn_name = "check_mdj_file_unmarking_on_recovery_test():";
    char filename[512];
    char journal_filename[H5AC2__MAX_JOURNAL_FILE_NAME_LEN + 1];
    hbool_t show_progress = FALSE;
    herr_t result;
    int cp = 0;
    hid_t file_id = -1;
    hid_t fapl_id = -1;
    H5AC2_jnl_config_t jnl_config;

    check_test_in_progress("mdj_file_unmarking_on_recovery_test");

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

        /* create a file access propertly list. */
        if ( pass2 ) {

            fapl_id = H5Pcreate(H5P_FILE_ACCESS);

            if ( fapl_id < 0 ) {

                pass2 = FALSE;
                failure_mssg2 = "H5Pcreate() failed.\n";
            }
        }

        if ( show_progress ) {
	    HDfprintf(stdout, "%s:%d cp = %d.\n", fcn_name, (int)pass2, cp++);
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
	    HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, (int)pass2, cp++);
	    HDfflush(stdout);
        }

	/* attempt to open the file -- should fail as the setup fcn
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

            HDfprintf(stdout, "%s%d: cp = %d.\n", fcn_name, (int)pass2, cp++);
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

            HDfprintf(stdout, "%s%d: cp = %d.\n", fcn_name, (int)pass2, cp++);
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

            HDfprintf(stdout, "%s%d: cp = %d.\n", fcn_name, (int)pass2, cp++);
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

            HDfprintf(stdout, "%s%d: cp = %d.\n", fcn_name, (int)pass2, cp++);
	    HDfflush(stdout);
        }

	if ( pass2 ) {

	    if ( H5Fclose(file_id) < 0 ) {

                pass2 = FALSE;
	        failure_mssg2 = "H5Fclose() failed(1).";
	    }
	}

        if ( show_progress ) {

            HDfprintf(stdout, "%s%d: cp = %d.\n", fcn_name, (int)pass2, cp++);
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

            HDfprintf(stdout, "%s%d: cp = %d.\n", fcn_name, (int)pass2, cp++);
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

            HDfprintf(stdout, "%s%d: cp = %d.\n", fcn_name, (int)pass2, cp++);
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

            HDfprintf(stdout, "%s%d: cp = %d.\n", fcn_name, (int)pass2, cp++);
	    HDfflush(stdout);
        }

        /* delete the HDF5 file and journal file */
#if 1
        HDremove(filename);
        HDremove(journal_filename);
#endif
	if ( show_progress ) {

            HDfprintf(stdout, "%s%d cp = %d done.\n", 
                      fcn_name, (int)pass2, cp++);
	    HDfflush(stdout);
        }
    } 

    return;

} /* check_mdj_file_unmarking_on_recovery_test() */


/*-------------------------------------------------------------------------
 * Function:	usage
 *
 * Purpose:	Display a brief message describing the purpose and use
 * 		of the program.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              10/8/08
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

void
usage(void)

{
    const char * s[] = 
    {
	"\n",
        "cache2_jnl_file_marking:\n",
        "\n",
        "Setup or check the results of the specified test.\n",
        "\n",
        "usage:	cache2_jnl_file_marking <test> <op> [verbose]\n",
        "\n",
        "where:\n",
        "\n",
        "	<test> ::= ( file_marking_after_open |\n",
        "		     file_marking_on_create |\n",
        "	             file_marking_on_open |\n",
        "	             file_unmarking_on_file_close |\n",
        "	             file_unmarking_on_journaling_shutdown |\n",
        "	             file_unmarking_on_recovery )\n",
        "\n",
        "	<op> :: ( setup | check )\n",
        "\n",
        "Returns 0 on success, 1 on failure.\n",
        "\n",
	NULL,
    };
    int i = 0;

    while ( s[i] != NULL ) {

        HDfprintf(stdout, "%s", s[i]);
	i++;
    }

    return;

} /* usage() */


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Run the specified setup or check of metadata journaling 
 * 		HDF5 file marking or unmarking.
 *
 * Return:	Success: 0
 *
 *		Failure: A positive integer.
 *
 * Programmer:	John Mainzer
 *              10/8/08
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

int
main(int argc,
     char * argv[])
{
    int express_test;
    int result = 0;
    hbool_t setup = FALSE;
    hbool_t check = FALSE;
    hbool_t verbose = FALSE;

    express_test = GetTestExpress();

    pass2 = TRUE;

    if ( argc == 4 ) {

        if ( strcmp("verbose", argv[3]) == 0 ) {

	    verbose = TRUE;

	} else {

            pass2 = FALSE;
	    usage();

	}

    } else if ( argc != 3 ) {

        pass2 = FALSE;
	usage();
    }

    if ( verbose ) {

	HDfprintf(stdout, "%s %s %s %s:\n", argv[0], argv[1], argv[2], argv[3]);
    }

    if ( pass2 ) {

        if ( strcmp("setup", argv[2]) == 0 ) {

	    setup = TRUE;

	} else if ( strcmp("check", argv[2]) == 0 ) {

            check = TRUE;

        } else {

	    pass2 = FALSE;
	    usage();
	}
    }

    if ( pass2 ) {

        H5open();

        if ( strcmp("file_marking_after_open", argv[1]) == 0 ) {

            if ( setup ) {

	        setup_mdj_file_marking_after_open_test(verbose);

	    } else if ( check ) {

                check_mdj_file_marking_after_open_test(verbose);

	    } else {
	
		pass2 = FALSE;
		failure_mssg2 = "setup and check both FALSE?";
	    }

	} else if ( strcmp("file_marking_on_create", argv[1]) == 0 ) {

            if ( setup ) {

	        setup_mdj_file_marking_on_create_test(verbose);

	    } else if ( check ) {

                check_mdj_file_marking_on_create_test(verbose);

	    } else {
	
		pass2 = FALSE;
		failure_mssg2 = "setup and check both FALSE?";
	    }

	} else if ( strcmp("file_marking_on_open", argv[1]) == 0 ) {

            if ( setup ) {

	        setup_mdj_file_marking_on_open_test(verbose);

	    } else if ( check ) {

                check_mdj_file_marking_on_open_test(verbose);

	    } else {
	
		pass2 = FALSE;
		failure_mssg2 = "setup and check both FALSE?";
	    }

	} else if ( strcmp("file_unmarking_on_file_close", argv[1]) == 0 ) {

            if ( setup ) {

	        setup_mdj_file_unmarking_on_file_close_test(verbose);

	    } else if ( check ) {

                check_mdj_file_unmarking_on_file_close_test(verbose);

	    } else {
	
		pass2 = FALSE;
		failure_mssg2 = "setup and check both FALSE?";
	    }

	} else if ( strcmp("file_unmarking_on_journaling_shutdown", argv[1]) 
		    == 0 ) {

            if ( setup ) {

	        setup_mdj_file_unmarking_on_journaling_shutdown_test(verbose);

	    } else if ( check ) {

                check_mdj_file_unmarking_on_journaling_shutdown_test(verbose);

	    } else {
	
		pass2 = FALSE;
		failure_mssg2 = "setup and check both FALSE?";
	    }

	} else if ( strcmp("file_unmarking_on_recovery", argv[1]) == 0 ) {

            if ( setup ) {

	        setup_mdj_file_unmarking_on_recovery_test(verbose);

	    } else if ( check ) {

                check_mdj_file_unmarking_on_recovery_test(verbose);

	    } else {
	
		pass2 = FALSE;
		failure_mssg2 = "setup and check both FALSE?";
	    }

	} else {

	    pass2 = FALSE;
	    failure_mssg2 = "unknown test requested.";
	    usage();
	}
    }

    if ( verbose ) {

        if ( pass2 ) {

	    if ( setup ) {

	        HDfprintf(stdout, "test setup succeeded.\n");

	    } else if ( check ) {

	        HDfprintf(stdout, "test passed.\n");

            }
	} else {

	    HDfprintf(stdout, "FAILED.  Failure mssg = \"%s\"\n", 
		      failure_mssg2);
        }
    }

    if ( ! pass2 ) {

        result = 1;
    }

    return(result);

} /* main() */

