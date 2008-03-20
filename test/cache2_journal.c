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
 *              11/10/05
 *
 *		This file contains tests for the metadata journaling
 *		features implemented in H5C2.c and friends.
 */

#define H5F_PACKAGE             /*suppress error about including H5Fpkg   */

#include "h5test.h"
#include "H5Eprivate.h"
#include "H5Iprivate.h"
#include "H5MMprivate.h"        /* Memory management                    */
#include "H5AC2private.h"
#include "cache2_common.h"
#include "H5Fpkg.h"

#define HDF5_FILE_NAME "HDF5.file"

/* global variable declarations: */

const char *FILENAMES[] = {
        "cache_test",
        "cache_journal_test",
	"cache_sb_test",
        NULL
};


/* private function declarations: */

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

static void test_mdj_conf_blk_read_write_discard(H5C2_t * cache_ptr,
		                                 const char * jrnl_file_path);

static void check_superblock_extensions(void);

static void check_message_format(void);

static void check_legal_calls(void);

static void check_transaction_tracking(void);

static void write_verify_trans_num(H5C2_jbrb_t * struct_ptr, 
                                   unsigned long trans_num, 
                                   unsigned long verify_val);



/**************************************************************************/
/**************************************************************************/
/********************************* tests: *********************************/
/**************************************************************************/
/**************************************************************************/


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

    /* call H5Pset_latest_format() on the fapl_id */
    if ( pass2 ) {

	if ( H5Pset_latest_format(fapl_id, TRUE) < 0 ) {

            pass2 = FALSE;
            failure_mssg2 = "H5Pset_latest_format() failed.\n";
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

    test_mdj_conf_blk_read_write_discard(cache_ptr, "a");

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    test_mdj_conf_blk_read_write_discard(cache_ptr, "ab");

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    test_mdj_conf_blk_read_write_discard(cache_ptr, "abc");

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    test_mdj_conf_blk_read_write_discard(cache_ptr, "abcd");

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    test_mdj_conf_blk_read_write_discard(cache_ptr, "abcde");

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    test_mdj_conf_blk_read_write_discard(cache_ptr, 
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
    
        result = H5C2_create_journal_config_block(cache_ptr,
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

	if ( H5Fclose(file_id) ) {

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
            failure_mssg2 = "H5Fopen() failed (2).\n";
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

	result = H5C2_load_journal_config_block(cache_ptr,
			                        H5P_DATASET_XFER_DEFAULT,
						block_addr,
						block_len);
	
	if ( result != SUCCEED ) {

	    pass2 = FALSE;
	    failure_mssg2 = "H5C2_load_journal_config_block() failed.";
	    H5Eprint1(stdout);
	
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

	if ( H5Fclose(file_id) ) {

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
test_mdj_conf_blk_read_write_discard(H5C2_t * cache_ptr,
		                     const char * jrnl_file_path)
{
    const char * fcn_name = "test_mdj_conf_blk_read_write_discard()";
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
    
        result = H5C2_create_journal_config_block(cache_ptr,
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

	result = H5C2_load_journal_config_block(cache_ptr,
			                        H5P_DATASET_XFER_DEFAULT,
						block_addr,
						block_len);
	
	if ( result != SUCCEED ) {

	    pass2 = FALSE;
	    failure_mssg2 = "H5C2_load_journal_config_block() failed.";
	    H5Eprint1(stdout);
	
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

        result = H5C2_discard_journal_config_block(cache_ptr, 
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

static void
check_superblock_extensions(void)
{
    const char * fcn_name = "check_superblock_extensions()";
    const char * journal_file_name = "journal_file.txt";
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
    hsize_t             dims[2];


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

    /* call H5Pset_latest_format() on the fapl_id */
    if ( pass2 ) {

	if ( H5Pset_latest_format(fapl_id, TRUE) < 0 ) {

            pass2 = FALSE;
            failure_mssg2 = "H5Pset_latest_format() failed.\n";
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
        dataset_id = H5Dcreate(file_id, "/dset", H5T_STD_I32BE, dataspace_id,
                               H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

	if ( dataspace_id < 0 ) {

	    pass2 = FALSE;
	    failure_mssg2 = "H5Dcreate() failed.";
        }
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    if ( pass2 ) {

        /* close the data set, the data space, and the file */
	if ( ( H5Dclose(dataset_id) < 0 ) ||
	     ( H5Sclose(dataspace_id) < 0 ) ||
	     ( H5Fclose(file_id) ) ) {

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
            failure_mssg2 = "H5Fopen() failed (1).\n";
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

	if ( H5Fclose(file_id) ) {

            pass2 = FALSE;
	    failure_mssg2 = "file close failed (1).";
	}
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);


    /*****************************************************************/
    /* 5) Open the file a third time, and verify that the superblock */
    /*    extension indicates that the file is being journaled.      */
    /*****************************************************************/

    /* open the file r/w using the default FAPL */
    if ( pass2 ) {

        file_id = H5Fopen(filename, H5F_ACC_RDWR, H5P_DEFAULT);

        if ( file_id < 0 ) {

            pass2 = FALSE;
            failure_mssg2 = "H5Fopen() failed (2).\n";
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

	if ( H5Fclose(file_id) ) {

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
            failure_mssg2 = "H5Fopen() failed (3).\n";
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

	if ( H5Fclose(file_id) ) {

            pass2 = FALSE;
	    failure_mssg2 = "file close failed (3).";
	}
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    
    /***************************************************************/
    /*  9) Re-open the file, and verify that the second write in 8 */
    /*     above took.                                             */
    /***************************************************************/

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

	if ( H5Fclose(file_id) ) {

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
            failure_mssg2 = "H5Fopen() failed (5).\n";
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

	if ( H5Fclose(file_id) ) {

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
            failure_mssg2 = "H5C2_jb_init failed";

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

	if ( H5C2_jb__write_to_buffer(struct_ptr, (size_t)size, data, 0, 0) 
           != SUCCEED ) {

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
	
            if ( H5C2_jb__write_to_buffer(struct_ptr, (size_t)size, data, 0, 0) 
               != SUCCEED ) {

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
            failure_mssg2 = "H5C2_jb_init failed";

       	} /* end if */

    } /* end if */

    if ( show_progress ) /* 1 */ 
	HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
		  checkpoint++, (int)pass2);
 
    /* Start a transaction */
    if ( pass2 ) {

        if ( H5C2_jb__start_transaction(/* H5C2_jbrb_t  */  &jbrb_struct, 
                                        /* trans number */  1) 
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
                                    /* trans number */  1, 
                                    /* base address */  (haddr_t)0, 
                                    /* data length  */  1, 
                                    /* data         */  "A") 
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
                                    /* trans number */  1, 
                                    /* base address */  (haddr_t)1, 
                                    /* data length  */  2, 
                                    /* data         */  "AB") 
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
                                    /* trans number */  1, 
                                    /* base address */  (haddr_t)3, 
                                    /* data length  */  4, 
                                    /* data         */  "CDEF") 
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
                                      /* trans number */  1) 
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
                                        /* trans number */  2) 
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
                                    /* trans number */  2, 
                                    /* base address */  (haddr_t)285, 
                                    /* data length  */  11, 
                                    /* data         */  "Test Data?!") 
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
                                      /* trans number */  2) 
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

    if ( show_progress ) /* 10 */ 
	HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
		  checkpoint++, (int)pass2);

    /* Fill out verify array with expected messages */
    sprintf(verify[0], "0 ver_num 1 target_file_name HDF5.file creation_date %s human_readable 1\n", __DATE__);
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
                                        /* trans number */  3) 
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
                                    /* trans number */  3, 
                                    /* base address */  (haddr_t)28591, 
                                    /* data length  */  6, 
                                    /* data         */  "#1nN`}") 
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
                                      /* trans number */  3) 
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

    if ( show_progress ) /* 15 */ 
	HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
		  checkpoint++, (int)pass2);

    /* Fill out verify array with expected messages */
    sprintf(verify[0], "0 ver_num 1 target_file_name HDF5.file creation_date %s human_readable 1\n", __DATE__);
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
            failure_mssg2 = "H5C2_jb_init failed";

       	} /* end if */

    } /* end if */
	
    if ( show_progress ) /* 2 */ 
        HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
                  checkpoint++, (int)pass2);

    /* Start transaction 2. This should FAIL because transaction 1 has
       not occurred yet. Ensure that it fails, and flag an error if it 
       does not. */
    if ( pass2 ) {

	if ( H5C2_jb__start_transaction(/* H5C2_jbrb_t   */  &jbrb_struct, 
                                        /* Transaction # */  2)
           == SUCCEED ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_jb__start_transaction should have failed";

	} /* end if */

    } /* end if */ 

    if ( show_progress ) /* 3 */ 
        HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
                  checkpoint++, (int)pass2);

    /* End transaction 1. This should FAIL because transaction 1 has
       not started yet. Ensure that it fails, and flag an error if it 
       does not. */
    if ( pass2 ) {

	if ( H5C2_jb__end_transaction(/* H5C2_jbrb_t   */  &jbrb_struct, 
                                      /* Transaction # */  1)
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
                                    /* Transaction # */  1,
                                    /* Base Address  */  (haddr_t)123456789, 
                                    /* Length        */  16, 
                                    /* Body          */  "This should fail")
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
                                        /* Transaction # */  1)
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
                                        /* Transaction # */  1)
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
                                      /* Transaction # */  1)
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
                                    /* Transaction # */  2,
                                    /* Base Address  */  (haddr_t)123456789, 
                                    /* Length        */  16, 
                                    /* Body          */  "This should fail")
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
                                    /* Transaction # */  1,
                                    /* Base Address  */  (haddr_t)123456789, 
                                    /* Length        */  51, 
                                    /* Body          */  "This is the first transaction during transaction 1.")
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
                                      /* Transaction # */  1)
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
                                        /* Transaction # */  1)
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
                                        /* Transaction # */  2)
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
                                    /* Transaction # */  2,
                                    /* Base Address  */  (haddr_t)7465, 
                                    /* Length        */  51, 
                                    /* Body          */  "This is the first transaction during transaction 2!")
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
                                    /* Transaction # */  2,
                                    /* Base Address  */  (haddr_t)123456789, 
                                    /* Length        */  60, 
                                    /* Body          */  "... And here's your second transaction during transaction 2.")
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
                                      /* Transaction # */  2)
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
            failure_mssg2 = "H5C2_jb_init failed";

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
                           /* transaction num */ (unsigned long)i, 
                           /* expected trans */(unsigned long)expected_tval[i]);

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
            failure_mssg2 = "H5C2_jb_init failed";

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
                               /* transaction num */(unsigned long)i, 
                               /* expected trans on disk */(unsigned long)i);

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
            failure_mssg2 = "H5C2_jb_init failed";

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
    write_verify_trans_num(&jbrb_struct, 1, 0); /* 1 in bufs, 0 on disk */
    write_verify_trans_num(&jbrb_struct, 2, 1); /* 2 in bufs, 1 on disk */
    write_verify_trans_num(&jbrb_struct, 3, 3); /* nothing in bufs, 3 on disk */
    H5C2_jb__write_to_buffer(&jbrb_struct, 10, "XXXXXXXXX\n", 0, 0);   
    write_verify_trans_num(&jbrb_struct, 4, 3); /* 1 in bufs, 0 on disk */
    write_verify_trans_num(&jbrb_struct, 5, 5); /* 2 in bufs, 1 on disk */
    write_verify_trans_num(&jbrb_struct, 6, 5); /* nothing in bufs, 3 on disk */
    H5C2_jb__write_to_buffer(&jbrb_struct, 10, "XXXXXXXXX\n", 0, 0);   
    write_verify_trans_num(&jbrb_struct, 7, 7); /* 1 in bufs, 0 on disk */
    write_verify_trans_num(&jbrb_struct, 8, 7); /* 2 in bufs, 1 on disk */
    write_verify_trans_num(&jbrb_struct, 9, 8); /* nothing in bufs, 3 on disk */
    H5C2_jb__write_to_buffer(&jbrb_struct, 10, "XXXXXXXXX\n", 0, 0);   
    write_verify_trans_num(&jbrb_struct, 10, 9); /* 1 in bufs, 0 on disk */
    write_verify_trans_num(&jbrb_struct, 11, 10); /* 2 in bufs, 1 on disk */
    write_verify_trans_num(&jbrb_struct, 12, 12); /* nothing in buf, 3 on disk */

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
            failure_mssg2 = "H5C2_jb_init failed";

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
                               /* transaction num */  (unsigned long)i, 
                               /* expected returned trans */  (unsigned long)i);

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
                       unsigned long trans_num, 
                       unsigned long verify_val)
{
    unsigned long trans_verify;
    
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
                                            /* Body          */  "XXXXXXXXX")
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
                                            /* Body          */  "XXXXXXXX")
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
                                            /* Body          */  "XXXXXXX")
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

    check_buffer_writes();
    check_legal_calls();
    check_message_format();
    check_transaction_tracking();
    check_superblock_extensions();

    check_mdj_config_block_IO();

    return(failures2);

} /* main() */

