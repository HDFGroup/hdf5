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

static void check_superblock_extensions(void);


/**************************************************************************/
/**************************************************************************/
/********************************* tests: *********************************/
/**************************************************************************/
/**************************************************************************/

/***************************************************************************
 * Function: 	check_buffer_writes
 *
 * Purpose:  	Verify the function H5C_jb__write_to_buffer properly writes
 *		messages of varying sizes into the journal buffers, and 
 *		that the journal buffers properly flush out when filled.
 *
 * Return:	void
 *
 * Programmer: 	Mike McGreevy <mcgreevy@hdfgroup.org>
 *		Thursday, February 21, 2008
 * 
 **************************************************************************/

static void 
check_buffer_writes(void)
{
    const char * fcn_name = "check_buffer_writes(): ";
    char filename[512];
    int i;
    herr_t result;
    H5C2_jbrb_t * struct_ptr;
    FILE * readback;
    hbool_t show_progress = FALSE;
    int32_t checkpoint = 1;
    char filldata[13][100];

    TESTING("metadata buffer & file writes");

    pass2 = TRUE;

    /* Initialize data to get written as tests */
    memcpy(filldata[0], "3456789\n", 9);
    memcpy(filldata[1], "abcdefghijklmn\n", 16);
    memcpy(filldata[2], "ABCDEFGHIJKLMNO\n", 17);
    memcpy(filldata[3], "AaBbCcDdEeFfGgHh\n", 18);
    memcpy(filldata[4], "ZAB-ZAB-ZAB-ZAB-ZAB-ZAB-ZAB-ZA\n", 32);
    memcpy(filldata[5], "ABC-ABC-ABC-ABC-ABC-ABC-ABC-ABC\n", 33);
    memcpy(filldata[6], "BCD-BCD-BCD-BCD-BCD-BCD-BCD-BCD-\n", 34);
    memcpy(filldata[7], "12345-12345-12345-12345-12345-12345-12345-1234\n", 48);
    memcpy(filldata[8], "01234-01234-01234-01234-01234-01234-01234-01234\n", 49);
    memcpy(filldata[9], "23456-23456-23456-23456-23456-23456-23456-23456-\n", 50);
    memcpy(filldata[10], "aaaa-bbbb-cccc-dddd-eeee-ffff-gggg-hhhh-iiii-jjjj-kkkk-llll-mmmm-nnnn-oooo-pppp-qqqq-rrrr-ssss\n", 96);
    memcpy(filldata[11], "bbbb-cccc-dddd-eeee-ffff-gggg-hhhh-iiii-jjjj-kkkk-llll-mmmm-nnnn-oooo-pppp-qqqq-rrrr-ssss-tttt-\n", 97);
    memcpy(filldata[12], "cccc-dddd-eeee-ffff-gggg-hhhh-iiii-jjjj-kkkk-llll-mmmm-nnnn-oooo-pppp-qqqq-rrrr-ssss-tttt-uuuu-v\n", 98);

    /* Assert that size of data is as expected */
    HDassert(strlen(filldata[0]) == 8);
    HDassert(strlen(filldata[1]) == 15);
    HDassert(strlen(filldata[2]) == 16);
    HDassert(strlen(filldata[3]) == 17);
    HDassert(strlen(filldata[4]) == 31);
    HDassert(strlen(filldata[5]) == 32);
    HDassert(strlen(filldata[6]) == 33);
    HDassert(strlen(filldata[7]) == 47);
    HDassert(strlen(filldata[8]) == 48);
    HDassert(strlen(filldata[9]) == 49);
    HDassert(strlen(filldata[10]) == 95);
    HDassert(strlen(filldata[11]) == 96);
    HDassert(strlen(filldata[12]) == 97);

    /* Allocate memory for H5C2_jbrb_t structure */
    /* JRM: why allocate this?  Why not just declare it as a local 
     *      variable?
     */
    if ( pass2 ) {

	if (NULL == (struct_ptr = malloc(sizeof(H5C2_jbrb_t)))) {

	    pass2 = FALSE;
	    failure_mssg2 = 
		    "Can't allocate memory for H5C2_jbrb_t structure.\n";
	}
	struct_ptr->magic = H5C2__H5C2_JBRB_T_MAGIC;
    }
	
    if ( show_progress ) /* 1 */ 
	HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
		  checkpoint++, (int)pass2);

    /* setup the file name */
    if ( pass2 ) {

        if ( h5_fixname(FILENAMES[1], H5P_DEFAULT, filename, sizeof(filename))
             == NULL ) {

            pass2 = FALSE;
            failure_mssg2 = "h5_fixname() failed.\n";
        }
    }
	
    if ( show_progress ) /* 2 */ 
	HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
		  checkpoint++, (int)pass2);

    /* Initialize H5C2_jbrb_t structure. */
    if ( pass2 ) {

       	result = H5C2_jb__init(/* H5C2_jbrb_t */            struct_ptr, 
			       /* HDF5 file name */         HDF5_FILE_NAME,
			       /* journal file name */      filename, 
			       /* Buffer size */            16, 
			       /* Number of Buffers */      3, 
			       /* Use Synchronois I/O */    FALSE, 
			       /* human readable journal */ TRUE);

        if ( result != 0) {

	    pass2 = FALSE;
            failure_mssg2 = "H5C2_jb_init failed.\n";
       	}
    }
	
    if ( show_progress ) /* 3 */ 
	HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
		  checkpoint++, (int)pass2);

    /* Flush out buffers to disk */
    if ( pass2 ) {
	
	if ( H5C2_jb__flush(struct_ptr) != SUCCEED ) {

            pass2 = FALSE;
	    failure_mssg2 = "H5C2_jb_flush failed.\n";
	}	
    }
	
    /* Truncate journal file */
    if ( pass2 ) {

	if ( H5C2_jb__trunc(struct_ptr) != SUCCEED ) {

		pass2 = FALSE;
		failure_mssg2 = "H5C2_jb_trunc failed.\n";
	}
    }

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

    for (i=1; i<13; i++) {

	write_flush_verify(struct_ptr, 
			   (int)strlen(filldata[i]), 
			   filldata[i], 
			   readback);

	if ( show_progress )
	    HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
		      checkpoint++, (int)pass2);

    }

    /* run a collection of calls to write_noflush_verify(). These 
     * calls write specific lengths of data into the journal buffers 
     * multiple times, but only flushes at the end of the set of writes. 
     * This tests to ensure that the automatic flush calls in 
     * H5C2_jb__write_to_buffer are working properly. The routine then 
     * ensures that what makes it it disk is as expected 
     */

    write_noflush_verify(struct_ptr, 15, filldata[1], readback, 16);
	
    if ( show_progress ) /* 17 */ 
	HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
		  checkpoint++, (int)pass2);

    write_noflush_verify(struct_ptr, 16, filldata[2], readback, 6);
	
    if ( show_progress ) /* 18 */ 
	HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
		  checkpoint++, (int)pass2);

    write_noflush_verify(struct_ptr, 17, filldata[3], readback, 16);
	
    if ( show_progress ) /* 19 */ 
	HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
		  checkpoint++, (int)pass2);

    write_noflush_verify(struct_ptr, 47, filldata[7], readback, 16);
	
    if ( show_progress ) /* 20 */ 
	HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
		  checkpoint++, (int)pass2);

    write_noflush_verify(struct_ptr, 48, filldata[8], readback, 6);
	
    if ( show_progress ) /* 21 */ 
	HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
		  checkpoint++, (int)pass2);

    write_noflush_verify(struct_ptr, 49, filldata[9], readback, 16);
	
    if ( show_progress ) /* 22 */ 
	HDfprintf(stdout, "%s%0d -- pass = %d\n", fcn_name, 
		  checkpoint++, (int)pass2);

    /* close the journal file's file pointer, and truncate the journal file */
    /* JRM -- Don't you want to close the readback file 
     *        if it is open, not just if there is a pass?
     */
    if ( pass2 ) {

	fclose(readback);

	if ( H5C2_jb__trunc(struct_ptr) != SUCCEED ) {

		pass2 = FALSE;
		failure_mssg2 = "H5C2_jb__trunc failed.\n";
	}
    }

    /* take down the journal file */
    if ( pass2 ) {

	if (H5C2_jb__takedown(struct_ptr) != SUCCEED) {

	    pass2 = FALSE;
	    failure_mssg2 = "H5C2_jb__takedown failed.\n";
	}

	free(struct_ptr);
    }

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
 *		piece of data of specified size into the journal buffer, then
 *		flushes the journal buffers. The data is read back and
 *		verified for correctness.
 *
 * Return:	void
 *
 * Programmer: 	Mike McGreevy <mcgreevy@hdfgroup.org>
 *		Thursday, February 21, 2008
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

	if ( H5C2_jb__write_to_buffer(struct_ptr, size, data) != SUCCEED ) {

	    pass2 = FALSE;
	    failure_mssg2 = "H5C2_jb__write_to_buffer failed.\n";
	}
    }

    if ( pass2 ) {

	if ( H5C2_jb__flush(struct_ptr) != SUCCEED ) {

	    pass2 = FALSE;
            failure_mssg2 = "H5C2_jb_flush failed.\n";
	}	
    }

    if ( pass2 ) {

	fgets(verify, size+10, readback);
		
	if (strcmp(verify, data) != 0) {

	    pass2 = FALSE;
	    failure_mssg2 = "Journal entry not written correctly.\n";
	    printf("message read from file : %s\n", verify);
	    printf("message supplied       : %s\n", data);
	}
    }

    return;

} /* write_flush_verify */


/***************************************************************************
 * Function: 	write_noflush_verify
 *
 * Purpose:  	Helper function for check_buffer_writes test. Writes a 
 * 		piece of data of specified size into the journal buffer
 *		multiple times, without calling H5C_jb__flush in between
 *		writes. After all writes are completed, H5C_jb__flush is 
 *		called, and the data is read back from the journal file and
 *		verified for correctness.
 *
 * Return:	void
 *
 * Programmer: 	Mike McGreevy <mcgreevy@hdfgroup.org>
 *		Thursday, February 21, 2008
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

	    if ( H5C2_jb__write_to_buffer(struct_ptr, size, data) != SUCCEED ) {

		pass2 = FALSE;
		failure_mssg2 = "H5C2_jb__write_to_buffer failed.\n";
	    } /* end if */
	} /* end if */
    } /* end for */

    if ( pass2 ) {

	if ( H5C2_jb__flush(struct_ptr) != SUCCEED ) {

	    pass2 = FALSE;
	    failure_mssg2 = "H5C2_jb_flush failed.\n";
	}	
    }

    for (i=0; i<repeats; i++) {

	if ( pass2 ) {
	    fgets(verify, size+10, readback);
	    if (strcmp(verify, data) != 0) {

		pass2 = FALSE;
		failure_mssg2 = "Journal entry not written correctly.\n";
		printf("message read from file : %s\n", verify);
		printf("message supplied       : %s\n", data);
	    } /* end if */
	} /* end if */
    } /* end for */

    return;

} /* write_noflush_verify */

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
    const int journal_file_name_len = strlen(journal_file_name);
    char filename[512];
    hbool_t show_progress = FALSE;
    haddr_t mdc_jrnl_block_loc = 0x1000;
    hsize_t mdc_jrnl_block_len = 0x100;
    int i;
    int cp = 0; 
    hid_t fapl_id = -1;
    hid_t file_id = -1;
    hid_t dataset_id = -1;
    hid_t dataspace_id = -1;
    H5F_t * file_ptr = NULL;
    hsize_t             dims[2];


    TESTING("superblock extensions");

    pass2 = TRUE;

    if ( pass2 ) { PASSED(); } else { H5_FAILED(); }

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

    if ( ! pass2 ) {

	failures2++;
        HDfprintf(stdout, "%s: failure_mssg2 = \"%s\".\n",
                  fcn_name, failure_mssg2);
    }

} /* check_superblock_extensions() */


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

    check_superblock_extensions();

    return(failures2);

} /* main() */
