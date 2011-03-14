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
 *              11/10
 *
 *		This file contains tests for the fsync and aio fsync
 *		vfd calls.
 */

#include <aio.h>

#define H5F_PACKAGE             /*suppress error about including H5Fpkg   */

#include "h5test.h"
#include "H5Eprivate.h"
#include "H5Iprivate.h"
#include "H5MMprivate.h"        /* Memory management                    */
#include "H5MFprivate.h"
#include "H5ACprivate.h"
#include "cache_common.h"
#include "H5Fpkg.h"


/* global variable declarations: */

#define KB              1024
#define FAMILY_SIZE_AIO (32 * KB * KB)

#define TYPE_SLICE ((haddr_t)0x20000000LL)

#define TEST_IN_PROGRESS_FILE_NAME_INDEX	0
const char *FILENAMES[] = {
        "test_in_progress",
        "sec2_fsync_test",
        "core_fsync_test",
        "stdio_fsync_test",
        "family_fsync_test",
        "multi_fsync_test",
        "sec2_aio_fsync_poll_test",
        "core_aio_fsync_poll_test",
        "stdio_aio_fsync_poll_test",
        "family_aio_fsync_poll_test",
        "multi_aio_fsync_poll_test",
        "sec2_aio_fsync_wait_test",
        "core_aio_fsync_wait_test",
        "stdio_aio_fsync_wait_test",
        "family_aio_fsync_wait_test",
        "multi_aio_fsync_wait_test",
        "targeted_multi_fsync_test",
        "targeted_multi_aio_fsync_poll_test",
        "targeted_multi_aio_fsync_wait_test",
        NULL
};


/* private function declarations: */

/* utility functions */

static void check_test_in_progress(const char * str, hbool_t verbose);

static hbool_t file_exists(const char * file_path_ptr, hbool_t verbose);

static void load_test_buffer(hsize_t buf_size, const char * tag, char * buf);

static void mark_test_in_progress(const char * str);

static void usage(void);

/* test functions */

static void setup_generic_aio_fsync_test(const int file_name_num,
                                         hid_t fapl_id,
                                         const char * tag,
                                         hsize_t write_size,
                                         haddr_t maxaddr,
			                 hbool_t wait_on_fsync,
                                         hbool_t verbose);

static void setup_generic_fsync_test(const int file_name_num,
                                     hid_t fapl_id,
                                     const char * tag,
                                     hsize_t write_size,
                                     haddr_t maxaddr,
                                     hbool_t verbose);

static void check_generic_fsync_test(const int file_name_num,
                                     hid_t fapl_id,
                                     const char * tag,
                                     hsize_t file_size,
                                     haddr_t maxaddr,
                                     hbool_t verbose);

static void setup_multi_file_driver_fsync_test(const int file_name_num,
                                               const char * tag,
                                               hsize_t write_size,
                                               hbool_t verbose);

static void setup_multi_file_driver_aio_fsync_test(const int file_name_num,
                                                   const char * tag,
                                                   hsize_t write_size,
                                                   hbool_t wait_on_fsync,
                                                   hbool_t verbose);

static void check_multi_file_driver_fsync_test(const int file_name_num,
                                               const char * tag,
                                               hsize_t file_size,
                                               hbool_t verbose);


/**************************************************************************/
/**************************************************************************/
/********************************* tests: *********************************/
/**************************************************************************/
/**************************************************************************/

/*** fsync and aio fsync test utility functions ***/

/*-------------------------------------------------------------------------
 * Function:    check_test_in_progress()
 *
 * Purpose:     If pass is true on entry, test to see if the test in
 *		progress file exists.  If it does not, set pass to FALSE
 *		and set a failure message.
 *
 *		If the test in progress file does exist, check to see if
 *		its contents matches the supplied string.  It it does not,
 *		set pass to FALSE and set the appropriate failure message.
 *
 *              Do nothing if pass is FALSE on entry.
 *
 * Return:      void
 *
 * Programmer:  John Mainzer
 *              11/9/10
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
check_test_in_progress(const char * str, 
                       hbool_t verbose)

{
    const char * fcn_name = "check_test_in_progress()";
    char buffer[512];
    char filename[512];
    hbool_t show_progress = FALSE;
    size_t input_len;
    ssize_t bytes_read;
    int cp = 0;
    int fd = -1;
    h5_stat_t buf;

    if ( show_progress ) {

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass, cp++);
	HDfflush(stdout);
    }

    if ( pass ) {

        if ( ( str == NULL ) ||
             ( strlen(str) <= 0 ) ||
             ( strlen(str) >= 512 ) ) {

            pass = FALSE;
            failure_mssg = "bad str on entry to check_test_in_progress().";
        }
    }

    if ( show_progress ) {

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass, cp++);
	HDfflush(stdout);
    }

    /* setup the test in progress file name */
    if ( pass ) {

        if ( h5_fixname(FILENAMES[TEST_IN_PROGRESS_FILE_NAME_INDEX], 
                        H5P_DEFAULT, filename, sizeof(filename)) == NULL ) {

            pass = FALSE;
            failure_mssg = "h5_fixname() failed.\n";
        }
        else if ( strlen(filename) >= 512 ) {

            pass = FALSE;
            failure_mssg = "test in progress file name too long.\n";
        }
    }

    if ( show_progress ) {

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass, cp++);
	HDfflush(stdout);
    }

    if ( verbose ) {
        HDfprintf(stdout, "%s filename = \"%s\".\n", fcn_name, filename);
	HDfflush(stdout);
    }

    if ( ( pass ) && ( ! file_exists(filename, verbose) ) ) {

        pass = FALSE;
        failure_mssg = "test not in progress?!?";
    }


    /* get the length of the test in progress file */
    if ( pass ) {

	if ( HDstat(filename, &buf) != 0 ) {

	    if ( verbose ) {

	        HDfprintf(stdout, "%s: HDstat() failed with errno = %d.\n",
                          fcn_name, errno);
	    }
	    failure_mssg = "stat() failed on test in progress file.";
	    pass = FALSE;

	} else {

	    if ( (buf.st_size) == 0 ) {

                failure_mssg = "test in progress file empty?!?";
	        pass = FALSE;

            } else if ( (buf.st_size) >= 512 ) {

                failure_mssg = "test in progress file too big?!?";
	        pass = FALSE;

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
    if ( pass ) {

	if ( (fd = HDopen(filename, O_RDONLY, 0777)) == -1 ) {

	    if ( verbose ) {

	        HDfprintf(stdout, "%s: HDopen(i) failed with errno = %d.\n",
                          fcn_name, errno);
	    }
            failure_mssg = "Can't open test in progress file.";
	    pass = FALSE;
        }
    }

    /* read the contents of the test in progress file */
    if ( pass )
    {
        bytes_read = HDread(fd, buffer, input_len);

        if ( bytes_read != (int)input_len ) {

            if ( verbose ) {

                HDfprintf(stdout,
                          "%s: HDread() failed. bytes_read = %d, errno = %d.\n",
                          fcn_name, (int)bytes_read, errno);
            }
            failure_mssg = "error reading test in progress file.";
            pass = FALSE;
        }

        buffer[input_len] = '\0';
    }

    if ( fd != -1 ) {

        if ( HDclose(fd) != 0 ) {

	    if ( verbose ) {

	        HDfprintf(stdout, "%s: HDclose() failed with errno = %d.\n",
                          fcn_name, errno);
	    }

	    if ( pass ) {

                failure_mssg = "Can't close test in progress file.";
	        pass = FALSE;
	    }
	}
    }

    HDremove(filename);

    if ( pass ) {

        if ( strcmp(str, buffer) != 0 ) {

	    if ( verbose ) {

	        HDfprintf(stdout,
                          "%s: expected/actual test in progress = %s/%s\n",
                          fcn_name, str, buffer);
	    }

            pass = FALSE;
            failure_mssg = "Unexpected test in progress?!?";
        }
    }

    return;

} /* check_test_in_progress() */


/*-------------------------------------------------------------------------
 * Function:    file_exists()
 *
 * Purpose:     If pass is true on entry, stat the target file, and
 * 		return TRUE if it exists, and FALSE if it does not.
 *
 * 		If any errors are detected in this process, set pass
 * 		to FALSE and set failure_mssg to point to an appropriate
 * 		error message.
 *
 *              Do nothing and return FALSE if pass is FALSE on entry.
 *
 * Return:      void
 *
 * Programmer:  John Mainzer
 *              11/9/10
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static hbool_t
file_exists(const char * file_path_ptr,
            hbool_t verbose)
{
    const char * fcn_name = "file_exists()";
    hbool_t ret_val = FALSE; /* will set to TRUE if necessary */
    h5_stat_t buf;

    if ( pass ) {

	if ( file_path_ptr == NULL ) {

            failure_mssg = "file_path_ptr NULL on entry?!?",
            pass = FALSE;
	}
    }

    if ( pass ) {

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

	    failure_mssg = "HDstat() returned unexpected value.";
	    pass = FALSE;

	}
    }

    return(ret_val);

} /* file_exists() */


/*-------------------------------------------------------------------------
 * Function:    mark_test_in_progress()
 *
 * Purpose:     If pass is true on entry, test to see if the test in
 *		progress file exists.  If it does, set pass to FALSE
 *		and set a failure message.
 *
 *		If the test in progress file doesn't exist, create it,
 *		open it, write the supplied string to it, and then close
 *		it.  If any errors are detected, set pass to FALSE, and
 *		set the appropriate failure message.
 *
 *              Do nothing if pass is FALSE on entry.
 *
 * Return:      void
 *
 * Programmer:  John Mainzer
 *              11/9/10
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
    ssize_t bytes_written;
    int fd = -1;

    if ( show_progress ) {

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass, cp++);
	HDfflush(stdout);
    }

    if ( pass ) {

        if ( ( str == NULL ) ||
             ( strlen(str) >= 512 ) ) {

            pass = FALSE;
            failure_mssg = "bad str on entry to mark_test_in_progress().";
        }
    }

    if ( show_progress ) {

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass, cp++);
	HDfflush(stdout);
    }

    /* setup the test in progress file name */
    if ( pass ) {

        if ( h5_fixname(FILENAMES[TEST_IN_PROGRESS_FILE_NAME_INDEX], 
                        H5P_DEFAULT, filename, sizeof(filename)) == NULL ) {

            pass = FALSE;
            failure_mssg = "h5_fixname() failed.\n";
        }
        else if ( strlen(filename) >= 512 ) {

            pass = FALSE;
            failure_mssg = "test in progress file name too long.\n";
        }
    }

    if ( show_progress ) {

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass, cp++);
	HDfflush(stdout);
    }

    if ( verbose ) {
        HDfprintf(stdout, "%s filename = \"%s\".\n", fcn_name, filename);
	HDfflush(stdout);
    }

    if ( ( pass ) && ( file_exists(filename, verbose) ) ) {

        pass = FALSE;
        failure_mssg = "test already in progress?!?";
    }

    /* open the test in progress file */
    if ( pass ) {

	if ( (fd = HDopen(filename, O_WRONLY|O_CREAT|O_TRUNC, 0777))
             == -1 ) {

	    if ( verbose ) {

	        HDfprintf(stdout, "%s: HDopen(i) failed with errno = %d.\n",
                          fcn_name, errno);
	    }
            failure_mssg = "Can't open test in progress file.";
	    pass = FALSE;
        }
    }

    if ( pass ) {

        bytes_written = HDwrite(fd, str, strlen(str));

        if ( bytes_written != (int)strlen(str) ) {

            if ( verbose ) {

                HDfprintf(stdout,
                          "%s: HDwrite() failed. bytes_written = %d, errno = %d.\n",
                          fcn_name, (int)bytes_written, errno);
            }
            failure_mssg = "error writing test in progress file.";
            pass = FALSE;
        }
    }

    if ( fd != -1 ) {

        if ( HDclose(fd) != 0 ) {

	    if ( verbose ) {

	        HDfprintf(stdout, "%s: HDclose() failed with errno = %d.\n",
                          fcn_name, errno);
	    }

	    if ( pass ) {

                failure_mssg = "Can't close test in progress file.";
	        pass = FALSE;
	    }
	}
    }

    return;

} /* mark_test_in_progress() */


/***************************************************************************
 * Function: 	load_test_buffer
 *
 * Purpose:  	If pass is TRUE, load the supplied buffer with test data. 
 *		Otherwise do nothing.
 *
 * Return:      void
 *
 * Programmer: 	John Mainzer
 *              11/9/11
 *
 **************************************************************************/

#define LINE_LEN	 64
#define LINE_NUM_BUF_LEN 16

static void
load_test_buffer(hsize_t buf_size,
                 const char * tag,
                 char * buf)
{
    /* const char * fcn_name = "load_test_buffer():"; */
    char line_num_buf[LINE_NUM_BUF_LEN + 1];
    int line_num_buf_len;
    int tag_len;
    hsize_t i;
    int j;
    int line_num = 0;

    HDassert( buf_size > 0 );
    HDassert( tag != NULL );
    HDassert( buf != NULL );

    if ( pass ) {

        tag_len = (int)HDstrlen(tag);

        HDassert( tag_len > 0 );
        HDassert( LINE_NUM_BUF_LEN + tag_len < LINE_LEN - 1 );

        j = 0;

	for ( i = 0; i < buf_size; i++ ) {

            if ( j == 0 ) { /* we are starting a new line */

                if ( line_num <= 999999999 ) {

                    HDsnprintf(line_num_buf, (size_t)LINE_NUM_BUF_LEN,
                               "%09d ", line_num);

                } else {

                    HDsnprintf(line_num_buf, (size_t)LINE_NUM_BUF_LEN, "xxxxxxxxx ");
                }

                line_num_buf_len = (int)HDstrlen(line_num_buf);

		HDassert( line_num_buf_len <= LINE_NUM_BUF_LEN );
            }

            if ( j < line_num_buf_len ) {

		buf[i] = line_num_buf[j];
                j++;

            } else if ( j < line_num_buf_len + tag_len ) {

                buf[i] = tag[j - line_num_buf_len];
	        j++;

            } else if ( j < LINE_LEN - 1 ) {

                buf[i] = ' ';
	        j++;

            } else {

                buf[i] = '\n';
                j = 0;
                line_num++;
            }            
        }
    }

    return;

} /* load_test_buffer() */

#undef LINE_LEN
#undef LINE_NUM_BUF_LEN


/***************************************************************************
 * Function: 	setup_generic_aio_fsync_test
 *
 * Purpose:  	Setup test to verify that the aio fsync vfd call works as 
 *		expected for the target file driver.
 *
 *		To do this, open the specified file with the driver
 *		indicated by the fapl_id, write data to the file with 
 *		the supplied tag, call an aio_fsync, wait or poll (as 
 *		directed) until done, and then exit without closing the 
 *		file.
 *
 * Return:      void
 *
 * Programmer: 	John Mainzer
 *              1/28/11
 *
 **************************************************************************/

static void
setup_generic_aio_fsync_test(const int file_name_num,
                             hid_t fapl_id,
                             const char * tag,
                             hsize_t write_size,
                             haddr_t maxaddr,
			     hbool_t wait_on_fsync,
                             hbool_t verbose)
{
    const char * fcn_name = "setup_generic_aio_fsync_test():";
    char * write_buf = NULL;
    char filename[1024];
    hbool_t done = FALSE;
    hbool_t show_progress = FALSE;
    int cp = 0;
    int error_num;
    herr_t result;
    void * wrt_ctlblk_ptr = NULL;
    void * fsync_ctlblk_ptr = NULL;
    H5FD_t * h5fd_ptr = NULL;

    /* setup the file name */
    if ( pass ) {

        if ( h5_fixname(FILENAMES[file_name_num], fapl_id, 
                        filename, sizeof(filename)) == NULL ) {

            pass = FALSE;
            failure_mssg = "h5_fixname() failed (1).\n";
        }
    }

    if ( show_progress ) { /* 0 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass, cp++);
	HDfflush(stdout);
    }

    if ( verbose ) {

        HDfprintf(stdout, "%s filename = \"%s\".\n", fcn_name, filename);
        HDfprintf(stdout, "%s tag = \"%s\".\n", fcn_name, tag);
        HDfprintf(stdout, "%s write_size = 0x%llx.\n", fcn_name, (long long)write_size);
        HDfprintf(stdout, "%s max_addr = 0x%llx.\n", fcn_name, (long long)write_size);
        HDfprintf(stdout, "%d wait_on_fsync = %d.\n", fcn_name, (int)wait_on_fsync);
        HDfprintf(stdout, "%s: FILENAMES[%d] = \"%s\".\n", fcn_name, file_name_num,
                  FILENAMES[file_name_num]);
	HDfflush(stdout);
    }

    if ( pass ) { /* allocate write buffer */

        write_buf = (char *)HDmalloc((size_t)(write_size + 1));

        if ( write_buf == NULL ) {

	    pass = FALSE;
            failure_mssg = "write buffer allocation failed.";
        }
    }

    if ( show_progress ) { /* 1 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass, cp++);
	HDfflush(stdout);
    }

    if ( pass ) { /* setup write buffer */

        load_test_buffer(write_size, tag, write_buf);
    }

    if ( verbose ) {

        HDfprintf(stdout, "%s strlen(write_buf) = %lld.\n", 
                  fcn_name, (long long)strlen(write_buf));
    }

    if ( show_progress ) { /* 2 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass, cp++);
	HDfflush(stdout);
    }

    /* create the file */
    if ( pass ) {

        h5fd_ptr = H5FDopen(filename, (H5F_ACC_RDWR | H5F_ACC_CREAT),
                            fapl_id, maxaddr);

        if ( h5fd_ptr == NULL ) {

            pass = FALSE;
            failure_mssg = "H5FDopen() failed.";

        } else {

            mark_test_in_progress("fsync_test");
        }
    }

    if ( show_progress ) { /* 3 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass, cp++);
	HDfflush(stdout);
    }

    /* set the EOA */
    if ( pass ) {

        result = H5FDset_eoa(h5fd_ptr, H5FD_MEM_DEFAULT, maxaddr);

        if ( result < 0 ) {

            pass = FALSE;
            failure_mssg = "H5FDset_eoa() failed.";
        }
    }

    if ( show_progress ) { /* 4 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass, cp++);
	HDfflush(stdout);
    }

    /* do the write */
    if ( pass ) {

	result = H5FDaio_write(h5fd_ptr, H5FD_MEM_DRAW, H5P_DEFAULT, (haddr_t)0,
                               (size_t)write_size, (void *)write_buf, &wrt_ctlblk_ptr);

        if ( ( result < 0 ) || ( wrt_ctlblk_ptr == NULL ) ) {

	    pass = FALSE;
            failure_mssg = "H5FDaio_write() failed.\n";
        }
    }

    if ( show_progress ) { /* 5 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass, cp++);
	HDfflush(stdout);
    }

    /* do the aio_fsync() */
    if ( pass ) {

        result = H5FDaio_fsync(h5fd_ptr, &fsync_ctlblk_ptr);

        if ( ( result < 0 ) || ( fsync_ctlblk_ptr ==  NULL ) ) {

            pass = FALSE;
            failure_mssg = "H5FDaio_fsync(0) failed.";
        }
    }

    if ( show_progress ) { /* 6 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass, cp++);
	HDfflush(stdout);
    }

    /* await the completion of the fsync */
    if ( pass ) {

        if ( wait_on_fsync ) {

            result = H5FDaio_wait(h5fd_ptr, fsync_ctlblk_ptr);

            if ( result < 0 ) {

                pass = FALSE;
                failure_mssg = "H5FDaio_wait(0) failed.";
            }
        } else {

            done = FALSE;

            while ( ( pass ) && ( ! done ) ) {

                result = H5FDaio_test(h5fd_ptr, &done, fsync_ctlblk_ptr);

                if ( result < 0 ) {

                    pass = FALSE;
                    failure_mssg = "H5FDaio_test(0) failed.";
                }
            }
        }
    }

    if ( show_progress ) { /* 7 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass, cp++);
	HDfflush(stdout);
    }

    /* complete the aio fsync */
    if ( pass ) {

        result = H5FDaio_finish(h5fd_ptr, &error_num, fsync_ctlblk_ptr);

        if ( ( result < 0 ) || ( error_num != 0 ) ) {

            pass = FALSE;
            failure_mssg = "H5FDaio_finish(0) failed.";
        }
    }

    if ( show_progress ) { /* 8 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass, cp++);
	HDfflush(stdout);
    }

    /* at this point, the aio write should be complete as well -- verify this
     * and then discard the write buffer.
     */
    if ( pass ) {

        done = FALSE;

        result = H5FDaio_test(h5fd_ptr, &done, wrt_ctlblk_ptr);

        if ( result < 0 ) {

            pass = FALSE;
            failure_mssg = "H5FDaio_test(1) failed.";

        } else if ( done != TRUE ) {

            pass = FALSE;
            failure_mssg = "aio_write() not complete after completion of aio_fsync().";

        }
    }

    if ( show_progress ) { /* 9 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass, cp++);
	HDfflush(stdout);
    }

    /* finish the aio_write() */
    if ( pass ) {

        result = H5FDaio_finish(h5fd_ptr, &error_num, wrt_ctlblk_ptr);

        if ( ( result < 0 ) || ( error_num != 0 ) ) {

            pass = FALSE;
            failure_mssg = "H5FDaio_finish(1) failed.";
        }
    }

    if ( show_progress ) { /* 10 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass, cp++);
	HDfflush(stdout);
    }

    /* discard the write buffer */

    if ( write_buf != NULL ) {

        HDfree(write_buf);
	write_buf = NULL;
    }

    if ( show_progress ) { /* 11 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass, cp++);
	HDfflush(stdout);
    }

    /* do the abort */
    if ( pass ) {

	abort();
    }

    if ( show_progress ) { /* 12 */ 

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass, cp++);
	HDfflush(stdout);
    }

    return;

} /* setup_generic_aio_fsync_test() */


/***************************************************************************
 * Function: 	setup_generic_fsync_test
 *
 * Purpose:  	Setup test to verify that the fsync vfd call works as 
 *		expected for the target file driver.
 *
 *		To do this, open the specified file with the driver
 *		indicated by the fapl_id, write data to the file with 
 *		the supplied tag, call an fsync, and then exit without 
 *		closing the file.
 *
 * Return:      void
 *
 * Programmer: 	John Mainzer
 *              11/9/11
 *
 **************************************************************************/

static void
setup_generic_fsync_test(const int file_name_num,
                         hid_t fapl_id,
                         const char * tag,
                         hsize_t write_size,
                         haddr_t maxaddr,
                         hbool_t verbose)
{
    const char * fcn_name = "setup_generic_fsync_test():";
    char * write_buf = NULL;
    char filename[1024];
    hbool_t show_progress = FALSE;
    int cp = 0;
    herr_t result;
    H5FD_t * h5fd_ptr = NULL;

    /* setup the file name */
    if ( pass ) {

        if ( h5_fixname(FILENAMES[file_name_num], fapl_id, 
                        filename, sizeof(filename)) == NULL ) {

            pass = FALSE;
            failure_mssg = "h5_fixname() failed (1).\n";
        }
    }

    if ( show_progress ) { /* 0 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass, cp++);
	HDfflush(stdout);
    }

    if ( verbose ) {

        HDfprintf(stdout, "%s filename = \"%s\".\n", fcn_name, filename);
        HDfprintf(stdout, "%s tag = \"%s\".\n", fcn_name, tag);
        HDfprintf(stdout, "%s write_size = 0x%llx.\n", fcn_name, (long long)write_size);
        HDfprintf(stdout, "%s max_addr = 0x%llx.\n", fcn_name, (long long)write_size);
        HDfprintf(stdout, "%s: FILENAMES[%d] = \"%s\".\n", fcn_name, file_name_num,
                  FILENAMES[file_name_num]);
	HDfflush(stdout);
    }

    if ( pass ) { /* allocate write buffer */

        write_buf = (char *)HDmalloc((size_t)(write_size + 1));

        if ( write_buf == NULL ) {

	    pass = FALSE;
            failure_mssg = "write buffer allocation failed.";
        }
    }

    if ( show_progress ) { /* 1 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass, cp++);
	HDfflush(stdout);
    }

    if ( pass ) { /* setup write buffer */

        load_test_buffer(write_size, tag, write_buf);
    }

    if ( verbose ) {

        HDfprintf(stdout, "%s strlen(write_buf) = %lld.\n", 
                  fcn_name, (long long)strlen(write_buf));
    }

    if ( show_progress ) { /* 2 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass, cp++);
	HDfflush(stdout);
    }

    /* create the file */
    if ( pass ) {

        h5fd_ptr = H5FDopen(filename, (H5F_ACC_RDWR | H5F_ACC_CREAT),
                           fapl_id, maxaddr);

        if ( h5fd_ptr == NULL ) {

            pass = FALSE;
            failure_mssg = "H5FDopen() failed.";

        } else {

            mark_test_in_progress("fsync_test");
        }
    }

    if ( show_progress ) { /* 3 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass, cp++);
	HDfflush(stdout);
    }

    /* set the EOA */
    if ( pass ) {

        result = H5FDset_eoa(h5fd_ptr, H5FD_MEM_DEFAULT, maxaddr);

        if ( result < 0 ) {

            pass = FALSE;
            failure_mssg = "H5FDset_eoa() failed.";
        }
    }

    if ( show_progress ) { /* 4 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass, cp++);
	HDfflush(stdout);
    }

    /* do the write */
    if ( pass ) {

	result = H5FDwrite(h5fd_ptr, H5FD_MEM_DRAW, H5P_DEFAULT, (haddr_t)0, 
                           (size_t)write_size, (void *)write_buf);

        if ( result < 0 ) {

	    pass = FALSE;
            failure_mssg = "H5FDwrite() failed.\n";
        }
    }

    if ( show_progress ) { /* 5 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass, cp++);
	HDfflush(stdout);
    }

    /* discard the write buffer */

    if ( write_buf != NULL ) {

        HDfree(write_buf);
	write_buf = NULL;
    }

    if ( show_progress ) { /* 6 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass, cp++);
	HDfflush(stdout);
    }


    /* do the fsync */
    if ( pass ) {

	result = H5FDfsync(h5fd_ptr, H5P_DEFAULT);

        if ( result < 0 ) {

	    pass = FALSE;
            failure_mssg = "H5FDfsync() failed.\n";
        }
    }

    if ( show_progress ) { /* 7 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass, cp++);
	HDfflush(stdout);
    }

    /* do the abort */
    if ( pass ) {

	abort();
    }

    if ( show_progress ) { /* 8 */ 

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass, cp++);
	HDfflush(stdout);
    }

    return;

} /* setup_generic_fsync_test() */


/***************************************************************************
 * Function: 	setup_multi_file_driver_fsync_test
 *
 * Purpose:  	Setup test to verify that the fsync vfd call works as 
 *		expected for the target file driver.
 *
 *		To do this, open the specified file with the driver
 *		indicated by the fapl_id, write data to the file with 
 *		the supplied tag, call an fsync, and then exit without 
 *		closing the file.
 *
 * Return:      void
 *
 * Programmer: 	John Mainzer
 *              2/4/11
 *
 **************************************************************************/

static void
setup_multi_file_driver_fsync_test(const int file_name_num,
                                   const char * tag,
                                   hsize_t write_size,
                                   hbool_t verbose)
{
    const char * fcn_name = "setup_multi_file_driver_fsync_test():";
    const char * memb_name[H5FD_MEM_NTYPES];
    const char * (type_names[H5FD_MEM_NTYPES]) = 
	{
	    "H5FD_MEM_DEFAULT",
	    "H5FD_MEM_SUPER",
	    "H5FD_MEM_BTREE",
	    "H5FD_MEM_DRAW",
	    "H5FD_MEM_GHEAP",
	    "H5FD_MEM_LHEAP",
	    "H5FD_MEM_OHDR"
	};
    char * (write_bufs[H5FD_MEM_NTYPES]) = { NULL, NULL, NULL, NULL, NULL, NULL, NULL };
    char filename[1024];
    hbool_t show_progress = FALSE;
    int cp = 0;
    unsigned int i;
    hid_t fapl_id;
    hid_t memb_fapl[H5FD_MEM_NTYPES];
    herr_t result;
    haddr_t eoa;
    haddr_t max_addr;
    haddr_t memb_addr[H5FD_MEM_NTYPES];
    H5FD_mem_t mt;
    H5FD_mem_t memb_map[H5FD_MEM_NTYPES];
    H5FD_t * h5fd_ptr = NULL;

    if ( show_progress ) { /* 0 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass, cp++);
	HDfflush(stdout);
    }

    if ( pass ) { /* setup fapl for multi file driver */

        for ( mt = 0; mt < H5FD_MEM_NTYPES; mt++ ) {

            memb_addr[mt] = HADDR_UNDEF;
            memb_fapl[mt] = H5P_DEFAULT;
            memb_map[mt]  = H5FD_MEM_DRAW;
            memb_name[mt] = NULL;
        }

        memb_map[H5FD_MEM_SUPER]  = H5FD_MEM_SUPER;
        memb_fapl[H5FD_MEM_SUPER] = H5P_DEFAULT;
        memb_name[H5FD_MEM_SUPER] = "%s-s.h5";
        memb_addr[H5FD_MEM_SUPER] = 0;

        memb_map[H5FD_MEM_BTREE]  = H5FD_MEM_BTREE;
        memb_fapl[H5FD_MEM_BTREE] = H5P_DEFAULT;
        memb_name[H5FD_MEM_BTREE] = "%s-b.h5";
        memb_addr[H5FD_MEM_BTREE] = memb_addr[H5FD_MEM_SUPER] + TYPE_SLICE; 

        memb_map[H5FD_MEM_DRAW]   = H5FD_MEM_DRAW;
        memb_fapl[H5FD_MEM_DRAW]  = H5P_DEFAULT;
        memb_name[H5FD_MEM_DRAW]  = "%s-r.h5";
        memb_addr[H5FD_MEM_DRAW]  =  memb_addr[H5FD_MEM_BTREE] + TYPE_SLICE;

        memb_map[H5FD_MEM_GHEAP]  = H5FD_MEM_GHEAP;
        memb_fapl[H5FD_MEM_GHEAP] = H5P_DEFAULT;
        memb_name[H5FD_MEM_GHEAP] = "%s-g.h5";
        memb_addr[H5FD_MEM_GHEAP] = memb_addr[H5FD_MEM_DRAW] + TYPE_SLICE;

        memb_map[H5FD_MEM_LHEAP]  = H5FD_MEM_LHEAP;
        memb_fapl[H5FD_MEM_LHEAP] = H5P_DEFAULT;
        memb_name[H5FD_MEM_LHEAP] = "%s-l.h5";
        memb_addr[H5FD_MEM_LHEAP] = memb_addr[H5FD_MEM_GHEAP] + TYPE_SLICE;

        memb_map[H5FD_MEM_OHDR]   = H5FD_MEM_OHDR;
        memb_fapl[H5FD_MEM_OHDR]  = H5P_DEFAULT;
        memb_name[H5FD_MEM_OHDR]  = "%s-o.h5";
        memb_addr[H5FD_MEM_OHDR]  = memb_addr[H5FD_MEM_LHEAP] + TYPE_SLICE;

        max_addr = memb_addr[H5FD_MEM_OHDR] + TYPE_SLICE;

        fapl_id = h5_fileaccess();

        if ( H5Pset_fapl_multi(fapl_id, memb_map, memb_fapl, memb_name,
                               memb_addr, FALSE) < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Pset_fapl_multi() failed.";

        }
    }

    if ( show_progress ) { /* 1 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass, cp++);
	HDfflush(stdout);
    }


    /* setup the file name */
    if ( pass ) {

        if ( h5_fixname(FILENAMES[file_name_num], fapl_id, 
                        filename, sizeof(filename)) == NULL ) {

            pass = FALSE;
            failure_mssg = "h5_fixname() failed (1).\n";
        }
    }

    if ( verbose ) {

        HDfprintf(stdout, "%s filename = \"%s\".\n", fcn_name, filename);
        HDfprintf(stdout, "%s tag = \"%s\".\n", fcn_name, tag);
        HDfprintf(stdout, "%s write_size = 0x%llx.\n", fcn_name, (long long)write_size);
        HDfprintf(stdout, "%s max_addr   = 0x%llx.\n", fcn_name, (long long)max_addr);
        HDfprintf(stdout, "%s: FILENAMES[%d] = \"%s\".\n", fcn_name, file_name_num,
                  FILENAMES[file_name_num]);
	HDfflush(stdout);
    }

    if ( show_progress ) { /* 2 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass, cp++);
	HDfflush(stdout);
    }

    if ( pass ) { /* allocate write buffer */

        for ( i = H5FD_MEM_SUPER; i <= H5FD_MEM_OHDR; i++ ) {

            write_bufs[i] = (char *)HDmalloc((size_t)(write_size + 1));

	    if ( write_bufs[i] == NULL ) {

	        pass = FALSE;
                failure_mssg = "write buffer allocation failed.";
            }
        }
    }

    if ( show_progress ) { /* 3 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass, cp++);
	HDfflush(stdout);
    }

    if ( pass ) { /* setup write buffers */

	char file_tag[80];

	HDassert( tag != NULL );
	HDassert( strlen(tag) <= 30 );

	for ( i = H5FD_MEM_SUPER; i <= H5FD_MEM_OHDR; i++ ) {

	    HDsnprintf(file_tag, (size_t)80, "%s:%s", tag, type_names[i]);
	    HDassert( strlen(file_tag) <= 48 );
            load_test_buffer(write_size, file_tag, write_bufs[i]);

            if ( verbose ) {

                HDfprintf(stdout, "%s strlen(write_bufs[%d]) = %lld.\n", 
                          fcn_name, i, (long long)strlen(write_bufs[i]));
            }
	}
    }

    if ( show_progress ) { /* 4 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass, cp++);
	HDfflush(stdout);
    }

    /* create the file */
    if ( pass ) {

        h5fd_ptr = H5FDopen(filename, (H5F_ACC_RDWR | H5F_ACC_CREAT),
                            fapl_id, max_addr);

        if ( h5fd_ptr == NULL ) {

            pass = FALSE;
            failure_mssg = "H5FDopen() failed.";

        } else {

            mark_test_in_progress("fsync_test");
        }
    }

    if ( show_progress ) { /* 5 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass, cp++);
	HDfflush(stdout);
    }

    /* set the EOA */

    if ( verbose ) {

        for ( mt = H5FD_MEM_SUPER; mt <= H5FD_MEM_OHDR; mt++ ) {

            eoa = H5FDget_eoa(h5fd_ptr, mt);

	    if ( eoa == HADDR_UNDEF ) {

                HDfprintf(stdout, "%s: H5FDget_eoa(h5fd_ptr, %s) failed.\n",
			  fcn_name, type_names[(int)mt]);

            } else {

                HDfprintf(stdout, "%s: H5FDget_eoa(h5fd_ptr, %s) returned 0x%llx.\n",
			  fcn_name, type_names[(int)mt], (unsigned long long)eoa);

            }
        }
    }

    mt = H5FD_MEM_SUPER;
    while ( ( pass ) && ( mt <= H5FD_MEM_OHDR ) ) {

        if ( verbose ) {

            HDfprintf(stdout, 
                      "calling H5FDset_eoa(h5fd_ptr, %s, (%d * TYPE_SLICE) - 1).\n",
                      type_names[(int)mt], (int)(mt));
        }

        result = H5FDset_eoa(h5fd_ptr, mt, (((haddr_t)(mt)) * TYPE_SLICE) - 1);

        if ( result < 0 ) {

            pass = FALSE;
            failure_mssg = "H5FDset_eoa() failed.";
        }

        mt++;
    }

    if ( verbose ) {

        for ( mt = H5FD_MEM_SUPER; mt <= H5FD_MEM_OHDR; mt++ ) {

            eoa = H5FDget_eoa(h5fd_ptr, mt);

	    if ( eoa == HADDR_UNDEF ) {

                HDfprintf(stdout, "%s: H5FDget_eoa(h5fd_ptr, %s) failed.\n",
			  fcn_name, type_names[(int)mt]);

            } else {

                HDfprintf(stdout, "%s: H5FDget_eoa(h5fd_ptr, %s) returned 0x%llx.\n",
			  fcn_name, type_names[(int)mt], (unsigned long long)eoa);

            }
        }
    }

    if ( show_progress ) { /* 6 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass, cp++);
	HDfflush(stdout);
    }

    /* do the write */
    if ( pass ) {

        i = H5FD_MEM_SUPER;

        while ( ( pass ) && ( i <= H5FD_MEM_OHDR ) ) {

	    if ( verbose ) {

                HDfprintf(stdout, 
	    "calling H5FDwrite(h5fd_ptr, %s, H5P_DEFAULT, 0x%llx, 0x%llx, write_bufs[%d])\n",
			  type_names[i], 
			  (unsigned long long)(((haddr_t)(i - 1)) * TYPE_SLICE),
			  (unsigned long long)write_size,
			  i);
            }

            result = H5FDwrite(h5fd_ptr, (H5F_mem_t)i, H5P_DEFAULT, 
                               ((haddr_t)(i - 1)) * TYPE_SLICE, 
                               (size_t)write_size, (void *)write_bufs[i]);

            if ( result < 0 ) {

	        pass = FALSE;
                failure_mssg = "H5FDwrite() failed.\n";
            }

            i++;
        }
    }

    if ( show_progress ) { /* 7 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass, cp++);
	HDfflush(stdout);
    }

    /* discard the write buffers */

    for ( i = H5FD_MEM_SUPER; i <= H5FD_MEM_OHDR; i++ ) {

        if ( write_bufs[i] != NULL ) {

	    HDfree(write_bufs[i]);
	    write_bufs[i] = NULL;
        }
    }

    if ( show_progress ) { /* 8 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass, cp++);
	HDfflush(stdout);
    }


    /* do the fsync */
    if ( pass ) {

	result = H5FDfsync(h5fd_ptr, H5P_DEFAULT);

        if ( result < 0 ) {

	    pass = FALSE;
            failure_mssg = "H5FDfsync() failed.\n";
        }
    }

    if ( show_progress ) { /* 9 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass, cp++);
	HDfflush(stdout);
    }

    /* do the abort */
    if ( pass ) {

	abort();
    }

    if ( show_progress ) { /* 10 */ 

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass, cp++);
	HDfflush(stdout);
    }

    return;

} /* setup_multi_file_driver_fsync_test() */


/***************************************************************************
 * Function: 	setup_multi_file_driver_aio_fsync_test
 *
 * Purpose:  	Setup test to verify that the aio fsync vfd call works as 
 *		expected for the multi file driver.
 *
 *		To do this, open the specified file with the multi file
 *		driver, write data to the file with the supplied tag, call 
 *		an aio fsync, and then exit without closing the file.
 *
 * Return:      void
 *
 * Programmer: 	John Mainzer
 *              2/4/11
 *
 **************************************************************************/

static void
setup_multi_file_driver_aio_fsync_test(const int file_name_num,
                                       const char * tag,
                                       hsize_t write_size,
                                       hbool_t wait_on_fsync,
                                       hbool_t verbose)
{
    const char * fcn_name = "setup_multi_file_driver_aio_fsync_test():";
    const char * memb_name[H5FD_MEM_NTYPES];
    const char * (type_names[H5FD_MEM_NTYPES]) = 
	{
	    "H5FD_MEM_DEFAULT",
	    "H5FD_MEM_SUPER",
	    "H5FD_MEM_BTREE",
	    "H5FD_MEM_DRAW",
	    "H5FD_MEM_GHEAP",
	    "H5FD_MEM_LHEAP",
	    "H5FD_MEM_OHDR"
	};
    char * (write_bufs[H5FD_MEM_NTYPES]) = { NULL, NULL, NULL, NULL, NULL, NULL, NULL };
    char filename[1024];
    hbool_t done; 
    hbool_t show_progress = FALSE;
    int cp = 0;
    int error_num;
    unsigned int i;
    hid_t fapl_id;
    hid_t memb_fapl[H5FD_MEM_NTYPES];
    herr_t result;
    haddr_t eoa;
    haddr_t max_addr;
    haddr_t memb_addr[H5FD_MEM_NTYPES];
    void * (wrt_ctlblk_ptrs[H5FD_MEM_NTYPES]) = { NULL, NULL, NULL, NULL, NULL, NULL, NULL };
    void * fsync_ctlblk_ptr = NULL;
    H5FD_mem_t mt;
    H5FD_mem_t memb_map[H5FD_MEM_NTYPES];
    H5FD_t * h5fd_ptr = NULL;

    if ( show_progress ) { /* 0 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass, cp++);
	HDfflush(stdout);
    }

    if ( pass ) { /* setup fapl for multi file driver */

        for ( mt = 0; mt < H5FD_MEM_NTYPES; mt++ ) {

            memb_addr[mt] = HADDR_UNDEF;
            memb_fapl[mt] = H5P_DEFAULT;
            memb_map[mt]  = H5FD_MEM_DRAW;
            memb_name[mt] = NULL;
        }

        memb_map[H5FD_MEM_SUPER]  = H5FD_MEM_SUPER;
        memb_fapl[H5FD_MEM_SUPER] = H5P_DEFAULT;
        memb_name[H5FD_MEM_SUPER] = "%s-s.h5";
        memb_addr[H5FD_MEM_SUPER] = 0;

        memb_map[H5FD_MEM_BTREE]  = H5FD_MEM_BTREE;
        memb_fapl[H5FD_MEM_BTREE] = H5P_DEFAULT;
        memb_name[H5FD_MEM_BTREE] = "%s-b.h5";
        memb_addr[H5FD_MEM_BTREE] = memb_addr[H5FD_MEM_SUPER] + TYPE_SLICE; 

        memb_map[H5FD_MEM_DRAW]   = H5FD_MEM_DRAW;
        memb_fapl[H5FD_MEM_DRAW]  = H5P_DEFAULT;
        memb_name[H5FD_MEM_DRAW]  = "%s-r.h5";
        memb_addr[H5FD_MEM_DRAW]  =  memb_addr[H5FD_MEM_BTREE] + TYPE_SLICE;

        memb_map[H5FD_MEM_GHEAP]  = H5FD_MEM_GHEAP;
        memb_fapl[H5FD_MEM_GHEAP] = H5P_DEFAULT;
        memb_name[H5FD_MEM_GHEAP] = "%s-g.h5";
        memb_addr[H5FD_MEM_GHEAP] = memb_addr[H5FD_MEM_DRAW] + TYPE_SLICE;

        memb_map[H5FD_MEM_LHEAP]  = H5FD_MEM_LHEAP;
        memb_fapl[H5FD_MEM_LHEAP] = H5P_DEFAULT;
        memb_name[H5FD_MEM_LHEAP] = "%s-l.h5";
        memb_addr[H5FD_MEM_LHEAP] = memb_addr[H5FD_MEM_GHEAP] + TYPE_SLICE;

        memb_map[H5FD_MEM_OHDR]   = H5FD_MEM_OHDR;
        memb_fapl[H5FD_MEM_OHDR]  = H5P_DEFAULT;
        memb_name[H5FD_MEM_OHDR]  = "%s-o.h5";
        memb_addr[H5FD_MEM_OHDR]  = memb_addr[H5FD_MEM_LHEAP] + TYPE_SLICE;

        max_addr = memb_addr[H5FD_MEM_OHDR] + TYPE_SLICE;

        fapl_id = h5_fileaccess();

        if ( H5Pset_fapl_multi(fapl_id, memb_map, memb_fapl, memb_name,
                               memb_addr, FALSE) < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Pset_fapl_multi() failed.";

        }
    }

    if ( show_progress ) { /* 1 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass, cp++);
	HDfflush(stdout);
    }


    /* setup the file name */
    if ( pass ) {

        if ( h5_fixname(FILENAMES[file_name_num], fapl_id, 
                        filename, sizeof(filename)) == NULL ) {

            pass = FALSE;
            failure_mssg = "h5_fixname() failed (1).\n";
        }
    }

    if ( verbose ) {

        HDfprintf(stdout, "%s filename = \"%s\".\n", fcn_name, filename);
        HDfprintf(stdout, "%s tag = \"%s\".\n", fcn_name, tag);
        HDfprintf(stdout, "%s write_size = 0x%llx.\n", fcn_name, (long long)write_size);
        HDfprintf(stdout, "%s max_addr   = 0x%llx.\n", fcn_name, (long long)max_addr);
        HDfprintf(stdout, "%s: FILENAMES[%d] = \"%s\".\n", fcn_name, file_name_num,
                  FILENAMES[file_name_num]);
	HDfflush(stdout);
    }

    if ( show_progress ) { /* 2 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass, cp++);
	HDfflush(stdout);
    }

    if ( pass ) { /* allocate write buffer */

        for ( i = H5FD_MEM_SUPER; i <= H5FD_MEM_OHDR; i++ ) {

            write_bufs[i] = (char *)HDmalloc((size_t)(write_size + 1));

	    if ( write_bufs[i] == NULL ) {

	        pass = FALSE;
                failure_mssg = "write buffer allocation failed.";
            }
        }
    }

    if ( show_progress ) { /* 3 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass, cp++);
	HDfflush(stdout);
    }

    if ( pass ) { /* setup write buffers */

	char file_tag[80];

	HDassert( tag != NULL );
	HDassert( strlen(tag) <= 30 );

	for ( i = H5FD_MEM_SUPER; i <= H5FD_MEM_OHDR; i++ ) {

	    HDsnprintf(file_tag, (size_t)80, "%s:%s", tag, type_names[i]);
	    HDassert( strlen(file_tag) <= 48 );
            load_test_buffer(write_size, file_tag, write_bufs[i]);

            if ( verbose ) {

                HDfprintf(stdout, "%s strlen(write_bufs[%d]) = %lld.\n", 
                          fcn_name, i, (long long)strlen(write_bufs[i]));
            }
	}
    }

    if ( show_progress ) { /* 4 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass, cp++);
	HDfflush(stdout);
    }

    /* create the file */
    if ( pass ) {

        h5fd_ptr = H5FDopen(filename, (H5F_ACC_RDWR | H5F_ACC_CREAT),
                            fapl_id, max_addr);

        if ( h5fd_ptr == NULL ) {

            pass = FALSE;
            failure_mssg = "H5FDopen() failed.";

        } else {

            mark_test_in_progress("fsync_test");
        }
    }

    if ( show_progress ) { /* 5 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass, cp++);
	HDfflush(stdout);
    }

    /* set the EOA */

    if ( verbose ) {

        for ( mt = H5FD_MEM_SUPER; mt <= H5FD_MEM_OHDR; mt++ ) {

            eoa = H5FDget_eoa(h5fd_ptr, mt);

	    if ( eoa == HADDR_UNDEF ) {

                HDfprintf(stdout, "%s: H5FDget_eoa(h5fd_ptr, %s) failed.\n",
			  fcn_name, type_names[(int)mt]);

            } else {

                HDfprintf(stdout, "%s: H5FDget_eoa(h5fd_ptr, %s) returned 0x%llx.\n",
			  fcn_name, type_names[(int)mt], (unsigned long long)eoa);

            }
        }
    }

    mt = H5FD_MEM_SUPER;
    while ( ( pass ) && ( mt <= H5FD_MEM_OHDR ) ) {

        if ( verbose ) {

            HDfprintf(stdout, 
                      "calling H5FDset_eoa(h5fd_ptr, %s, (%d * TYPE_SLICE) - 1).\n",
                      type_names[(int)mt], (int)(mt));
        }

        result = H5FDset_eoa(h5fd_ptr, mt, (((haddr_t)(mt)) * TYPE_SLICE) - 1);

        if ( result < 0 ) {

            pass = FALSE;
            failure_mssg = "H5FDset_eoa() failed.";
        }

        mt++;
    }

    if ( verbose ) {

        for ( mt = H5FD_MEM_SUPER; mt <= H5FD_MEM_OHDR; mt++ ) {

            eoa = H5FDget_eoa(h5fd_ptr, mt);

	    if ( eoa == HADDR_UNDEF ) {

                HDfprintf(stdout, "%s: H5FDget_eoa(h5fd_ptr, %s) failed.\n",
			  fcn_name, type_names[(int)mt]);

            } else {

                HDfprintf(stdout, "%s: H5FDget_eoa(h5fd_ptr, %s) returned 0x%llx.\n",
			  fcn_name, type_names[(int)mt], (unsigned long long)eoa);

            }
        }
    }

    if ( show_progress ) { /* 6 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass, cp++);
	HDfflush(stdout);
    }

    /* queue the writes */
    if ( pass ) {

        i = H5FD_MEM_SUPER;

        while ( ( pass ) && ( i <= H5FD_MEM_OHDR ) ) {

	    if ( verbose ) {

                HDfprintf(stdout, 
    "calling H5FDaio_write(%s, %s, %s, 0x%llx, 0x%llx, write_bufs[%d], &(wrt_ctlblk_ptrs[%d])\n",
                          "h5fd_ptr",
			  type_names[i], 
                          "H5P_DEFAULT",
			  (unsigned long long)(((haddr_t)(i - 1)) * TYPE_SLICE),
			  (unsigned long long)write_size,
			  i,
                          i);
            }

            result = H5FDaio_write(h5fd_ptr, (H5F_mem_t)i, H5P_DEFAULT, 
                                   ((haddr_t)(i - 1)) * TYPE_SLICE, 
                                   (size_t)write_size, (void *)write_bufs[i],
                                   &(wrt_ctlblk_ptrs[i]));

            if ( ( result < 0 ) || ( wrt_ctlblk_ptrs[i] == NULL ) ) {

	        pass = FALSE;
                failure_mssg = "H5FDaio_write() failed.\n";
            }

            i++;
        }
    }

    if ( show_progress ) { /* 7 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass, cp++);
	HDfflush(stdout);
    }

    /* do the aio_fsync() */
    if ( pass ) {

        result = H5FDaio_fsync(h5fd_ptr, &fsync_ctlblk_ptr);

        if ( ( result < 0 ) || ( fsync_ctlblk_ptr ==  NULL ) ) {

            pass = FALSE;
            failure_mssg = "H5FDaio_fsync(0) failed.";
        }
    }

    if ( show_progress ) { /* 8 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass, cp++);
        HDfflush(stdout);
    }

    /* await the completion of the fsync */
    if ( pass ) {

        if ( wait_on_fsync ) {

            result = H5FDaio_wait(h5fd_ptr, fsync_ctlblk_ptr);

            if ( result < 0 ) {

                pass = FALSE;
                failure_mssg = "H5FDaio_wait(0) failed.";
            }
        } else {

            done = FALSE;

            while ( ( pass ) && ( ! done ) ) {

                result = H5FDaio_test(h5fd_ptr, &done, fsync_ctlblk_ptr);

                if ( result < 0 ) {

                    pass = FALSE;
                    failure_mssg = "H5FDaio_test(0) failed.";
                }
            }
        }
    }

    if ( show_progress ) { /* 9 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass, cp++);
        HDfflush(stdout);
    }

    /* complete the aio fsync */
    if ( pass ) {

        result = H5FDaio_finish(h5fd_ptr, &error_num, fsync_ctlblk_ptr);

        if ( ( result < 0 ) || ( error_num != 0 ) ) {

            pass = FALSE;
            failure_mssg = "H5FDaio_finish(0) failed.";
        }
    }

    if ( show_progress ) { /* 10 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass, cp++);
        HDfflush(stdout);
    }

    /* at this point, the aio writes should be complete as well -- verify this. */
    if ( pass ) {

        i = H5FD_MEM_SUPER;

        while ( ( pass ) && ( i <= H5FD_MEM_OHDR ) ) {

            done = FALSE;

            result = H5FDaio_test(h5fd_ptr, &done, wrt_ctlblk_ptrs[i]);

            if ( result < 0 ) {

                pass = FALSE;
                failure_mssg = "H5FDaio_test(1) failed.";

            } else if ( done != TRUE ) {

                pass = FALSE;
                failure_mssg = "aio_write() not complete after completion of aio_fsync().";

            }

            i++;
        }
    }

    if ( show_progress ) { /* 11 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass, cp++);
        HDfflush(stdout);
    }

    /* discard the write buffers */

    for ( i = H5FD_MEM_SUPER; i <= H5FD_MEM_OHDR; i++ ) {

        if ( write_bufs[i] != NULL ) {

	    HDfree(write_bufs[i]);
	    write_bufs[i] = NULL;
        }
    }

    if ( show_progress ) { /* 12 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass, cp++);
	HDfflush(stdout);
    }

    /* do the abort */
    if ( pass ) {

	abort();
    }

    if ( show_progress ) { /* 13 */ 

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass, cp++);
	HDfflush(stdout);
    }

    return;

} /* setup_multi_file_driver_aio_fsync_test() */


/***************************************************************************
 * Function: 	check_generic_fsync_test
 *
 * Purpose:  	Check to verify that the specified file contains the 
 *		expected data.  
 *
 *		To do this, first check to verify that a test is in 
 *		progress.
 *
 *		Then open the test file created by 
 *		setup_generic_fsync_test(), and verify that it 
 *		contains the expected data.
 *
 *              On either success or failure, clean up the test file.
 *
 * Return:      void
 *
 * Programmer: 	John Mainzer
 *              11/9/10
 *
 **************************************************************************/

static void
check_generic_fsync_test(const int file_name_num,
                         hid_t fapl_id,
                         const char * tag,
                         hsize_t file_size,
                         haddr_t maxaddr,
                         hbool_t verbose)
{
    const char * fcn_name = "check_generic_fsync_test():";
    char * read_buf = NULL;
    char * test_buf = NULL;
    char file_name[1024];
    hbool_t show_progress = FALSE;
    hbool_t skip_file_existance_and_size_checks = FALSE;
    int cp = 0;
    herr_t result;
    hsize_t i;
    hid_t driver_id;
    H5FD_t * h5fd_ptr = NULL;
    h5_stat_t buf;

    check_test_in_progress("fsync_test", verbose);

    if ( pass ) {

        driver_id = H5Pget_driver(fapl_id);

        if ( driver_id < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Pget_driver() failed.\n";

        } else if ( ( driver_id == H5FD_FAMILY ) || ( driver_id == H5FD_MULTI ) ) {

	    /* file appears to be a family or multi file -- thus the usual 
             * existance and size checks will fail.
             */
	    skip_file_existance_and_size_checks = TRUE;
        }
    }

    /* setup the file name */
    if ( pass ) {

        if ( h5_fixname(FILENAMES[file_name_num], fapl_id, file_name,
	                sizeof(file_name)) == NULL ) {

            pass = FALSE;
            failure_mssg = "h5_fixname() failed.\n";
        }
    }

    if ( show_progress ) { /* 0 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass, cp++);
	HDfflush(stdout);
    }

    if ( verbose ) {
        HDfprintf(stdout, "%s file_name = \"%s\".\n", fcn_name, file_name);
        HDfprintf(stdout, "%s skip_file_existance_and_size_checks = %d.\n", 
                  fcn_name, (int)skip_file_existance_and_size_checks);
	HDfflush(stdout);
    }

    /* allocate the read and expected data buffers */
    if ( pass ) {

        read_buf = (char *)HDmalloc((size_t)(file_size + 1));
        test_buf = (char *)HDmalloc((size_t)(file_size + 1));

        if ( ( read_buf == NULL ) || 
             ( test_buf == NULL ) ) {

	    pass = FALSE;
            failure_mssg = "buffer allocation(s) failed.";
        }
    }

    if ( show_progress ) { /* 1 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, (int)pass, cp++);
	HDfflush(stdout);
    }

    /* initialize the expected data buffer */
    if ( pass ) {

        load_test_buffer(file_size, tag, test_buf);
    }

    if ( show_progress ) { /* 2 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, (int)pass, cp++);
	HDfflush(stdout);
    }

    /* verify that the target file exists.  Note that we skip this test if
     * skip_file_existance_and_size_checks is true -- in this case we are 
     * dealing with either a family or multi file name, and thus that no 
     * file of the exact name provided can be expected to exist.
     */
    if ( ( pass ) && 
         ( ! skip_file_existance_and_size_checks ) &&
         ( ! file_exists(file_name, verbose) ) ) {

        pass = FALSE;
        failure_mssg = "target file doesn't exist?";
    }

    if ( show_progress ) { /* 3 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, (int)pass, cp++);
	HDfflush(stdout);
    }

    /* verify that the target file is of the expected length */
    if ( ( pass ) && ( ! skip_file_existance_and_size_checks ) ) {

	if ( HDstat(file_name, &buf) != 0 ) {

	    if ( verbose ) {

	        HDfprintf(stdout, "%s: HDstat() failed with errno = %d.\n",
                          fcn_name, errno);
	    }

	    failure_mssg = "stat() failed on test in progress file.";
	    pass = FALSE;

	} else {

	    if ( (buf.st_size) == 0 ) {

                failure_mssg = "target file is empty?!?";
	        pass = FALSE;

            } else if ( (hsize_t)(buf.st_size) < file_size ) {

                failure_mssg = "target file is too small?!?";
	        pass = FALSE;

            } else if ( (hsize_t)(buf.st_size) > file_size ) {

                failure_mssg = "target file is too big?!?";
	        pass = FALSE;
            }
	}
    }

    if ( show_progress ) { /* 4 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, (int)pass, cp++);
	HDfflush(stdout);
    }

    /* open the indicated file with the specifiec file driver */
    if ( pass ) {

        h5fd_ptr = H5FDopen(file_name, H5F_ACC_RDWR, fapl_id, maxaddr);

        if ( h5fd_ptr == NULL ) {

            pass = FALSE;
            failure_mssg = "H5FDopen() failed.";
        }
    }

    if ( show_progress ) { /* 5 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, (int)pass, cp++);
	HDfflush(stdout);
    }

    /* set the eoa */
    if ( pass ) {

        result = H5FDset_eoa(h5fd_ptr, H5FD_MEM_DEFAULT, maxaddr);

        if ( result < 0 ) {

            pass = FALSE;
            failure_mssg = "H5FDset_eoa() failed.";
        }
    }

    if ( show_progress ) { /* 6 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, (int)pass, cp++);
	HDfflush(stdout);
    }

    /* read the contents of the target file */
    if ( pass ) {

        result = H5FDread(h5fd_ptr, H5FD_MEM_DEFAULT, H5P_DEFAULT, (haddr_t)0,
                          (size_t)file_size, (void *)read_buf);

        if ( result < 0 ) {

            pass = FALSE;
            failure_mssg = "H5FDread() failed.";
        }
    }

    if ( show_progress ) { /* 7 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, (int)pass, cp++);
	HDfflush(stdout);
    }

    /* compare the actual file target file data with the expected contents */
    if ( pass ) {

        /* observe that this test is weak if skip_file_existance_and_size_checks
         * TRUE -- it only tells us if the file contains the expected data --
         * not if it contains extranious data.  Unfortunately, to the best of 
         * my knowlege we don't have any easy way to determine file size in the 
         * family and multi file case.  Thus this is as best we can do right now
         * without building extensive knowlege of these formats into the test.
         */

        i = 0;
        while ( ( pass ) && ( i < file_size ) ) {

            if ( read_buf[i] != test_buf[i] ) {

                if ( verbose ) {

                    HDfprintf(stdout, "data mismatch at %d.\n", i);
	            HDfflush(stdout);
                }

                pass = FALSE;
                failure_mssg = "data mismatch";
            }

            i++;
        }
    }

    if ( show_progress ) { /* 8 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, (int)pass, cp++);
	HDfflush(stdout);
    }

    /* close target file */
    if ( pass ) {

        result = H5FDclose(h5fd_ptr);

        if ( result < 0 ) {

            pass = FALSE;
            failure_mssg = "H5FDclose() failed.";
        }
    }

    if ( show_progress ) { /* 9 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, (int)pass, cp++);
	HDfflush(stdout);
    }

    /* discard the read and expected buffers */

    if ( read_buf != NULL ) {

        HDfree(read_buf);
	read_buf = NULL;
    }

    if ( test_buf != NULL ) {

        HDfree(test_buf);
	test_buf = NULL;
    }

    if ( show_progress ) { /* 10 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, (int)pass, cp++);
	HDfflush(stdout);
    }


    /* delete the target file */
    if ( pass ) {
#if 1
        h5_cleanup(FILENAMES, fapl_id);
#endif
    }

    return;

} /* check_generic_fsync_test() */


/***************************************************************************
 * Function: 	check_multi_file_driver_fsync_test
 *
 * Purpose:  	Check to verify that the specified file contains the 
 *		expected data.  
 *
 *		To do this, first check to verify that a test is in 
 *		progress.
 *
 *		Then open the test file created by 
 *		setup_generic_fsync_test(), and verify that it 
 *		contains the expected data.
 *
 *              On either success or failure, clean up the test file.
 *
 * Return:      void
 *
 * Programmer: 	John Mainzer
 *              11/9/10
 *
 **************************************************************************/

static void
check_multi_file_driver_fsync_test(const int file_name_num,
                                   const char * tag,
                                   hsize_t write_size,
                                   hbool_t verbose)
{
    const char * fcn_name = "check_multi_file_driver_fsync_test():";
    const char * memb_name[H5FD_MEM_NTYPES];
    const char * (type_names[H5FD_MEM_NTYPES]) =
        {
            "H5FD_MEM_DEFAULT",
            "H5FD_MEM_SUPER",
            "H5FD_MEM_BTREE",
            "H5FD_MEM_DRAW",
            "H5FD_MEM_GHEAP",
            "H5FD_MEM_LHEAP",
            "H5FD_MEM_OHDR"
        };
    char * (read_bufs[H5FD_MEM_NTYPES]) = { NULL, NULL, NULL, NULL, NULL, NULL, NULL };
    char * (test_bufs[H5FD_MEM_NTYPES]) = { NULL, NULL, NULL, NULL, NULL, NULL, NULL };
    char file_name[1024];
    hbool_t show_progress = FALSE;
    int cp = 0;
    herr_t result;
    hsize_t i;
    hsize_t j;
    hid_t fapl_id;
    hid_t memb_fapl[H5FD_MEM_NTYPES];
    haddr_t eoa;
    haddr_t max_addr;
    haddr_t memb_addr[H5FD_MEM_NTYPES];
    H5FD_mem_t mt;
    H5FD_mem_t memb_map[H5FD_MEM_NTYPES];
    H5FD_t * h5fd_ptr = NULL;

    check_test_in_progress("fsync_test", verbose);

    if ( show_progress ) { /* 0 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass, cp++);
	HDfflush(stdout);
    }

    if ( pass ) { /* setup fapl for multi file driver */

        for ( mt = 0; mt < H5FD_MEM_NTYPES; mt++ ) {

            memb_addr[mt] = HADDR_UNDEF;
            memb_fapl[mt] = H5P_DEFAULT;
            memb_map[mt]  = H5FD_MEM_DRAW;
            memb_name[mt] = NULL;
        }

        memb_map[H5FD_MEM_SUPER]  = H5FD_MEM_SUPER;
        memb_fapl[H5FD_MEM_SUPER] = H5P_DEFAULT;
        memb_name[H5FD_MEM_SUPER] = "%s-s.h5";
        memb_addr[H5FD_MEM_SUPER] = 0;

        memb_map[H5FD_MEM_BTREE]  = H5FD_MEM_BTREE;
        memb_fapl[H5FD_MEM_BTREE] = H5P_DEFAULT;
        memb_name[H5FD_MEM_BTREE] = "%s-b.h5";
        memb_addr[H5FD_MEM_BTREE] = memb_addr[H5FD_MEM_SUPER] + TYPE_SLICE;

        memb_map[H5FD_MEM_DRAW]   = H5FD_MEM_DRAW;
        memb_fapl[H5FD_MEM_DRAW]  = H5P_DEFAULT;
        memb_name[H5FD_MEM_DRAW]  = "%s-r.h5";
        memb_addr[H5FD_MEM_DRAW]  =  memb_addr[H5FD_MEM_BTREE] + TYPE_SLICE;

        memb_map[H5FD_MEM_GHEAP]  = H5FD_MEM_GHEAP;
        memb_fapl[H5FD_MEM_GHEAP] = H5P_DEFAULT;
        memb_name[H5FD_MEM_GHEAP] = "%s-g.h5";
        memb_addr[H5FD_MEM_GHEAP] = memb_addr[H5FD_MEM_DRAW] + TYPE_SLICE;

        memb_map[H5FD_MEM_LHEAP]  = H5FD_MEM_LHEAP;
        memb_fapl[H5FD_MEM_LHEAP] = H5P_DEFAULT;
        memb_name[H5FD_MEM_LHEAP] = "%s-l.h5";
        memb_addr[H5FD_MEM_LHEAP] = memb_addr[H5FD_MEM_GHEAP] + TYPE_SLICE;

        memb_map[H5FD_MEM_OHDR]   = H5FD_MEM_OHDR;
        memb_fapl[H5FD_MEM_OHDR]  = H5P_DEFAULT;
        memb_name[H5FD_MEM_OHDR]  = "%s-o.h5";
        memb_addr[H5FD_MEM_OHDR]  = memb_addr[H5FD_MEM_LHEAP] + TYPE_SLICE;

        max_addr = memb_addr[H5FD_MEM_OHDR] + TYPE_SLICE;

        fapl_id = h5_fileaccess();

        if ( H5Pset_fapl_multi(fapl_id, memb_map, memb_fapl, memb_name,
                               memb_addr, FALSE) < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Pset_fapl_multi() failed.";

        }
    }

    if ( show_progress ) { /* 1 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass, cp++);
	HDfflush(stdout);
    }

    /* setup the file name */
    if ( pass ) {

        if ( h5_fixname(FILENAMES[file_name_num], fapl_id, file_name,
	                sizeof(file_name)) == NULL ) {

            pass = FALSE;
            failure_mssg = "h5_fixname() failed.\n";
        }
    }

    if ( verbose ) {
        HDfprintf(stdout, "%s file_name = \"%s\".\n", fcn_name, file_name);
	HDfflush(stdout);
    }

    if ( show_progress ) { /* 2 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, pass, cp++);
	HDfflush(stdout);
    }


    /* allocate the read and test buffers */
    if ( pass ) { 

        for ( i = H5FD_MEM_SUPER; i <= H5FD_MEM_OHDR; i++ ) {

            read_bufs[i] = (char *)HDmalloc((size_t)(write_size + 1));
            test_bufs[i] = (char *)HDmalloc((size_t)(write_size + 1));

            if ( read_bufs[i] == NULL ) {

                pass = FALSE;
                failure_mssg = "read buffer allocation failed.";

            } else if ( test_bufs[i] == NULL ) {

                pass = FALSE;
                failure_mssg = "test buffer allocation failed.";
            }
        }
    }

    if ( show_progress ) { /* 3 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, (int)pass, cp++);
	HDfflush(stdout);
    }

    if ( pass ) { /* setup test buffers */

        char file_tag[80];

        HDassert( tag != NULL );
        HDassert( strlen(tag) <= 30 );

        for ( i = H5FD_MEM_SUPER; i <= H5FD_MEM_OHDR; i++ ) {

            HDsnprintf(file_tag, (size_t)80, "%s:%s", tag, type_names[i]);
            HDassert( strlen(file_tag) <= 48 );
            load_test_buffer(write_size, file_tag, test_bufs[i]);

            if ( verbose ) {

                HDfprintf(stdout, "%s strlen(test_bufs[%d]) = %lld.\n",
                          fcn_name, i, (long long)strlen(test_bufs[i]));
            }
        }
    }

    if ( show_progress ) { /* 4 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, (int)pass, cp++);
	HDfflush(stdout);
    }

    /* open the indicated file with the specifiec file driver */
    if ( pass ) {

        h5fd_ptr = H5FDopen(file_name, H5F_ACC_RDWR, fapl_id, max_addr);

        if ( h5fd_ptr == NULL ) {

            pass = FALSE;
            failure_mssg = "H5FDopen() failed.";
        }
    }

    if ( show_progress ) { /* 5 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, (int)pass, cp++);
	HDfflush(stdout);
    }

    /* set the EOA */

    if ( verbose ) {

        for ( mt = H5FD_MEM_SUPER; mt <= H5FD_MEM_OHDR; mt++ ) {

            eoa = H5FDget_eoa(h5fd_ptr, mt);

	    if ( eoa == HADDR_UNDEF ) {

                HDfprintf(stdout, "%s: H5FDget_eoa(h5fd_ptr, %s) failed.\n",
			  fcn_name, type_names[(int)mt]);

            } else {

                HDfprintf(stdout, "%s: H5FDget_eoa(h5fd_ptr, %s) returned 0x%llx.\n",
			  fcn_name, type_names[(int)mt], (unsigned long long)eoa);

            }
        }
    }

    mt = H5FD_MEM_SUPER;
    while ( ( pass ) && ( mt <= H5FD_MEM_OHDR ) ) {

        if ( verbose ) {

            HDfprintf(stdout, 
                      "calling H5FDset_eoa(h5fd_ptr, %s, (%d * TYPE_SLICE) - 1).\n",
                      type_names[(int)mt], (int)(mt));
        }

        result = H5FDset_eoa(h5fd_ptr, mt, (((haddr_t)(mt)) * TYPE_SLICE) - 1);

        if ( result < 0 ) {

            pass = FALSE;
            failure_mssg = "H5FDset_eoa() failed.";
        }

        mt++;
    }

    if ( verbose ) {

        for ( mt = H5FD_MEM_SUPER; mt <= H5FD_MEM_OHDR; mt++ ) {

            eoa = H5FDget_eoa(h5fd_ptr, mt);

	    if ( eoa == HADDR_UNDEF ) {

                HDfprintf(stdout, "%s: H5FDget_eoa(h5fd_ptr, %s) failed.\n",
			  fcn_name, type_names[(int)mt]);

            } else {

                HDfprintf(stdout, "%s: H5FDget_eoa(h5fd_ptr, %s) returned 0x%llx.\n",
			  fcn_name, type_names[(int)mt], (unsigned long long)eoa);

            }
        }
    }

    if ( show_progress ) { /* 6 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, (int)pass, cp++);
	HDfflush(stdout);
    }

    /* read the contents of the target file */
    if ( pass ) {

        i = H5FD_MEM_SUPER;

        while ( ( pass ) && ( i <= H5FD_MEM_OHDR ) ) {

            result = H5FDread(h5fd_ptr, (H5F_mem_t)i, H5P_DEFAULT,
                              ((haddr_t)(i - 1)) * TYPE_SLICE,
                              (size_t)write_size, (void *)read_bufs[i]);

            if ( result < 0 ) {

                pass = FALSE;
                failure_mssg = "H5FDread() failed.\n";
            }

            i++;
        }
    }

    if ( show_progress ) { /* 7 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, (int)pass, cp++);
	HDfflush(stdout);
    }

    /* compare the actual file target file data with the expected contents */
    if ( pass ) {

        i = H5FD_MEM_SUPER;
	while ( ( pass ) && ( i <= H5FD_MEM_OHDR ) ) {

            j = 0;
            while ( ( pass ) && ( j < write_size ) ) {

                if ( (read_bufs[i])[j] != (test_bufs[i])[j] ) {

                    if ( verbose ) {

                        HDfprintf(stdout, "data mismatch at %d/%d.\n", (int)i, (int)j);
	                HDfflush(stdout);
                    }

                    pass = FALSE;
                    failure_mssg = "data mismatch";
                }

                j++;
            }

            i++;
        }
    }

    if ( show_progress ) { /* 8 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, (int)pass, cp++);
	HDfflush(stdout);
    }

    /* close target file */
    if ( pass ) {

        result = H5FDclose(h5fd_ptr);

        if ( result < 0 ) {

            pass = FALSE;
            failure_mssg = "H5FDclose() failed.";
        }
    }

    if ( show_progress ) { /* 9 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, (int)pass, cp++);
	HDfflush(stdout);
    }

    /* discard the read and test buffers */
    for ( i = H5FD_MEM_SUPER; i <= H5FD_MEM_OHDR; i++ ) {

        if ( test_bufs[i] != NULL ) {

            HDfree(test_bufs[i]);
            test_bufs[i] = NULL;
        }

        if ( read_bufs[i] != NULL ) {

            HDfree(read_bufs[i]);
            read_bufs[i] = NULL;
        }
    }

    if ( show_progress ) { /* 10 */

        HDfprintf(stdout, "%s%d cp = %d.\n", fcn_name, (int)pass, cp++);
	HDfflush(stdout);
    }


    /* delete the target file */
    if ( pass ) {
#if 1
        h5_cleanup(FILENAMES, fapl_id);
#endif
    }

    return;

} /* check_multi_file_driver_fsync_test() */


/*-------------------------------------------------------------------------
 * Function:	usage
 *
 * Purpose:	Display a brief message describing the purpose and use
 * 		of the program.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              11/10/10
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
        "fsync_tests:\n",
        "\n",
        "Setup or check the results of the specified fsync test.\n",
        "\n",
        "usage:	fsync_tests <test> <driver> <op> [<write_size>] [verbose]\n",
        "\n",
        "where:\n",
        "\n",
        "	<test> ::= ( generic_fsync_test |\n",
        "	             generic_aio_fsync_wait_test |\n",
        "	             generic_aio_fsync_poll_test |\n",
	"	             multi_fd_fsync_test |\n",
	"	             multi_fd_aio_fsync_wait_test |\n",
	"	             multi_fd_aio_fsync_poll_test)\n",
        "\n",
        "	<driver> ::= ( sec2 | core | stdio | family | multi)\n",
        "\n",
        "	<op> ::= ( setup | check )\n",
	"\n",
	"	<write_size> ::= positive decimal integer (default 1048576).\n"
        "\n",
        "Returns 0 on success, 1 on failure.\n",
        "\n",
        "multi_fd_fsync_test, multi_fd_aio_fsync_wait_test, and multi_fd_aio_fsync_poll_test\n",
        "all require use of the multi file driver.\n"
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
 * Purpose:	Run the specified setup or check of fsync.
 *
 * Return:	Success: 0
 *
 *		Failure: A positive integer.
 *
 * Programmer:	John Mainzer
 *              10/10/10
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

int
main(int argc,
     char * argv[])
{
    const char * tag;
    const char * memb_name[H5FD_MEM_NTYPES];
    char * test_ptr;
    int express_test;
    int file_name_num;
    int result = 0;
    hbool_t all_digits = TRUE;
    hbool_t have_verbose = FALSE;
    hbool_t verbose = FALSE;
    enum operation { setup, check } op;
    enum file_driver { sec2_fd, core_fd, stdio_fd, family_fd, multi_fd } tgt_driver;
    enum test_to_run { generic_fsync_test, 
                       generic_aio_fsync_poll_test, 
                       generic_aio_fsync_wait_test,
                       multi_fd_fsync_test,
                       multi_fd_aio_fsync_poll_test,
                       multi_fd_aio_fsync_wait_test } tgt_test;
    hid_t fapl;
    hid_t memb_fapl[H5FD_MEM_NTYPES];
    hsize_t write_size = (1024 * 1024);
    haddr_t max_addr;
    haddr_t memb_addr[H5FD_MEM_NTYPES];
    H5FD_mem_t mt;
    H5FD_mem_t memb_map[H5FD_MEM_NTYPES];

    express_test = GetTestExpress();

    pass = TRUE;

    if ( H5open() < 0 ) {

        pass = FALSE;
        failure_mssg = "H5open() failed.";
    }

    if ( ( argc == 6 ) || ( argc == 5 ) ) {

        if ( argc == 6 ) {

            if ( strcmp("verbose", argv[5]) == 0 ) {

	        have_verbose = TRUE;
	        verbose = TRUE;

	    } else {

                pass = FALSE;
	        usage();

	    }
        }

        if ( ( ! have_verbose ) && ( strcmp("verbose", argv[4]) == 0 ) ) {

	    have_verbose = TRUE;
	    verbose = TRUE;

        } else {

	    test_ptr = argv[4];

	    while ( ( *test_ptr != '\0' ) && ( all_digits ) ) {

		all_digits = ( ( all_digits ) && ( isdigit(*test_ptr) != 0 ) );
                test_ptr++;
            }

	    if ( ! all_digits ) {

                pass = FALSE;
	        usage();

            } else {

                write_size = (hsize_t)atoll(argv[4]);

		if ( write_size <= 0 ) {

                    pass = FALSE;
	            usage();
	        }
            }
        }
    } else if ( argc != 4 ) {

        pass = FALSE;
	usage();
    }

    if ( verbose ) {

	if ( argc == 6 ) {

	    HDfprintf(stdout, "%s %s %s %s %s %s\n", 
                      argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
	} else if ( argc == 5 ) {

	    HDfprintf(stdout, "%s %s %s %s %s\n", 
                      argv[0], argv[1], argv[2], argv[3], argv[4]);
	} else {

	    HDfprintf(stdout, "%s %s %s %s\n", 
                      argv[0], argv[1], argv[2], argv[3]);
        }
    }

    if ( pass ) {

        if ( strcmp("setup", argv[3]) == 0 ) {

	    op = setup;

	} else if ( strcmp("check", argv[3]) == 0 ) {

            op = check;

        } else {

	    pass = FALSE;
	    usage();
	}
    }

    if ( pass ) {

        if ( strcmp("sec2", argv[2]) == 0 ) {

	    tgt_driver = sec2_fd;

        } else if( strcmp("core", argv[2]) == 0 ) {

	    tgt_driver = core_fd;

        } else if( strcmp("stdio", argv[2]) == 0 ) {

	    tgt_driver = stdio_fd;

        } else if( strcmp("family", argv[2]) == 0 ) {

	    tgt_driver = family_fd;

        } else if( strcmp("multi", argv[2]) == 0 ) {

	    tgt_driver = multi_fd;

        } else {

	    pass = FALSE;
	    usage();
        }
    }

    if ( pass ) {

        if ( strcmp("generic_fsync_test", argv[1]) == 0 ) {

            tgt_test = generic_fsync_test;

        } else if ( strcmp("generic_aio_fsync_wait_test", argv[1]) == 0 ) {

            tgt_test = generic_aio_fsync_wait_test;

        } else if ( strcmp("generic_aio_fsync_poll_test", argv[1]) == 0 ) {

            tgt_test = generic_aio_fsync_poll_test;

        } else if ( strcmp("multi_fd_fsync_test", argv[1]) == 0 ) {

            tgt_test = multi_fd_fsync_test;

        } else if ( strcmp("multi_fd_aio_fsync_wait_test", argv[1]) == 0 ) {

            tgt_test = multi_fd_aio_fsync_wait_test;

        } else if ( strcmp("multi_fd_aio_fsync_poll_test", argv[1]) == 0 ) {

            tgt_test = multi_fd_aio_fsync_poll_test;

        } else {

            pass = FALSE;
            usage();
        }
    }

    if ( ( pass ) &&
         ( ( tgt_test == multi_fd_fsync_test ) ||
           ( tgt_test == multi_fd_aio_fsync_wait_test ) ||
           ( tgt_test == multi_fd_aio_fsync_poll_test ) ) &&
         ( tgt_driver != multi_fd ) ) {

        pass = FALSE;
        usage();
    }

    if ( verbose ) {

	const char * test_string;
	const char * driver_string;
	const char * op_string;

	switch ( tgt_test ) {

	    case generic_fsync_test:     
                test_string = "generic_fsync_test";     
                break;

	    case generic_aio_fsync_poll_test: 
                test_string = "generic_aio_fsync_poll_test"; 
                break;

	    case generic_aio_fsync_wait_test: 
                test_string = "generic_aio_fsync_wait_test"; 
                break;

	    case multi_fd_fsync_test: 
                test_string = "multi_fd_fsync_test"; 
                break;

            case multi_fd_aio_fsync_poll_test:
                test_string = "multi_fd_aio_fsync_poll_test";
                break;

            case multi_fd_aio_fsync_wait_test:
                test_string = "multi_fd_aio_fsync_wait_test";
                break;

	    default:			 
                test_string = "???";			 
                break;
	}

	switch ( tgt_driver ) {

	    case sec2_fd:	driver_string = "sec2";		break;
	    case core_fd:	driver_string = "core";		break;
	    case stdio_fd:	driver_string = "stdio";	break;
	    case family_fd:	driver_string = "family";	break;
	    case multi_fd:	driver_string = "multi";	break;
	    default:		driver_string = "???";		break;
	}

	switch ( op ) {

	    case setup:		op_string = "setup";		break;
	    case check:		op_string = "check";		break;
	    default:		op_string = "???";		break;
	}

        HDfprintf(stdout, "fsync_tests %s %s %s %lld verbose\n", test_string,
		  driver_string, op_string, (long long)write_size);
    }

    if ( pass ) {

        H5open();

        switch ( tgt_test ) {

	    case generic_fsync_test:

		switch ( tgt_driver ) {

		    case sec2_fd:
			max_addr = 0x40000000;
			file_name_num = 1;
			tag = "SEC2 SIO fsync";
                        fapl = h5_fileaccess();
                        if ( H5Pset_fapl_sec2(fapl) < 0 ) {

                            pass = FALSE;
			    failure_mssg = "H5Pset_fapl_sec2() failed.";

                        }
			break;

		    case core_fd:
			if ( op == setup ) {

			    max_addr = (haddr_t)write_size;
			    file_name_num = 2;
			    tag = "CORE SIO fsync";
			    fapl = h5_fileaccess();
    			    if ( H5Pset_fapl_core(fapl, (size_t)max_addr, TRUE ) < 0 ) {

                                pass = FALSE;
			        failure_mssg = "H5Pset_fapl_core() failed.";

                            }
			} else /* op == check */ {

			    max_addr = 0x40000000;
			    file_name_num = 2;
			    tag = "CORE SIO fsync";
			    fapl = h5_fileaccess();

			    /* At present, the core file driver doesn't allow us to load
			     * and existing file and use that -- hence we use  the stdio 
                             * file driver to check the core file 
                             */
    			    if ( H5Pset_fapl_stdio(fapl) < 0 ) {

                                pass = FALSE;
			        failure_mssg = "H5Pset_fapl_stdio() failed.";

                            }
			}
			break;

		    case stdio_fd:
			max_addr = 0x40000000;
			file_name_num = 3;
			tag = "STDIO SIO fsync";
			fapl = h5_fileaccess();
    			if ( H5Pset_fapl_stdio(fapl) < 0 ) {

                            pass = FALSE;
			    failure_mssg = "H5Pset_fapl_stdio() failed.";

                        }
			break;

		    case family_fd:
			max_addr = 0x40000000;
			file_name_num = 4;
			tag = "Family File SIO fsync";
			fapl = h5_fileaccess();
			if ( H5Pset_fapl_family(fapl, (hsize_t)FAMILY_SIZE_AIO, 
                                                H5P_DEFAULT) < 0 ) {

                            pass = FALSE;
			    failure_mssg = "H5Pset_fapl_family() failed.";

                        }
			break;

		    case multi_fd:
			max_addr = 0x40000000;
			file_name_num = 5;
			tag = "Multi File SIO fsync";

                        for ( mt = 0; mt < H5FD_MEM_NTYPES; mt++ ) {

                            memb_addr[mt] = HADDR_UNDEF;
                            memb_fapl[mt] = H5P_DEFAULT;
                            memb_map[mt]  = H5FD_MEM_SUPER;
                            memb_name[mt] = NULL;
                        }
                        memb_map[H5FD_MEM_DRAW] = H5FD_MEM_DRAW;

                        memb_fapl[H5FD_MEM_SUPER] = H5P_DEFAULT;
                        memb_name[H5FD_MEM_SUPER] = "%s-m.h5";
                        memb_addr[H5FD_MEM_SUPER] = 0;

                        memb_fapl[H5FD_MEM_DRAW] = H5P_DEFAULT;
                        memb_name[H5FD_MEM_DRAW] = "%s-r.h5";
                        memb_addr[H5FD_MEM_DRAW] = HADDR_MAX/2;

                        fapl = h5_fileaccess();

                        if ( H5Pset_fapl_multi(fapl, memb_map, memb_fapl, memb_name,
                                               memb_addr, FALSE) < 0 ) {

                            pass = FALSE;
			    failure_mssg = "H5Pset_fapl_multi() failed.";
                        }
			break;

		    default:
			pass = FALSE;
			failure_mssg = "unknown driver.";
			break;
                }

		if ( pass ) {

		    switch ( op ) {

			case setup:
			    setup_generic_fsync_test(file_name_num, fapl, tag, write_size,
                                                     max_addr, verbose);
			    break;

			case check:
                            check_generic_fsync_test(file_name_num, fapl, tag, write_size,
                                                     max_addr, verbose);
			    break;

			default:
			    pass = FALSE;
			    failure_mssg = "unknown op.";
			    break;
                    }
                }
		break;

	    case generic_aio_fsync_poll_test:

		switch ( tgt_driver ) {

		    case sec2_fd:
			max_addr = 0x40000000;
			file_name_num = 6;
			tag = "SEC2 AIO fsync poll";
                        fapl = h5_fileaccess();
                        if ( H5Pset_fapl_sec2(fapl) < 0 ) {

                            pass = FALSE;
			    failure_mssg = "H5Pset_fapl_sec2() failed.";

                        }
			break;

		    case core_fd:
			if ( op == setup ) {

			    max_addr = (haddr_t)write_size;
			    file_name_num = 7;
			    tag = "CORE AIO fsync poll";
			    fapl = h5_fileaccess();
    			    if ( H5Pset_fapl_core(fapl, (size_t)max_addr, TRUE ) < 0 ) {

                                pass = FALSE;
			        failure_mssg = "H5Pset_fapl_core() failed.";

                            }
			} else /* op == check */ {

			    max_addr = 0x40000000;
			    file_name_num = 7;
			    tag = "CORE AIO fsync poll";
			    fapl = h5_fileaccess();

			    /* At present, the core file driver doesn't allow us to load
			     * and existing file and use that -- hence we use  the stdio 
                             * file driver to check the core file 
                             */
    			    if ( H5Pset_fapl_stdio(fapl) < 0 ) {

                                pass = FALSE;
			        failure_mssg = "H5Pset_fapl_stdio() failed.";

                            }
			}
			break;

		    case stdio_fd:
			max_addr = 0x40000000;
			file_name_num = 8;
			tag = "STDIO AIO fsync poll";
			fapl = h5_fileaccess();
    			if ( H5Pset_fapl_stdio(fapl) < 0 ) {

                            pass = FALSE;
			    failure_mssg = "H5Pset_fapl_stdio() failed.";

                        }
			break;

		    case family_fd:
			max_addr = 0x40000000;
			file_name_num = 9;
			tag = "Family File AIO fsync poll";
			fapl = h5_fileaccess();
			if ( H5Pset_fapl_family(fapl, (hsize_t)FAMILY_SIZE_AIO, 
                                                H5P_DEFAULT) < 0 ) {

                            pass = FALSE;
			    failure_mssg = "H5Pset_fapl_family() failed.";

                        }
			break;

		    case multi_fd:
			max_addr = 0x40000000;
			file_name_num = 10;
			tag = "Multi File AIO fsync poll";

                        for ( mt = 0; mt < H5FD_MEM_NTYPES; mt++ ) {

                            memb_addr[mt] = HADDR_UNDEF;
                            memb_fapl[mt] = H5P_DEFAULT;
                            memb_map[mt]  = H5FD_MEM_SUPER;
                            memb_name[mt] = NULL;
                        }
                        memb_map[H5FD_MEM_DRAW] = H5FD_MEM_DRAW;

                        memb_fapl[H5FD_MEM_SUPER] = H5P_DEFAULT;
                        memb_name[H5FD_MEM_SUPER] = "%s-m.h5";
                        memb_addr[H5FD_MEM_SUPER] = 0;

                        memb_fapl[H5FD_MEM_DRAW] = H5P_DEFAULT;
                        memb_name[H5FD_MEM_DRAW] = "%s-r.h5";
                        memb_addr[H5FD_MEM_DRAW] = HADDR_MAX/2;

                        fapl = h5_fileaccess();

                        if ( H5Pset_fapl_multi(fapl, memb_map, memb_fapl, memb_name,
                                               memb_addr, FALSE) < 0 ) {

                            pass = FALSE;
			    failure_mssg = "H5Pset_fapl_multi() failed.";
                        }
			break;

		    default:
			pass = FALSE;
			failure_mssg = "unknown driver.";
			break;
                }

		if ( pass ) {

		    switch ( op ) {

			case setup:
			    setup_generic_aio_fsync_test(file_name_num, fapl, tag, 
                                                         write_size, max_addr, FALSE,
                                                         verbose);
			    break;

			case check:
                            check_generic_fsync_test(file_name_num, fapl, tag, write_size,
                                                     max_addr, verbose);
			    break;

			default:
			    pass = FALSE;
			    failure_mssg = "unknown op.";
			    break;
                    }
                }
		break;

	    case generic_aio_fsync_wait_test:

		switch ( tgt_driver ) {

		    case sec2_fd:
			max_addr = 0x40000000;
			file_name_num = 11;
			tag = "SEC2 AIO fsync wait";
                        fapl = h5_fileaccess();
                        if ( H5Pset_fapl_sec2(fapl) < 0 ) {

                            pass = FALSE;
			    failure_mssg = "H5Pset_fapl_sec2() failed.";

                        }
			break;

		    case core_fd:
			if ( op == setup ) {

			    max_addr = (haddr_t)write_size;
			    file_name_num = 12;
			    tag = "CORE AIO fsync wait";
			    fapl = h5_fileaccess();
    			    if ( H5Pset_fapl_core(fapl, (size_t)max_addr, TRUE ) < 0 ) {

                                pass = FALSE;
			        failure_mssg = "H5Pset_fapl_core() failed.";

                            }
			} else /* op == check */ {

			    max_addr = 0x40000000;
			    file_name_num = 12;
			    tag = "CORE AIO fsync wait";
			    fapl = h5_fileaccess();

			    /* At present, the core file driver doesn't allow us to load
			     * and existing file and use that -- hence we use  the stdio 
                             * file driver to check the core file 
                             */
    			    if ( H5Pset_fapl_stdio(fapl) < 0 ) {

                                pass = FALSE;
			        failure_mssg = "H5Pset_fapl_stdio() failed.";

                            }
			}
			break;

		    case stdio_fd:
			max_addr = 0x40000000;
			file_name_num = 13;
			tag = "STDIO AIO fsync wait";
			fapl = h5_fileaccess();
    			if ( H5Pset_fapl_stdio(fapl) < 0 ) {

                            pass = FALSE;
			    failure_mssg = "H5Pset_fapl_stdio() failed.";

                        }
			break;

		    case family_fd:
			max_addr = 0x40000000;
			file_name_num = 14;
			tag = "Family File AIO fsync wait";
			fapl = h5_fileaccess();
			if ( H5Pset_fapl_family(fapl, (hsize_t)FAMILY_SIZE_AIO, 
                                                H5P_DEFAULT) < 0 ) {

                            pass = FALSE;
			    failure_mssg = "H5Pset_fapl_family() failed.";

                        }
			break;

		    case multi_fd:
			max_addr = 0x40000000;
			file_name_num = 15;
			tag = "Multi File AIO fsync wait";

                        for ( mt = 0; mt < H5FD_MEM_NTYPES; mt++ ) {

                            memb_addr[mt] = HADDR_UNDEF;
                            memb_fapl[mt] = H5P_DEFAULT;
                            memb_map[mt]  = H5FD_MEM_SUPER;
                            memb_name[mt] = NULL;
                        }
                        memb_map[H5FD_MEM_DRAW] = H5FD_MEM_DRAW;

                        memb_fapl[H5FD_MEM_SUPER] = H5P_DEFAULT;
                        memb_name[H5FD_MEM_SUPER] = "%s-m.h5";
                        memb_addr[H5FD_MEM_SUPER] = 0;

                        memb_fapl[H5FD_MEM_DRAW] = H5P_DEFAULT;
                        memb_name[H5FD_MEM_DRAW] = "%s-r.h5";
                        memb_addr[H5FD_MEM_DRAW] = HADDR_MAX/2;

                        fapl = h5_fileaccess();

                        if ( H5Pset_fapl_multi(fapl, memb_map, memb_fapl, memb_name,
                                               memb_addr, FALSE) < 0 ) {

                            pass = FALSE;
			    failure_mssg = "H5Pset_fapl_multi() failed.";
                        }
			break;

		    default:
			pass = FALSE;
			failure_mssg = "unknown driver.";
			break;
                }

		if ( pass ) {

		    switch ( op ) {

			case setup:
			    setup_generic_aio_fsync_test(file_name_num, fapl, tag, 
                                                         write_size, max_addr, TRUE,
                                                         verbose);
			    break;

			case check:
                            check_generic_fsync_test(file_name_num, fapl, tag, write_size,
                                                     max_addr, verbose);
			    break;

			default:
			    pass = FALSE;
			    failure_mssg = "unknown op.";
			    break;
                    }
                }
		break;

	    case multi_fd_fsync_test: 
		HDassert( tgt_driver == multi_fd );
                file_name_num = 16;
                tag = "targeted multi fd fsync test";
		if ( pass ) {

		    switch ( op ) {

			case setup:
                            setup_multi_file_driver_fsync_test(file_name_num, tag,
                                                               write_size, verbose);
			    break;

			case check:
                            check_multi_file_driver_fsync_test(file_name_num, tag,
                                                               write_size, verbose);
			    break;

			default:
			    pass = FALSE;
			    failure_mssg = "unknown op.";
			    break;
                    }
                }
		break;

	    case multi_fd_aio_fsync_wait_test: 
		HDassert( tgt_driver == multi_fd );
                file_name_num = 17;
                tag = "multi fd aio fsync wait test";
		if ( pass ) {

		    switch ( op ) {

			case setup:
                            setup_multi_file_driver_aio_fsync_test(file_name_num, tag,
                                                                   write_size, TRUE, verbose);
			    break;

			case check:
                            check_multi_file_driver_fsync_test(file_name_num, tag,
                                                               write_size, verbose);
			    break;

			default:
			    pass = FALSE;
			    failure_mssg = "unknown op.";
			    break;
                    }
                }
		break;

	    case multi_fd_aio_fsync_poll_test: 
		HDassert( tgt_driver == multi_fd );
                file_name_num = 18;
                tag = "multi fd aio fsync poll test";
		if ( pass ) {

		    switch ( op ) {

			case setup:
                            setup_multi_file_driver_aio_fsync_test(file_name_num, tag,
                                                                   write_size, FALSE, verbose);
			    break;

			case check:
                            check_multi_file_driver_fsync_test(file_name_num, tag,
                                                               write_size, verbose);
			    break;

			default:
			    pass = FALSE;
			    failure_mssg = "unknown op.";
			    break;
                    }
                }
		break;

            default:
		pass = FALSE;
		failure_mssg = "unknown tgt_test.";
                break;
        }
    }

    if ( verbose ) {

        if ( pass ) {

	    if ( setup ) {

                /* this should be unreachable as, in the event of success,  we 
                 * should have aborted by now.
                 */
	        HDfprintf(stdout, "test setup succeeded.\n");

	    } else if ( check ) {

	        HDfprintf(stdout, "test passed.\n");

            }
	} else {

	    HDfprintf(stdout, "FAILED.  Failure mssg = \"%s\"\n",
		      failure_mssg);
        }
    }

    if ( ! pass ) {

        result = 1;
    }

    if ( result != 0 ) {

        HDfprintf(stderr, "fail\n");
    }

    return(result);

} /* main() */

