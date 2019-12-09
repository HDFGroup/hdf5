/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Use Case 1.7	Appending a single chunk
 * Description:
 *     Appending a single chunk of raw data to a dataset along an unlimited
 *     dimension within a pre-created file and reading the new data back.
 * Goal:
 *     Read data appended by the Writer to a pre-existing dataset in a
 *     file. The dataset has one or more unlimited dimensions. The data is
 *     appended by a hyperslab that is contained in one chunk (for example,
 *     appending 2-dim planes along the slowest changing dimension in the
 *     3-dim dataset).
 * Level:
 *     User Level
 * Guarantees:
 *     o	Readers will see the modified dimension sizes after the Writer
 * 	finishes HDF5 metadata updates and issues H5Fflush or H5Oflush calls.
 *     o	Readers will see newly appended data after the Writer finishes
 * 	the flush operation.
 * 
 * Preconditions:
 *     o	Readers are not allowed to modify the file.  o	 All datasets
 * 	that are modified by the Writer exist when the Writer opens the file.
 *     o	All datasets that are modified by the Writer exist when a Reader
 * 	opens the file.  o	 Data is written by a hyperslab contained in
 * 	one chunk.
 * 
 * Main Success Scenario:
 *     1.	An application creates a file with required objects (groups,
 * 	datasets, and attributes).
 *     2.	The Writer application opens the file and datasets in the file
 * 	and starts adding data along the unlimited dimension using a hyperslab
 * 	selection that corresponds to an HDF5 chunk.
 *     3.	A Reader opens the file and a dataset in a file, and queries
 * 	the sizes of the dataset; if the extent of the dataset has changed,
 * 	reads the appended data back.
 * 
 * Discussion points:
 *     1.	Since the new data is written to the file, and metadata update
 * 	operation of adding pointer to the newly written chunk is atomic and
 * 	happens after the chunk is on the disk, only two things may happen
 * 	to the Reader:
 * 	    o	The Reader will not see new data.
 * 	    o	The Reader will see all new data written by Writer.
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* Created: Albert Cheng, 2013/5/28 */

#include "h5test.h"

/* This test uses many POSIX things that are not available on
 * Windows. We're using a check for fork(2) here as a proxy for
 * all POSIX/Unix/Linux things until this test can be made
 * more platform-independent.
 */
#ifdef H5_HAVE_FORK

#include "use.h"

/* Global Variable definitions */
options_t UC_opts;	/* Use Case Options */
const char *progname_g="use_append_chunk";	/* program name */

/* Setup parameters for the use case.
 * Return: 0 succeed; -1 fail.
 */
int setup_parameters(int argc, char * const argv[])
{
    /* use case defaults */
    HDmemset(&UC_opts, 0, sizeof(options_t));
    UC_opts.chunksize = Chunksize_DFT;
    UC_opts.use_swmr = TRUE;	/* use swmr open */
    UC_opts.iterations = 1;
    UC_opts.chunkplanes = 1;

    /* parse options */
    if (parse_option(argc, argv) < 0)
	return(-1);

    /* set chunk dims */
    UC_opts.chunkdims[0] = UC_opts.chunkplanes;
    UC_opts.chunkdims[1] = UC_opts.chunkdims[2] = UC_opts.chunksize;

    /* set dataset initial and max dims */
    UC_opts.dims[0] = 0;
    UC_opts.max_dims[0] = H5S_UNLIMITED;
    UC_opts.dims[1] = UC_opts.dims[2] = UC_opts.max_dims[1] = UC_opts.max_dims[2] = UC_opts.chunksize;

    /* set nplanes */
    if (UC_opts.nplanes == 0)
        UC_opts.nplanes = (hsize_t)UC_opts.chunksize;

    /* show parameters and return */
    show_parameters();
    return(0);
}


/* Overall Algorithm: 
 * Parse options from user;
 * Generate/pre-created test files needed and close it;
 * fork: child process becomes the reader process;
 *       while parent process continues as the writer process;
 * both run till ending conditions are met.
 */
int
main(int argc, char *argv[])
{
    pid_t childpid=0;
    pid_t mypid, tmppid;
    int	child_status;
    int child_wait_option=0;
    int ret_value = 0;
    int child_ret_value;
    hbool_t send_wait = FALSE;
    hid_t fapl = -1;    /* File access property list */
    hid_t fid = -1;     /* File ID */
    char *name;         /* Test file name */

    /* initialization */
    if (setup_parameters(argc, argv) < 0){
        Hgoto_error(1);
    }

    /* Determine the need to send/wait message file*/
    if(UC_opts.launch == UC_READWRITE) {
        HDunlink(WRITER_MESSAGE);
        send_wait = TRUE;
    }

    /* ==============================================================*/
    /* UC_READWRITE: create datafile, launch both reader and writer. */
    /* UC_WRITER:    create datafile, skip reader, launch writer.    */
    /* UC_READER:    skip create, launch reader, exit.               */
    /* ==============================================================*/
    /* ============*/
    /* Create file */
    /* ============*/
    if (UC_opts.launch != UC_READER){
        HDprintf("Creating skeleton data file for test...\n");
        if (create_uc_file() < 0){
            HDfprintf(stderr, "***encounter error\n");
            Hgoto_error(1);
        }else
            HDprintf("File created.\n");
    }

    if (UC_opts.launch==UC_READWRITE){
        /* fork process */
        if((childpid = HDfork()) < 0) {
            HDperror("fork");
            Hgoto_error(1);
        };
    };
    mypid = HDgetpid();

    /* ============= */
    /* launch reader */
    /* ============= */
    if (UC_opts.launch != UC_WRITER){
        /* child process launch the reader */
        if(0 == childpid) {
            HDprintf("%d: launch reader process\n", mypid);
            if (read_uc_file(send_wait) < 0){
                HDfprintf(stderr, "read_uc_file encountered error\n");
                HDexit(EXIT_FAILURE);
            }
            HDexit(EXIT_SUCCESS);
        }
    }

    /* ============= */
    /* launch writer */
    /* ============= */
    /* this process continues to launch the writer */
    HDprintf("%d: continue as the writer process\n", mypid);

    name = UC_opts.filename;

    /* Set file access proeprty list */
    if((fapl = h5_fileaccess()) < 0)
        Hgoto_error(1);

    if(UC_opts.use_swmr)
        if(H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
            Hgoto_error(1);

    /* Open the file */
    if((fid = H5Fopen(name, H5F_ACC_RDWR | (UC_opts.use_swmr ? H5F_ACC_SWMR_WRITE : 0), fapl)) < 0) {
        HDfprintf(stderr, "H5Fopen failed\n");
        Hgoto_error(1);
    }

    if(write_uc_file(send_wait, fid) < 0) {
        HDfprintf(stderr, "write_uc_file encountered error\n");
        Hgoto_error(1);
    }

    /* ================================================ */
    /* If readwrite, collect exit code of child process */
    /* ================================================ */
    if (UC_opts.launch == UC_READWRITE){
        if ((tmppid = HDwaitpid(childpid, &child_status, child_wait_option)) < 0){
            HDperror("waitpid");
            Hgoto_error(1);
        }

        /* Close the file */
        if(H5Fclose(fid) < 0) {
            HDfprintf(stderr, "Failed to close file id\n");
            Hgoto_error(1);
        }

        /* Close the property list */
        if(H5Pclose(fapl) < 0) {
            HDfprintf(stderr, "Failed to close the property list\n");
            Hgoto_error(1);
        }

        if (WIFEXITED(child_status)){
            if ((child_ret_value=WEXITSTATUS(child_status)) != 0){
                HDprintf("%d: child process exited with non-zero code (%d)\n",
                        mypid, child_ret_value);
                Hgoto_error(2);
            }
         } else {
                HDprintf("%d: child process terminated abnormally\n", mypid);
                Hgoto_error(2);
         }
    }
    
done:
    /* Print result and exit */
    if (ret_value != 0){
        HDprintf("Error(s) encountered\n");
    }else{
        HDprintf("All passed\n");
    }

    return(ret_value);
}

#else /* H5_HAVE_FORK */

int
main(void)
{
    HDfprintf(stderr, "Non-POSIX platform. Skipping.\n");
    return EXIT_SUCCESS;
} /* end main() */

#endif /* H5_HAVE_FORK */

