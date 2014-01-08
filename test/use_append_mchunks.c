/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
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

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Use Case 1.8 Appending a hyperslab of multiple chunks.
 * Description:
 *     Appending a hyperslab that spans several chunks of a dataset with
 *     unlimited dimensions within a pre-created file and reading the new
 *     data back.
 * Goal:
 *     Read data appended by the Writer to a pre-existing dataset in a
 *     file. The dataset has one or more unlimited dimensions. The data
 *     is appended by a hyperslab that is contained in several chunks (for
 *     example, appending 2-dim planes along the slowest changing dimension
 *     in the 3-dim dataset and each plane is covered by 4 chunks).
 * Level:
 *     User Level
 * Guarantees:
 *   o Readers will see the modified dimension sizes after the Writer
 *     finishes HDF5 metadata updates and issues H5Fflush or H5Oflush calls.
 *   o Readers will see newly appended data after the Writer finishes
 *     the flush operation.
 * 
 * Preconditions:
 *   o Readers are not allowed to modify the file.
 *   o All datasets that are modified by the Writer exist when the
 *     Writer opens the file.
 *   o All datasets that are modified by the Writer exist when a Reader
 *     opens the file.
 * 
 * Main Success Scenario:
 *  1. An application creates a file with required objects (groups,
 *     datasets, and attributes).
 *  2. The Writer opens the file and datasets in the file and starts
 *     adding data using H5Dwrite call with a hyperslab selection that
 *     spans several chunks.
 *  3. A Reader opens the file and a dataset in a file; if the size of
 *     the unlimited dimension has changed, reads the appended data back.
 * 
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* Created: Albert Cheng, 2013/6/1.
 * Modified:
 */

#include "use.h"

/* Global Variable definitions */
options_t UC_opts;	/* Use Case Options */
const char *progname_g="use_append_mchunks";	/* program name */

/* Setup parameters for the use case.
 * Return: 0 succeed; -1 fail.
 */
int setup_parameters(int argc, char * const argv[])
{
    /* use case defaults */
    HDmemset(&UC_opts, 0, sizeof(options_t));
    UC_opts.chunksize = Chunksize_DFT;
    UC_opts.use_swmr = 1;	/* use swmr open */
    UC_opts.iterations = 1;
    UC_opts.chunkplanes = 1;

    /* parse options */
    if (parse_option(argc, argv) < 0){
	return(-1);
    }
    /* set chunk dims */
    UC_opts.chunkdims[0] = UC_opts.chunkplanes;
    UC_opts.chunkdims[1]=UC_opts.chunkdims[2]=UC_opts.chunksize;

    /* set dataset initial and max dims */
    UC_opts.dims[0] = 0;
    UC_opts.max_dims[0] = H5S_UNLIMITED;
    UC_opts.dims[1] = UC_opts.dims[2] = UC_opts.max_dims[1]=UC_opts.max_dims[2]=2*UC_opts.chunksize;

    /* set nplanes */
    if (UC_opts.nplanes == 0)
        UC_opts.nplanes = 2*UC_opts.chunksize;

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
    hbool_t send_wait = 0;

    /* initialization */
    if (setup_parameters(argc, argv) < 0){
	Hgoto_error(1);
    }

    /* Determine the need to send/wait message file*/
    if(UC_opts.launch == UC_READWRITE) {
        HDunlink(WRITER_MESSAGE);
        send_wait = 1;
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
	printf("Creating skeleton data file for test...\n");
	if (create_uc_file() < 0){
	    fprintf(stderr, "***encounter error\n");
	    Hgoto_error(1);
	}else
	    printf("File created.\n");
    }

    if (UC_opts.launch==UC_READWRITE){
	/* fork process */
	if((childpid = fork()) < 0) {
	    perror("fork");
	    Hgoto_error(1);
	};
    };
    mypid = getpid();

    /* ============= */
    /* launch reader */
    /* ============= */
    if (UC_opts.launch != UC_WRITER){
	/* child process launch the reader */
	if(0 == childpid) {
	    printf("%d: launch reader process\n", mypid);
	    if (read_uc_file(send_wait) < 0){
		fprintf(stderr, "read_uc_file encountered error\n");
		exit(1);
	    }
	    exit(0);
	}
    }

    /* ============= */
    /* launch writer */
    /* ============= */
    /* this process continues to launch the writer */
    printf("%d: continue as the writer process\n", mypid);
    if (write_uc_file(send_wait) < 0){
	fprintf(stderr, "write_uc_file encountered error\n");
	Hgoto_error(1);
    }

    /* ================================================ */
    /* If readwrite, collect exit code of child process */
    /* ================================================ */
    if (UC_opts.launch == UC_READWRITE){
	if ((tmppid = waitpid(childpid, &child_status, child_wait_option)) < 0){
	    perror("waitpid");
	    Hgoto_error(1);
	}
	if (WIFEXITED(child_status)){
	    if ((child_ret_value=WEXITSTATUS(child_status)) != 0){
		printf("%d: child process exited with non-zero code (%d)\n",
		    mypid, child_ret_value);
		Hgoto_error(2);
	    }
	} else {
	    printf("%d: child process terminated abnormally\n", mypid);
	    Hgoto_error(2);
	}
    }
    
done:
    /* Print result and exit */
    if (ret_value != 0){
	printf("Error(s) encountered\n");
    }else{
	printf("All passed\n");
    }

    return(ret_value);
}
