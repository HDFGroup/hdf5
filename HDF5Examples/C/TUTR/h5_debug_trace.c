/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* This example demonstrates debug trace output.
 *
 * Debug/trace/performance output is not tested as a regular part of our
 * testing so this program gives a quick check that it's all working.
 *
 * Preconditions:
 *
 * You need to set an environment variable named HDF5_DEBUG to have a value
 * of "+all trace ttimes".  In the bash shell, you'd use:
 *
 *      export HDF5_DEBUG="+all trace ttimes"
 *
 * When you are done with this test program, you can set the variable back
 * to "-all" to suppress trace output.
 *
 * Usage:
 *
 * Compile and run the test program, then inspect the output.  You should see
 * trace information for each HDF5 function call that increase over time.
 * Each time stamp is in seconds and designated with an '@' sign.  The
 * elapsed time for the function call is given in seconds in the [dt= ]
 * part.
 *
 * You will also get summary output for the shuffle filter performance and
 * data type conversion performance.  These will include the elapsed time
 * (always) and the system and user times (if available on your system).  On
 * fast machines, these numbers may be 0.0.  Adjust the loop variables in
 * the program as needed to generate reasonable output.
 */

#include <stdio.h>
#include <stdlib.h>

#include "hdf5.h"

#define BUF_SIZE 1048576
#define N_LOOPS  64

#define TESTFILE "h5_debug_trace_out.h5"

int
main(int argc, char **argv)
{
    int  i, j;
    int *data;

    hid_t fid;
    hid_t pid;
    hid_t did;
    hid_t sid;

    hsize_t dims[1]        = {BUF_SIZE};
    hsize_t chunk_sizes[1] = {1024};

    herr_t err;

    /*************************************************************************/

    /* Warn the user about trace deluge to come */

    printf("Testing debug/trace/performance data generation\n");
    printf("\n");
    printf("This test should generate a large amount of trace data\n");
    printf("\n");
    printf("*** BEGIN TRACE OUTPUT ***\n");
    printf("\n");
    fflush(stdout);

    /* This will emit H5Tconvert() performance information */

    for (i = 0; i < N_LOOPS; i++) {

        /* The buffer has to be large enough to hold the conversion output */
        data = (int *)malloc(BUF_SIZE * sizeof(double));

        for (j = 0; j < BUF_SIZE; j++) {
            data[j] = j;
        }

        err = H5Tconvert(H5T_NATIVE_INT, H5T_NATIVE_DOUBLE, BUF_SIZE, data, NULL, H5P_DEFAULT);

        if (err < 0) {
            fprintf(stderr, "ERROR: Conversion failed\n");
            free(data);
            return err;
        }

        free(data);
    }

    /* This will emit H5Z performance information */

    data = (int *)malloc(BUF_SIZE * sizeof(int));

    for (i = 0; i < BUF_SIZE; i++) {
        data[i] = i;
    }

    fid = H5Fcreate(TESTFILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    pid = H5Pcreate(H5P_DATASET_CREATE);
    err = H5Pset_chunk(pid, 1, chunk_sizes);
    err = H5Pset_shuffle(pid);

    sid = H5Screate_simple(1, dims, dims);
    did = H5Dcreate2(fid, "somedata", H5T_NATIVE_INT, sid, H5P_DEFAULT, pid, H5P_DEFAULT);
    err = H5Dwrite(did, H5T_NATIVE_INT, sid, sid, H5P_DEFAULT, data);

    H5Sclose(sid);
    H5Dclose(did);
    H5Pclose(pid);
    H5Fclose(fid);

    free(data);

    /* Finished */
    fflush(stdout);
    printf("\n");
    printf("*** END TRACE OUTPUT ***\n");
    printf("\n");

    remove(TESTFILE);

    return 0;
}
