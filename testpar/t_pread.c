/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Collective file open optimization tests
 *
 */


#include "h5test.h"
#include "testpar.h"

#define NFILENAME 3
const char *FILENAMES[NFILENAME + 1]={"t_pread_data_file", 
                                     "reloc_t_pread_data_file", 
                                     "prefix_file", 
                                     NULL};
#define FILENAME_BUF_SIZE 1024

#define COUNT 1000

hbool_t pass = true;
static const char *random_hdf5_text = 
"Now is the time for all first-time-users of HDF5 to read their \
manual or go thru the tutorials!\n\
While you\'re at it, now is also the time to read up on MPI-IO.";

static const char *hitchhiker_quote =
"A common mistake that people make when trying to design something\n\
completely foolproof is to underestimate the ingenuity of complete\n\
fools.\n";

static int generate_test_file(MPI_Comm comm, int mpi_rank, int group);
static int test_parallel_read(MPI_Comm comm, int mpi_rank, int group);


/*-------------------------------------------------------------------------
 * Function:    generate_test_file
 *
 * Purpose:     This function is called to produce an HDF5 data file
 *              whose superblock is relocated to a non-zero offset by
 *              utilizing the 'h5jam' utility to write random text
 *              at the start of the file.  Unlike simple concatenation
 *              of files, h5jam is used to place the superblock on a
 *              power-of-2 boundary.
 *
 *              Since data will be read back and validated, we generate
 *              data in a predictable manner rather than randomly.
 *              For now, we simply use the mpi_rank of the writing
 *              process as a starting component of the data generation.
 *              Subsequent writes are increments from the initial start
 *              value.
 *
 *              In the overall scheme of running the test, we'll call
 *              this function twice so as to create two seperate files.
 *              Each file will serve as the input data for two
 *              independent parallel reads.
 *
 * Return:      Success: 0
 *
 *              Failure: 1
 *
 * Programmer:  Richard Warren
 *              10/1/17
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
generate_test_file( MPI_Comm comm, int mpi_rank, int group_id )
{
    FILE *header;
    const char *fcn_name = "generate_test_file()";
    const char *failure_mssg = NULL;
    char group_file[FILENAME_BUF_SIZE];
    char data_filename[FILENAME_BUF_SIZE];
    char reloc_data_filename[FILENAME_BUF_SIZE];
    char prolog_filename[FILENAME_BUF_SIZE];
    int group_size;
    int group_rank;
    int local_failure = 0;
    int global_failures = 0; 
    hsize_t count = COUNT;
    hsize_t i;
    hsize_t offset;
    hsize_t dims[1] = {0};
    hid_t file_id;
    hid_t memspace; 
    hid_t filespace; 
    hid_t fapl_id;
    hid_t dxpl_id;
    hid_t dset_id;
    float nextValue;
    float *data_slice = NULL;

    pass = true;

    HDassert(comm != MPI_COMM_NULL);

    if ( (MPI_Comm_rank(comm, &group_rank)) != MPI_SUCCESS) {
        pass = FALSE;
        failure_mssg = "generate_test_file: MPI_Comm_rank failed.\n";
    }

    if ( (MPI_Comm_size(comm, &group_size)) != MPI_SUCCESS) {
        pass = FALSE;
        failure_mssg = "generate_test_file: MPI_Comm_size failed.\n";
    }

    if ( mpi_rank == 0 ) {

        HDfprintf(stdout, "Constructing test files...");
    }

    /* setup the file names */
    if ( pass ) {
        HDassert(FILENAMES[0]);
	if ( HDsprintf(group_file, "%s_%d", FILENAMES[0], group_id) < 0) {
            pass = FALSE;
            failure_mssg = "HDsprintf(0) failed.\n";
	}
        else if ( h5_fixname(group_file, H5P_DEFAULT, data_filename,
                        sizeof(data_filename)) == NULL ) {
            pass = FALSE;
            failure_mssg = "h5_fixname(0) failed.\n";
        }
    }

    if ( pass ) {
        HDassert(FILENAMES[1]);
	if ( HDsprintf(group_file, "%s_%d", FILENAMES[1], group_id) < 0) {
            pass = FALSE;
            failure_mssg = "HDsprintf(1) failed.\n";
	}
        else if ( h5_fixname(group_file, H5P_DEFAULT, reloc_data_filename,
                        sizeof(reloc_data_filename)) == NULL ) {

            pass = FALSE;
            failure_mssg = "h5_fixname(1) failed.\n";
        }
    }

    if ( pass ) {
        HDassert(FILENAMES[2]);
	if ( HDsprintf(group_file, "%s_%d", FILENAMES[2], group_id) < 0) {
            pass = FALSE;
            failure_mssg = "HDsprintf(2) failed.\n";
	}
        else if ( h5_fixname(group_file, H5P_DEFAULT, prolog_filename,
                        sizeof(prolog_filename)) == NULL ) {
            pass = FALSE;
            failure_mssg = "h5_fixname(2) failed.\n";
        }
    }

    /* setup data to write */
    if ( pass ) {
        if ( (data_slice = (float *)HDmalloc(COUNT * sizeof(float))) == NULL ) {
            pass = FALSE;
            failure_mssg = "malloc of data_slice failed.\n";
        }
    }

    if ( pass ) {
        nextValue = (float)(mpi_rank * COUNT);

        for(i=0; i<COUNT; i++) {
            data_slice[i] = nextValue;
	    nextValue += 1;
        }
    }

    /* setup FAPL */
    if ( pass ) {
        if ( (fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0 ) {
            pass = FALSE;
            failure_mssg = "H5Pcreate(H5P_FILE_ACCESS) failed.\n";
        }
    }

    if ( pass ) {
        if ( (H5Pset_fapl_mpio(fapl_id, comm, MPI_INFO_NULL)) < 0 ) {
            pass = FALSE;
            failure_mssg = "H5Pset_fapl_mpio() failed\n";
        }
    }

    /* create the data file */ 
    if ( pass ) {
        if ( (file_id = H5Fcreate(data_filename, H5F_ACC_TRUNC, 
                                  H5P_DEFAULT, fapl_id)) < 0 ) {
            pass = FALSE;
            failure_mssg = "H5Fcreate() failed.\n";
        }
    }

    /* create and write the dataset */
    if ( pass ) {
        if ( (dxpl_id = H5Pcreate(H5P_DATASET_XFER)) < 0 ) {
            pass = FALSE;
            failure_mssg = "H5Pcreate(H5P_DATASET_XFER) failed.\n";
        }
    }

    if ( pass ) {
        if ( (H5Pset_dxpl_mpio(dxpl_id, H5FD_MPIO_COLLECTIVE)) < 0 ) {
            pass = FALSE;
            failure_mssg = "H5Pset_dxpl_mpio() failed.\n";
        }
    }

    if ( pass ) {
        dims[0] = COUNT;
        if ( (memspace = H5Screate_simple(1, dims, NULL)) < 0 ) {
            pass = FALSE;
            failure_mssg = "H5Screate_simple(1, dims, NULL) failed (1).\n";
        }
    }

    if ( pass ) {
        dims[0] *= (hsize_t)group_size;
        if ( (filespace = H5Screate_simple(1, dims, NULL)) < 0 ) {
            pass = FALSE;
            failure_mssg = "H5Screate_simple(1, dims, NULL) failed (2).\n";
        }
    }

    if ( pass ) {
        offset = (hsize_t)group_rank * (hsize_t)COUNT;
        if ( (H5Sselect_hyperslab(filespace, H5S_SELECT_SET, &offset, 
                                  NULL, &count, NULL)) < 0 ) {
            pass = FALSE;
            failure_mssg = "H5Sselect_hyperslab() failed.\n";
        }
    }

    if ( pass ) {
        if ( (dset_id = H5Dcreate2(file_id, "dataset0", H5T_NATIVE_FLOAT, 
                                   filespace, H5P_DEFAULT, H5P_DEFAULT, 
                                   H5P_DEFAULT)) < 0 ) {
            pass = false;
            failure_mssg = "H5Dcreate2() failed.\n";
        }
    }

    if ( pass ) {
        if ( (H5Dwrite(dset_id, H5T_NATIVE_FLOAT, memspace, 
                       filespace, dxpl_id, data_slice)) < 0 ) {
            pass = false;
            failure_mssg = "H5Dwrite() failed.\n";
        }
    }

    /* close file, etc. */
    if ( pass ) {
        if ( H5Dclose(dset_id) < 0 ) {
            pass = false;
            failure_mssg = "H5Dclose(dset_id) failed.\n";
        }
    }

    if ( pass ) {
        if ( H5Sclose(memspace) < 0 ) {
            pass = false;
            failure_mssg = "H5Sclose(memspace) failed.\n";
        }
    }

    if ( pass ) {
        if ( H5Sclose(filespace) < 0 ) {
            pass = false;
            failure_mssg = "H5Sclose(filespace) failed.\n";
        }
    }

    if ( pass ) {
        if ( H5Fclose(file_id) < 0 ) {
            pass = false;
            failure_mssg = "H5Fclose(file_id) failed.\n";
        }
    }

    if ( pass ) {
        if ( H5Pclose(dxpl_id) < 0 ) {
            pass = false;
            failure_mssg = "H5Pclose(dxpl_id) failed.\n";
        }
    }

    if ( pass ) {
        if ( H5Pclose(fapl_id) < 0 ) {
            pass = false;
            failure_mssg = "H5Pclose(fapl_id) failed.\n";
        }
    }

    /* add a userblock to the head of the datafile.
     * We will use this to for a functional test of the 
     * file open optimization.
     *
     * Also delete files that are no longer needed.
     */
    if ( group_rank == 0 ) {

        const char *text_to_write;
        size_t bytes_to_write; 

        if (group_id == 0)
            text_to_write = random_hdf5_text;
	else
            text_to_write = hitchhiker_quote;

	bytes_to_write = strlen(text_to_write);

        if ( pass ) {
            if ( (header = HDfopen(prolog_filename, "w+")) == NULL ) {
                pass = FALSE;
                failure_mssg = "HDfopen(prolog_filename, \"w+\") failed.\n";
            }
        }

        if ( pass ) {

            if ( HDfwrite(text_to_write, 1, bytes_to_write, header) !=
                 bytes_to_write ) {
                pass = FALSE;
                failure_mssg = "Unable to write header file.\n";
            }
        }

        if ( pass ) {
            if ( HDfclose(header) != 0 ) {
                pass = FALSE;
                failure_mssg = "HDfclose() failed.\n";
            }
        }

        if ( pass ) {
            char cmd[256];

            HDsprintf(cmd, "../tools/src/h5jam/h5jam -i %s -u %s -o %s", 
                      data_filename, prolog_filename, reloc_data_filename);

	    if ( system(cmd) != 0 ) {
                pass = FALSE;
                failure_mssg = "invocation of h5jam failed.\n";
            }
        }

        if ( pass ) {
            HDremove(prolog_filename);
            HDremove(data_filename);
        }
    }

    /* collect results from other processes.
     * Only overwrite the failure message if no preveious error 
     * has been detected
     */
    local_failure = ( pass ? 0 : 1 );

    /* This is a global all reduce (NOT group specific) */
    if ( MPI_Allreduce(&local_failure, &global_failures, 1, 
                       MPI_INT, MPI_SUM, MPI_COMM_WORLD) != MPI_SUCCESS ) {
        if ( pass ) {
            pass = FALSE;
            failure_mssg = "MPI_Allreduce() failed.\n";
        }
    } else if ( ( pass ) && ( global_failures > 0 ) ) {
        pass = FALSE;
        failure_mssg = "One or more processes report failure.\n";
    }

    /* report results */
    if ( mpi_rank == 0 ) {
        if ( pass ) {
            HDfprintf(stdout, "Done.\n");
        } else {
            HDfprintf(stdout, "FAILED.\n");
            HDfprintf(stdout, "%s: failure_mssg = \"%s\"\n",
                      fcn_name, failure_mssg);
        }
    }

    /* free data_slice if it has been allocated */
    if ( data_slice != NULL ) {
        HDfree(data_slice);
        data_slice = NULL;
    }

    return(! pass);

} /* generate_test_file() */


/*-------------------------------------------------------------------------
 * Function:    test_parallel_read
 *
 * Purpose:     This actually tests the superblock optimization
 *              and covers the two primary cases we're interested in.
 *              1). That HDF5 files can be opened in parallel by
 *                  the rank 0 process and that the superblock
 *                  offset is correctly broadcast to the other
 *                  parallel file readers.
 *              2). That a parallel application can correctly
 *                  handle reading multiple files by using
 *                  subgroups of MPI_COMM_WORLD and that each
 *                  subgroup operates as described in (1) to
 *                  collectively read the data.
 *
 *              The global MPI rank is used for reading and
 *              writing data for process specific data in the
 *              dataset.  We do this rather simplisticly, i.e.
 *               rank 0:  writes/reads 0-9999
 *               rank 1:  writes/reads 1000-1999
 *               rank 2:  writes/reads 2000-2999
 *               ...
 *
 * Return:      Success: 0
 *
 *              Failure: 1
 *
 * Programmer:  Richard Warren
 *              10/1/17
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_parallel_read(MPI_Comm comm, int mpi_rank, int group_id)
{
    const char *failure_mssg;
    const char *fcn_name = "test_parallel_read()";
    char group_file[FILENAME_BUF_SIZE];
    char reloc_data_filename[FILENAME_BUF_SIZE];
    int local_failure = 0;
    int global_failures = 0; 
    int group_size;
    int group_rank;
    hid_t fapl_id;
    hid_t file_id;
    hid_t dset_id;
    hid_t memspace = -1;
    hid_t filespace = -1;
    hsize_t i;
    hsize_t offset;
    hsize_t count = COUNT;
    hsize_t dims[1] = {0};
    float nextValue;
    float *data_slice = NULL;

    pass = TRUE;

    HDassert(comm != MPI_COMM_NULL);

    if ( (MPI_Comm_rank(comm, &group_rank)) != MPI_SUCCESS) {
        pass = FALSE;
        failure_mssg = "test_parallel_read: MPI_Comm_rank failed.\n";
    }

    if ( (MPI_Comm_size(comm, &group_size)) != MPI_SUCCESS) {
        pass = FALSE;
        failure_mssg = "test_parallel_read: MPI_Comm_size failed.\n";
    }

    if ( mpi_rank == 0 ) {

        TESTING("parallel file open test 1");
    }

    /* allocate space for the data_slice array */
    if ( pass ) {
        if ( (data_slice = (float *)HDmalloc(COUNT * sizeof(float))) == NULL ) {
            pass = FALSE;
            failure_mssg = "malloc of data_slice failed.\n";
        }
    }


    /* construct file file name */
    if ( pass ) {
        HDassert(FILENAMES[1]);
	if ( HDsprintf(group_file, "%s_%d", FILENAMES[1], group_id) < 0) {
            pass = FALSE;
            failure_mssg = "HDsprintf(0) failed.\n";
        }
        else if ( h5_fixname(group_file, H5P_DEFAULT, reloc_data_filename,
                        sizeof(reloc_data_filename)) == NULL ) {

            pass = FALSE;
            failure_mssg = "h5_fixname(1) failed.\n";
        }
    }

    /* setup FAPL */
    if ( pass ) {
        if ( (fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0 ) {
            pass = FALSE;
            failure_mssg = "H5Pcreate(H5P_FILE_ACCESS) failed.\n";
        }
    }

    if ( pass ) {
        if ( (H5Pset_fapl_mpio(fapl_id, comm, MPI_INFO_NULL)) < 0 ) {
            pass = FALSE;
            failure_mssg = "H5Pset_fapl_mpio() failed\n";
        }
    }

    /* open the file -- should have user block, exercising the optimization */
    if ( pass ) {
        if ( (file_id = H5Fopen(reloc_data_filename, 
                                H5F_ACC_RDONLY, fapl_id)) < 0 ) {
            pass = FALSE;
            failure_mssg = "H5Fopen() failed\n";
        }
    }

    /* open the data set */
    if ( pass ) {
        if ( (dset_id = H5Dopen2(file_id, "dataset0", H5P_DEFAULT)) < 0 ) {
            pass = FALSE;
            failure_mssg = "H5Dopen2() failed\n";
        }
    }

    /* setup memspace */
    if ( pass ) {
        dims[0] = count;
        if ( (memspace = H5Screate_simple(1, dims, NULL)) < 0 ) {
            pass = FALSE;
            failure_mssg = "H5Screate_simple(1, dims, NULL) failed\n";
        }
    }

    /* setup filespace */
    if ( pass ) {
        if ( (filespace = H5Dget_space(dset_id)) < 0 ) {
            pass = FALSE;
            failure_mssg = "H5Dget_space(dataset) failed\n";
        }
    }

    if ( pass ) {
        offset = (hsize_t)group_rank * count;
        if ( (H5Sselect_hyperslab(filespace, H5S_SELECT_SET, 
                                  &offset, NULL, &count, NULL)) < 0 ) {
            pass = FALSE;
            failure_mssg = "H5Sselect_hyperslab() failed\n";
        }
    }

    /* read this processes section of the data */
    if ( pass ) {
        if ( (H5Dread(dset_id, H5T_NATIVE_FLOAT, memspace, 
                      filespace, H5P_DEFAULT, data_slice)) < 0 ) {
            pass = FALSE;
            failure_mssg = "H5Dread() failed\n";
        }
    }
    
    /* verify the data */ 
    if ( pass ) {
        nextValue = (float)((hsize_t)mpi_rank * count);
        i = 0;
        while ( ( pass ) && ( i < count ) ) {
            /* what we really want is data_slice[i] != nextValue --
             * the following is a circumlocution to shut up the
             * the compiler.
             */
            if ( ( data_slice[i] > nextValue ) ||
                 ( data_slice[i] < nextValue ) ) {
                pass = FALSE;
                failure_mssg = "Unexpected dset contents.\n";
            }
            nextValue += 1;
            i++;
        }
    }

    /* close file, etc. */
    if ( pass ) {
        if ( H5Dclose(dset_id) < 0 ) {
            pass = false;
            failure_mssg = "H5Dclose(dset_id) failed.\n";
        }
    }

    if ( pass ) {
        if ( H5Sclose(memspace) < 0 ) {
            pass = false;
            failure_mssg = "H5Sclose(memspace) failed.\n";
        }
    }

    if ( pass ) {
        if ( H5Sclose(filespace) < 0 ) {
            pass = false;
            failure_mssg = "H5Sclose(filespace) failed.\n";
        }
    }

    if ( pass ) {
        if ( H5Fclose(file_id) < 0 ) {
            pass = false;
            failure_mssg = "H5Fclose(file_id) failed.\n";
        }
    }

    if ( pass ) {
        if ( H5Pclose(fapl_id) < 0 ) {
            pass = false;
            failure_mssg = "H5Pclose(fapl_id) failed.\n";
        }
    }

    /* collect results from other processes.
     * Only overwrite the failure message if no preveious error 
     * has been detected
     */
    local_failure = ( pass ? 0 : 1 );

    if ( MPI_Allreduce( &local_failure, &global_failures, 1, 
                        MPI_INT, MPI_SUM, MPI_COMM_WORLD) != MPI_SUCCESS ) {
        if ( pass ) {
            pass = FALSE;
            failure_mssg = "MPI_Allreduce() failed.\n";
        }
    } else if ( ( pass ) && ( global_failures > 0 ) ) {
        pass = FALSE;
        failure_mssg = "One or more processes report failure.\n";
    }

    /* report results and finish cleanup */
    if ( group_rank == 0 ) {
        if ( pass ) {
            PASSED();
        } else {
            H5_FAILED();
            HDfprintf(stdout, "%s: failure_mssg = \"%s\"\n",
                      fcn_name, failure_mssg);
        }

        HDremove(reloc_data_filename);
    }

    /* free data_slice if it has been allocated */
    if ( data_slice != NULL ) {
        HDfree(data_slice);
        data_slice = NULL;
    }


    return( ! pass );     

} /* test_parallel_read() */


/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     To implement a parallel test which validates whether the
 *              new superblock lookup functionality is working correctly.
 *
 *              The test consists of creating two seperate HDF datasets
 *              in which random text is inserted at the start of each
 *              file using the 'j5jam' application.  This forces the
 *              HDF5 file superblock to a non-zero offset.
 *              Having created the two independant files, we create two
 *              non-overlapping MPI groups, each of which is then tasked
 *              with the opening and validation of the data contained
 *              therein.
 *
 *              WARNING: This test uses fork() and execve(), and
 *                       therefore will not run on Windows.
 *
 * Return:      Success: 0
 *
 *              Failure: 1
 *
 * Programmer:  Richard Warren
 *              10/1/17
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

int
main( int argc, char **argv)
{
    int nerrs = 0;
    int which_group;
    int mpi_rank;
    int mpi_size;
    int split_size;
    MPI_Comm group_comm = MPI_COMM_NULL;

    if ( (MPI_Init(&argc, &argv)) != MPI_SUCCESS) {
       HDfprintf(stderr, "FATAL: Unable to initialize MPI\n");
       exit(1);
    }

    if ( (MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank)) != MPI_SUCCESS) {
        HDfprintf(stderr, "FATAL: MPI_Comm_rank returned an error\n");
        exit(2);
    }

    if ( (MPI_Comm_size(MPI_COMM_WORLD, &mpi_size)) != MPI_SUCCESS) {
        HDfprintf(stderr, "FATAL: MPI_Comm_size returned an error\n");
        exit(2);
    }

    H5open();

    if ( mpi_rank == 0 ) {
        HDfprintf(stdout, "========================================\n");
        HDfprintf(stdout, "Collective file open optimization tests\n");
        HDfprintf(stdout, "        mpi_size     = %d\n", mpi_size);
        HDfprintf(stdout, "========================================\n");
    }

    if ( mpi_size < 4 ) {

        if ( mpi_rank == 0 ) {

            HDprintf("    Need at least 4 processes.  Exiting.\n");
        }
        goto finish;
    }

    /* Divide the available processes into two groups
     * that are the same size (plus or minus).
     */
    split_size = mpi_size / 2;
    which_group = (mpi_rank < split_size ? 0 : 1);

    if ( (MPI_Comm_split(MPI_COMM_WORLD,
			 which_group,
			 0,
			 &group_comm)) != MPI_SUCCESS) {

        HDfprintf(stderr, "FATAL: MPI_Comm_split returned an error\n");
	exit(2);
    }


    /* create the test files & verify that the process 
     * succeeded.  If not, abort the remaining tests as 
     * they depend on the test files.
     */

    nerrs += generate_test_file( group_comm, mpi_rank, which_group );

    /* abort tests if there were any errors in test file construction */
    if ( nerrs > 0 ) {
        if ( mpi_rank == 0 ) {
            HDprintf("    Test file construction failed -- skipping tests.\n");
        }
        goto finish;
    }

    /* run the tests */
    nerrs += test_parallel_read(group_comm, mpi_rank, which_group);

finish:

    if ((group_comm != MPI_COMM_NULL) &&
	(MPI_Comm_free(&group_comm)) != MPI_SUCCESS) {
        HDfprintf(stderr, "MPI_Comm_free failed!\n");
    }


    /* make sure all processes are finished before final report, cleanup
     * and exit.
     */
    MPI_Barrier(MPI_COMM_WORLD);

    if ( mpi_rank == 0 ) {           /* only process 0 reports */
        const char *header = "Collective file open optimization tests";

        HDfprintf(stdout, "===================================\n");
        if ( nerrs > 0 ) {
            
            HDfprintf(stdout, "***%s detected %d failures***\n", header, nerrs);
        }
        else {
            HDfprintf(stdout, "%s finished with no failures\n", header);
        }
        HDfprintf(stdout, "===================================\n");
    }

    /* close HDF5 library */
    H5close();

    /* MPI_Finalize must be called AFTER H5close which may use MPI calls */
    MPI_Finalize();

    /* cannot just return (nerrs) because exit code is limited to 1byte */
    return(nerrs > 0);

} /* main() */
