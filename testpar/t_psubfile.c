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

#include "testpar.h"

/* File_Access_type bits */
#define FACC_DEFAULT    0x0     /* default */
#define FACC_MPIO       0x1     /* MPIO */
#define FACC_SPLIT      0x2     /* Split File */


/* The collection of files is included below to aid
 * an external "cleanup" process if required.
 *
 * Note that the code below relies on the ordering of this array
 * since each set of three is used by the tests either to construct
 * or to read and validate.
 */
#define NFILENAME 3

const char *FILENAMES[NFILENAME + 1]={"vds_src_0",
                                      "vds_src_1",
                                      "reloc_t_pread_group_1_file",
                                      NULL};
#define FILENAME_BUF_SIZE 1024

#define COUNT 1000

hbool_t pass = true;

/* Set some minimum values to work without actually calling MPI */
int mpi_global_rank = 0;
int mpi_global_size = 1;


/*
 * Create the appropriate File access property list
 */
hid_t
create_faccess_plist(MPI_Comm comm, MPI_Info info, int l_facc_type)
{
    hid_t ret_pl = -1;
    herr_t ret;                 /* generic return value */
    int mpi_rank = mpi_global_rank; /* local mpi variable for error reporting */
    int nerrors = 0;

    ret_pl = H5Pcreate (H5P_FILE_ACCESS);
    VRFY((ret_pl >= 0), "H5P_FILE_ACCESS");

    if (l_facc_type == FACC_DEFAULT)
	return (ret_pl);

    if (l_facc_type == FACC_MPIO){
	/* set Parallel access with communicator */
	ret = H5Pset_fapl_mpio(ret_pl, comm, info);
	VRFY((ret >= 0), "");
        ret = H5Pset_all_coll_metadata_ops(ret_pl, TRUE);
	VRFY((ret >= 0), "");
        ret = H5Pset_coll_metadata_write(ret_pl, TRUE);
	VRFY((ret >= 0), "");
	return(ret_pl);
    }

    if (l_facc_type == (FACC_MPIO | FACC_SPLIT)){
	hid_t mpio_pl;

	mpio_pl = H5Pcreate (H5P_FILE_ACCESS);
	VRFY((mpio_pl >= 0), "");
	/* set Parallel access with communicator */
	ret = H5Pset_fapl_mpio(mpio_pl, comm, info);
	VRFY((ret >= 0), "");

	/* setup file access template */
	ret_pl = H5Pcreate (H5P_FILE_ACCESS);
	VRFY((ret_pl >= 0), "");
	/* set Parallel access with communicator */
	ret = H5Pset_fapl_split(ret_pl, ".meta", mpio_pl, ".raw", mpio_pl);
	VRFY((ret >= 0), "H5Pset_fapl_split succeeded");
	H5Pclose(mpio_pl);
	return(ret_pl);
    }

    /* unknown file access types */
    return (ret_pl);
}



/*-------------------------------------------------------------------------
 * Function:    generate_test_file
 *
 * Purpose:     This function is called to produce an HDF5 data file
 *              whose superblock is relocated to a power-of-2 boundary.
 *
 *              Since data will be read back and validated, we generate
 *              data in a predictable manner rather than randomly.
 *              For now, we simply use the global mpi_rank of the writing
 *              process as a starting component for the data generation.
 *              Subsequent writes are increments from the initial start
 *              value.
 *
 *              In the overall scheme of running the test, we'll call
 *              this function twice: first as a collection of all MPI
 *              processes and then a second time with the processes split
 *              more or less in half. Each sub group will operate
 *              collectively on their assigned file.  This split into
 *              subgroups validates that parallel groups can successfully
 *              open and read data independantly from the other parallel
 *              operations taking place.
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
generate_test_files( MPI_Comm comm, int mpi_global_rank, int group_id )
{
    int header = -1;
    const char *fcn_name = "generate_test_files()";
    const char *failure_mssg = NULL;
    const char *group_filename = NULL;
    H5D_space_status_t space_status; /* Dataset space status */

    char data_filename[FILENAME_BUF_SIZE];
    int file_index = 0;
    int group_size;
    int group_rank;
    int local_failure = 0;
    int global_failures = 0;
    hsize_t count = COUNT;
    hsize_t i;
    hsize_t offset;
    hsize_t dims[1] = {0};
    hid_t file   = -1;
    hid_t memspace  = -1;
    hid_t filespace = -1;
    hid_t fctmpl    = -1;
    hid_t fapl   = -1;
    hid_t dcpl   = -1;
    hid_t dset   = -1;
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

    if ((fapl = create_faccess_plist(comm, MPI_INFO_NULL, FACC_MPIO)) < 0) {
        pass = FALSE;
        failure_mssg = "generate_test_file: create_faccess_plist failed.\n";
    }      

    h5_fixname(FILENAMES[group_id], fapl, data_filename, sizeof data_filename);

    /* Create DCPL */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) {
        pass = FALSE;
        failure_mssg = "generate_test_file: unable to create dcpl.\n";
    }

    /* Create file and dataset */
    if((file = H5Fcreate(data_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) {
        pass = FALSE;
        failure_mssg = "generate_test_file: H5Fcreate() failed.\n";
    }
    if((dset = H5Dcreate2(file, "vdset", H5T_NATIVE_INT, vspace, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0) {
        pass = FALSE;
        failure_mssg = "generate_test_file: H5Dcreate2() failed.\n";
    }

    /* Test H5Dget_space_status */
    if(H5Dget_space_status(dset, &space_status) < 0) {
        pass = FALSE;
        failure_mssg = "generate_test_file: H5D_space_status() failed.\n";
    } 
    if(space_status != H5D_SPACE_STATUS_ALLOCATED) {
        pass = FALSE;
        failure_mssg = "generate_test_file: unexpected space_status.\n";
    }

    if(H5Dclose(dset) < 0) {
        pass = FALSE;
        failure_mssg = "generate_test_file: H5Dclose failed.\n";
    }
    if(H5Fclose(file) < 0) {
        pass = FALSE;
        failure_mssg = "generate_test_file: H5Fclose failed.\n";
    }

    /* collect results from other processes.
     * Only overwrite the failure message if no previous error
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
    if ( mpi_global_rank == 0 ) {
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
 * Function:    generate_vds_container
 *
 * Purpose:     
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
generate_vds_container(MPI_Comm comm, int mpi_rank, int group_id)
{
    const char *failure_mssg;
    const char *group_filename = NULL;
    const char *fcn_name = "generate_vds_container()";

    char reloc_data_filename[FILENAME_BUF_SIZE];
    int local_failure = 0;
    int global_failures = 0;
    int group_size;
    int group_rank;
    hid_t fapl_id   = -1;
    hid_t file_id   = -1;
    hid_t dset_id   = -1;
    hid_t memspace  = -1;
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
        failure_mssg = "generate_vds_container: MPI_Comm_rank failed.\n";
    }

    if ( (MPI_Comm_size(comm, &group_size)) != MPI_SUCCESS) {
        pass = FALSE;
        failure_mssg = "generate_vds_container: MPI_Comm_size failed.\n";
    }

    /* collect results from other processes.
     * Only overwrite the failure message if no previous error
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

} /* generate_vds_container */


/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     To implement a parallel test which emulates the subfiling
 *              feature (to-be-implemented) which in turn utilizes parallel
 *              VDS.
 *
 *              The test normally requires at least four (4) MPI ranks
 *              and consists of creating two seperate VDS datasets
 *              which are subsequently combined into a single VDS.
 *
 * Return:      Success: 0
 *
 *              Failure: 1
 *
 * Programmer:  Richard Warren
 *              07/10/2018
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

int
main( int argc, char **argv)
{
    int nerrs = 0;
    int which_group = 0;
    int split_size;
    int mpi_rank, mpi_size;
    MPI_Comm group_comm = MPI_COMM_NULL;

    if ( (MPI_Init(&argc, &argv)) != MPI_SUCCESS) {
       HDfprintf(stderr, "FATAL: Unable to initialize MPI\n");
       HDexit(EXIT_FAILURE);
    }

    if ( (MPI_Comm_rank(MPI_COMM_WORLD, &mpi_global_rank)) != MPI_SUCCESS) {
        HDfprintf(stderr, "FATAL: MPI_Comm_rank returned an error\n");
        HDexit(EXIT_FAILURE);
    }

    if ( (MPI_Comm_size(MPI_COMM_WORLD, &mpi_global_size)) != MPI_SUCCESS) {
        HDfprintf(stderr, "FATAL: MPI_Comm_size returned an error\n");
        HDexit(EXIT_FAILURE);
    }

    H5open();

    if ((mpi_size = mpi_global_size) < 4 ) {
        if ( mpi_global_rank == 0 ) {
            HDprintf("    Need at least 4 processes.  Exiting.\n");
        }
        goto finish;
    }

    if ((mpi_rank = mpi_global_rank) == 0 ) {
        HDfprintf(stdout, "============================================\n");
        HDfprintf(stdout, "Subfiling functionality (parallel VDS) tests\n");
        HDfprintf(stdout, "        mpi_size     = %d\n", mpi_size);
        HDfprintf(stdout, "============================================\n");
    }


    /* ------  Create two (2) MPI groups  ------
     *
     * We split MPI_COMM_WORLD into 2 more or less equal sized
     * groups.  The resulting communicators will be used to generate
     * two HDF files which in turn will be opened in parallel and the
     * contents verified in the second read test below.
     */
    split_size = mpi_size / 2;
    which_group = (mpi_rank < split_size ? 0 : 1);

    if ( (MPI_Comm_split(MPI_COMM_WORLD,
                         which_group,
                         0,
                         &group_comm)) != MPI_SUCCESS) {

        HDfprintf(stderr, "FATAL: MPI_Comm_split returned an error\n");
        HDexit(EXIT_FAILURE);
    }

    /* ------  Generate all files ------ */
    /* We generate the file used for test 1 */
    nerrs += generate_test_files( group_comm, mpi_rank, which_group );

    if ( nerrs > 0 ) {
        if ( mpi_rank == 0 ) {
            HDprintf("    SubFile construction failed -- skipping tests.\n");
        }
        goto finish;
    }

    /* We generate the file used for test 2 */
    nerrs += generate_vds_container( MPI_COMM_WORLD, mpi_rank, which_group );

    if ( nerrs > 0 ) {
        if ( mpi_rank == 0 ) {
            HDprintf("    VDS file construction failed -- skipping tests.\n");
        }
        goto finish;
    }

#if 0
    /* Now read the generated test file (still using MPI_COMM_WORLD) */
    nerrs += test_parallel_read( MPI_COMM_WORLD, mpi_rank, which_group);

    if ( nerrs > 0 ) {
        if ( mpi_rank == 0 ) {
            HDprintf("    Parallel read test(1) failed -- skipping tests.\n");
        }
        goto finish;
    }

    if ( mpi_rank == 0 ) {
        HDprintf("    Parallel VDS read succeeded\n");
    }
#endif

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
        const char *header = "Subfiling validation tests";

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
    if (H5close() != SUCCEED) {
        HDfprintf(stdout, "H5close() failed. (Ignoring)\n");
    }

    /* MPI_Finalize must be called AFTER H5close which may use MPI calls */
    MPI_Finalize();

    /* cannot just return (nerrs) because exit code is limited to 1byte */
    return((nerrs > 0) ? EXIT_FAILURE : EXIT_SUCCESS );

} /* main() */
