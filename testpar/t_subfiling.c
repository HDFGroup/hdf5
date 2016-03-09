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

/*
 * Parallel tests for subfiling feature
 */

#include "testphdf5.h"

/* FILENAME and filenames must have the same number of names.
 * Use PARATESTFILE in general and use a separated filename only if the file
 * created in one test is accessed by a different test.
 * filenames[0] is reserved as the file name for PARATESTFILE.
 */
#define NFILENAME 2
#define PARATESTFILE filenames[0]
const char *FILENAME[NFILENAME]={
	    "SubFileTest",
	    NULL};
char	filenames[NFILENAME][PATH_MAX];

int nerrors = 0;			/* errors count */
hid_t	fapl;				/* file access property list */

#define DATASET1      "Dataset1_subfiled"
#define RANK          2
#define DIMS0         60
#define DIMS1         120

void subfiling_api(void);

void
subfiling_api(void)
{
    hid_t fid;                  /* HDF5 file ID */
    hid_t did;
    hid_t sid;
    hid_t fcpl_id, fapl_id, dcpl_id;	/* Property Lists */

    const char *filename;
    char subfile_name[50];
    hsize_t dims[RANK];   	/* dataset dim sizes */
    hsize_t start[RANK], count[RANK], stride[RANK], block[RANK];

    int mpi_size, mpi_rank;
    MPI_Comm comm = MPI_COMM_WORLD;
    MPI_Info info = MPI_INFO_NULL;

    herr_t ret;         	/* Generic return value */

    filename = (const char *)GetTestParameters();

    /* set up MPI parameters */
    MPI_Comm_size(MPI_COMM_WORLD,&mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD,&mpi_rank);

    /* create file creation and access property lists */
    fcpl_id = H5Pcreate(H5P_FILE_CREATE);
    VRFY((fcpl_id >= 0), "");
    fapl_id = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((fapl_id >= 0), "");

    ret = H5Pset_fapl_mpio(fapl_id, comm, info);
    VRFY((ret == 0), "");

    /* set number of subfiles to be 1 per mpi rank */
    ret = H5Pset_num_subfiles(fcpl_id, (unsigned)mpi_size);
    VRFY((ret == 0), "");

    sprintf(subfile_name, "subfile_%d", mpi_rank);
    /* set number of process groups to be equal to the mpi size */
    ret = H5Pset_subfiling_access(fapl_id, (unsigned)mpi_size, subfile_name, MPI_COMM_SELF, MPI_INFO_NULL);
    VRFY((ret == 0), "");

    fid = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl_id, fapl_id);
    VRFY((fid >= 0), "H5Fcreate succeeded");

    ret = H5Pclose(fapl_id);
    VRFY((ret == 0), "");
    ret = H5Pclose(fcpl_id);
    VRFY((ret == 0), "");

    fapl_id = H5Fget_access_plist(fid);
    VRFY((fapl_id >= 0), "");

    {
        unsigned num_groups;
        MPI_Comm get_comm;
        MPI_Info get_info;
        int comm_size;
        char *temp_name = NULL;

        ret = H5Pget_subfiling_access(fapl_id, &num_groups, &temp_name, &get_comm, &get_info);
        VRFY((ret == 0), "");

        VRFY((num_groups == (unsigned)mpi_size), "number of subfiling groups verified");
        VRFY((strcmp(temp_name, subfile_name) == 0), "Subfile name verification succeeded");

        MPI_Comm_size(get_comm, &comm_size);
        VRFY((comm_size == 1), "subfiling communicator verified");

        MPI_Comm_free(&get_comm);
        HDfree(temp_name);
    }

    fcpl_id = H5Fget_create_plist(fid);
    VRFY((fcpl_id >= 0), "");
    {
        unsigned num_subfiles;

        ret = H5Pget_num_subfiles(fcpl_id, &num_subfiles);
        VRFY((ret == 0), "");

        VRFY((num_subfiles == (unsigned)mpi_size), "number of subfiles verified");
    }

    ret = H5Pclose(fapl_id);
    VRFY((ret == 0), "");
    ret = H5Pclose(fcpl_id);
    VRFY((ret == 0), "");

    dims[0] = DIMS0 * mpi_size;
    dims[1] = DIMS1 * mpi_size;

    /* create the dataspace for the master dataset */
    sid = H5Screate_simple (2, dims, NULL);
    VRFY((sid >= 0), "");

    start[0] = 0;
    start[1] = DIMS1 * mpi_rank;
    block[0] = dims[0];
    block[1] = DIMS1;
    count[0] = 1;
    count[1] = 1;
    stride[0] = 1;
    stride[1] = 1;

    /* set the selection for this dataset that this process will write to */
    ret = H5Sselect_hyperslab(sid, H5S_SELECT_SET, start, stride, count, block);
    VRFY((ret == 0), "H5Sset_hyperslab succeeded");

    /* create the dataset creation property list */
    dcpl_id = H5Pcreate(H5P_DATASET_CREATE);
    VRFY((dcpl_id >= 0), "");

    /* Set the selection that this process will access the dataset with */
    ret = H5Pset_subfiling(dcpl_id, sid);
    VRFY((ret == 0), "");

    /* create the dataset with the subfiling dcpl settings */
    did = H5Dcreate2(fid, DATASET1, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl_id, H5P_DEFAULT);
    VRFY((did >= 0), "");

    {
        hid_t temp_sid;

        ret = H5Pget_subfiling(dcpl_id, &temp_sid);
        VRFY((ret == 0), "");

        VRFY((H5Sget_select_npoints(sid) == H5Sget_select_npoints(temp_sid)), "Subfiling selection verified");
        VRFY((H5Sget_simple_extent_ndims(sid) == H5Sget_simple_extent_ndims(temp_sid)), "Subfiling selection verified");

        ret = H5Sclose(temp_sid);
        VRFY((ret == 0), "");
    }
    ret = H5Pclose(dcpl_id);
    VRFY((ret == 0), "");

    ret = H5Dclose(did);
    VRFY((ret == 0), "");

    ret = H5Fclose(fid);
    VRFY((ret == 0), "");

    MPI_File_delete(subfile_name, MPI_INFO_NULL);
}

int main(int argc, char **argv)
{
    int mpi_size, mpi_rank;				/* mpi variables */

#ifndef H5_HAVE_WIN32_API
    /* Un-buffer the stdout and stderr */
    HDsetbuf(stderr, NULL);
    HDsetbuf(stdout, NULL);
#endif

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

    if (MAINPROCESS){
	printf("===================================\n");
	printf("Sub-filing Tests Start\n");
	printf("===================================\n");
    }

    /* Attempt to turn off atexit post processing so that in case errors
     * happen during the test and the process is aborted, it will not get
     * hang in the atexit post processing in which it may try to make MPI
     * calls.  By then, MPI calls may not work.
     */
    if (H5dont_atexit() < 0){
	printf("%d: Failed to turn off atexit processing. Continue.\n", mpi_rank);
    };
    H5open();
    h5_show_hostname();

    /* Initialize testing framework */
    TestInit(argv[0], NULL, NULL);

    /* compose the test filenames */
    {
	int i, n;

	n = sizeof(FILENAME)/sizeof(FILENAME[0]) - 1;	/* exclude the NULL */

	for (i=0; i < n; i++)
	    if (h5_fixname(FILENAME[i],fapl,filenames[i],sizeof(filenames[i]))
		== NULL){
		printf("h5_fixname failed\n");
		nerrors++;
		return(1);
	    }
	printf("Test filenames are:\n");
	for (i=0; i < n; i++)
	    printf("    %s\n", filenames[i]);
    }

    AddTest("subfiling_api", subfiling_api, NULL, "test subfiling API", PARATESTFILE);

    /* Display testing information */
    TestInfo(argv[0]);

    /* setup file access property list */
    fapl = H5Pcreate (H5P_FILE_ACCESS);
    H5Pset_fapl_mpio(fapl, MPI_COMM_WORLD, MPI_INFO_NULL);

    /* Parse command line arguments */
    TestParseCmdLine(argc, argv);

    /* Perform requested testing */
    PerformTests();

    /* make sure all processes are finished before final report, cleanup
     * and exit.
     */
    MPI_Barrier(MPI_COMM_WORLD);

    /* Display test summary, if requested */
    if (MAINPROCESS && GetTestSummary())
        TestSummary();


    /* Clean up test files */
    h5_clean_files(FILENAME, fapl);

    nerrors += GetTestNumErrs();

    /* Gather errors from all processes */
    {
        int temp;
        MPI_Allreduce(&nerrors, &temp, 1, MPI_INT, MPI_MAX, MPI_COMM_WORLD);
	nerrors=temp;
    }

    MPI_Finalize();
    return(nerrors!=0);
}
