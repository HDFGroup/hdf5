
/*
 * Example of using the parallel HDF5 library to access datasets.
 *
 * This program contains two parts.  In the first part, the mpi processes
 * collectively create a new parallel HDF5 file and create two fixed
 * dimension datasets in it.  Then each process writes a hyperslab into
 * each dataset in an independent mode.  All processes collectively
 * close the datasets and the file.
 * In the second part, the processes collectively open the created file
 * and the two datasets in it.  Then each process reads a hyperslab from
 * each dataset in an independent mode and prints them out.
 * All processes collectively close the datasets and the file.
 */

#include <assert.h>
#include <hdf5.h>
#include <mpi.h>
#include <mpio.h>

/* Temporary source code */
#include <phdf5sup.c>
/* temporary code end */

/* Constants definitions */
char    *filenames[]={
#ifdef HAVE_PARALLEL
"ParaEg1.h5f", "ParaEg2.h5f"
#else
"Eg1.h5f", "Eg2.h5f"
#endif
};


/* 24 is a multiple of 2, 3, 4, 6, 8, 12.  Neat for parallel tests. */
#define SPACE1_DIM1	8
#define SPACE1_DIM2	12
#define SPACE1_RANK	2
#define DATASETNAME1	"Data1"
#define DATASETNAME2	"Data2"
#define DATASETNAME3	"Data3"

#define FAIL -1

/* Example of using the parallel HDF5 library to create a dataset */
void
phdf5write()
{
    hid_t fid1, fid2;		/* HDF5 file IDs */
    hid_t acc_tpl1;		/* File access templates */
    hid_t sid1,sid2;   		/* Dataspace ID */
    hid_t file_dataspace;	/* File dataspace ID */
    hid_t mem_dataspace;	/* memory dataspace ID */
    hid_t dataset1, dataset2;	/* Dataset ID */
    int rank = SPACE1_RANK; 	/* Logical rank of dataspace */
    size_t dims1[SPACE1_RANK] = {SPACE1_DIM1,SPACE1_DIM2};   	/* dataspace dim sizes */
    int data_array1[SPACE1_DIM1][SPACE1_DIM2];	/* data buffer */

    int   start[SPACE1_RANK];				/* for hyperslab setting */
    size_t count[SPACE1_RANK], stride[SPACE1_RANK];	/* for hyperslab setting */

    herr_t ret;         	/* Generic return value */
    int   i, j;
    int numprocs, myid;
    char *fname;
    int	color = 0;			/* used for MPI_Comm_split */
    int mrc;			/* mpi return code */
    
#ifdef HAVE_PARALLEL
    MPI_Comm comm = MPI_COMM_WORLD;
    MPI_Info info = MPI_INFO_NULL;

    /* set up MPI parameters */
    MPI_Comm_size(MPI_COMM_WORLD,&numprocs);
    MPI_Comm_rank(MPI_COMM_WORLD,&myid);
#else
    numprocs = 1;
    myid = 0;
#endif

#ifdef NO
    /* split into two new communicators, one contains the originally */
    /* odd rank processes, the other the even ones. */
    color = myid%2;
    mrc = MPI_Comm_split (MPI_COMM_WORLD, color, myid, &comm);
    assert(mrc==MPI_SUCCESS);
#endif

    /* setup file access template */
    acc_tpl1 = H5Pcreate (H5P_FILE_ACCESS);
    assert(acc_tpl1 != FAIL);
    MESG("H5Pcreate access succeed");
#ifdef HAVE_PARALLEL
    /* set Independent Parallel access with communicator */
    ret = H5Pset_mpi(acc_tpl1, comm, info, H5ACC_INDEPENDENT);     
    assert(ret != FAIL);
    MESG("H5Pset_mpi succeed");
#endif

    /* create the file collectively */
    fid1=H5Fcreate(filenames[color],H5F_ACC_TRUNC,H5P_DEFAULT,acc_tpl1);
    assert(fid1 != FAIL);
    MESG("H5Fcreate succeed");

    /* Release file-access template */
    ret=H5Mclose(acc_tpl1);
    assert(ret != FAIL);


    /* setup dimensionality object */
    sid1 = H5Screate_simple (SPACE1_RANK, dims1, NULL);
    assert (sid1 != FAIL);
    MESG("H5Screate_simple succeed");

    
    /* create a dataset collectively */
    dataset1 = H5Dcreate(fid1, DATASETNAME1, H5T_NATIVE_INT, sid1,
			H5P_DEFAULT);
    assert(dataset1 != FAIL);
    MESG("H5Dcreate succeed");

    /* create another dataset collectively */
    dataset2 = H5Dcreate(fid1, DATASETNAME2, H5T_NATIVE_INT, sid1,
			H5P_DEFAULT);
    assert(dataset2 != FAIL);
    MESG("H5Dcreate succeed");



    /* set up dimensions of the slab this process accesses */
    start[0] = myid*SPACE1_DIM1/numprocs;
    start[1] = 0;
    count[0] = SPACE1_DIM1/numprocs;
    count[1] = SPACE1_DIM2;
    stride[0] = 1;
    stride[1] =1;
printf("start[]=(%d,%d), count[]=(%lu,%lu), total datapoints=%lu\n",
start[0], start[1], count[0], count[1], count[0]*count[1]);

    /* put some trivial data in the data_array */
    for (i=0; i < count[0]; i++){
	for (j=0; j < count[1]; j++){
	    data_array1[i][j] = (i+start[0])*100 + (j+1);
	}
    }
    MESG("data_array initialized");

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset1);				    
    assert(file_dataspace != FAIL);					    
    MESG("H5Dget_space succeed");
    ret=H5Sset_hyperslab(file_dataspace, start, count, stride); 
    assert(ret != FAIL);
    MESG("H5Sset_hyperslab succeed");

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (SPACE1_RANK, count, NULL);
    assert (mem_dataspace != FAIL);

    /* write data independently */
    ret = H5Dwrite(dataset1, H5T_NATIVE_INT, mem_dataspace, file_dataspace,	    
	    H5P_DEFAULT, data_array1);					    
    assert(ret != FAIL);
    MESG("H5Dwrite succeed");

    /* write data independently */
    ret = H5Dwrite(dataset2, H5T_NATIVE_INT, mem_dataspace, file_dataspace,	    
	    H5P_DEFAULT, data_array1);					    
    assert(ret != FAIL);
    MESG("H5Dwrite succeed");

    /* release dataspace ID */
    H5Sclose(file_dataspace);

    /* close dataset collectively */					    
    ret=H5Dclose(dataset1);
    assert(ret != FAIL);
    MESG("H5Dclose1 succeed");
    ret=H5Dclose(dataset2);
    assert(ret != FAIL);
    MESG("H5Dclose2 succeed");

    /* release all IDs created */
    H5Sclose(sid1);

    /* close the file collectively */					    
    H5Fclose(fid1);							    
}

/* Example of using the parallel HDF5 library to read a dataset */
void
phdf5read()
{
    hid_t fid1, fid2;		/* HDF5 file IDs */
    hid_t acc_tpl1;		/* File access templates */
    hid_t sid1,sid2;   		/* Dataspace ID */
    hid_t file_dataspace;	/* File dataspace ID */
    hid_t mem_dataspace;	/* memory dataspace ID */
    hid_t dataset1, dataset2;	/* Dataset ID */
    int rank = SPACE1_RANK; 	/* Logical rank of dataspace */
    size_t dims1[] = {SPACE1_DIM1,SPACE1_DIM2};   	/* dataspace dim sizes */
    int data_array1[SPACE1_DIM1][SPACE1_DIM2];	/* data buffer */

    int   start[SPACE1_RANK];				/* for hyperslab setting */
    size_t count[SPACE1_RANK], stride[SPACE1_RANK];	/* for hyperslab setting */

    herr_t ret;         	/* Generic return value */
    int   i, j;
    int numprocs, myid;
#ifdef HAVE_PARALLEL
    MPI_Comm comm = MPI_COMM_WORLD;
    MPI_Info info = MPI_INFO_NULL;

    /* set up MPI parameters */
    MPI_Comm_size(MPI_COMM_WORLD,&numprocs);
    MPI_Comm_rank(MPI_COMM_WORLD,&myid);
#else
    numprocs = 1;
    myid = 0;
#endif


    /* setup file access template */
    acc_tpl1 = H5Pcreate (H5P_FILE_ACCESS);
    assert(acc_tpl1 != FAIL);
#ifdef HAVE_PARALLEL
    /* set Independent Parallel access with communicator */
    ret = H5Pset_mpi(acc_tpl1, comm, info, H5ACC_INDEPENDENT);     
    assert(ret != FAIL);
#endif


    /* open the file collectively */
    fid1=H5Fopen(filenames[0],H5F_ACC_RDWR,acc_tpl1);
    assert(fid1 != FAIL);

    /* Release file-access template */
    ret=H5Mclose(acc_tpl1);
    assert(ret != FAIL);

    /* open the dataset1 collectively */
    dataset1 = H5Dopen(fid1, DATASETNAME1);
    assert(dataset1 != FAIL);

    /* open another dataset collectively */
    dataset2 = H5Dopen(fid1, DATASETNAME1);
    assert(dataset2 != FAIL);


    /* set up dimensions of the slab this process accesses */
    start[0] = myid*SPACE1_DIM1/numprocs;
    start[1] = 0;
    count[0] = SPACE1_DIM1/numprocs;
    count[1] = SPACE1_DIM2;
    stride[0] = 1;
    stride[1] =1;
printf("start[]=(%d,%d), count[]=(%lu,%lu), total datapoints=%lu\n",
start[0], start[1], count[0], count[1], count[0]*count[1]);

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset1);
    assert(file_dataspace != FAIL);
    ret=H5Sset_hyperslab(file_dataspace, start, count, stride); 
    assert(ret != FAIL);

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (SPACE1_RANK, count, NULL);
    assert (mem_dataspace != FAIL);

    

    /* read data independently */
    ret = H5Dread(dataset1, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    H5P_DEFAULT, data_array1);
    assert(ret != FAIL);

    /* print the slab read */
    for (i=0; i < count[0]; i++){
	printf("Row %d: ", i+start[0]);
	for (j=0; j < count[1]; j++){
	    printf("%d ", data_array1[i][j]);
	}
	printf("\n");
    }

    /* read data independently */
    ret = H5Dread(dataset2, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    H5P_DEFAULT, data_array1);
    assert(ret != FAIL);

    /* print the slab read */
    for (i=0; i < count[0]; i++){
	printf("Row %d: ", i+start[0]);
	for (j=0; j < count[1]; j++){
	    printf("%d ", data_array1[i][j]);
	}
	printf("\n");
    }

    /* close dataset collectively */
    ret=H5Dclose(dataset1);
    assert(ret != FAIL);
    ret=H5Dclose(dataset2);
    assert(ret != FAIL);

    /* release all IDs created */
    H5Sclose(file_dataspace);

    /* close the file collectively */
    H5Fclose(fid1);
}

#ifdef HAVE_PARALLEL
/*
 * test file access by communicator besides COMM_WORLD.
 * Split COMM_WORLD into two, one (even_comm) contains the original
 * processes of even ranks.  The other (odd_comm) contains the original
 * processes of odd ranks.  Processes in even_comm creates a file, then
 * cloose it, using even_comm.  Processes in old_comm just do a barrier
 * using odd_comm.  Then they all do a barrier using COMM_WORLD.
 * If the file creation and cloose does not do correct collective action
 * according to the communicator argument, the processes will freeze up
 * sooner or later due to barrier mixed up.
 */
void
test_split_comm_access()
{
    int numprocs, myrank;
    MPI_Comm comm;
    MPI_Info info = MPI_INFO_NULL;
    int color, mrc;
    int newrank, newprocs;
    hid_t fid;			/* file IDs */
    hid_t acc_tpl;		/* File access properties */
    herr_t ret;			/* generic return value */

    /* set up MPI parameters */
    MPI_Comm_size(MPI_COMM_WORLD,&numprocs);
    MPI_Comm_rank(MPI_COMM_WORLD,&myrank);
    color = myrank%2;
    mrc = MPI_Comm_split (MPI_COMM_WORLD, color, myrank, &comm);
    assert(mrc==MPI_SUCCESS);
    MPI_Comm_size(comm,&newprocs);
    MPI_Comm_rank(comm,&newrank);
    printf("oldrank/oldprocs=%d/%d, newrank/newprocs=%d/%d\n",
	myrank, numprocs, newrank, newprocs);

    if (color){
	/* odd-rank processes */
	mrc = MPI_Barrier(comm);
	assert(mrc==MPI_SUCCESS);
    }else{
	/* even-rank processes */
	/* setup file access template */
	acc_tpl = H5Pcreate (H5P_FILE_ACCESS);
	assert(acc_tpl != FAIL);
	
	/* set Independent Parallel access with communicator */
	ret = H5Pset_mpi(acc_tpl, comm, info, H5ACC_INDEPENDENT);     
	assert(ret != FAIL);

	printf("filenames[%d]=%s\n", color, filenames[color]);
	/* create the file collectively */
	fid=H5Fcreate(filenames[color],H5F_ACC_TRUNC,H5P_DEFAULT,acc_tpl);
	assert(fid != FAIL);
	MESG("H5Fcreate succeed");

	/* Release file-access template */
	ret=H5Pclose(acc_tpl);
	assert(ret != FAIL);

	ret=H5Fclose(fid);
	assert(ret != FAIL);
    }
    if (myrank == 0){
	mrc = MPI_File_delete(filenames[color], info);
	assert(mrc==MPI_SUCCESS);
    }
}
#endif

void
usage()
{
    printf("Usage: testphdf5 [-r] [-w]\n");
    printf("\t-r\b\bno read\n");
    printf("\t-w\b\bno write\n");
    printf("\tdefault do write then read\n");
    printf("\n");
}


    
main(int argc, char **argv)
{
    int numprocs, myid, namelen;
    char processor_name[MPI_MAX_PROCESSOR_NAME];
    int doread=1;			/* read test */
    int dowrite=1;			/* write test */

    void usage();
#ifdef HAVE_PARALLEL
    MPI_Init(&argc,&argv);
    MPI_Comm_size(MPI_COMM_WORLD,&numprocs);
    MPI_Comm_rank(MPI_COMM_WORLD,&myid);
    MPI_Get_processor_name(processor_name,&namelen);
pause_proc(MPI_COMM_WORLD, myid, processor_name, namelen, argc, argv);
#endif

    /* parse option */
    while (--argc){
	if (**(++argv) != '-'){
	    break;
	}else{
	    switch(*(*argv+1)){
		case 'r':   doread = 0; break;
		case 'w':   dowrite = 0; break;
		default: usage(); break;
	    }
	}
    }


    if (dowrite){
#ifdef HAVE_PARALLEL
	test_split_comm_access();
#endif
	MPI_BANNER("testing PHDF5 writing dataset ...");
	phdf5write();
    }
    if (doread){
	MPI_BANNER("testing PHDF5 reading dataset ...");
	phdf5read();
    }

    if (!(dowrite || doread))
	usage();
    else
	MPI_BANNER("PHDF5 tests finished");

#ifdef HAVE_PARALLEL
    MPI_Finalize();
#endif

    return(0);
}

